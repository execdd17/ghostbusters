import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math._

object PersistentStunDB {
  val busterIdToLastStun = mutable.Map.empty[Int, Int]
  val coolDownPeriod = 20

  def canBusterStun(buster: Buster): Boolean = {
    if (busterIdToLastStun.keySet.contains(buster.id)) {
      busterIdToLastStun(buster.id) == 0
    } else {
      true
    }
  }

  def updateForStun(buster: Buster): Unit = {
    busterIdToLastStun(buster.id) = coolDownPeriod
  }

  def updateAfterRound(): Unit = busterIdToLastStun.keySet.foreach(key =>
    busterIdToLastStun(key) = max(busterIdToLastStun(key) - 1, 0)
  )
}

object PersistentGhostCatalog {
  val ghostIdToGhost = mutable.Map.empty[Int, Ghost]

  def updateGhost(ghost: Ghost): Unit = ghostIdToGhost.put(ghost.id, ghost)

  def updateAfterRound(): Unit = ghostIdToGhost.keySet.foreach { ghostId =>
    val staleGhost = ghostIdToGhost(ghostId)

    ghostIdToGhost(ghostId) = new Ghost(
      id = staleGhost.id,
      name = staleGhost.name,
      point = staleGhost.point,
      numTiedBusters = staleGhost.numTiedBusters,
      seenThisRound = false,
      heldByBusterId = staleGhost.heldByBusterId
    )
  }

  def annotateCapture(ghostId: Int, buster: Buster): Unit = {
    if (ghostIdToGhost.get(ghostId).isEmpty) {    // happens when we catch an enemy buster with a capture
      ghostIdToGhost(ghostId) = new Ghost(
        id = ghostId,
        name = "N/A",
        point = buster.point,
        numTiedBusters = 0,
        seenThisRound = true,
        heldByBusterId = Some(buster.id)
      )
    } else {                                      // happens when we see a ghost
      val staleGhost = ghostIdToGhost(ghostId)

      ghostIdToGhost(ghostId) = new Ghost(
        id = staleGhost.id,
        name = staleGhost.name,
        point = staleGhost.point,
        numTiedBusters = staleGhost.numTiedBusters,
        seenThisRound = true,
        heldByBusterId = Some(buster.id)
      )
    }
  }

  def getGhostHeldByBuster(buster: Buster): Option[Ghost] =
    ghostIdToGhost.values.find(ghost => ghost.heldByBusterId == Some(buster.id))

  def annotateSuccessfulCapture(ghostId: Int): Unit = ghostIdToGhost -= ghostId
  def removeStaleGhost(ghost: Ghost): Unit = ghostIdToGhost -= ghost.id
  def getFreshGhosts: List[Ghost] = ghostIdToGhost.values.filter(_.seenThisRound == true).toList
  def getStaleGhosts: List[Ghost] = ghostIdToGhost.values.filter(_.seenThisRound == false).toList
}

object PathCatalog {
  def getPathsForGame(bustersPerTeam: Int, startingPoint: Point): List[Path] = {
    bustersPerTeam match {
      case 2 =>
        List(new ZigZagPath(startingPoint), new InvertedZigZagPath(startingPoint))
      case 3 =>
        List(
          new BorderPath(startingPoint),
          new TrianglePath(startingPoint),
          new InvertedTrianglePath(startingPoint)
        )
      case 4 =>
        List(
          new BorderPath(startingPoint),
          new InvertedBorderPath(startingPoint),
          new TrianglePath(startingPoint),
          new InvertedTrianglePath(startingPoint)
        )
    }
  }
}

/**
  * Send your busters out into the fog to trap ghosts and bring them home!
  **/
object Player extends App {
  val bustersperplayer = readInt // the amount of busters you control
  val ghostcount = readInt // the amount of ghosts on the map
  val myteamid = readInt // if this is 0, your base is on the top left of the map, if it is one, on the bottom right
  val enemyTeamId = if (myteamid == 0) 1 else 0
  val basePoint = if (myteamid == 0) Point(0,0) else Point(16000, 9000)
  Console.err.println(s"bustersperplayer:$bustersperplayer ghostcount:$ghostcount myteamid:$myteamid")

  val paths = PathCatalog.getPathsForGame(bustersperplayer, basePoint)
  val offset = if (myteamid == 0) 0 else bustersperplayer

  val immutableMap: Map[Int, Buster] = (0 until bustersperplayer).toList.map { i =>
    val buster = new Buster(i + offset, Buster.NAMES(i + offset), Point(0,0), paths(i))
    (buster.id, buster)
  }.toMap

  val busterIdToBuster = mutable.Map(immutableMap.toSeq: _*)

  // game loop
  while(true) {
    val entities = readInt // the number of busters and ghosts visible to you
    val enemyBusters = ListBuffer.empty[Buster]

    System.err.println(PersistentGhostCatalog.ghostIdToGhost)

    for(i <- 0 until entities) {
      // entityid: buster id or ghost id
      // y: position of this buster / ghost
      // entitytype: the team id if it is a buster, -1 if it is a ghost.
      // state: For busters: 0=idle, 1=carrying a ghost.
      // value: For busters: Ghost id being carried. For ghosts: number of busters attempting to trap this ghost.
      val Array(entityid, x, y, entitytype, state, value) = for(i <- readLine split " ") yield i.toInt
      Console.err.println(s"entityid:$entityid x:$x y:$y, entitytype:$entitytype state:$state value:$value")

      if (entitytype == myteamid) {   // friendly buster
        updateBusterLocation(entityid, Point(x, y))

        if (state == 1)               // buster carrying a ghost
          PersistentGhostCatalog.annotateCapture(value, busterIdToBuster(entityid))

      } else if (entitytype == -1) {  // a ghost
        PersistentGhostCatalog.updateGhost(
          new Ghost(entityid, Ghost.NAMES(entityid), Point(x, y), value, seenThisRound = true, heldByBusterId = None)
        )
      } else {                        // enemy buster
        val enemyBuster = new Buster(entityid, "Huge Loser", Point(x, y), null)
        enemyBusters += enemyBuster

        if (state == 1)               // buster carrying a ghost
          PersistentGhostCatalog.annotateCapture(value, enemyBuster)
      }
    }

    val coldCaseGhosts = PersistentGhostCatalog.getStaleGhosts.to[ListBuffer]

    for (buster <- busterIdToBuster.values.toList.sortBy(buster => buster.id)) {
      val maybeGhostInPossesion = PersistentGhostCatalog.getGhostHeldByBuster(buster)
      val maybeEnemyBusterToStun = enemyBusters.find(enemy => buster.isAbleToStun(enemy))

      val maybeGhostToBust = PersistentGhostCatalog.getFreshGhosts.find(ghost =>
        ghost.heldByBusterId.isEmpty && buster.isAbleToBust(ghost)
      )

      val maybeGhostToChase = PersistentGhostCatalog.getFreshGhosts.find(ghost =>
        ghost.heldByBusterId.isEmpty && buster.shouldPursueGhost(ghost)
      )

      if (maybeEnemyBusterToStun.nonEmpty) {
        val enemy = maybeEnemyBusterToStun.get
        buster.stun(enemy)
        Console.out.println(s"STUN ${enemy.id} ${buster.name}:Stunning!")
      } else if (maybeGhostInPossesion.nonEmpty) {
        if (buster.point.x == basePoint.x && buster.point.y == basePoint.y) {
          val ghostId = maybeGhostInPossesion.get.id
          val ghostName = Ghost.NAMES(ghostId)
          Console.out.println(s"RELEASE ${buster.name}:Releasing $ghostName")
          PersistentGhostCatalog.annotateSuccessfulCapture(ghostId)
          Console.err.println(s"Removing ghost $ghostId from Catalog")
        } else {
          Console.out.println(s"MOVE ${basePoint.x} ${basePoint.y} ${buster.name}:Moving to Base")
        }
      } else if (maybeGhostToBust.nonEmpty) {
          PersistentGhostCatalog.annotateCapture(maybeGhostToBust.get.id, buster)
          Console.out.println(s"BUST ${maybeGhostToBust.get.id} ${buster.name}:Busting")
      } else if (maybeGhostToChase.nonEmpty) {
        val destination = Point(maybeGhostToChase.get.point.x, maybeGhostToChase.get.point.y)
        Console.out.println(s"MOVE ${destination.x} ${destination.y} ${buster.name}:Chasing ${maybeGhostToChase.get.name}")
      } else if (coldCaseGhosts.nonEmpty) {
        val ghostsThatAreNotHere = PersistentGhostCatalog.getStaleGhosts.to[ListBuffer].filter(ghost =>
          ghost.point.x == buster.point.x && ghost.point.y == buster.point.y
        )

        if (ghostsThatAreNotHere.nonEmpty) {
          ghostsThatAreNotHere.foreach(ghost => PersistentGhostCatalog.removeStaleGhost(ghost))
          Console.out.println(resumePath(buster))
        } else {
          val coldCaseGhost = coldCaseGhosts.remove(0)
          val destination = Point(coldCaseGhost.point.x, coldCaseGhost.point.y)
          Console.out.println(s"MOVE ${destination.x} ${destination.y} ${buster.name}:Cold Case ${coldCaseGhost.name}")
        }
      } else {
        Console.out.println(resumePath(buster))
      }
    }

    PersistentStunDB.updateAfterRound()
    PersistentGhostCatalog.updateAfterRound()
  }

  def resumePath(buster: Buster): String = {
    buster.getPathPoint match {
      case Some(path) => s"MOVE ${path.x} ${path.y} ${buster.name}:Pathing"
      case None =>
        Console.err.println("NO PATH LEFT; Defaulting to BorderPath")
        val updatedBuster = new Buster(buster.id, buster.name, buster.point, new BorderPath(basePoint))
        busterIdToBuster(buster.id) = updatedBuster
        s"MOVE ${updatedBuster.getPathPoint.get.x} ${updatedBuster.getPathPoint.get.y} ${updatedBuster.name}:BORDER PATH"
    }
  }

  def updateBusterLocation(busterId: Int, point: Point): Unit = {
    val entry:Buster = busterIdToBuster(busterId)
    busterIdToBuster.put(busterId, new Buster(busterId, entry.name, point, entry.path))
  }
}

object Path {
  def increasePointsOnLine(pointsToProcess: List[Point], iterations: Int): List[Point] = {
    (1 to iterations).foldLeft(pointsToProcess) { (points, i) =>
      lineMaker(pointsToProcess = points, List.empty[List[Point]])
    }
  }

  def lineMaker(pointsToProcess: List[Point], lines: List[List[Point]]): List[Point] = {
    if (pointsToProcess.isEmpty || pointsToProcess.size == 1)
      return lines.flatten.distinct

    val point1 = pointsToProcess.head
    val point2 = pointsToProcess(1)

    val augmentedLine = enhanceWithMidpoint(List(point1, point2))
    lineMaker(pointsToProcess.tail, lines ::: List(augmentedLine))
  }

  def enhanceWithMidpoint(line: List[Point]): List[Point] = {
    if (line.size != 2)
      throw new IllegalArgumentException("Your path must have 2 points!")

    val point1 =  line.head
    val point2 =  line(1)
    var newPointX: Int = 0
    var newPointY: Int = 0

    if (point2.x < point1.x)
      newPointX = point2.x + (abs(point1.x - point2.x) / 2)
    else
      newPointX = point1.x + (abs(point1.x - point2.x) / 2)

    if (point2.y < point1.y)
      newPointY = point2.y + (abs(point1.y - point2.y) / 2)
    else
      newPointY = point1.y + (abs(point1.y - point2.y) / 2)

    List(point1, Point(newPointX, newPointY), point2)
  }
}

abstract class Path(startingLocation: Point) {
  private var currentPointIndex = 0
  private val points: List[Point] = startingLocation match {
    case Point(0,0) => Path.increasePointsOnLine(getPoints, iterations = 1)
    case _ => Path.increasePointsOnLine(getPoints.reverse, iterations = 1)
  }

  Console.err.println(s"Path $points")

  def getPoints: List[Point]
  def isLooped: Boolean

  def getCurrentPointInPath: Point = points(currentPointIndex)

  def getNextPointAndAdvance: Option[Point] = {
    if (currentPointIndex == points.size - 1 && isLooped) {
      currentPointIndex = 0
      Some(points(currentPointIndex))
    } else if (currentPointIndex == points.size - 1 && !isLooped) {
      None
    } else {
      currentPointIndex += 1
      Some(points(currentPointIndex))
    }
  }
}

class ZigZagPath(startingLocation: Point) extends Path(startingLocation) {
  override def getPoints: List[Point] =
    List(Point(2000, 2000), Point(4000,7000), Point(8000,2000), Point(12000, 7000), Point(14000, 2000))

  override def isLooped: Boolean = false
}

class InvertedZigZagPath(startingLocation: Point) extends Path(startingLocation) {
  override def getPoints: List[Point] =
    List(Point(7000, 2000), Point(4000,2000), Point(8000,7000), Point(12000, 2000), Point(14000, 7000))

  override def isLooped: Boolean = false
}

class BorderPath(startingLocation: Point) extends Path(startingLocation) {
  override def getPoints: List[Point] =
    List(Point(2000, 2000), Point(14000,2000), Point(14000,7000), Point(2000, 7000))

  override def isLooped: Boolean = true
}

class InvertedBorderPath(startingLocation: Point) extends Path(startingLocation) {
  override def getPoints: List[Point] =
    List(Point(2000, 2000), Point(2000, 7000), Point(14000,7000), Point(14000, 2000))

  override def isLooped: Boolean = true
}

class TrianglePath(startingLocation: Point) extends Path(startingLocation) {
  override def getPoints: List[Point] =
    List(Point(2000, 7000), Point(8000,2000), Point(14000,7000))

  override def isLooped: Boolean = true
}

class InvertedTrianglePath(startingLocation: Point) extends Path(startingLocation) {
  override def getPoints: List[Point] =
    List(Point(2000, 2000), Point(8000,7000), Point(14000,2000))

  override def isLooped: Boolean = true
}

case class Point(x: Int, y: Int)
abstract class Agent(val id: Int, val name: String, val point: Point)

object Ghost {
  val NAMES = List("Stephanie", "Erick", "Vimesh", "Jeff") ::: (5 to 30).toList.map(i => s"Ghost $i")
}

sealed class Ghost(
    id: Int,
    name: String,
    point: Point,
    val numTiedBusters: Int,
    val seenThisRound: Boolean,
    val heldByBusterId: Option[Int])
  extends Agent(id, name, point)


object Buster {
  val NAMES = List("Alex", "Eric", "Thonny", "Brian", "Dennis", "Justin", "Nathan", "Drew", "Bill",
    "Ian", "Lisa", "Jenn")
}

sealed class Buster(
    id: Int,
    name: String,
    point: Point,
    val path: Path)
  extends Agent(id, name, point) {

  def stun(other: Buster): Unit = {
    if (!isAbleToStun(other))
      throw new IllegalStateException("This buster can not stun!")

    PersistentStunDB.updateForStun(this)
  }

  def isAbleToStun(other: Buster): Boolean = {
    val distance = distanceBetween(other)
    distance <= 1760 && PersistentStunDB.canBusterStun(this)
  }

  def isAbleToBust(ghost: Ghost): Boolean = {
    val distance = distanceBetween(ghost)
    distance <= 1760 && distance >= 900 && ghost.heldByBusterId.isEmpty
  }

  def shouldPursueGhost(ghost: Ghost): Boolean = {
    val distance = distanceBetween(ghost)
    distance <= 2200 && distance > 1760
  }

  def getPathPoint: Option[Point] = {
    if (point.x == path.getCurrentPointInPath.x && point.y == path.getCurrentPointInPath.y) {
      path.getNextPointAndAdvance
    } else {
      Some(path.getCurrentPointInPath)
    }
  }

  private def distanceBetween(agent: Agent): Double = {
    val distance = sqrt(
      pow(point.x.toDouble - agent.point.x.toDouble, 2) +
        pow(point.y.toDouble - agent.point.y.toDouble, 2)
    )

    abs(distance)
  }
}
