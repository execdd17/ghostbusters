package codinggame

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math._
import scala.util._

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
  val ghostToPoint = mutable.Map.empty[Int, Ghost]

  def updateGhost(ghost: Ghost): Unit = ghostToPoint.put(ghost.id, ghost)

  def updateAfterRound(): Unit = ghostToPoint.keySet.foreach { ghostId =>
    val staleGhost = ghostToPoint(ghostId)

    ghostToPoint(ghostId) = new Ghost(
      id = staleGhost.id,
      name = staleGhost.name,
      point = staleGhost.point,
      numTiedBusters = staleGhost.numTiedBusters,
      seenThisRound = false,
      heldByGhostId = staleGhost.heldByGhostId
    )
  }

  def annotateCapture(ghostId: Int, busterId: Int): Unit = {
    val staleGhost = ghostToPoint(ghostId)

    ghostToPoint(ghostId) = new Ghost(
      id = staleGhost.id,
      name = staleGhost.name,
      point = staleGhost.point,
      numTiedBusters = staleGhost.numTiedBusters,
      seenThisRound = true,
      heldByGhostId = Some(busterId)
    )
  }

  def getGhostHeldByBuster(buster: Buster): Option[Ghost] =
    ghostToPoint.values.find(ghost => ghost.heldByGhostId == Some(buster.id))

  def annotateSuccessfulCapture(ghostId: Int): Unit = ghostToPoint -= ghostId
  def getFreshGhosts: List[Ghost] = ghostToPoint.values.filter(_.seenThisRound == true).toList
  def getStaleGhosts: List[Ghost] = ghostToPoint.values.filter(_.seenThisRound == false).toList

}

/**
  * Send your busters out into the fog to trap ghosts and bring them home!
  **/
object Player extends App {
  val bustersperplayer = readInt // the amount of busters you control
  val ghostcount = readInt // the amount of ghosts on the map
  val myteamid = readInt // if this is 0, your base is on the top left of the map, if it is one, on the bottom right
  val enemyTeamId = if (myteamid == 0) 1 else 0
  Console.err.println(s"bustersperplayer:$bustersperplayer ghostcount:$ghostcount myteamid:$myteamid")

  val partitioner = new BoardPartitioner(16001, 9001)
  val zones = partitioner.partition(bustersperplayer)
  Console.err.println(s"Zones are $zones")

  val basePoint = if (myteamid == 0) Point(0,0) else Point(16000, 9000)

  // game loop
  while(true) {
    val entities = readInt // the number of busters and ghosts visible to you
    val busters = ListBuffer.empty[Buster]
    val enemyBusters = ListBuffer.empty[Buster]
    var zoneCounter = 0

    System.err.println(PersistentGhostCatalog.ghostToPoint)

    for(i <- 0 until entities) {
      // entityid: buster id or ghost id
      // y: position of this buster / ghost
      // entitytype: the team id if it is a buster, -1 if it is a ghost.
      // state: For busters: 0=idle, 1=carrying a ghost.
      // value: For busters: Ghost id being carried. For ghosts: number of busters attempting to trap this ghost.
      val Array(entityid, x, y, entitytype, state, value) = for(i <- readLine split " ") yield i.toInt
      Console.err.println(s"entityid:$entityid x:$x y:$y, entitytype:$entitytype state:$state value:$value")

      if (entitytype == myteamid) {   // friendly buster
        busters += new Buster(entityid, Buster.NAMES(entityid), Point(x, y), zones(zoneCounter))

        if (state == 1)               // buster carrying a ghost
          PersistentGhostCatalog.annotateCapture(value, entityid)

      } else if (entitytype == -1) {  // a ghost
        PersistentGhostCatalog.updateGhost(
          new Ghost(entityid, Ghost.NAMES(entityid), Point(x, y), value, seenThisRound = true, heldByGhostId = None)
        )
      } else {                        // enemy buster
        if (state == 1)               // buster carrying a ghost
          PersistentGhostCatalog.annotateCapture(value, entityid)

        enemyBusters += new Buster(entityid, "Huge Loser", Point(x, y), null)
      }

      zoneCounter += 1
    }

    val rand = new Random()

    for (buster <- busters) {
      val maybeGhostInPossesion = PersistentGhostCatalog.getGhostHeldByBuster(buster)
      val maybeEnemyBusterToStun = enemyBusters.find(enemy => buster.isAbleToStun(enemy))
      val maybeGhostToBust = PersistentGhostCatalog.getFreshGhosts.find(ghost => buster.isAbleToBust(ghost))
      val maybeGhostToChase = PersistentGhostCatalog.getFreshGhosts.find(ghost => buster.shouldPursueGhost(ghost))
      val coldCaseGhosts = PersistentGhostCatalog.getStaleGhosts.to[ListBuffer]

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
          Console.out.println(s"BUST ${maybeGhostToBust.get.id} ${buster.name}:Busting")
      } else if (maybeGhostToChase.nonEmpty) {
        val destination = Point(maybeGhostToChase.get.point.x, maybeGhostToChase.get.point.y)
        Console.out.println(s"MOVE ${destination.x} ${destination.y} ${buster.name}:Chasing ${maybeGhostToChase.get.name}")
      } else if (coldCaseGhosts.nonEmpty) {
        val coldCaseGhost = coldCaseGhosts.remove(0)
        val destination = Point(coldCaseGhost.point.x, coldCaseGhost.point.y)
        Console.out.println(s"MOVE ${destination.x} ${destination.y} ${buster.name}:Cold Case ${coldCaseGhost.name}")
      } else {
        val zone = buster.zone
        val xSpread = abs(zone.topLeftPoint.x - zone.topRightPoint.x)
        val randX = zone.topLeftPoint.x + rand.nextInt(xSpread)

        Console.out.println(s"MOVE $randX ${rand.nextInt(9000)} ${buster.name}:Moving")
      }
    }

    PersistentStunDB.updateAfterRound()
    PersistentGhostCatalog.updateAfterRound()
  }
}

case class Point(x: Integer, y: Integer)
case class RectangularBoundingBox(topLeftPoint: Point, bottomLeftPoint: Point, topRightPoint: Point, bottomRightPoint: Point)

abstract class Agent(val id: Integer, val name: String, val point: Point)


object Ghost {
  val NAMES = List("Stephanie", "Erick", "Vimesh", "Jeff") ::: (5 to 30).toList.map(i => s"Ghost $i")
}

sealed class Ghost(
    id: Integer,
    name: String,
    point: Point,
    val numTiedBusters: Integer,
    val seenThisRound: Boolean,
    val heldByGhostId: Option[Int])
  extends Agent(id, name, point)


object Buster {
  val NAMES = List("Alex", "Eric", "Thonny", "Brian", "Dennis", "Justin", "Nathan", "Drew", "Bill",
    "Ian", "Lisa", "Jenn")
}

sealed class Buster(
    id: Integer,
    name: String,
    point: Point,
    val zone: RectangularBoundingBox)
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
    distance <= 1760 && distance >= 900
  }

  def shouldPursueGhost(ghost: Ghost): Boolean = {
    val distance = distanceBetween(ghost)
    distance <= 2200 && distance > 1760
  }

  private def distanceBetween(agent: Agent): Double = {
    val distance = sqrt(
      pow(point.x.toDouble - agent.point.x.toDouble, 2) +
        pow(point.y.toDouble - agent.point.y.toDouble, 2)
    )

    abs(distance)
  }

}

sealed class BoardPartitioner(width: Integer, height: Integer) {

  def partition(numParts: Integer): List[RectangularBoundingBox] = {
    val columnWidth = ceil(width.toDouble / numParts).toInt

    val head = RectangularBoundingBox(
      topLeftPoint = Point(0,0),
      bottomLeftPoint = Point(0, height),
      topRightPoint = Point(columnWidth, 0),
      bottomRightPoint = Point(columnWidth, height)
    )

    val tail = (1 until numParts).toList.map { index =>
      RectangularBoundingBox(
        topLeftPoint = Point(columnWidth * index, 0),
        bottomLeftPoint = Point(columnWidth * index, height),
        topRightPoint = Point(columnWidth * (index+1), 0),
        bottomRightPoint = Point(columnWidth * (index+1), height)
      )
    }

    head :: tail
  }
}
