package codinggame

import scala.collection.mutable.ListBuffer
import scala.math._
import scala.util._

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
    val ghosts = ListBuffer.empty[Ghost]
    var zoneCounter = 0

    for(i <- 0 until entities) {
      // entityid: buster id or ghost id
      // y: position of this buster / ghost
      // entitytype: the team id if it is a buster, -1 if it is a ghost.
      // state: For busters: 0=idle, 1=carrying a ghost.
      // value: For busters: Ghost id being carried. For ghosts: number of busters attempting to trap this ghost.
      val Array(entityid, x, y, entitytype, state, value) = for(i <- readLine split " ") yield i.toInt
      Console.err.println(s"entityid:$entityid x:$x y:$y, entitytype:$entitytype state:$state value:$value")

      if (entitytype == myteamid) {
        var ghostId: Option[Integer] = None

        if (state == 1)
          ghostId = Some(value)

        busters += new Buster(entityid, Buster.NAMES(entityid), Point(x, y), zones(zoneCounter), ghostId)
      } else if (entitytype == -1) {
        ghosts += new Ghost(entityid, Ghost.NAMES(entityid), Point(x, y), value)
      } else {
        var ghostId: Option[Integer] = None

        if (state == 1)
          ghostId = Some(value)

        enemyBusters += new Buster(entityid, Buster.NAMES(entityid), Point(x, y), zones(zoneCounter), ghostId)
      }

      zoneCounter += 1
    }

    Console.err.println(s"busters:$busters")
    Console.err.println(s"ghosts:$ghosts")
    val rand = new Random()

    for (buster <- busters) {
      val maybeEnemyBuster = enemyBusters.find(enemy => buster.isAbleToStun(enemy))

      if (maybeEnemyBuster.nonEmpty) {
        val enemyId = maybeEnemyBuster.get.id
        buster.stun()
        Console.out.println(s"STUN $enemyId ${buster.name}:Stunning!")
      } else if (buster.ghostId.nonEmpty) {
        if (buster.point.x == basePoint.x && buster.point.y == basePoint.y) {
          val ghostName = Ghost.NAMES(buster.ghostId.get)
          Console.out.println(s"RELEASE ${buster.name}:Releasing $ghostName")
        } else {
          Console.out.println(s"MOVE ${basePoint.x} ${basePoint.y} ${buster.name}:Moving to Base")
        }
      } else {
        val maybeGhost = ghosts.find(ghost => buster.isAbleToBust(ghost))

        if (maybeGhost.nonEmpty) {
          Console.out.println(s"BUST ${maybeGhost.get.id} ${buster.name}:Busting")
        } else {
          val maybeGhost = ghosts.find(ghost => buster.shouldPursueGhost(ghost))
          if (maybeGhost.nonEmpty) {

            // TODO: make the destination where the ghost is going, not where it is
            val destination = Point(maybeGhost.get.point.x, maybeGhost.get.point.y)

            Console.out.println(s"MOVE ${destination.x} ${destination.y} ${buster.name}:Chasing ${maybeGhost.get.name}")
          } else {
            val zone = buster.zone
            val xSpread = abs(zone.topLeftPoint.x - zone.topRightPoint.x)
            val randX = zone.topLeftPoint.x + rand.nextInt(xSpread)

            Console.out.println(s"MOVE $randX ${rand.nextInt(9000)} ${buster.name}:Moving")
          }
        }
      }
    }
  }
}

case class Point(x: Integer, y: Integer)
case class RectangularBoundingBox(topLeftPoint: Point, bottomLeftPoint: Point, topRightPoint: Point, bottomRightPoint: Point)

abstract class Agent(val id: Integer, val name: String, val point: Point)


object Ghost {
  val NAMES = List("Stephanie", "Erick", "Vimesh", "Jeff") ::: (5 to 30).toList.map(i => s"Ghost $i")
}

sealed class Ghost(id: Integer, name: String, point: Point, val numTiedBusters: Integer)
  extends Agent(id, name, point)


object Buster {
  val NAMES = List("Alex", "Eric", "Thonny", "Brian", "Denis", "Justin", "Nathan", "Drew", "Bill",
    "Ian", "Lisa", "Jenn")
}

trait Stunable {
  var onCooldown = false
  var roundCounter = 0

  def canStun: Boolean = !onCooldown || (onCooldown && roundCounter == 20)
  def incrementRoundCounter():Unit = roundCounter += 1

  def stun(): Unit = {
    if (!canStun)
      throw new IllegalStateException("You are not allowed to stun!")

    onCooldown = true
    roundCounter = 0
  }
}

sealed class Buster(
    id: Integer,
    name: String,
    point: Point,
    val zone: RectangularBoundingBox,
    val ghostId: Option[Integer])
  extends Agent(id, name, point)
  with Stunable {

  def isAbleToStun(other: Buster): Boolean = {
    val distance = distanceBetween(other)
    distance <= 1760 && canStun
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
