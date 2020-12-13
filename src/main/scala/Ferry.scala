import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.math.{Pi, abs, atan, cos, pow, sin, sqrt, toRadians, toDegrees}


object Ferry extends App {

  case class ShipState(x: Double, y: Double, heading: Double) {
    def updateHeading(headingChange: Double): ShipState = {
      this.copy(heading = heading + headingChange)
    }

    def turnLeft(degrees: Double): ShipState = {
      updateHeading(degrees)
    }

    def turnRight(degrees: Double): ShipState = {
      updateHeading(-1 * degrees)
    }

    def goForward(distance: Double): ShipState = {
      val xMove: Double = cos(toRadians(heading)) * distance
      val yMove: Double = sin(toRadians(heading)) * distance
      this.copy(x = x + xMove, y = y + yMove)
    }

    def goNorth(distance: Double): ShipState = this.copy(y = y + distance)
    def goSouth(distance: Double): ShipState = this.copy(y = y - distance)
    def goEast(distance: Double): ShipState = this.copy(x = x + distance)
    def goWest(distance: Double): ShipState = this.copy(x = x - distance)

    def getManhattanDistance: Double = abs(x) + abs(y)
  }

  case class ShipStateWithWaypoint(x: Long,
                                   y: Long,
                                   waypointX: Long,
                                   waypointY: Long) {

    def turnLeft(degrees: Int): ShipStateWithWaypoint = {
      degrees match {
        case 0 => this.copy()
        case 90 => this.copy(waypointX = -1 * waypointY, waypointY = waypointX)
        case 180 => this.turnLeft(90).turnLeft(90)
        case 270 => this.turnLeft(90).turnLeft(90).turnLeft(90)
        case 360 => this.copy()
        case _ => throw new IllegalArgumentException
      }
    }

    def turnRight(degrees: Int): ShipStateWithWaypoint = {
      degrees match {
        case 0 => this.copy()
        case 90 => this.copy(waypointX = waypointY, waypointY = -1 * waypointX)
        case 180 => this.turnRight(90).turnRight(90)
        case 270 => this.turnRight(90).turnRight(90).turnRight(90)
        case 360 => this.copy()
        case _ => throw new IllegalArgumentException
      }
    }

    def goForward(times: Int): ShipStateWithWaypoint = {
      val xMove = times * waypointX
      val yMove = times * waypointY
      this.copy(x = x + xMove, y = y + yMove)
    }

    def goNorth(distance: Int): ShipStateWithWaypoint = this.copy(waypointY = waypointY + distance)
    def goSouth(distance: Int): ShipStateWithWaypoint = this.copy(waypointY = waypointY - distance)
    def goEast(distance: Int): ShipStateWithWaypoint = this.copy(waypointX = waypointX + distance)
    def goWest(distance: Int): ShipStateWithWaypoint = this.copy(waypointX = waypointX - distance)

    def getManhattanDistance: Long = abs(x) + abs(y)
  }

  @tailrec
  def followDirections(directions: List[String], state: ShipState): ShipState = {
    //println(s"state = $state, direction = ${directions.headOption}")
    directions match {
      case x::xs =>
        val dir = x(0)
        val num = x.substring(1).toInt
        val updatedState = dir match {
          case 'N' => state.goNorth(num)
          case 'S' => state.goSouth(num)
          case 'E' => state.goEast(num)
          case 'W' => state.goWest(num)
          case 'L' => state.turnLeft(num)
          case 'R' => state.turnRight(num)
          case 'F' => state.goForward(num)
          case _ => throw new IllegalArgumentException
        }
        followDirections(xs, updatedState)
      case Nil => state
    }
  }

  @tailrec
  def followWaypointDirections(directions: List[String],
                               state: ShipStateWithWaypoint): ShipStateWithWaypoint = {
    println(s"state = $state, direction = ${directions.headOption}")
    directions match {
      case x::xs =>
        val dir = x(0)
        val num = x.substring(1).toInt
        val updatedState = dir match {
          case 'N' => state.goNorth(num)
          case 'S' => state.goSouth(num)
          case 'E' => state.goEast(num)
          case 'W' => state.goWest(num)
          case 'L' => state.turnLeft(num)
          case 'R' => state.turnRight(num)
          case 'F' => state.goForward(num)
          case _ => throw new IllegalArgumentException
        }
        followWaypointDirections(xs, updatedState)
      case Nil => state
    }
  }

  val input = Using(Source.fromFile("src/main/resources/ferry.orig")) {
    _.getLines().toList
  }.get

  val finalState = followDirections(input, ShipState(x = 0, y = 0, heading = 0))
  println(s"final distance = ${finalState.getManhattanDistance}")

  val finalWaypointState = followWaypointDirections(input,
    ShipStateWithWaypoint(x = 0, y = 0, waypointX = 10, waypointY = 1))
  println(s"final distance = ${finalWaypointState.getManhattanDistance}")

}
