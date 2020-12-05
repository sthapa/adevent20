import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using


object Seats extends App {

  case class Seat(row: Int, column: Int) {
    val getSeatId: Int = row * 8 + column
  }
  @tailrec
  final def getRow(identifier: List[Char], rowRange: Range = 0 to 127): Option[Int] = {
    identifier match {
      case Nil =>
        if (rowRange.length == 1)
          rowRange.headOption
        else
          None
      case x::xs =>
        val midPoint = (rowRange.start + rowRange.end).toDouble / 2
        if (x == 'F')
          getRow(xs, rowRange.start to midPoint.floor.toInt)
        else if (x == 'B')
          getRow(xs, midPoint.ceil.toInt to rowRange.end)
        else
          None
      case _ => None
    }
  }

  @tailrec
  final def getCol(identifier: List[Char], colRange: Range = 0 to 7): Option[Int] = {
    identifier match {
      case Nil =>
        if (colRange.length == 1)
          colRange.headOption
        else
          None
      case x::xs =>
        val midPoint = (colRange.start + colRange.end).toDouble / 2
        if (x == 'R')
          getCol(xs, midPoint.ceil.toInt to colRange.end)
        else if (x == 'L')
          getCol(xs, colRange.start to midPoint.floor.toInt)
        else
          None
      case _ => None
    }
  }

  final def getSeat(identifier: String): Option[Seat] = {
    if (identifier.length == 10) {
      val rowIdentifier = identifier.substring(0, 7)
      val colIdentifier = identifier.substring(7, 10)
      val row = getRow(rowIdentifier.toList)
      val col = getCol(colIdentifier.toList)
      row match {
        case Some(r) =>
          col match {
            case Some(c) => Some(Seat(r, c))
            case None => None
          }
        case None => None
      }

    } else
      None
  }

  def mapIds(seat: Option[Seat]): Int =
    seat match {
      case Some(s) => s.getSeatId
      case None => 0
    }

  def getMissingSeat(seats: List[Int]) = {
    val sortedSeats = seats.sorted
    val seatInfo = for ( i <- sortedSeats.indices) yield {
      if (i == (sortedSeats.length - 1))
        (0, 0)
      else
        (sortedSeats(i + 1) - sortedSeats(i), (sortedSeats(i) + sortedSeats(i+1)) / 2)
    }
    val missing = seatInfo.filter(elem => elem._1 == 2)
    println(missing)
    if (missing.length == 1)
      missing.head._2
    else
      0

  }

  val input = Using(Source.fromFile("src/main/resources/seats"))  {_.getLines().toList}.getOrElse(List[String]())
  val seats: List[Option[Seat]] = input.map(getSeat)
  val seatIds = seats.map(mapIds)
  println(s"Max id: ${seatIds.max}")
  println(s"Missing seat: ${getMissingSeat(seatIds)}")
}
