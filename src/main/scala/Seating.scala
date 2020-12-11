import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.math.{max, min}


object Seating extends App {

  type SeatMap = Array[Array[Char]]

  def tooCrowded(row: Int, col: Int, seating: SeatMap) = {
    val occupied =
      for {
        seatRow <- max(0, row - 1) to min(seating.length - 1, row + 1)
        seatCol <- max(0, col - 1) to min(seating(0).length - 1, col + 1)
      } yield seating(seatRow)(seatCol) == '#'

    occupied.count(e => e) > 4
  }

  def getFirstDefined(s: Seq[Option[(Int, Int)]]): Option[(Int, Int)] =
    s.filter(_.isDefined).map(_.get).headOption

  def generateLOSSeats(row: Int, col: Int, seating: SeatMap): Seq[(Int,Int)] = {
    val leftCols = max(0, col - 1) to 0 by -1
    val rightCols = min(col + 1, seating(0).length) until seating(0).length
    val upRows = max(0, row - 1) to 0 by -1
    val downRows = min(seating.length, row + 1) until seating.length
    val leftSeat = for (x <- leftCols) yield {
      if (x == col) // at left edge
        None
      else if (seating(row)(x) != '.')
        Some((row, x))
      else
        None
    }
    val rightSeat = for (x <- rightCols) yield {
      if (x  == col) // at right edge
        None
      else if (seating(row)(x) != '.')
        Some((row, x))
      else
        None
    }
    val upSeat = for (y <- upRows) yield {
      if (y == row) // at top
        None
      else if (seating(y)(col) != '.')
        Some((y, col))
      else
        None
    }
    val downSeat = for (y <- downRows) yield {
      if (y == row) // at bottom
        None
      else if (seating(y)(col) != '.')
        Some((y, col))
      else
        None
    }
    val neSeat = for {
      (seatRow, seatCol) <- upRows.zip(leftCols)
      } yield {
      if ((seatCol == col) | (seatRow == row))  // at an edge
        None
      else if (seating(seatRow)(seatCol) != '.')
        Some((seatRow, seatCol))
      else
        None
    }
    val nwSeat = for {
      (seatRow, seatCol) <- upRows.zip(rightCols)
      } yield {
      if ((seatCol == col) | (seatRow == row))  // at an edge
        None
      else if (seating(seatRow)(seatCol) != '.')
        Some((seatRow, seatCol))
      else
        None
    }
    val seSeat = for {
      (seatRow, seatCol) <- downRows.zip(leftCols)
    } yield {
      if ((seatCol == col) | (seatRow == row))  // at an edge
        None
      else if (seating(seatRow)(seatCol) != '.')
        Some((seatRow, seatCol))
      else
        None
    }
    val swSeat = for {
      (seatRow, seatCol) <- downRows.zip(rightCols)
    } yield {
      if ((seatCol == col) | (seatRow == row))  // at an edge
        None
      else if (seating(seatRow)(seatCol) != '.')
        Some((seatRow, seatCol))
      else
        None
    }
    Seq(getFirstDefined(leftSeat),
      getFirstDefined(rightSeat),
      getFirstDefined(upSeat),
      getFirstDefined(downSeat),
      getFirstDefined(neSeat),
      getFirstDefined(nwSeat),
      getFirstDefined(seSeat),
      getFirstDefined(swSeat)).filter(_.isDefined).map(_.get)
  }

  def tooCrowdedLOS(row: Int, col: Int, seating: SeatMap) = {

    val occupied =
      for {
        (seatRow, seatCol) <- generateLOSSeats(row, col, seating)
      } yield seating(seatRow)(seatCol) == '#'
    occupied.count(e => e) >= 5
  }

  def canOccupy(row: Int, col: Int, seating: SeatMap) = {
    val occupied =
      for {
        seatRow <- max(0, row - 1) to min(seating.length - 1, row + 1)
        seatCol <- max(0, col - 1) to min(seating(0).length - 1, col + 1)
      } yield seating(seatRow)(seatCol) == '#'

    occupied.count(e => e) == 0
  }

  def canOccupyLOS(row: Int, col: Int, seating: SeatMap) = {
    val occupied =
      for {
        (seatRow, seatCol) <- generateLOSSeats(row, col, seating)
      } yield {
        seating(seatRow)(seatCol) == '#'
      }

    occupied.count(e => e) == 0
  }

  def updateSeating(seating: SeatMap): SeatMap = {
    val updated = for {
      row <- seating.indices
      col <- seating(row).indices
    } yield {
      seating(row)(col) match {
        case '.' => '.'
        case 'L' => if (canOccupy(row, col, seating)) '#' else 'L'
        case '#' => if (tooCrowded(row, col, seating)) 'L' else '#'
        case _ => 'E'
      }
    }
    updated.toArray.grouped(seating(0).length).toArray
  }

  def updateLOSSeating(seating: SeatMap): SeatMap = {
    val updated = for {
      row <- seating.indices
      col <- seating(row).indices
    } yield {
      seating(row)(col) match {
        case '.' => '.'
        case 'L' => if (canOccupyLOS(row, col, seating)) '#' else 'L'
        case '#' => if (tooCrowdedLOS(row, col, seating)) 'L' else '#'
        case _ => 'E'
      }
    }
    updated.toArray.grouped(seating(0).length).toArray
  }

  def sameMap(seating1: SeatMap, seating2: SeatMap): Boolean = {
    if ((seating1.length == seating2.length) & (seating1(0).length == seating2(0).length)) {
      val comps = for {
        row <- seating1.indices
        col <- seating1(0).indices
      } yield seating1(row)(col) == seating2(row)(col)
      comps.foldRight(true)((acc, elem) => acc & elem)
    } else
      false
  }

  @tailrec
  def iterateSeating(seating: SeatMap, iterations: Int = 1): SeatMap = {
    if (iterations == 0)
      seating
    else
      iterateSeating(updateSeating(seating), iterations - 1)
  }

  @tailrec
  def iterateLOSSeating(seating: SeatMap, iterations: Int = 1): SeatMap = {
    if (iterations == 0)
      seating
    else
      iterateLOSSeating(updateLOSSeating(seating), iterations - 1)
  }

  @tailrec
  def getStableSeating(seating: SeatMap): SeatMap = {
    val updated = updateSeating(seating)
    if (sameMap(updated, seating))
      updated
    else
      getStableSeating(updateSeating(seating))
  }

  @tailrec
  def getStableLOSSeating(seating: SeatMap): SeatMap = {
    val updated = updateLOSSeating(seating)
    if (sameMap(updated, seating))
      updated
    else
      getStableLOSSeating(updateLOSSeating(seating))
  }

  def occupied(seating: SeatMap) = {
    (for {
      y <- seating.indices
      x <- seating(0).indices
    } yield {
      seating(x)(y) == '#'
    }).count(elem => elem)
  }



  val input: SeatMap = Using(Source.fromFile("src/main/resources/seating.orig")) {
    _.getLines().toArray
  }.get.map(_.toCharArray)

  val iteratedMap = getStableSeating(input)
  println(s"Occupied = ${occupied(iteratedMap)}")

  val iteratedLOSMap = getStableLOSSeating(input)
  println(s"Occupied = ${occupied(iteratedLOSMap)}")

}
