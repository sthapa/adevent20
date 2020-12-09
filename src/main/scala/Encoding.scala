import scala.io.Source
import scala.util.Using

object Encoding extends App {

  def findWeakness(input: Array[Long], sum: Long, length: Int): Option[Long] = {
    val sols = for {
      window <- input.sliding(length)
    } yield (window.sum, window.max + window.min)
    val results = sols.filter(x => x._1 == sum)
      .toList.headOption
    results match {
      case Some(x) => Some(x._2)
      case None => None
    }
  }

  val input: Array[Long] = Using(Source.fromFile("src/main/resources/encoding.orig")) {
    _.getLines().toList
  }.getOrElse(List[String]()).map(_.toLong).toArray

  val preambleLength = 25
  val windowed = input.sliding(preambleLength).zipWithIndex
  val results = for ( (window, index) <- windowed ) yield {
    if ((index + preambleLength) < input.length) {
      val summed = input(index + preambleLength)
      val windowedDiffSet = window.map(summed - _).toSet
      val intersection = windowedDiffSet.intersect(window.toSet)
      if (intersection.isEmpty)
        Some(summed)
      else
        None

    } else
      None
  }
  val solution = results.filter(_.isDefined).map(_.get).toList.head
  println(s"solution = ${solution} when completed")

  val break = for ( solLength <- 1 to input.length) yield findWeakness(input, solution, solLength)
  println(s"break = ${break.filter(_.isDefined).map(_.get).toList} when completed")
}
