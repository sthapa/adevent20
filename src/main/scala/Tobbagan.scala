import scala.io.Source
import scala.util.Using


object Tobbagan extends App {
  case class MapRow(treeLocations: Array[Boolean]) {
    val length = treeLocations.length
    val width = if (treeLocations.length > 0)
      treeLocations.length
    else 0
    def treeAt(index: Int): Boolean = treeLocations(index)

  }

  def parseLine(line: String): MapRow = {
    MapRow(treeLocations = line.toArray.map(_ == '#'))
  }

  def getCollisions(map: Array[MapRow], right: Int, down: Int): Int = {
    val yLocations = Range(0, map.length, down)
    val width = map(0).treeLocations.length
    println(yLocations.length)
    val collisions =
      for ( y <- yLocations)
      yield {
        val step = yLocations.indexOf(y)
        val xLocation = (1 + step * right - 1) % width
        if (yLocations.length == 162) {
          println(s"step = $step, x = $xLocation, y = $y")
        }
        map(y).treeAt(xLocation)
      }
    collisions.count(x => x)
  }

  val input: Array[MapRow] = Using(Source.fromFile("/home/ssthapa/advent/tobbagan"))
  {_.getLines()
    .map(l => parseLine(l))
    .toArray}.getOrElse(Array[MapRow]())

  val wraparound = input(0).length + 1
  println(s"# of collisions: ${getCollisions(input, 3, 1)}")
  val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
  val totalCollisions =
    slopes.map(x => getCollisions(input, x._1, x._2))
      .map(x => x.toLong)

  println(totalCollisions)
  println(s"# of collisions: ${totalCollisions.fold(1: Long)({case(a, b) => a*b})}")
}
