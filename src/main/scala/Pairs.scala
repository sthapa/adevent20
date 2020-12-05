import scala.io.Source

object Pairs extends App {
  def getTwoCombos(inputs: List[Int]): List[(Int, Int)] =
    for {
      x <- inputs
      y <- inputs}
      yield (x, y)

  def getThreeCombos(inputs: List[Int]): List[(Int, Int, Int)] =
    for {
      x <- inputs
      y <- inputs
      z <- inputs}
      yield (x, y, z)

  val input: List[Int] =
    (for (line <- Source.fromFile("src/main/resources/inputs").getLines)
      yield line.toInt).toList


  val combos: List[(Int, Int)] = getTwoCombos(input)
  println("Two elements")
  val sols2 = combos.filter( x => x._1 + x._2 == 2020)
  println(s"Solution = ${sols2.take(1)}")
  sols2.map(x => x._1 * x._2)
    .take(1)
    .foreach( x => println(x))

  val combos3 = getThreeCombos(input)
  println("Three elements")
  val sols3 = combos3.filter( x => x._1 + x._2 + x._3 == 2020)
  println(s"Solution = ${sols3.take(1)}")
  sols3.map(x => x._1 * x._2 * x._3)
    .take(1)
    .foreach( x => println(x))


}