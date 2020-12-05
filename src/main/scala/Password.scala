import scala.io.Source
import scala.util.Using

// advent of code day 2 solution
object Password extends App {
  case class Policy(min: Int, max: Int, char: Char)

  // check passwords for part 1
  def passwordOk(policy: Policy, password: String): Boolean = {
    val count = password.count(c => c == policy.char)
    count >= policy.min & count <= policy.max
  }

  // check passwords for part 2
  def passwordOkPosition(policy: Policy, password: String): Boolean = {
    val firstChar = password(policy.min  - 1)
    val secondChar = password(policy.max  - 1)
    (firstChar == policy.char) & (secondChar != policy.char) |
      (firstChar != policy.char) & (secondChar == policy.char)
  }

  def parseLine(entry: String): (Policy, String) = {
    val fields = entry.split(" ")
    val minMax = fields(0).split("-")
    (Policy(min = minMax(0).toInt, max = minMax(1).toInt, char = fields(1).head), fields(2))
  }

  val input: List[(Policy, String)] = Using(Source.fromFile("/home/ssthapa/advent/passwords"))
      {_.getLines()
       .map(l => parseLine(l))
       .toList}.getOrElse(List((Policy(1, 1, 'a'), "b")))
  val validPasswords1 =
    input.map({case(policy: Policy, pass: String) => passwordOk(policy, pass)})
      .count(x => x)
  println(s"${input.filter({case(policy: Policy, pass: String) => passwordOk(policy, pass)})}")
  println(s"Valid passwords (pt 1): $validPasswords1")
  val validPasswords2 =
    input.map({case(policy: Policy, pass: String) => passwordOkPosition(policy, pass)})
      .count(x => x)
  println(s"Valid passwords (pt 2): $validPasswords2")



}
