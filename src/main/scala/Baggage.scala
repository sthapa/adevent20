import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using


object Baggage extends App {

  type Description = String
  type BagRules = Map[Description, List[(Int, Description)]]

  //  case class Bag(description: Description, contains: List[(Int, Description)])

  final def parseBagContains(input: String) = {
    val containedBagRe = raw"(\d)+ ([a-z]+ [a-z]+)".r
    (for {
      m <- containedBagRe.findAllMatchIn(input)
    } yield {
      (m.group(1).toInt, m.group(2))
    }).toList
  }

  val bagRuleRe = raw"([a-z]+ [a-z]+) bags contain (.*)".r

  @tailrec
  final def parseBags(input: List[String],
                      rules: BagRules = Map()): BagRules = {
    input match {
      case x :: xs =>
        x match {
          case bagRuleRe(desc, bags) =>
            if (bags.strip() == "contains no other bags") {
              Map()
            } else {
              parseBags(xs, rules + (desc -> parseBagContains(bags)))
            }
          case _ =>
            Map()
        }
      case Nil => rules
      case _ => Map()
    }
  }

  def getRuleCount(queryBag: Description, rules: BagRules): Int = {
    val contain =
      for ((k, _) <- rules) yield containedIn(queryBag, k, rules)
    contain.foldLeft(0)({ (acc, elem) => if (elem) acc + 1 else acc })
  }


  def containedIn(queryBag: Description, ruleBag: Description, rules: BagRules): Boolean = {
    val bagRule = rules.get(ruleBag)
    if (queryBag == ruleBag)
      false
    bagRule match {
      case Some(contained) =>
        if (contained.filter({ case (_, desc) => desc == queryBag }).nonEmpty)
          true
        else {
          val containedInRules = contained.map({ case (_, desc: String) => containedIn(queryBag, desc, rules) })
          containedInRules.foldLeft(false)((acc, elem) => acc | elem)
        }
      case None => false
    }
  }

  def getBagCount(queryBag: Description, rules: BagRules): Int = {
    val bagRule = rules.get(queryBag)
    bagRule match {
      case Some(rule) =>
        if (rule.isEmpty)
          0
        else {
          val bagCounts = rule.map({ case (quantity, bag) => quantity + quantity * getBagCount(bag, rules) })
          bagCounts.sum
        }
      case None => 0
    }
  }


  val input: List[String] = Using(Source.fromFile("src/main/resources/bags.orig")) {
    _.getLines().toList
  }.getOrElse(List[String]())

  val bagRules = parseBags(input)
  val shinySum = getRuleCount("shiny gold", bagRules)
  println(s"shiny gold bag can be found in ${shinySum} rules")
  //  val testBags = List("faded blue", "dotted black", "vibrant plum", "dark olive", "shiny gold")
  //  testBags.foreach( bag => println(s"$bag contains ${getBagCount(bag, bagRules)} bags"))
  val testBags = List("shiny gold")
  testBags.foreach(bag => println(s"$bag contains ${getBagCount(bag, bagRules)} bags"))

}
