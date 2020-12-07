import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Customs extends App {

  @tailrec
  final def parseGroupAnswers(str: List[Char], answers: Set[Char] = Set[Char]()): Set[Char] = {
    str match {
      case x::xs => parseGroupAnswers(xs, answers + x)
      case Nil => answers
      case _ => answers
    }
  }

  final def parseGroupAnswersIntersection(strList: List[String]): Set[Char] = {
    val individualAnswers = for (
      line <- strList
    ) yield {
      parseGroupAnswers(line.trim.toList)
    }
    individualAnswers.reduce((set1, set2) => set1.intersect(set2))
  }

  def parseCustomsForms(fileBuf: String) = {
    val recordSplitRe = raw"\n\n".r
    val groupAnswers: Array[Set[Char]]  = for {
      record <- recordSplitRe.split(fileBuf)
    } yield {
      parseGroupAnswers(record.strip()
        .replace("\n", "")
        .toList)
    }
    groupAnswers.map(_.size)
  }

  def parseCustomsForms2(fileBuf: String) = {
    val recordSplitRe = raw"\n\n".r
    val groupAnswers: Array[Set[Char]] = for {
      record <- recordSplitRe.split(fileBuf)
    } yield {
      parseGroupAnswersIntersection(record.strip().split("\n").toList)
    }
    groupAnswers.map(_.size)
  }

  val input: String = Using(Source.fromFile("src/main/resources/customs"))  {_.mkString}.getOrElse("")
  val groupAnswers = parseCustomsForms(input)

  println(s"Sum of group answers: ${groupAnswers.sum}")

  val intersectGroupAnswers = parseCustomsForms2(input)
  println(s"Sum of intersected group answers: ${intersectGroupAnswers.sum}")


}
