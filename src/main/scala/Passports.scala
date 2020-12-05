import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex

object Passports extends  App {
  case class Passport(fields: Map[String, String]) {
    def numFields = fields.size

    def validField(fieldName: String): Boolean = {
      val fieldVal = fields.get(fieldName)
      fieldVal match {
        case None => false
        case Some(value) =>
          fieldName match {
            case "byr" => value.toInt >= 1920 & value.toInt <= 2002
            case "iyr" => value.toInt >= 2010 & value.toInt <= 2020
            case "eyr" => value.toInt >= 2020 & value.toInt <= 2030
            case "hgt" =>
              val length = value.length
              if (length <= 2)
                false
              else {
                val units = value.substring(length - 2, length)
                val measurement = value.substring(0, length - 2).toInt
                units match {
                  case "cm" => measurement >= 150 & measurement <= 193
                  case "in" => measurement >= 59 & measurement <= 76
                  case _ => false
                }
              }
            case "hcl" =>
              val colorRe = raw"#[0-9a-f]{6}".r
              colorRe.matches(value)
            case "ecl" => value match {
              case "amb" => true
              case "blu" => true
              case "brn" => true
              case "gry" => true
              case "grn" => true
              case "hzl" => true
              case "oth" => true
              case _ => false
            }
            case "pid" =>
              val pidRe = raw"[0-9a-f]{9}".r
              pidRe.matches(value)
            case "cid" => true
            case _ => false
          }
      }
    }


    def valid: Boolean = {
      val fieldList = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
      if (fields.size >= 7) {
        fieldList.map(validField(_)).fold(true)((x, y) => x & y)
      } else
        false
    }
  }

  def parsePassports(fileBuf: String): Array[Passport] = {
    val recordSplitRe = raw"\n\n".r
    val passportRe = raw"(([a-z]+):([#a-z0-9]+))".r
    for {
      record <- recordSplitRe.split(fileBuf)
    } yield {
      val matches  = (for (m <- passportRe.findAllMatchIn(record))
        yield {
          (m.group(2), m.group(3))
        }).toList
//     Passport(fields = matches.length, cid = matches.contains(true))
      Passport(fields = matches.toMap)
    }
  }


  val input: String = Using(Source.fromFile("src/main/resources/passports"))  {_.mkString}.getOrElse("")
  val passports = parsePassports(input)

  val validPassports = passports.filter(_.valid).length
  println(s"Valid passports = ${validPassports}")


}
