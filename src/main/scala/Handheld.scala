import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Handheld extends App {


  case class Instruction(opcode: String, offset: Int)
  case class HandheldState(index: Int, acc: Int, executed: Set[Int])

  type Program = Array[Instruction]

  def  parseInstructions(input: List[String]): Program = {
    val instructionRe = raw"([a-z]{3}) ([+-])(\d+)".r
    (for ( line <- input ) yield {
      line match {
        case instructionRe(opcode, sign, offset) =>
          val signedOffset = if (sign == "+")
            offset.toInt
          else
            -1 * offset.toInt
          Instruction(opcode, signedOffset)
        case _ => Instruction("nop", 0)
      }
    }).toArray
  }

  @tailrec
  final def getLoopedAcc(state: HandheldState = HandheldState(0, 0, Set()),
                         program: Handheld.Program): Option[Int] = {
    if (state.index >= program.length)
      None
    else if (state.executed.contains(state.index))
      Some(state.acc)
    else {
      val currentInstruction = program(state.index)
      println(s"executing: $currentInstruction")
      val newExecuted = state.executed + state.index
      currentInstruction.opcode match {
        case "acc" =>
          val newState =
            HandheldState(index = state.index + 1,
              acc = state.acc + currentInstruction.offset,
              executed = newExecuted)
          getLoopedAcc(newState, program)
        case "nop" =>
          val newState =
            HandheldState(index = state.index + 1,
              acc = state.acc,
              executed = newExecuted)
          getLoopedAcc(newState, program)
        case "jmp" =>
          val newState =
            HandheldState(index = state.index + currentInstruction.offset,
              acc = state.acc,
              executed = newExecuted)
          getLoopedAcc(newState, program)
        case _ =>
          None
      }
    }
  }

  @tailrec
  final def getCompletedAcc(state: HandheldState = HandheldState(0, 0, Set()),
                            program: Handheld.Program): Option[Int] = {
    if (state.index == program.length)
      Some(state.acc)
    else if (state.executed.contains(state.index))
      None
    else {
      val currentInstruction = program(state.index)
      val newExecuted = state.executed + state.index
      currentInstruction.opcode match {
        case "acc" =>
          val newState =
            HandheldState(index = state.index + 1,
              acc = state.acc + currentInstruction.offset,
              executed = newExecuted)
          getCompletedAcc(newState, program)
        case "nop" =>
          val newState =
            HandheldState(index = state.index + 1,
              acc = state.acc,
              executed = newExecuted)
          getCompletedAcc(newState, program)
        case "jmp" =>
          val newState =
            HandheldState(index = state.index + currentInstruction.offset,
              acc = state.acc,
              executed = newExecuted)
          getCompletedAcc(newState, program)
        case _ =>
          None
      }
    }
  }

  def updateProgram(index: Int, program: Program): Program = {
    val currentInstruction: Instruction = program(index)
    currentInstruction.opcode match {
      case "jmp" =>
        val newInstruction = currentInstruction.copy(opcode = "nop")
        val newProgram = program.clone
        newProgram.update(index, newInstruction)
        newProgram
      case "nop" =>
        val newInstruction = currentInstruction.copy(opcode = "nop")
        val newProgram = program.clone
        newProgram.update(index, newInstruction)
        newProgram
      case _ => Array()
    }
  }

  def fixProgram(program: Program): Option[Int] = {
    val annotatedProgram = program.zipWithIndex
    val completions = for {
      (_, index) <- annotatedProgram.filter({case(inst, _) => inst.opcode == "jmp" | inst.opcode =="nop"})
    } yield {
      val modifiedProgram = updateProgram(index, program)
      getCompletedAcc(HandheldState(0, 0, Set()), modifiedProgram)
    }
    completions.filter(x => x.isDefined).head
  }

  val input: List[String] = Using(Source.fromFile("src/main/resources/handheld.orig")) {
    _.getLines().toList
  }.getOrElse(List[String]())

  val instructionList = parseInstructions(input)

  val acc = getLoopedAcc(HandheldState(0, 0, Set()), instructionList)
  acc match {
    case Some(x) =>   println(s"acc = $x when looped")
    case _ => println("invalid program")
  }

  val completedAcc = fixProgram(instructionList)
  completedAcc match {
    case Some(x) =>   println(s"acc = $x when completed")
    case _ => println("invalid program")
  }

}
