import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using


// Day 10 solution

object Joltage extends App {

  val input: Set[Int] = Using(Source.fromFile("src/main/resources/joltage.example2")) {
    _.getLines().toList
  }.getOrElse(List[String]()).map(_.toInt).toSet

  @tailrec
  final def getAdapterChain(deviceInput: Int, adapters: Set[Int], chain: List[Int]): List[Int] = {
    // println(s"adapters = $adapters chain = $chain")
    if (adapters.isEmpty) {
      if ((deviceInput - chain.head) <= 3)
        chain.reverse
      else
        List()
    } else {
      val inputJolt = chain.headOption.getOrElse(0)
      val candidates = adapters.map(_ - inputJolt).filter(elem => elem <= 3 & elem > 0)
      if (candidates.nonEmpty) {
        val candidate = candidates.min
        getAdapterChain(deviceInput,
          adapters - (candidate + inputJolt),
          (candidate + inputJolt)::chain)
      } else
        List()
    }
  }

  type AdapterSet = Set[Int]
  type AdapterChain = List[Int]
  var memoCache: scala.collection.mutable.Map[AdapterSet, Set[AdapterChain]] =
    scala.collection.mutable.Map[AdapterSet, Set[AdapterChain]]()
/*

  final def getAdapterChains(deviceInput: Int,
                              adapters: AdapterSet) = {
    if (memoCache.contains(adapters)) {
      println(s"using cached value ${memoCache(adapters)}")
      memoCache(adapters)
    } else {
      val sols = getAdapterChains(deviceInput, adapters, List())
      memoCache += (adapters -> sols)
      sols
    }
  }
  final def getAdapterChains(deviceInput: Int,
                             adapters: AdapterSet,
                             chain: AdapterChain): Set[AdapterChain] = {
    //println(s"adapters = $adapters chain = $chain")
    if (memoCache.contains(adapters)) {
      println(s"using cached value ${memoCache(adapters)}")
      memoCache(adapters)
    } else if (adapters.isEmpty) {
      if ((deviceInput - chain.head) <= 3)
        Set(chain.reverse)
      else
        Set()
    } else {
      val inputJolt = chain.headOption.getOrElse(0)
      val candidates = (adapters + deviceInput).map(_ - inputJolt).filter(elem => elem <= 3 & elem > 0)
      if (candidates.nonEmpty) {
        val solutions = for {
          candidate <- candidates.toList
        } yield {
          if (candidate == (deviceInput - inputJolt))
            Set(chain.reverse)
          else {
            val newSet = adapters.filter(x => x > (candidate + inputJolt))
            getAdapterChains(deviceInput, newSet, (candidate + inputJolt)::chain)
          }
        }
        solutions.flatten.filter(_.nonEmpty).toSet
      } else
        Set()
    }
  }
*/

  def getPartOneSolution(solution: List[Int], deviceInput: Int) = {
    val diff = (0::solution).zip(solution.appended(deviceInput)).map(elem => elem._2 - elem._1)
    diff.count(_ == 1) * diff.count(_ == 3)

  }

  val deviceInput = input.max + 3
  val solution = getAdapterChain(deviceInput, input, List())
  println(s"chain = $solution")
  println(s"Answer = ${getPartOneSolution(solution, deviceInput)}")

//  val solutionSet = getAdapterChains(deviceInput, input, List())
//  println(s"chains = $solutionSet")
//  println(s"# solutions = ${solutionSet.size}")

}
