package Example

import GeneticAlgorithmFP._

//models
case class Letter(c: Char)
object Letter {
  def randomChar: Char = {
    import scala.util.Random

    Random.alphanumeric.filter(_.isLetter).head.toUpper
  }
}

case class Word(letters: List[Letter])
object Word {
  val correctWord = Word(List(Letter('B'), Letter('I'), Letter('R'), Letter('D')))

  def applyRandom: Word = Word(List.fill(4)(Letter(Letter.randomChar)))
}

case class GenerationOfWordBird(words: List[Word])

//functions implementation
object Gene {
  implicit object LetterGene extends GeneDSL[Letter] {
    override def mutate(gene: Letter): Letter = gene.copy(c = Letter.randomChar)
  }
}

object Fitness {
  implicit object WordFitnessCount extends FitnessDSL[Word] {
    import Word.correctWord

    override def fitness(individual: Word): Double = correctWord.letters.zip(individual.letters).count(x => x._1 == x._2).toDouble
  }

  implicit object WordFitnessCalc extends FitnessDSL[Word] {
    import Word.correctWord

    override def fitness(individual: Word): Double = correctWord.letters.zip(individual.letters).count(x => x._1 == x._2).toDouble + 0.1
  }
}

object Mating {
  implicit object MatingCrossover extends MatingDSL[Word] {
    override def mating(parent1: Word, parent2: Word): Word = {
      val (startGene, endGene): (Int, Int) = {
        import scala.util.Random
        import scala.annotation.tailrec

        @tailrec def getTupleIndex(index1: Int, index2: Int): (Int, Int) = {
          if (index1 != index2) {
            if (index1 < index2) (index1, index2) else (index2, index1)
          } else {
            getTupleIndex(Random.nextInt(parent1.letters.size), Random.nextInt(parent1.letters.size))
          }
        }

        getTupleIndex(Random.nextInt(parent1.letters.size), Random.nextInt(parent1.letters.size))
      }

      val childP1 = parent1.letters.slice(startGene, endGene)
      val childGenes = parent2.letters.slice(0, startGene) ++ childP1 ++ parent2.letters.slice(endGene, parent2.letters.size)

      Word(childGenes)
    }
  }
}

object ReBirth {
  implicit object WordReBirth extends ReBirthDSL[Word] {
    import Gene.LetterGene

    override def reBirth(individual: Word, mutationRate: Double): Word = individual.copy(letters = individual.letters map { letter =>
      import GeneDSL._
      import scala.util.Random

      if (Random.nextDouble < mutationRate) mutate(letter) else letter
    })
  }
}

object Selection {
  implicit object RouletteWheelSelection extends SelectionDSL[GenerationOfWordBird, Word] {
    import FitnessDSL._
    import Fitness.WordFitnessCount

    override def evaluate(population: GenerationOfWordBird): List[(Double, Word)] = population.words.map(word => fitness(word) -> word).sorted

    override def selection(eliteSize: Int, scores: List[(Double, Word)]): List[Word] = {
      import scala.util.Random

      //add lucky loosers to the selection (roulette wheel)
      val loosers = scores.drop(eliteSize)
      val sumFitness: Double = loosers.map(_._1).sum
      if (sumFitness == 0.0) {
        scores.map(_._2)
      } else {
        lazy val remainingPopulation = scores.size - eliteSize
        lazy val probFitness: List[(Double, Int)] = loosers.map(_._1 / sumFitness).zipWithIndex

        val selected = List.fill(remainingPopulation) {
          import scala.annotation.tailrec

          @tailrec def pickLuckyLooser(maybeLuckyLooser: Option[Word]): Word = {
            if (maybeLuckyLooser.nonEmpty) {
              maybeLuckyLooser.get
            } else {
              val pick = 100 * Random.nextDouble
              val found = Random.shuffle(probFitness) collectFirst { case (prob, index) if pick <= prob => loosers(index)._2 }
              pickLuckyLooser(found)
            }
          }

          pickLuckyLooser(None)
        }

        scores.take(eliteSize).map(_._2) ++ selected
      }
    }
  }
}

object GeneticAlgorithm {
  implicit object GARouletteWheelCrossover extends GeneticAlgorithmDSL[GenerationOfWordBird] {
    import MatingDSL._
    import ReBirthDSL._
    import SelectionDSL._
    import Mating.MatingCrossover
    import ReBirth.WordReBirth
    import Selection.RouletteWheelSelection

    override def init(size: Int): GenerationOfWordBird = GenerationOfWordBird(List.fill(size)(Word.applyRandom))

    override def nextGeneration(population: GenerationOfWordBird, eliteSize: Int, mutationRate: Double): GenerationOfWordBird = {
      def generateChildren(selectedIndividuals: List[Word]): List[Word] = {
        import scala.util.Random

        val matingPool = Random.shuffle(selectedIndividuals).toArray
        List.tabulate(population.words.size - eliteSize) { i =>
          reBirth(mating(matingPool(i), matingPool(matingPool.length - i - 1)), mutationRate)
        }
      }

      //evaluate and select population
      val selectedPopulation = selection(eliteSize, evaluate(population))

      GenerationOfWordBird(population.words.take(eliteSize) ++ generateChildren(selectedPopulation))
    }
  }
}

object Runner {
  implicit object RunnerGAWordBird extends RunnerDSL[GenerationOfWordBird, Word] {
    import GeneticAlgorithmDSL._
    import SelectionDSL._
    import Selection.RouletteWheelSelection
    import GeneticAlgorithm.GARouletteWheelCrossover

    override def runGA(populationSize: Int, times: Int, eliteSize: Int, mutationRate: Double): List[SelectionDSL[GenerationOfWordBird, Word]#Score] = {
      import scala.annotation.tailrec

      @tailrec
      def run(nth: Int, population: GenerationOfWordBird): List[SelectionDSL[GenerationOfWordBird, Word]#Score] = {
        if (nth < times) {
          run(nth + 1, nextGeneration(population, eliteSize, mutationRate))
        } else {
          evaluate(population)
        }
      }
      lazy val gen: GenerationOfWordBird = init(populationSize)
      run(0, gen)
    }
  }
}

//main program to run Genetic Algorithm
object Program extends App {
  import RunnerDSL._
  import Runner.RunnerGAWordBird

  println("======= RUN GA =======")
  val scores = runGA(20, 45, 1, 0.15)
  println("======= RESULT =======")
  println(scores.take(5))
}
