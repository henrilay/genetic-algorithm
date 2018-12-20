package GeneticAlgorithm

import org.scalatest.{MustMatchers, WordSpec}

import scala.util.Random

class GeneticAlgorithmSpec extends WordSpec with MustMatchers {
  case class Letter(char: Char) extends Gene
  object Letter {
    def applyRandom: Letter = new Letter(Random.alphanumeric.filter(_.isLetter).head.toUpper)
  }

  object TestGA extends GeneticAlgorithm[Letter] {
    val correctResult = List(Letter('B'), Letter('I'), Letter('R'), Letter('D'))

    override def fitness(chromosome: Chromosome[Letter]): Double = correctResult.zip(chromosome.genes).count(x => x._1 == x._2)
  }

  val genesPool = (1 to 100).map(_ => Gene.apply[Letter](_ => Letter.applyRandom)).distinct.toList

  "GeneticAlgorithm" should {
    "produce a correct initial Population" in {
      val popu = TestGA.initialPopulation(genesPool, 20, 4)

      popu.size must ===(20)
      popu.head.genes.size must ===(4)
      popu.last.genes.size must ===(4)
    }

    "evaluate properly the population" in {
      val popu = TestGA.initialPopulation(genesPool, 100, 4)

      val rankedPopu = TestGA.evaluatePopulation(popu)
      rankedPopu.size must ===(100)

      val firstTwo = rankedPopu.take(2)
      (firstTwo.head._1 >= firstTwo.last._1) mustBe true
    }

    "correctly select the population" when {
      object TestGAWithFitness extends GeneticAlgorithm[Letter] {
        override def fitness(chromosome: Chromosome[Letter]): Double = Random.nextDouble * 100 + 1.0
      }

      "elite number is not provided" in {
        val popu = TestGAWithFitness.initialPopulation(genesPool, 20, 4)
        val rankedPopu = TestGAWithFitness.evaluatePopulation(popu)
        val selectedPopu = TestGAWithFitness.selection(rankedPopu, 0)

        selectedPopu.size <= rankedPopu.size mustBe true
        selectedPopu.take(3) must !==(rankedPopu.take(3))
      }

      "elite number is provided" in {
        val eliteSize = 5
        val popu = TestGAWithFitness.initialPopulation(genesPool, 20, 4)
        val rankedPopu = TestGAWithFitness.evaluatePopulation(popu)
        val selectedPopu = TestGAWithFitness.selection(rankedPopu, eliteSize)

        selectedPopu.size <= rankedPopu.size mustBe true
        selectedPopu.take(eliteSize) must ===(rankedPopu.take(eliteSize))
      }

      "the fitness score for the whole population is 0.0" in {
        object TestGAZeroFitness extends GeneticAlgorithm[Letter] {
          override def fitness(chromosome: Chromosome[Letter]): Double = 0.0
        }

        val popu = TestGAZeroFitness.initialPopulation(genesPool, 20, 4)
        val rankedPopu = TestGAZeroFitness.evaluatePopulation(popu)
        val selectedPopu = TestGAZeroFitness.selection(rankedPopu, 5)

        selectedPopu must ===(rankedPopu)
      }
    }

    "correctly crossover 2 parents" in {
      val parent1 = Chromosome(List(Letter('S'), Letter('K'), Letter('Y'), Letter('E')))
      val parent2 = Chromosome(List(Letter('A'), Letter('P'), Letter('L'), Letter('U')))
      val child = TestGA.orderedCrossover(parent1, parent2)

      child.genes.size must ===(parent1.genes.size)

      val subsetOfParent1 = child.genes.intersect(parent1.genes)
      subsetOfParent1.nonEmpty mustBe true
      parent1.genes.containsSlice(subsetOfParent1) mustBe true

      val subsetOfParent2 = child.genes.intersect(parent2.genes)
      subsetOfParent2.nonEmpty mustBe true
    }

    "mutate a chromosome if mutation rate is 100" in {
      val chromosome = Chromosome(List(Letter('S'), Letter('K'), Letter('Y'), Letter('E')))
      val mutant = TestGA.mutate(chromosome, _ => Letter.applyRandom, 1.0)

      mutant must !==(chromosome)
    }

    "keep Elite" when {
      "elite size is zero" in {
        val popu = TestGA.initialPopulation(genesPool, 20, 4)
        TestGA.keepElite(popu.toList, 0) must ===(Set.empty[Chromosome[Letter]])
      }

      "elite size is greater than zero" in {
        val popu = TestGA.initialPopulation(genesPool, 20, 4)
        TestGA.keepElite(popu.toList, 5).toList must ===(popu.take(5).toList)
      }
    }

    "generate children" in {
      val popu = TestGA.initialPopulation(genesPool, 20, 4)
      TestGA.generateChildren(popu.toList, 15, _ => Letter.applyRandom).size mustBe 15
    }

    "correctly produce next generation" in {
      val popu = TestGA.initialPopulation(genesPool, 100, 4)

      val nextGen = TestGA.nextGeneration(popu, _ => Letter.applyRandom, 5)
    }
  }
}
