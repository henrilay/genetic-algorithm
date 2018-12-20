package org.scalaga.GeneticAlgorithm

import scala.util.Random

trait Gene
object Gene {
  def apply[T <: Gene](f: Unit => T): T = f()
}

case class Chromosome[T <: Gene](genes: List[T])
object Chromosome {
  val MAX_NUMBER_OF_GENE: Int = 3

  def apply[T <: Gene](genes: List[T], chromosomeSize: Int = MAX_NUMBER_OF_GENE): Chromosome[T] = Chromosome(Random.shuffle(genes).take(chromosomeSize))
}

trait GeneticAlgorithm[T <: Gene] {
  implicit val ordering = new Ordering[(Double, Chromosome[T])] {
    override def compare(x: (Double, Chromosome[T]), y: (Double, Chromosome[T])): Int = {
      if (x._1 == y._1) {
        0
      } else if (x._1 < y._1) {
        1
      } else -1
    }
  }

  def initialPopulation(genes: List[T], popuSize: Int = 100, chromosomeSize: Int = Chromosome.MAX_NUMBER_OF_GENE): List[Chromosome[T]] = {
    List.fill(popuSize)(Chromosome(genes, chromosomeSize))
  }

  def fitness(chromosome: Chromosome[T]): Double = ???

  def evaluatePopulation(population: List[Chromosome[T]]): List[(Double, Chromosome[T])] = {
    population.map(chromosome => fitness(chromosome) -> chromosome).toList.sorted
  }

  def selection(rankedPopulation: List[(Double, Chromosome[T])], eliteSize: Int): List[(Double, Chromosome[T])] = {
    import scala.collection.mutable.ListBuffer

    val selected: ListBuffer[(Double, Chromosome[T])] = ListBuffer.empty[(Double, Chromosome[T])]

    //add lucky loosers to the selection (roulette wheel)
    val losers = rankedPopulation.drop(eliteSize)
    val sumFitness: Double = losers.map(_._1).sum
    if (sumFitness == 0.0) {
      rankedPopulation
    } else {
      lazy val remainingPopulation = rankedPopulation.size - eliteSize
      lazy val probFitness: List[(Double, Int)] = losers.map(_._1 / sumFitness).zipWithIndex
      lazy val probMax = probFitness.max._1

      for (i <- 0 to remainingPopulation if i < remainingPopulation) {
        val pick = Random.nextDouble * probMax
        val maybeLuckyLooser = Random.shuffle(probFitness).find { case (prob, _) => pick <= prob }
        selected += losers(maybeLuckyLooser.fold(Random.nextInt(losers.size))(_._2))
      }

      //add elite
      selected.++=:(rankedPopulation.take(eliteSize))

      selected.toList.sorted
    }
  }

  def orderedCrossover(parent1: Chromosome[T], parent2: Chromosome[T]): Chromosome[T] = {
    val (startGene, endGene): (Int, Int) = {
      import scala.util.Random
      import scala.annotation.tailrec

      @tailrec def getTupleIndex(index1: Int, index2: Int): (Int, Int) = {
        if (index1 != index2) {
          if (index1 < index2) (index1, index2) else (index2, index1)
        } else {
          getTupleIndex(Random.nextInt(parent1.genes.size), Random.nextInt(parent1.genes.size))
        }
      }

      getTupleIndex(Random.nextInt(parent1.genes.size), Random.nextInt(parent1.genes.size))
    }

    val childP1 = parent1.genes.slice(startGene, endGene)
    val childGenes = parent2.genes.slice(0, startGene) ++ childP1 ++ parent2.genes.slice(endGene, parent2.genes.size)

    Chromosome(childGenes)
  }

  def mutate(chromosome: Chromosome[T], applyGene: Unit => T, mutationRate: Double): Chromosome[T] = {
    val newGenes = chromosome.genes map { gene =>
      if (Random.nextDouble < mutationRate) {
        Gene.apply(applyGene)
      } else {
        gene
      }
    }

    Chromosome(newGenes)
  }

  def keepElite(selectedPopulation: List[Chromosome[T]], eliteSize: Int = 0): List[Chromosome[T]] = {
    selectedPopulation.take(eliteSize)
  }

  def generateChildren(selectedPopulation: List[Chromosome[T]], maxChildren: Int, applyGene: Unit => T, mutationRate: Double = 0.01): List[Chromosome[T]] = {

    val matingPool = Random.shuffle(selectedPopulation).toArray
    List.tabulate(maxChildren)(i => mutate(orderedCrossover(matingPool(i), matingPool(selectedPopulation.size - i - 1)), applyGene, mutationRate))
  }

  def nextGeneration(population: List[Chromosome[T]], applyGene: Unit => T, eliteSize: Int = 0, mutationRate: Double = 0.01): List[Chromosome[T]] = {
    //evaluate and select population
    val selectedPopulation = selection(evaluatePopulation(population), eliteSize).map(_._2)

    //retain elite for next generation
    keepElite(selectedPopulation, eliteSize) ++ generateChildren(selectedPopulation, selectedPopulation.size - eliteSize, applyGene, mutationRate)
  }
}
