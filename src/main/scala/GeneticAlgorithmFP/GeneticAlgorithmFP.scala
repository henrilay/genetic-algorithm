package GeneticAlgorithmFP

trait GeneDSL[A] {
  def mutate(gene: A): A
}
object GeneDSL {
  def mutate[A : GeneDSL](gene: A): A = implicitly[GeneDSL[A]].mutate(gene)
}

trait FitnessDSL[A] {
  def fitness(individual: A): Double
}
object FitnessDSL {
  def fitness[A : FitnessDSL](individual: A): Double = implicitly[FitnessDSL[A]].fitness(individual)
}

trait MatingDSL[A] {
  def mating(parent1: A, parent2: A): A
}
object MatingDSL {
  def mating[A : MatingDSL](parent1: A, parent2: A): A = implicitly[MatingDSL[A]].mating(parent1, parent2)
}

trait ReBirthDSL[A] {
  def reBirth(individual: A, mutationRate: Double): A
}
object ReBirthDSL{
  def reBirth[A : ReBirthDSL](individual: A, mutationRate: Double): A = implicitly[ReBirthDSL[A]].reBirth(individual, mutationRate)
}

trait SelectionDSL[A, B] {
  type Score = (Double, B)
  implicit val ordering: Ordering[Score] = new Ordering[Score] {
    override def compare(x: Score, y: Score): Int = {
      if (x._1 == y._1) {
        0
      } else if (x._1 < y._1) {
        1
      } else -1
    }
  }

  def evaluate(population: A): List[Score]
  def selection(eliteSize: Int, scores: List[Score]): List[B]
}
object SelectionDSL {
  def evaluate[A, B](population: A)(implicit select: SelectionDSL[A, B]): List[SelectionDSL[A, B]#Score] = select.evaluate(population)
  def selection[A, B](eliteSize: Int, scores: List[SelectionDSL[A, B]#Score])(implicit select: SelectionDSL[A, B]): List[B] = select.selection(eliteSize, scores)
}

trait GeneticAlgorithmDSL[A] {
  def init(size: Int): A
  def nextGeneration(population: A, eliteSize: Int, mutationRate: Double): A
}
object GeneticAlgorithmDSL {
  def init[A : GeneticAlgorithmDSL](size: Int): A = implicitly[GeneticAlgorithmDSL[A]].init(size)
  def nextGeneration[A : GeneticAlgorithmDSL](population: A, eliteSize: Int, mutationRate: Double): A = implicitly[GeneticAlgorithmDSL[A]].nextGeneration(population, eliteSize, mutationRate)
}

trait RunnerDSL[A, B] {
  def runGA(populationSize: Int, times: Int, eliteSize: Int, mutationRate: Double): List[SelectionDSL[A, B]#Score]
}
object RunnerDSL {
  def runGA[A, B](populationSize: Int, times: Int, eliteSize: Int, mutationRate: Double)(implicit runner: RunnerDSL[A, B]): List[SelectionDSL[A, B]#Score] = runner.runGA(populationSize, times, eliteSize, mutationRate)
}
