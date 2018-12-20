# Genetic Algorithm in Scala

## Introduction
The purpose of this repo is to try out the Genetic Algorithm and implement it in Scala.

The problem we want to cover is allowing a population to self evolve to finally match the expected word `BIRD`.

For that, we will have to implement some logic to calculate the fitness score for a population, the selection and mating strategies.


I started with an example case with:
* a population of words of 4 letters randomly generated
* a simple fitness score calculation, counting the correct letters matching the correct word `BIRD`
* a selection of the best words based on roulette wheel logic
* an implementation of the elite strategy
* a mating logic based on crossover to generate the next population
* an implementation of the mutation rate

## Run test on implementation example
Tests are run only on the imperative code but the code is similar in the functional one

```sbt test```

## Run program on functional example
```sbt run```

## Conclusion
This is just an example of a Genetic Algorithm, the numbers used in `Program` are not optimal.
It would be interesting to run many times the Genetic Algorithm with different values for the number of generation and the mutation rate.

I am starting to have more interest in Functional Programming in Scala hence the functional implementation that allows more modularity in the code.

I find the Genetic Algorithm very interesting, there are other algorithm like Reinforcement Learning that is worth reading in the field of Machine Learning. 