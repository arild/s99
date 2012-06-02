package s99

import org.specs2.mutable.Specification
import org.specs2.matcher._

class ArithmeticSpec extends Specification with ArithmeticSolutions {

  "Determine whether a given integer number is prime" >>
  { foreach(Seq(7, 13, 19)) { i => i must bePrime }
    foreach(Seq(4, 9, 51))  { i => i must not(bePrime) } }

  "Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm" >>
  { gcd(36, 63) === 9 }

  """ Determine whether two positive integer numbers are coprime
  Two numbers are coprime if their greatest common divisor equals 1""" >>
  { 35 must beCoprimeTo(64) }

  """ Calculate Euler's totient function phi(m)
  Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are
  coprime to m""" >>
  { 10.totient === 4 }

  """ Determine the prime factors of a given positive integer
  Construct a flat list containing the prime factors in ascending order""" >>
  { 315.primeFactors === List(3, 3, 5, 7) }

  """ Determine the prime factors of a given positive integer (2)
  Construct a list containing the prime factors and their multiplicity. Alternately, use a Map for the result""" >>
  { 315.primeFactorMultiplicityMap === Map(3 -> 2, 5 -> 1, 7 -> 1)
    315.primeFactorMultiplicity === List((3,2), (5,1), (7,1)) }

  """ Calculate Euler's totient function phi(m) (improved)
  See problem P34 for the definition of Euler's totient function. If the list of the prime factors of a number m is
  known in the form of problem P36 then the function phi(m>1) can be efficiently calculated as follows:
  Let [[p1, m1], [p2, m2], [p3, m3], ...] be the list of prime factors (and their multiplicities) of a given number m.
  Then phi(m) can be calculated with the following formula:

    phi(m) = (p1-1)*p1(m1-1) * (p2-1)*p2(m2-1) * (p3-1)*p3(m3-1) * ...

  Note that ab stands for the bth power of a.""" >>
  { 10.improvedTotient === 4 }

  """ Compare the two methods of calculating Euler's totient function
  Use the solutions of problems P34 and P37 to compare the algorithms. Try to calculate phi(10090) as an example""" >>
  { val (r1, t1) = withTime(10090.totient)
    val (r2, t2) = withTime(10090.improvedTotient)
    r1 === r2
    t1 must be_>(t2) }

  """ A list of prime numbers
  Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range""" >>
  {  listPrimesinRange(7 to 31) === List(7, 11, 13, 17, 19, 23, 29, 31) }

  """ Goldbach's conjecture
  Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
  E.g. 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the
  general case. It has been numerically confirmed up to very large numbers (much larger than Scala's Int can represent).
  Write a function to find the two prime numbers that sum up to a given even integer""" >>
  { 28.goldbach === (5, 23) }

  """ A list of Goldbach compositions
  Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach
  composition. In most cases, if an even number is written as the sum of two prime numbers, one of them is very small.
  Very rarely, the primes are both bigger than, say, 50. Try to find out how many such cases there are in the
  range 2..3000""" >>
  { printGoldbachList(9 to 20) === List(
      "9 = 2 + 7",
      "10 = 3 + 7",
      "11 = 11 + 0",
      "12 = 1 + 11",
      "13 = 2 + 11",
      "14 = 1 + 13",
      "15 = 2 + 13",
      "16 = 3 + 13",
      "17 = 17 + 0",
      "18 = 1 + 17",
      "19 = 2 + 17",
      "20 = 1 + 19")

    printGoldbachListLimited(1 to 2000, 50) === List(
      "992 = 73 + 919",
      "1382 = 61 + 1321",
      "1856 = 67 + 1789",
      "1928 = 61 + 1867") }

  def bePrime: Matcher[Int]             = (i: Int) => (i.isPrime, i+" is not prime")
  def beCoprimeTo(j: Int): Matcher[Int] = (i: Int) => (i.isCoprimeTo(j), i+" is not coprime to"+j)

  /**
   * @return the result of a computation with the time in millis to compute it
   */
  def withTime[T](t: =>T): (T, Long) = {
    val start = System.currentTimeMillis()
    (t, System.currentTimeMillis() - start)
  }
}
