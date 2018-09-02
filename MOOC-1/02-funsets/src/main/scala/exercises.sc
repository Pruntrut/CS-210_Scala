import math.abs

def sum(f: Int => Int)(a: Int, b: Int): Int = mapReduce((x, y) => x+y, 0, f)(a, b)

def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce((x, y) => x*y, 1, f)(a, b)

def mapReduce(op: (Int, Int) => Int, id: Int, f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) id else op(f(a), mapReduce(op, id, f)(a + 1, b))

sum(identity)(1, 3)

// ====================================== //

val tolerance = 0.0001

def isCloseEnough(x: Double, y: Double) =
  (abs((x - y) / x) / x) < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
  def iterate(guess: Double): Double = {
    val next = f(guess)

    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }

  iterate(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2

def sqrt(x: Double): Double = fixedPoint(averageDamp(x => 1 + x/2))(1)

// ===================================== //

class Rational(x: Int, y: Int) {
  require(y > 0, "Denominator must be positive")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)

  val numer: Int = x / g
  val denom: Int = y / g

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this < that) that else this

  def +(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)

  def neg: Rational = new Rational(-numer, denom)

  def - (that: Rational) = this + that.neg

  override def toString: String = {
    numer + "/" + denom
  }
}
