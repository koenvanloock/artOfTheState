
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def badInts(count: Int)(rng: RNG) = if (count <= 0) {
    (List(), rng)
  } else {
    val (x, r1) = rng.nextInt
    val (xs, r2) = ints(count - 1)(r1)
    (x :: xs, r2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    getInts(count)(Nil, rng)
  }

  def getInts(count: Int)(intsUntilNow: List[Int], rngNow: RNG): (List[Int], RNG) = {
    count match {
      case c: Int if (c == 0) => (intsUntilNow, rngNow)
      case _ =>
        val int = rngNow.nextInt
        getInts(count - 1)(int._1 :: intsUntilNow, int._2)
    }
  }

}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, rng: RNG) => (Int.MaxValue, rng)
    case (i: Int, rng: RNG) => if (i < 0) (-1 * i, rng) else (i, rng)
  }

  def double(rng: RNG): (Double, RNG) = {

    val (nominator, rng1) = nonNegativeInt(rng)
    (nominator / (Int.MaxValue.toDouble + 1), rng1)
  }


  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val intRng = rng.nextInt
    val rng2 = double(intRng._2)
    ((intRng._1, rng2._1), rng2._2)
  }


  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val reversedTuple = intDouble(rng)
    ((reversedTuple._1._2, reversedTuple._1._1), reversedTuple._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val double1 = double(rng)
    val double2 = double(double1._2)
    val double3 = double(double2._2)
    ((double1._1, double2._1, double3._1), double3._2)
  }
}