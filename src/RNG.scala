import state.State

trait RNG {
  def nextInt: (Int, RNG)
  val int: Rand[Int] = _.nextInt
}

type Rand[A] = State[RNG, A]

case class SimpleRNG(seed: Long) extends RNG{
  def nextInt:(Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match{
    case (Int.MinValue, rng: RNG) => (Int.MaxValue, rng)
    case ( i: Int, rng: RNG) => if(i<0)(-1 * i, rng)  else (i, rng)
  }

  /** primitive implementation
    * def double(rng: RNG): (Double, RNG) = {
    * val tuple = nonNegativeInt(rng)
    * (tuple._1 / Int.MaxValue, tuple._2)
    * }
    */

  def double(rng: RNG): (Double, RNG) = map(nonNegativeInt) (x => x.toDouble / Int.MaxValue)(rng)


  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val intRng = rng.nextInt
    val rng2 = double(intRng._2)
    ((intRng._1, rng2._1),rng2._2)
  }


  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val reversedTuple = intDouble(rng)
    ((reversedTuple._1._2, reversedTuple._1._1), reversedTuple._2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
     val double1 = double(rng)
    val double2 = double(double1._2)
    val double3 = double(double2._2)
    ((double1._1, double2._1, double3._1),double3._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    getInts(count)(Nil,rng)
  }

    def getInts(count: Int)(intsUntilNow: List[Int], rngNow: RNG): (List[Int], RNG) = {
      count match {
        case c: Int if (c == 0) => (intsUntilNow, rngNow)
        case _ =>
          val int = rngNow.nextInt
          getInts(count - 1)(int._1 :: intsUntilNow, int._2)
      }
    }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a,rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a,b), rng2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((f, acc) => map2(f,acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a,state) = f(rng)
    g(a)(state)

  }

  def mapWflatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
  def map2WflatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))
}