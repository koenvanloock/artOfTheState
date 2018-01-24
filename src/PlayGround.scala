
object PlayGround {

  type Rand[A] = State[RNG, A]


  def main(args: Array[String]): Unit = {

    println(State[Unit, String](state => ("Hello", state))
      .map(_ + " world!")
      .run(())._1)


    val randomGenerator = State[RNG, Int](rng => rng.nextInt)

    println(randomGenerator.run(SimpleRNG(2345678L)))
    println(randomGenerator.run(SimpleRNG(2345678L)))

    println(State.sequence(List(
      State[RNG, Int](_.nextInt),
      State[RNG, Int](_.nextInt),
      State[RNG, Int](_.nextInt),
      State(RNG.double3),
      State[RNG, Int](_.nextInt)
    )).run(SimpleRNG(34567890L))._1)


    val test = for {
      a <- randomGenerator
      b <- State(RNG.double3)
    } yield (a, b)


    val test2 = randomGenerator
      .flatMap{ a =>
        State(RNG.double3).map(b => (a,b))
    }

    println(test.run(SimpleRNG(456789l)))
    println(test2.run(SimpleRNG(456789l)))
  }
}
