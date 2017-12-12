
object PlayGround {

  import RNG._
  def main(args: Array[String]): Unit = {


    println(SimpleRNG(5).nextInt)

    println(ints(900)(SimpleRNG(45678)))

    def rollDie: Rand[Int] = nonNegativeLessThan(6)

    println(rollDie(SimpleRNG(5))._1)

  }
}
