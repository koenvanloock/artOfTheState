package com.tothepoint.artofthestate


object PlayGround {

  import RNG._
  def main(args: Array[String]): Unit = {


    println(SimpleRNG(5).nextInt)

    //println(badInts(800990)(SimpleRNG(45678)))
    println(ints(800990)(SimpleRNG(45678)))
  }
}
