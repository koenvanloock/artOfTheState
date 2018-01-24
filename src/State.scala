
object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))
  }
}

case class State[S, +A](run: S => (A, S)) {
  import State._

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s1) = run(s)
    f(a).run(s1)
  }

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
}