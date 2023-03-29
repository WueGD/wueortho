package wueortho.util

class State[S, +A] private (f: S => (S, A)):
  def map[B](m: A => B) = State: (s: S) =>
    val (s1, a) = f(s)
    s1 -> m(a)

  def flatMap[B](fm: A => State[S, B]) = State: (s: S) =>
    val (s1, a) = f(s)
    val (s2, b) = fm(a).run(s1)
    s2 -> b

  def run(s0: S)  = f(s0)
  def runA(s0: S) = run(s0)._2
  def runS(s0: S) = run(s0)._1

object State:
  def apply[S, A](f: S => (S, A))  = new State(f)
  def pure[S, A](a: A)             = State((s: S) => s -> a)
  def set[S](s: S): State[S, Unit] = State(_ => s -> ())
  def get[S]                       = State((s: S) => s -> s)
  def modify[S](f: S => S)         = State((s: S) => f(s) -> ())

  extension [S, A](ss: List[State[S, A]])
    def sequence = ss.foldLeft(State.pure[S, List[A]](Nil))((s, i) => s.flatMap(l => i.map(a => a :: l))).map(_.reverse)
  extension [S, A](ss: Vector[State[S, A]])
    def sequence = ss.foldLeft(State.pure[S, Vector[A]](Vector.empty))((s, i) => s.flatMap(l => i.map(a => l :+ a)))
