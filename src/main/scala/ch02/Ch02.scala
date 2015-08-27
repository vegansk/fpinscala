object Ch02 {

  def isSorted[A](xs: Seq[A], p: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def sorted(i: Int): Boolean = 
      if(i > xs.length - 2) true
      else {
        if(p(xs(i), xs(i+1))) sorted(i+1)
        else false
      }
    sorted(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A,B,C](f: A => B, g: B => C): A => C = a => g(f(a))

  def main(args: Array[String]): Unit =
    println(isSorted[Int](Array(2,2), _ < _))
}
