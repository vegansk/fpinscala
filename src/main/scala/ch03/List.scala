package ch03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))

  def head[A](as: List[A]): Option[A] = as match {
    case Cons(x, _) => Some(x)
    case _ => None
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Cons(_, xs) => xs
    case _ => Nil
  }

  def setTail[A](as: List[A], a: A): List[A] = as match {
    case Cons(_, xs) => Cons(a, xs)
    case _ => Nil
  }

  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Cons(_, xs) => if(n <= 0) xs else drop(xs, n -1)
    case _ => Nil
  }

  def dropWhile[A](as: List[A])(p: A => Boolean): List[A] = as match {
    case Cons(x, xs) => if(p(x)) dropWhile(xs)(p) else as
    case _ => Nil
  }

  def init[A](as: List[A]): List[A] = as match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case _ => Nil
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    case Nil => z
  }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    case Nil => z
  }

  def sum2(as: List[Int]) = foldRight(as, 0)(_ + _)
  def sum3(as: List[Int]) = foldLeft(as, 0)(_ + _)
  def product2(as: List[Double]) = foldRight(as, 1.0)(_ * _)
  def product3(as: List[Double]) = foldLeft(as, 1.0)(_ * _)
  def length[A](as: List[A]) = foldRight(as, 0)((_, y) => 1 + y)
  def length2[A](as: List[A]) = foldLeft(as, 0)((x, _) => x + 1)

  // Ex 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft2(as, Nil:List[A])((xs, x) => Cons(x, xs))

  // Ex 3.13
  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((x, g) => b => g(f(b, x)))(z)

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, x) => b => g(f(x, b)))(z)

  // Ex 3.14
  def append[A](xs: List[A], ys: List[A]): List[A] = foldRight2(xs, ys)(Cons(_, _))

  // Ex 3.15
  def concat[A](as: List[List[A]]): List[A] = foldRight2(as, Nil:List[A])(append)

  // Ex 3.16
  def incInts(as: List[Int]): List[Int] = foldRight(as, Nil:List[Int])((x, xs) => Cons(x + 1, xs))

  // Ex 3.17
  def stringifyDoubles(as: List[Double]): List[String] = foldRight(as, Nil:List[String])((x, xs) => Cons(x.toString, xs))

  // Ex 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil:List[B])((x, xs) => Cons(f(x), xs))

  // Ex 3.19
  def filter[A](as: List[A])(p: A => Boolean): List[A] = foldRight(as, Nil:List[A])((x, xs) => if(p(x)) Cons(x, xs) else xs)

  // Ex 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(map(as)(f), Nil:List[B])(append)

  // Ex 3.21
  def filterWithFlatMap[A](as: List[A])(p: A => Boolean): List[A] = flatMap(as)(x => if(p(x)) List(x) else Nil)

  // Ex 3.22
  def addLists(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addLists(xs, ys))
    case _ => Nil
  }

  // Ex 3.23
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    case _ => Nil
  }

  // Ex 3.24
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Cons(x, xs), Cons(y, ys)) if(x == y) => startsWith(xs, ys)
    case _ => false
  }
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    dropWhile(sup)(_ != head(sub).get) match {
      case Cons(x, xs) => startsWith(xs, tail(sub))
      case _ => false
    }
  }

}

object App {
  def main(args: Array[String]) {
    import List._
    println(dropWhile(List(1,2,3,4,5))(_ < 3))
    println(hasSubsequence(List(1,2,3,4,5), List(2,3)))
  }
}
 
