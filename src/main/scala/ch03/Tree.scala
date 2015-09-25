package ch03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Ex. 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Branch(x, y) => 1 + size(x) + size(y)
    case _ => 1
  }

  // Ex. 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(x, y) => maximum(x) max maximum(y)
  }

  // Ex. 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(x, y) => 1 + (depth(x) max depth(y))
  }

  // Ex. 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(x, y) => Branch(map(x)(f), map(y)(f))
  }

  // Ex. 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(x, y) => g(fold(x)(f)(g), fold(y)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((x, y) => 1 + (x max y))

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(x => Leaf(f(x)):Tree[B])((x, y) => Branch(x, y))

}

object TreeApp {

  def main(args: Array[String]): Unit = {

    import Tree._

    val t = Branch(Leaf(10), Branch(Leaf(20), Leaf(30)))

    println(s"Size is ${size(t)} or ${sizeViaFold(t)}")
    println(s"Maximum is ${maximum(t)} or ${maximumViaFold(t)}")
    println(s"Depth is ${depth(t)} or ${depthViaFold(t)}")
    println(map(t)("Val" + _))
    println(mapViaFold(t)("Val" + _))

  }

}
