sealed trait Option[+T] {

  // Ex. 4.1
  def map[U](f: T => U): Option[U] = this match {
    case Some(v) => Some(f(v))
    case _ => None
  } 

  def flatMap[U](f: T => Option[U]): Option[U] = this match {
    case Some(v) => f(v)
    case _ => None
  }

  def getOrElse[U >: T](default: => U): U = this match {
    case Some(v) => v
    case _ => default
  }

  def orElse[U >: T](ou: => Option[U]): Option[U] = this match {
    case Some(v) => this
    case _ => ou
  }

  def filter(p: T => Boolean): Option[T] = this match {
    case Some(v) if p(v) => this
    case _ => None
  }

}

case class Some[+T](get: T) extends Option[T]
case object None extends Option[Nothing]

object Option {

  def lift[T,U](f: T => U): Option[T] => Option[U] = _ map f

  // Ex. 4.2
  def mean(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }
  }

  // Ex. 4.3
  def map2[T,U,V](x: Option[T], y: Option[U])(f: (T, U) => V): Option[V] = (x, y) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _ => None
  }
}

object OptionApp extends App {

  import Option._

  val xs = Seq(1.0, 2.0, 3.0, 4.0, 5.0)

  println(mean(xs))
  println(variance(xs))

  println(map2(Some(1), Some(2))(_ + _))
}
