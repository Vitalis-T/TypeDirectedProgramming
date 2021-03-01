package td.Ordering
import td.Rational._
//import math.Ordering
trait Ordering[A] {
  def compare(a1: A, a2: A): Int
}

// Instances of the Ordering type class
object Ordering {
  implicit val Int: Ordering[Int] = 
    new Ordering[Int] {
      def compare(x: Int, y: Int) = if (x < y) -1 else if (x > y) 1 else 0
    }

  implicit val String: Ordering[String] = 
    new Ordering[String] {
      def compare(s: String, t: String) = s.compareTo(t)
    }

  implicit val Rational: Ordering[Rational] = new Ordering[Rational] {
    def compare(q: Rational, r: Rational): Int = 
      q.num * r.denom - r.num * q.denom
  }

  implicit def orderingList[A](implicit ord: Ordering[A]): Ordering[List[A]] =
    new Ordering[List[A]] {
      def compare(xs: List[A], ys: List[A]) =
        (xs, ys) match {
          case (x :: xsTail, y :: ysTail) =>
            val c = ord.compare(x, y)
            if (c != 0) c else compare(xsTail, ysTail)
          case (Nil, Nil) => 0
          case (_, Nil)   => 1
          case (Nil, _)   => -1
        }
    }
  implicit def orderingPair[A, B](implicit orderingA: Ordering[A], orderingB: Ordering[B]): Ordering[(A, B)] =
    new Ordering[(A, B)] {
      def compare(pair1: (A, B), pair2: (A, B)): Int = {
        val firstCriteria = orderingA.compare(pair1._1, pair2._1)
        if (firstCriteria != 0) firstCriteria
        else orderingB.compare(pair1._2, pair2._2)
      }
    }
}


object mergeSort {
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case(x :: xs1, y :: ys1) =>
          if (ord.compare(x, y) <= 0) x :: merge(xs1, ys)
          else y :: merge(xs, ys1) 
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  def seqSort[A, B](elements: Seq[A])(critera: A => B)(implicit ord: Ordering[B]): Seq[A] = {
    val n = elements.length / 2
    if (n == 0) elements
    else {
      def merge(xs: Seq[A], ys: Seq[A]): Seq[A] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x +: xs1, y +: ys1) =>
          if (ord.compare(critera(x), critera(y)) <= 0) x +: merge(xs1, ys)
          else y +: merge(xs, ys1) 
      }
      val (fst, snd) = elements splitAt n
      merge(seqSort(fst)(critera), seqSort(snd)(critera))
    }
  }

}
