package td
import cats.Functor

object TreeFunctor {
  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    implicit val treeFunctor: Functor[Tree] = 
      new Functor[Tree] {
        def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
          case Leaf(value) => Leaf(f(value))
          case Branch(l, r) => Branch(map(l)(f), map(r)(f))   
        }
      }

    implicit class FunctorOps[F[_], A](src: F[A]) {
      def map[B](func: A => B)(implicit f: Functor[F]): F[B] = 
        f.map(src)(func)
    }

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = 
      Branch(left, right)

    def leaf[A](value: A): Tree[A] = Leaf(value)
  }
}