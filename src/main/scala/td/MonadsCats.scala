package td
import cats.syntax.either._
import td.CustomMonad._
import ErrorHandlingWithEither._
//import cats.syntax.functor._ // for map
//import cats.syntax.flatMap._ // for flatMap

/*
The monad type class is `cats.Monad`. Monad extends two other type classes: `FlatMap`, which provides the 'flatMap' method,
and `Applicative`, which provides 'pure'. Applicative also extends Functor, which gives every Monad a 'map' method
*/
object CustomMonad {
  type Id[A] = A
  // Simplified version of the `Monad` type class in Cats
  trait Monad[F[_]] {
    // `pure` abstracts over constructors, providing a way to create a new monadic context from a plain value.
    def pure[A](value: A): F[A]

    // `flatMap` provides the sequencing step, extracting the value from a context and generating 
    // the next context in the sequence.
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] = 
      flatMap(value)(x => pure(func(x)))
  }

  object Monad {
    def apply[F[_]: Monad](implicit m: Monad[F]): Monad[F] = m 

    // Identity Monad
    implicit val idMonad: Monad[Id] = 
      new Monad[Id] {
        def pure[A](value: A): Id[A] = value

        def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)

        override def map[A, B](value: Id[A])(func: A => B): Id[B] = func(value)
      }
  }
}

// One of the approaches used to implement fail-fast error handling is to define an algebraic data type to 
// represent errors that may occur in our program. It gives us a fixed set of expected error types and a 
// catch‐all for anything else that we didn’t expect. We also get the safety of exhaustivity checking on any pattern matching we do.
object ErrorHandlingWithEither {
  object Wrapper {
    sealed trait LoginError extends Product with Serializable

    final case class UserNotFound(username: String) extends LoginError

    final case class PasswordIncorrect(username: String) extends LoginError

    case object UnexpectedError extends LoginError
  }

  import Wrapper._
  case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit = 
    error match {
      case UserNotFound(u) =>
        println(s"User not found: $u")

      case PasswordIncorrect(u) =>
        println(s"Password incorrect: $u")

      case UnexpectedError => println("Unexpected error") 
    }
}

object MonadUses extends App {
  import cats.Eval
  import td.ErrorHandlingWithEither.Wrapper._
  /*
  def sunSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = 
    a.flatMap(x => b.map(y => x * x + y * y))
  */
  def bingo(implicit m: Monad[Id]): Unit = 
    println(s"Soso Pavliashvili: ${m.pure(44)}")


/*
This code fails to compile for two reasons:
  1.the compiler infers the type of the accumulator as Right instead of
Either;
  2.we didn’t specify type parameters for Right.apply so the compiler
infers the left parameter as Nothing.

  def countPositiveIncorrect(nums: List[Int]) = 
    nums.foldLeft(Right(0)){ (accumulator, num) =>
      if (num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative. Stopping!")
      }
    }
*/

  // Switching to `asRight` avoids both of these problems.
  def countPositive(nums: List[Int]) = 
    nums.foldLeft(0.asRight[String]){ (accumulator, num) => 
      if (num > 0) {
        accumulator.map(_ + 1)
      }
      else {
        Left("Negative! Stopping")
      }
    }

  val e1 = countPositive(List(3, 5, 6))
  val e2 = countPositive(List(4, 6, -5, 5))
  //println(s"e1: $e1, e2: $e2")

  // Error handling
  val result1: LoginResult = User("Vitali", "qwerty").asRight[LoginError]
  val result2: LoginResult = UserNotFound("Dave").asLeft[User]
  
  //result1.fold(handleError(_), println(_))
  //result2.fold(handleError(_), println(_))

  // The `Eval` Monad
  // Eval has a 'memoize' method that allows us to memoize a chain of computations. The result of the chain up to 
  // the call to memoize is cached, whereas calculations after the call retain their original semantics.
  // Eval.always == lazy and not-memoized evaluation.
  val saying: Eval[String] = Eval.always{ println("Step1"); "The cat" }
    .map { str => println("Step2"); s"$str sat on" }
    .memoize
    .map { str => println("Step3"); s"$str the mat." } 

  saying.value
  saying.value
  saying.value

  /*
  One useful property of Eval is that its `map` and `flatMap` methods are trampolined. This means we can nest calls 
  to map and flatMap arbitrarily without consuming stack frames.(stack safety)
    Consider function for calculating factorials. It is relatively easy to make this function stack overflow, but 
  we can rewrite the method using `Eval` to make it stack safe:
  */
  def factorial(n: BigInt): Eval[BigInt] = 
    if (n == 1) 
      Eval.now(n)
    else
      Eval.defer{ factorial(n-1).map(_ * n) }

  // factorial(5000).value

 // Stack-safe `foldRight` method.
  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
    case head :: tail =>
      Eval.defer(fn(head, foldRight(tail, acc)(fn)))
    case Nil =>
      acc  
  }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = 
    foldRightEval(as, Eval.now(acc)){ (a, b) => 
      b.map(fn(a, _))
    }.value

  // foldRight((1 to 1000000).toList, 0L)(_ + _)
}