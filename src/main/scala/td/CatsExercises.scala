package td
import cats.Eq
import cats.syntax.eq._ // for === and =!=
import cats.instances.string._ // for Eq
import cats.instances.int._ // for Eq
import cats.instances.option._
import cats.syntax.semigroup._
// import cats.kernel.Monoid

object CatEq {
  final case class Cat(name: String, age: Int, color: String)

  implicit val catEqual: Eq[Cat] = 
    Eq.instance[Cat] { (cat1, cat2) => 
      (cat1.name === cat2.name) && 
      (cat1.age === cat2.age) && 
      (cat1.color === cat2.color) }

  implicit val catOptionEqual: Eq[Option[Cat]] = 
    Eq.instance[Option[Cat]] { (op1, op2) => 
      (op1, op2) match {
        case (Some(cat1), Some(cat2)) => cat1 === cat2
        case (None, None) => true 
        case _ => false 
      }
    }

  def main(args: Array[String]): Unit = {
    val cat1 = Cat("Fedor", 6, "redhead")
    val cat2 = Cat("Pedor", 5, "black")
    // println(cat1 =!= cat2)
    val optionCat1 = Option(cat1)
    val optionCat2 = Option(cat2)
    val optionCat3 = Option.empty[Cat]
    println(optionCat3 === optionCat3)
  }
}

object MonoidInstances {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  case class Order(totalCost: Double, quantity: Double)

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) = 
      monoid

  implicit val intMonoid: Monoid[Int] = 
    new Monoid[Int] {
      def combine(x: Int, y: Int): Int = x + y
      def empty: Int = 0
    }

  implicit val stringMonoid: Monoid[String] = 
    new Monoid[String] {
      def combine(x: String, y: String): String = x ++ y
      def empty: String = ""
    }

  implicit val booleanAndMonoid: Monoid[Boolean] = 
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean): Boolean = x && y

      def empty: Boolean = true
    }

  implicit def listMonoid[A]: Monoid[List[A]] = 
    new Monoid[List[A]] {
      def combine(x: List[A], y: List[A]): List[A] = x ++ y

      def empty: List[A] = Nil
    } 

  implicit val booleanOrMonoid: Monoid[Boolean] = 
    new Monoid[Boolean]{
      def combine(x: Boolean, y: Boolean): Boolean = x || y

      def empty: Boolean = false
    }

  // Exclusive OR
  implicit val booleanEitherMonoid: Monoid[Boolean] = 
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)

      def empty: Boolean = false
    }

  // Exclusive NOR (the negation of exclusive or)
  implicit val booleanXnorMonoid: Monoid[Boolean] = 
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean): Boolean = 
        (!x || y) && (x || !y)

      def empty: Boolean = true
    }

  def add[A](items: List[A])(implicit m: Monoid[A]): A =
    items.foldLeft(m.empty)((x, y) => m.combine(x, y)) 

  // We can optionally use Scala’s 'context bound syntax' to write the same code in a shorter way:
  def add2[A: Monoid](items: List[A]): A = 
    items.foldLeft(Monoid[A].empty)((x, y) => Monoid[A].combine(x, y))

  implicit val orderMonoid: Monoid[Order] = 
    new Monoid[Order] {
      def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)

      def empty: Order = Order(0, 0)
    }
  }

  def main(args: Array[String]): Unit = {
    import Monoid._
    val order1 = Order(100.0, 5.0)
    val order2 = Order(200.0, 5.0)
    val order3 = Order(250.0, 10.0)
    val ordersList = List(order1, order2, order3)
    println(add(ordersList))
  }
}

object MonoidLaws {
  import MonoidInstances._

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean =
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) &&
      (m.combine(m.empty, x) == x)
  }
}



