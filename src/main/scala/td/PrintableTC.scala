package td
import scala.language.implicitConversions

object PrintableTypeClass {

  trait Printable[A] { self =>
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] = 
      new Printable[B] {
        def format(value: B): String = 
          self.format(func(value))
      }
  }

  final case class Cat(name: String, age: Int, color: String)

  final case class Box[A](value: A)

  object PrintableInstances {

    // Pure functional approach
    implicit def boxPrintable[A](implicit printer: Printable[A]): Printable[Box[A]] =
      printer.contramap[Box[A]](self => self.value)


    implicit val booleanPrintable: Printable[Boolean] = 
      new Printable[Boolean] {
        def format(value: Boolean): String = 
          if (value) "yes"
          else "no"
      }

    implicit val intPrintable: Printable[Int] = 
      new Printable[Int] {
        def format(value: Int): String = s"$value"
      }

    implicit val strPrintable: Printable[String] = 
      new Printable[String] {
        def format(value: String): String = value
      }

    implicit val catPrintable: Printable[Cat] = 
      new Printable[Cat] {
        def format(value: Cat): String = {
          val name = Printable.format(value.name)
          val age = Printable.format(value.age)
          val color = Printable.format(value.color)
          s"$name is a $age year-old $color cat."
        }
      }
  }

  object Printable {
    def format[A](value: A)(implicit printer: Printable[A]): String = 
      printer.format(value)

    def print[A](value: A)(implicit printer: Printable[A]): Unit = 
      println(format(value))
  }

  object PrintableSyntax {
    implicit class PrintableOps[A](value: A) {
      def format(implicit p : Printable[A]): String = 
        p.format(value)

      def print(implicit p: Printable[A]): Unit = 
        println(format(p))
    }
  }
}

