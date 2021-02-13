package td
import scala.language.implicitConversions
import td.JsonTypeClass.Json

object JsonTypeClass {

	// Define a very simple JSON AST(Abstract Syntax Tree)
	sealed trait Json
	final case class JsObject(get: Map[String, Json]) extends Json
	final case class JsString(get: String) extends Json
	final case class JsNumber(get: Double) extends Json
	final case object JsNull extends Json

	// Traits == type classes
	// The "serialize to JSON" behavior is encoded in this trait
	// 'JsonWriter' is our type class in this example, with Json and its subtypes providing supporting code
	trait JsonWriter[A] {
		def write(value: A): Json
	}

	// Implicit values == type class instances
	// The instances of a type class provide implementations of the type class for specific types we care about, 
	// which can include types from the Scala standard library and types from our domain model.

	final case class Person(name: String, email: String) // type for our imaginary "domain"
}

object JsonWriterInstances {
	import JsonTypeClass._
	implicit val stringWriter: JsonWriter[String] = 
		new JsonWriter[String] {
			def write(value: String): Json = JsString(value)
		}

	implicit val personWriter: JsonWriter[Person] = 
		new JsonWriter[Person] {
			def write(value: Person): Json = JsObject(Map(
				"name" -> JsString(value.name),
				"email" -> JsString(value.email)
			))
		}

	implicit val numWriter: JsonWriter[Double] = 
		new JsonWriter[Double] {
			def write(value: Double): Json = JsNumber(value)
		}
	// etc ...	

	// Recursive Implicit Resolution
	// This method constructs a JsonWriter for Option[A] by relying on an implicit parameter to fill in the 
	// A‐specific functionality.
	implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] = 
		new JsonWriter[Option[A]] {
			def write(option: Option[A]): Json = option match {
				case Some(value) => writer.write(value)
				case None => JsNull  
			}
		}
}

// Implicit parameters == Type class use
// A type class use is any functionality that requires a type class instance to work.
// In Scala this means any method that accepts instances of the type class as implicit parameters
// There are two ways it does this: 'Interface Objects' and 'Interface Syntax'.

// Interface Objects =>
object Json {
	import JsonTypeClass._
	def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = 
		w.write(value)
}

// Implicit classes == optional utilities that make type classes easier to use => 
// We can alternatively use extension methods to extend existing types with interface methods
object JsonSyntax {
	import JsonTypeClass._
	implicit class JsonWriterOps[A](value: A) {
		def toJson(implicit w: JsonWriter[A]): Json = 
			w.write(value)
	}

	// Recursive Implicit Resolution
	/*
	The power of 'type classes' and 'implicits' lies in the compiler’s ability to combine implicit definitions 
	when searching for candidate instances. This is sometimes known as type 'class composition'.
	Why would we construct instances from other instances? As a motivational example, consider defining a JsonWriter 
	for Option. We would need a JsonWriter[Option[A]] for every A we care about in our application.
	We could try to brute force the problem by creating a library of implicit vals:

	implicit val optionIntWriter: JsonWriter[Option[Int]] =
		???
	implicit val optionPersonWriter: JsonWriter[Option[Person]] =
	???

	and so on...

	However, this approach clearly doesn’t scale. We end up requiring two implicit vals for every type A 
	in our application: one for A and one for Option[A].

	Fortunately, we can abstract the code for handling Option[A] into a common constructor based on the instance for A.
	====> SEE IN JsonWriterInstances TRAIT


	*/
}

