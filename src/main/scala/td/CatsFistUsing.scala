package td
import cats.Show._
import cats.instances.int._ // for Show and other type classes
import cats.instances.string._ // for Show and other type classes

// Defining custom instances
// Cats also provides a couple of convenient methods to simplify the process. There are two construction methods 
// on the companion object of `Show` that we can use to define instances for our own types:
object Show {
	// Convert a function to a `Show` instance:
	def show[A](f: A => String): cats.Show[A] = 
		???

}
