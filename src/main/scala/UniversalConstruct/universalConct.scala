package UniversalConstruct

object Category {
	// First example
	// val product: (A, B) = ???

	// pi1: product._1
	// pi2: product._2

	// h: C => (A, B)
	// f: C => A == (_._1) compose h -- implementation
	// g: C => B == (_._2) compose h -- implementation

	/* If we have a 'product' type and you want to define funct into this 'product'(mapping in) =>
	You can do this very easy by splitting it into two more easier problems => defining func 'f' and 'g'*/
	def fanout[C, A, B](f: C => A, g: C => B): C => (A, B) = 
		c => (f(c), g(c))

	def fanin[C, A, B](h: C => (A, B)): (C => A, C => B) = 
		(h(_)._1, h(_)._2)

	// Second example
	/* Mapping from Unit type to some type A is just pick one element in A 
	So, if we able to pick an element from A, and pick an element from B --> there is unique func 'h': Unit => A x B
	Summary: pick an element from product type (A, B) == A x B is equivalent to define next func: */
	val unit: Unit = ()

	// val x: Unit => A
	// val y: Unit => B
	// val z: Unit => (A, B) = fanout(x, y)

	// Third example
	/* You can lift a function using some kind of 'map'
	Bifunctor: it lifts two function by once
	Assume --> f : A => A`, g: B => B` ----> h: (A, B) => (A`, B`)
	*/
	def bimap[A, A1, B, B1](f: A => A1, g: B => B1): ((A, B)) => (A1, B1) =
		fanout(f compose (_._1), g compose(_._2)) 


	// 4 example - Sum type(For instance - Either[A, B]).Every time a Sum type there is unique mapping-out this type.
	// So, any time you want to define a fuction from a Sum it's enough to provide two hepler func
	// f: A => C == h compose Left(_)
	// g: B => C == h compose Right(_)
	// h: Either[A, B] => C
	def left[A](a: A): Either[A, Nothing] = Left(a)
	def right[B](b: B): Either[Nothing, B] = Right(b)

	def either[C, A, B](f: A => C, g: B => C): Either[A, B] => C = {
		case Left(a) => f(a)
		case Right(b) => g(b) 
	}

	def choice[C, A, B](h: Either[A, B] => C): (A => C, B => C) = {
		(a => h(Left(a)), b => h(Right(b)))
	}

	// (A + B) * C = (A * C) + (B * C) -- for types
	// We want func into type (A + B) * C
	// f: (A, C) => (Either[A, B], C) ----> f = bimap(left, id)
	// g: (B, C) => (Either[A, B], C) ----> g = bimap(right, id)
	def distRight[C, A, B]: Either[(A, C), (B, C)] => (Either[A, B], C) = 
		either(bimap(left(_), identity(_)), bimap(right(_), identity(_)))

}
