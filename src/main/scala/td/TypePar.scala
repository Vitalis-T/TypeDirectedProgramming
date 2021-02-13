package td.FuncQueue
trait Queue[T] {
	def head: T
	def tail: Queue[T]
	def enqueue(x: T): Queue[T]
}

/* Companion object has the same access rights as its class. 
Because of this, the 'apply' method in object Queue can create a new Queue object.
Clients can create queues with an expression such as Queue(1, 2, 3).
This expression expands to Queue.apply(1, 2, 3) since Queue is an object instead of a function.*/
object Queue {
	def apply[T](xs: T*):Queue[T] = new QueueImpl[T](xs.toList, Nil)

	private class QueueImpl[T](
		private val leading: List[T],
		private val trailing: List[T]
	) extends Queue[T]
	{
		private def mirror = 
			if (leading.isEmpty)
				new QueueImpl(trailing.reverse, Nil)
			else
				this

		def head: T = mirror.leading.head
		def tail: QueueImpl[T] = {
			val q = mirror
			new QueueImpl(q.leading.tail, q.trailing)
		}

		def enqueue(x: T) = 
			new QueueImpl(leading, x :: trailing)
	}


	def search(xs: List[Int], x: Int): Any = {
		val l = xs.length
		var i = 0
		var res: Any = null
		while (i < l) {
			if (xs(i) == x)
				res = i
			i += 1
		}
		res
	}
	def search2(xs: List[Int], x: Int): Any = {
		def helper(xs: List[Int], x: Int, idx: Int): Any = xs match {
			case Nil => null
			case (y :: ys) if (x == y) => idx
			case (y :: ys) => helper(ys, x, idx + 1)  
		}
		helper(xs, x, 0)
	}
}

