package td.Rational

case class Rational(num: Int, denom: Int) {
	override def toString: String = 
		this.num + "/" + this.denom
}
