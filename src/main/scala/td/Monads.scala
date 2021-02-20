package td
object MonadsForBegginers {
  case class SafeValue[+T](private val internalValue: T) {
    def get: T = synchronized {
      // does sth interesting
      internalValue
    }

    def transform[U](transformer: T => SafeValue[U]): SafeValue[U] = 
      transformer(internalValue)
  }

  // external API
  def gimmeSafeValue[T](value: T): SafeValue[T] = SafeValue(value)

  val safeString = gimmeSafeValue("Scala is cool")
  // extract
  val string = safeString.get
  // transform
  val upperString = string.toUpperCase()
  //wrap
  val upperSafeString = SafeValue(upperString)
  // ETW - extract-transform-wrap pattern

  val upperSafeString2 = safeString.transform(s =>  SafeValue(s.toUpperCase())) // concise  ETW
}