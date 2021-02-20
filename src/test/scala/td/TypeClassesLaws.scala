package td
import td.RingTypeClass.CheckLaws._
import org.scalatest.funspec.AnyFunSpec

 class RingLawsSpec extends AnyFunSpec {
  describe("RingLaws") {
    it("should obey associativity law for addition") {
      val result = plusAssociativity(5, 10, 15)
      assert(result == true)
    }

    it("should obey associativity law for multiplication") {
      val result = multAssociativity(5, 10, 15)
      assert(result == true)
    }

    it("should obey inversion law") {
      val result = inverseIdentity(10)
      assert(result == true)
    }
  }
}