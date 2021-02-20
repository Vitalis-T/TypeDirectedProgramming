package td
import td.RingTypeClass.CheckLaws._
import td.MonoidLaws._
import org.scalatest.funspec.AnyFunSpec

 class TypeClassesLawsSpec extends AnyFunSpec {
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

  describe("MonoidLaws") {
    it("should obey associativity law for combine func") {
      val result = associativeLaw("Functional", "Programming", " is cool")
      assert(result == true)
    }

    it("should obey associativity law for combine func(list)") {
      val result = associativeLaw(List(1, 2, 3, 4), List(5, 6, 7, 8), List(9, 10, 11, 12))
      assert(result == true)
    }
  }
}