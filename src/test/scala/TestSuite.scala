import org.scalatest._


class TestSuite extends FunSuite with DiagrammedAssertions {
	test("Hello should start with H") {
		assert("Hello".startsWith("H"))
	}

	test("An empty Set should have size 0") {
    assert(Set.empty.size == 0)
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    assertThrows[NoSuchElementException] {
      Set.empty.head
    }
  }

}