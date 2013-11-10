package linguistica.stemmer.porter

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.Assertions._

class MeasureSpec extends FunSuite with PorterStemmer {

  test("test that result of measure() should be 0") {
    assert(measure("TR") === 0)
    assert(measure("EE") === 0)
    assert(measure("TREE") === 0)
    assert(measure("Y") === 0)
    assert(measure("BY") === 0)
    assert(measure("O") === 0)
  }

  test("test that result of measure() should be 1") {
    assert(measure("TROUBLE") === 1)
    assert(measure("OATS") === 1)
    assert(measure("TREES") === 1)
    assert(measure("IVY") === 1)
    assert(measure("YAY") === 1)
  }

  test("test that result of measure() should be 2") {
    assert(measure("TROUBLES") === 2)
    assert(measure("PRIVATE") === 2)
    assert(measure("OATEN") === 2)
    assert(measure("ORRERY") === 2)
    assert(measure("YELLOW") === 2)
  }
}