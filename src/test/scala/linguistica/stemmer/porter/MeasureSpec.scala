package linguistica.stemmer.porter

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.Assertions._

class MeasureSpec extends FunSuite with PorterStemmer {

  test("test that result of Word() should be 0") {
    assert(Word("TR").measure === 0)
    assert(Word("EE").measure === 0)
    assert(Word("TREE").measure === 0)
    assert(Word("Y").measure === 0)
    assert(Word("BY").measure === 0)
    assert(Word("O").measure === 0)
  }

  test("test that result of Word() should be 1") {
    assert(Word("TROUBLE").measure === 1)
    assert(Word("OATS").measure === 1)
    assert(Word("TREES").measure === 1)
    assert(Word("IVY").measure === 1)
    assert(Word("YAY").measure === 1)
  }

  test("test that result of Word() should be 2") {
    assert(Word("TROUBLES").measure === 2)
    assert(Word("PRIVATE").measure === 2)
    assert(Word("OATEN").measure === 2)
    assert(Word("ORRERY").measure === 2)
    assert(Word("YELLOW").measure === 2)
  }
}