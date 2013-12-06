package linguistica.stemmer.porter

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.Assertions._

class PrecomputeSpec extends FunSuite with PorterStemmer {

  import LetterType._

  test("test that precompute returns everything right") {
    assert(precompute("TR") === List(('T', Consonant), ('R', Consonant)))
    assert(precompute("TREE") === List(('T', Consonant), ('R', Consonant), ('E', Vowel), ('E', Vowel)))
    assert(precompute("YEL") === List(('Y', Consonant), ('E', Vowel), ('L', Consonant)))
    assert(precompute("HEY") === List(('H', Consonant), ('E', Vowel), ('Y', Consonant)))
    assert(precompute("BY") === List(('B', Consonant), ('Y', Vowel)))
  }
}