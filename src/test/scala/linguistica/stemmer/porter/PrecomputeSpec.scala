package linguistica.stemmer.porter

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.Assertions._

class PrecomputeSpec extends FunSuite with PorterStemmer {

  import LetterType._

  test("test that precompute returns everything right") {
    assert(Word("TR").letters === List(('T', Consonant), ('R', Consonant)))
    assert(Word("TREE").letters === List(('T', Consonant), ('R', Consonant), ('E', Vowel), ('E', Vowel)))
    assert(Word("YEL").letters === List(('Y', Consonant), ('E', Vowel), ('L', Consonant)))
    assert(Word("HEY").letters === List(('H', Consonant), ('E', Vowel), ('Y', Consonant)))
    assert(Word("BY").letters === List(('B', Consonant), ('Y', Vowel)))
  }
}