package linguistica.stemmer.porter

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.Assertions._

class RuleSpec extends FunSuite with PorterStemmer {
  
  test("test that result of stem and apply is right") {
    val sses = Rule("SSES", "SS")
    assert(sses.stem(Word("CARESSES")) === Some(Word("CARE")))
    assert(sses(Word("CARESSES")) === Some(Word("CARESS")))
    
    val ies = Rule("IES", "I")
    assert(ies.stem(Word("PONIES")) === Some(Word("PON")))
    assert(ies(Word("PONIES")) === Some(Word("PONI")))
    assert(ies.stem(Word("TIES")) === Some(Word("T")))
    assert(ies(Word("TIES")) === Some(Word("TI")))
    
    val ss = Rule("SS", "SS")
    assert(ss.stem(Word("CARESS")) === Some(Word("CARE")))
    assert(ss(Word("CARESS")) === Some(Word("CARESS")))
  }
  
  test("test that stem or apply return None for unmatched suffix") {
    val sses = Rule("SSES", "SS")
    assert(sses.stem(Word("CONSES")) === None)
    assert(sses.apply(Word("CONSES")) === None)
  }
}