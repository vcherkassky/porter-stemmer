package linguistica.stemmer.porter

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.Assertions._

class RuleSpec extends FunSuite with PorterStemmer {
  
  test("test that result of stem and apply is right") {
    val sses = Rule("SSES", "SS")
    assert(sses.stem("CARESSES") === Some("CARE"))
    assert(sses.apply("CARESSES") === Some("CARESS"))
    
    val ies = Rule("IES", "I")
    assert(ies.stem("PONIES") === Some("PON"))
    assert(ies.apply("PONIES") === Some("PONI"))
    assert(ies.stem("TIES") === Some("T"))
    assert(ies.apply("TIES") === Some("TI"))
    
    val ss = Rule("SS", "SS")
    assert(ss.stem("CARESS") === Some("CARE"))
    assert(ss.apply("CARESS") === Some("CARESS"))
  }
  
  test("test that stem or apply return None for unmatched suffix") {
    val sses = Rule("SSES", "SS")
    assert(sses.stem("CONSES") === None)
    assert(sses.apply("CONSES") === None)
  }
}