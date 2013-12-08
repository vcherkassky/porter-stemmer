package linguistica.stemmer.porter

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.Assertions._

class RuleSpec extends FunSuite with PorterStemmer {

  def checkStem(rule: SimpleRule, word: String, stem: String) = rule.stem(Word(word)) === Some(Word(stem))
  def checkApply(rule: Rule, word: String, stem: String) = rule(Word(word)) === Some(Word(stem))

  test("test that result of stem and apply is right [step 1a]") {
    val sses = Rule("SSES", "SS")
    checkStem(sses, "CARESSES", "CARE")
    checkApply(sses, "CARESSES", "CARESS")

    val ies = Rule("IES", "I")
    checkStem(ies, "PONIES", "PON")
    checkApply(ies, "PONIES", "PONI")
    checkStem(ies, "TIES", "T")
    checkApply(ies, "TIES", "TI")

    val ss = Rule("SS", "SS")
    checkStem(ss, "CARESS", "CARE")
    checkApply(ss, "CARESS", "CARESS")
  }

  test("test that stem or apply return None for unmatched suffix") {
    val sses = Rule("SSES", "SS")
    assert(sses.stem(Word("CONSES")) === None)
    assert(sses.apply(Word("CONSES")) === None)
  }

  test("test that result of stem and apply is right [step 1b | first part]") {
    val eed_m_gt_0 = Rule("EED", "EE", Condition.measureIs(m => m > 0))
    checkStem(eed_m_gt_0, "FEED", "F")
    assert(eed_m_gt_0(Word("FEED")) === None)

    checkStem(eed_m_gt_0, "AGREED", "AGR")
    checkApply(eed_m_gt_0, "AGREED", "AGREE")

    val ed = Rule("ED", "", Condition.containsVowel)
    checkStem(ed, "RED", "R")
    assert(ed(Word("RED")) === None)

    checkStem(ed, "PLASTERED", "PLASTER")
    checkApply(ed, "PLASTERED", "PLASTER")

    checkStem(ed, "BLED", "BL")
    assert(ed(Word("BLED")) === None)

    val ing = Rule("ING", "", Condition.containsVowel)
    checkStem(ing, "MOTORING", "MOTOR")
    checkApply(ing, "MOTORING", "MOTOR")

    checkStem(ing, "SING", "S")
    assert(ing(Word("SING")) === None)
  }

  test("test that result of apply is right for complex rules [step 1b | second part]") {
    val doubleNotLsz = Rule.doubleToSingle(Condition.not(Condition.endCharIn("LSZ")))
    checkApply(doubleNotLsz, "HOPP", "HOP")
    checkApply(doubleNotLsz, "TANN", "TAN")
    assert(doubleNotLsz(Word("FALL")) === None)
    assert(doubleNotLsz(Word("HISS")) === None)
    assert(doubleNotLsz(Word("FIZZ")) === None)

    val m_gt_1_o = Rule("", "E", Condition.and(Condition.measureIs(_ == 1), Condition.endsCvcNotWxy))
    assert(m_gt_1_o(Word("FAIL")) === None)
    checkStem(m_gt_1_o, "FIL", "FIL")
    checkApply(m_gt_1_o, "FIL", "FILE")
  }
}