package linguistica.stemmer.porter

object StemmerEngine extends PorterStemmer {

  import Condition._
  
  object Step1a {
    val sses = Rule("SSES", "SS")
    val ies = Rule("IES", "I")
    val ss = Rule("SS", "SS")
    val s = Rule("S", "")
  }
  
  object Step1b {
    val m_gt_0_eed = Rule("EED", "ED", measureIs(_ > 0))
    val vowel_ed = Rule("ED", "", containsVowel)
    val vowel_ing = Rule("ING", "", containsVowel)
    
    object EdIngSuccessful {
      val at = Rule("AT", "ATE")
      val bl = Rule("BL", "BLE")
      val iz = Rule("IZ", "IZE")
      val doubleNotLsz = Rule.doubleToSingle(not(endCharIn("LSZ")))
      val m_gt_1_o = Rule("", "E", and(measureIs(_ == 1), endsCvcNotWxy))
    }
  }
}