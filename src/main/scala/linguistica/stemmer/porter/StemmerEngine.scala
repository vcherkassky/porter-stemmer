package linguistica.stemmer.porter

trait StemmerEngine extends PorterStemmer with Stemmer {

  import Condition._

  val step1a = List(
    Rule("SSES", "SS"),
    Rule("IES", "I"),
    Rule("SS", "SS"),
    Rule("S", ""))

  val step1b_simple = List(Rule("EED", "ED", measureIs(_ > 0)))

  val step1b_complex_first = List(
    Rule("ED", "", containsVowel),
    Rule("ING", "", containsVowel))

  val step1b_complex_second = List(
    Rule("AT", "ATE"),
    Rule("BL", "BLE"),
    Rule("IZ", "IZE"),
    Rule.doubleToSingle(not(endCharIn("LSZ"))),
    Rule("", "E", and(measureIs(_ == 1), endsCvcNotWxy)))

  val step1c = List(Rule("Y", "I", containsVowel))

  val step2 = List(
    Rule("ATIONAL", "ATE", measureIs(_ > 0)),
    Rule("TIONAL", "TION", measureIs(_ > 0)),
    Rule("ENCI", "ENCE", measureIs(_ > 0)),
    Rule("ANCI", "ANCE", measureIs(_ > 0)),
    Rule("IZER", "IZE", measureIs(_ > 0)),
    Rule("ABLI", "ABLE", measureIs(_ > 0)),
    Rule("ALLI", "AL", measureIs(_ > 0)),
    Rule("ENTLI", "ENT", measureIs(_ > 0)),
    Rule("ELI", "E", measureIs(_ > 0)),
    Rule("OUSLI", "OUS", measureIs(_ > 0)),
    Rule("IZATION", "IZE", measureIs(_ > 0)),
    Rule("ATION", "ATE", measureIs(_ > 0)),
    Rule("ATOR", "ATE", measureIs(_ > 0)),
    Rule("ALISM", "AL", measureIs(_ > 0)),
    Rule("IVENESS", "IVE", measureIs(_ > 0)),
    Rule("FULNESS", "FUL", measureIs(_ > 0)),
    Rule("OUSNESS", "OUS", measureIs(_ > 0)),
    Rule("ALITI", "AL", measureIs(_ > 0)),
    Rule("IVITI", "IVE", measureIs(_ > 0)),
    Rule("BILITI", "BLE", measureIs(_ > 0)))

  val step3 = List(
    Rule("ICATE", "IC", measureIs(_ > 0)),
    Rule("ATIVE", "", measureIs(_ > 0)),
    Rule("ALIZE", "AL", measureIs(_ > 0)),
    Rule("ICITI", "IC", measureIs(_ > 0)),
    Rule("ICAL", "IC", measureIs(_ > 0)),
    Rule("FUL", "", measureIs(_ > 0)),
    Rule("NESS", "", measureIs(_ > 0)))

  val step4 = List(
    Rule("AL", "", measureIs(_ > 1)),
    Rule("ANCE", "", measureIs(_ > 1)),
    Rule("ENCE", "", measureIs(_ > 1)),
    Rule("ER", "", measureIs(_ > 1)),
    Rule("IC", "", measureIs(_ > 1)),
    Rule("ABLE", "", measureIs(_ > 1)),
    Rule("IBLE", "", measureIs(_ > 1)),
    Rule("ANT", "", measureIs(_ > 1)),
    Rule("EMENT", "", measureIs(_ > 1)),
    Rule("MENT", "", measureIs(_ > 1)),
    Rule("ENT", "", measureIs(_ > 1)),
    Rule("ION", "", and(measureIs(_ > 1), endCharIn("ST"))),
    Rule("OU", "", measureIs(_ > 1)),
    Rule("ISM", "", measureIs(_ > 1)),
    Rule("ATE", "", measureIs(_ > 1)),
    Rule("ITI", "", measureIs(_ > 1)),
    Rule("OUS", "", measureIs(_ > 1)),
    Rule("IVE", "", measureIs(_ > 1)),
    Rule("IZE", "", measureIs(_ > 1)))

  val step5a = List(
    Rule("E", "", measureIs(_ > 1)),
    Rule("E", "", and(measureIs(_ == 1), not(endsCvcNotWxy))))

  val step5b = List(Rule.doubleToSingle(and(measureIs(_ > 1), endsWith("L"))))

  def applyRules(word: Word, rules: List[Rule]): Option[Word] = {
    def loop(word: Word, prev: Option[Word], rules: List[Rule]): Option[Word] = rules match {
      case Nil => prev
      case r :: rs => prev match {
        case None => loop(word, r(word), rs)
        case some => some
      }
    }
    loop(word, rules.head(word), rules.tail)
  }

  def applyStep(word: Word, rules: List[Rule]): Word = applyRules(word, rules) match {
    case None => word
    case Some(w) => w
  }

  def stem(word: String): String = {
    try {
      var w = Word(word)
      w = applyStep(w, step1a)
      w = applyStep(w, step1b_simple)
      w = applyRules(w, step1b_complex_first) match {
        case None => w
        case Some(wrd) => applyStep(wrd, step1b_complex_second)
      }
      w = applyStep(w, step1c)
      w = applyStep(w, step2)
      w = applyStep(w, step3)
      w = applyStep(w, step4)
      w = applyStep(w, step5a)
      w = applyStep(w, step5b)

      w.text
    } catch {
      case t: Throwable => System.err.println(s"Could not process '$word', ${t.getMessage}")
      word
    }
  }
}