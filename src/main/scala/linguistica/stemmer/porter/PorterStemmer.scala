package linguistica.stemmer.porter

/**
 * Note: Any word will be processed in UPPERCASE
 */
trait PorterStemmer {

  def measure(word: String): Int = {
    def countM(p: (Int, Char), cur: Char): (Int, Char) = {
      val (m, prevType) = p
      cur match {
        case 'A' | 'O' | 'U' | 'I' | 'E' => (m, 'v') // 'v' as prevType means 'vowel'
        case 'Y' => prevType match {
          case '0' => (m, 'c') 		// 'c' as prevType means 'consonant'
        		  					// '0' is for 'no value yet'
          case 'v' => (m + 1, 'c') 	// count m for consonant after vowel combinations 
          case 'c' => (m, 'v')
          case _ => (m + 1, 'c')
        }
        case _ => prevType match {
          case 'v' => (m + 1, 'c')
          case _ => (m, 'c')
        }
      }
    }
    val result: (Int, Char) = word.foldLeft( (0, '0') )(countM)
    result._1
  }
  
  class Rule(suffix: String, replacement: String, condition: String => Boolean) {
    def stem(word: String): Option[String] = {
      if (word.endsWith(suffix))
        Some(word.dropRight(suffix.length))
      else
        None
    }
    def apply(word: String): Option[String] = stem(word) match {
      case Some(stem) => 
        if(condition(stem)) 
          if(replacement != null) Some(stem + replacement)
          else Some(stem)
        else None
      case None => None
    }
  } 
  
  object Rule {
    def apply(suffix: String, replacement: String, condition: String => Boolean) = 
      new Rule(suffix, replacement, condition)
    
    def apply(suffix: String, replacement: String) = new Rule(suffix, replacement, noCondition)
    
    def noCondition(stem: String) = true
  }
}