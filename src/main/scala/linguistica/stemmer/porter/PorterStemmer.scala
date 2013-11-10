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
}