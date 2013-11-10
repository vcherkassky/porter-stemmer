package linguistica.stemmer.porter

/**
 * Note: Any word will be processed in UPPERCASE
 */
trait PorterStemmer {

  def measure(word: String): Int = {
    def countM(p: (Int, Char), cur: Char): (Int, Char) = {
      val (m, prev) = p
      cur match {
        case 'A' | 'O' | 'U' | 'I' | 'E' => (m, 'v')
        case 'Y' => prev match {
          case '0' => (m, 'c')
          case 'v' => (m + 1, 'c')
          case 'c' => (m, 'v')
          case _ => (m + 1, 'c')
        }
        case _ => prev match {
          case 'v' => (m + 1, 'c')
          case _ => (m, 'c')
        }
      }
    }

    val result: (Int, Char) = word.foldLeft( (0, '0') )(countM)
    result._1
  }
}