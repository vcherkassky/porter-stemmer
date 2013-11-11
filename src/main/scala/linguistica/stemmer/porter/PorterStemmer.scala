package linguistica.stemmer.porter

import scala.collection.immutable.StringOps

/**
 * Note: Any word will be processed in UPPERCASE
 */
trait PorterStemmer {

  type Word = String
  type Stem = String

//  def precompute(word: Word): List[]
  
  def measure(word: Word): Int = {
    def countM(p: (Int, Char), cur: Char): (Int, Char) = {
      val (m, prevType) = p
      cur match {
        case 'A' | 'O' | 'U' | 'I' | 'E' => (m, 'v') // 'v' as prevType means 'vowel'
        case 'Y' => prevType match {
          case '0' => (m, 'c') // 'c' as prevType means 'consonant'
          // '0' is for 'no value yet'
          case 'v' => (m + 1, 'c') // count m for consonant after vowel combinations 
          case 'c' => (m, 'v')
          case _ => (m + 1, 'c')
        }
        case _ => prevType match {
          case 'v' => (m + 1, 'c')
          case _ => (m, 'c')
        }
      }
    }
    val result: (Int, Char) = word.foldLeft((0, '0'))(countM)
    result._1
  }

  class Rule(suffix: String, replacement: String, condition: String => Boolean) {
    def stem(word: Word): Option[Stem] = {
      if (word.endsWith(suffix))
        Some(word.dropRight(suffix.length))
      else
        None
    }
    def apply(word: Word): Option[Word] = stem(word) match {
      case Some(stem) =>
        if (condition(stem))
          if (replacement != null) Some(stem + replacement)
          else Some(stem)
        else None
      case None => None
    }
  }

  object Condition {
    def any(stem: Stem): Boolean = true
    def endsWith(ending: String)(stem: Stem): Boolean = stem.endsWith(ending)
    def containsVowel(stem: Stem): Boolean = {
      val hasPureVowel = stem.exists(isPureVowel)
      val indexOfY = stem.indexOf("Y")
      val consonantBeforeY = indexOfY > 0 && !isPureVowel(stem.charAt(indexOfY - 1)) // Y is a vowel after a consonant
      hasPureVowel && consonantBeforeY
    }
    def endsDoubleConsonant(stem: Stem): Boolean = {
      if (stem.length() < 2) false
      else {
        val end = stem.takeRight(2)
        end.head == end.tail.head
      }
    }
    
    private def isPureVowel(c: Char) = c match {
      case 'A' | 'O' | 'U' | 'I' | 'E' => true
      case _ => false
    }
  }

  object Rule {
    def apply(suffix: String, replacement: String, condition: String => Boolean): Rule =
      new Rule(suffix, replacement, condition)
    def apply(suffix: String, replacement: String): Rule = new Rule(suffix, replacement, Condition.any)
  }
}