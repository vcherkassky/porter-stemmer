package linguistica.stemmer.porter

import scala.collection.immutable.StringOps

/**
 * Note: Any word will be processed in UPPERCASE
 */
trait Logic {

  //TODO: apply snowball optimization http://snowball.tartarus.org/algorithms/porter/stemmer.html
  object LetterType extends Enumeration {
    type LetterType = Value
    val Vowel, Consonant = Value
  }
  import LetterType._

  
  case class Word(val text: String, val measure: Int, val letters: List[(Char, LetterType)]) {
    def stem(suffix: String): Option[Word] = Word.stem(this, suffix)
  }

  
  object Word {

    def apply(word: String): Word = {
      val letters = precompute(word)
      new Word(word, measure(letters.map(_._2)), letters)
    }

    def stem(word: Word, suffix: String): Option[Word] = {
      if (word.text.endsWith(suffix)) {
        val letters = word.letters.dropRight(suffix.length)
        val stem = word.text.dropRight(suffix.length)
        Some(new Word(stem, measure(letters.map(_._2)), letters))
      } else None
    }

    private def precompute(word: String): List[(Char, LetterType)] = {
      var prev: Char = '0'
      var result = List[(Char, LetterType)]()
      for (cur <- word) yield {
        val pair = cur match {
          case 'A' | 'O' | 'U' | 'I' | 'E' => (cur, Vowel)
          case 'Y' => prev match {
            case '0' => (cur, Consonant)
            case 'A' | 'O' | 'U' | 'I' | 'E' => (cur, Consonant)
            case _ => (cur, Vowel)
          }
          case _ => (cur, Consonant)
        }
        result = pair :: result
        prev = cur
      }
      result.reverse
    }

    /**
     * measure is m in [C](VC)^m^[V]
     */
    private def measure(letters: Seq[LetterType]): Int = {
      var prev = letters.head
      var m = 0
      for (cur <- letters.tail) yield {
        if (prev == Vowel && cur == Consonant)
          m = m + 1
        prev = cur
      }
      m
    }
  }

  
  type Rule = Function[Word, Option[Word]]

  case class SimpleRule(suffix: String, replacement: String, condition: Word => Boolean)
    extends Rule {

    if (suffix.exists(Character.isLowerCase))
      throw new IllegalArgumentException(s"suffix should be uppercase, but was $suffix")
    if (replacement.exists(Character.isLowerCase))
      throw new IllegalArgumentException(s"replacement should be uppercase, but was $replacement")

    def stem(word: Word): Option[Word] = word.stem(suffix)

    def apply(word: Word): Option[Word] = stem(word) flatMap { stem =>
      if (condition(stem))
        if (replacement != null) Some(Word(stem.text + replacement))
        else Some(stem)
      else None
    }
  }

  
  object Condition {

    def any(stem: Word): Boolean = true

    def endsWith(ending: String)(stem: Word): Boolean = stem.text.endsWith(ending)

    def containsVowel(stem: Word): Boolean = stem.letters.exists(_._2 == Vowel)

    def endsDoubleConsonant(stem: Word): Boolean = {
      if (stem.text.length() < 2) false
      else {
        val end = stem.text.takeRight(2)
        val endIsConsonant = stem.letters.takeRight(1).head._2 == Consonant
        end.head == end.tail.head && endIsConsonant
      }
    }

    def endsCvcNotWxy(stem: Word): Boolean = {
      if (stem.text.length < 3 || endCharIn("WXY")(stem))
        false
      else {
        stem.letters.takeRight(3).map(_._2) match {
          case List(Consonant, Vowel, Consonant) => true
          case _ => false
        }
      }
    }

    def endCharIn(chars: String)(stem: Word): Boolean = chars.contains(stem.text takeRight 1)

    def measureIs(p: Int => Boolean)(stem: Word): Boolean = p(stem.measure)

    @inline def and(conditions: (Word => Boolean)*)(stem: Word): Boolean =
      conditions.find(condition => !condition(stem)) == None

    @inline def or(conditions: (Word => Boolean)*)(stem: Word): Boolean =
      conditions.exists(condition => condition(stem))

    @inline def not(condition: Word => Boolean)(stem: Word): Boolean = !condition(stem)
  }

  
  object Rule {

    def apply(suffix: String, replacement: String, condition: Word => Boolean): SimpleRule =
      new SimpleRule(suffix, replacement, condition)

    def apply(suffix: String, replacement: String): SimpleRule =
      new SimpleRule(suffix, replacement, Condition.any)

    def doubleToSingle(condition: Word => Boolean): Rule = word =>
      if (Condition.endsDoubleConsonant(word) && condition(word))
        Some(Word(word.text dropRight 1))
      else
        None
  }
}