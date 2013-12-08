package linguistica.stemmer.porter

trait Stemmer {
  def stem(word: String): String
}