package linguistica.stemmer.porter

object Test extends StemmerEngine {

  def main(args: Array[String]): Unit = {
    val text: String = """The Unix operating system was conceived and implemented in 1969 at AT&T's 
      Bell Laboratories in the United States by Ken Thompson, Dennis Ritchie, Douglas McIlroy, 
      and Joe Ossanna. It was first released in 1971, and initially, was written entirely in 
      assembly language, a common practice at the time. Later, in a key pioneering approach in 1973, 
      Unix was re-written in the programming language C by Dennis Ritchie (with exceptions to the 
      kernel and I/O). The availability of an operating system written in a high-level language 
      allowed easier portability to different computer platforms."""

    println(text)

    println(text.split("\\s|\\p{Punct}").filterNot(_.isEmpty).toList)
    
    val stems = text.split("\\s|\\p{Punct}").filterNot(_.isEmpty)
      .map(word => stem(word.toUpperCase())).mkString(" ")

    println(stems)
    
    val initially = "INITIALLY"
    val initi = stem(initially)
    println(s"$initially -> $initi")
  }
}