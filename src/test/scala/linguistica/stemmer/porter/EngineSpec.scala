package linguistica.stemmer.porter

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.Assertions._

class EngineSpec extends FunSuite with StemmerEngine {

  test("test that step1b returns right stem") {
    def applyStep1b(word: String): String = {
      val w = Word(word)
      val result = applyRules(w, step1b_complex_first) match {
        case None => w
        case Some(wrd) => applyStep(wrd, step1b_complex_second)
      }
      result.text
    }

    assert(applyStep1b("PLASTERED") === "PLASTER")
    assert(applyStep1b("BLED") === "BLED")
    assert(applyStep1b("MOTORING") === "MOTOR")
    assert(applyStep1b("SING") === "SING")
    assert(applyStep1b("CONFLATED") === "CONFLATE")
    assert(applyStep1b("TROUBLED") === "TROUBLE")
    assert(applyStep1b("SIZED") === "SIZE")
    assert(applyStep1b("HOPPING") === "HOP")
    assert(applyStep1b("TANNED") === "TAN")
    assert(applyStep1b("FALLING") === "FALL")
    assert(applyStep1b("HISSING") === "HISS")
    assert(applyStep1b("FIZZING") === "FIZZ")
    assert(applyStep1b("FAILING") === "FAIL")
    assert(applyStep1b("FILING") === "FILE")
  }
}