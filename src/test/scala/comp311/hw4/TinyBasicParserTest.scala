package comp311.hw4

import org.junit.Test
import org.junit.Assert._
import scala.io.Source

class TinyBasicParserTest {
  @Test
  def testParseTicTacToe(): Unit = {
    val inputProgramPath = "tinybasic/tictactoe.txt"
    for (line <- Source.fromFile(inputProgramPath).getLines) {
      val res = TinyBasicLineParser.parseAll(TinyBasicLineParser.line, line)
      assertTrue(res.toString, res.successful)
    }
  }
}
