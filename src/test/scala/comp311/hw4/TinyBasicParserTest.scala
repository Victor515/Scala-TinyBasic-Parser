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

  @Test
  def testParseAdventure(): Unit = {
    val inputProgramPath = "tinybasic/adventure.txt"
    for (line <- Source.fromFile(inputProgramPath).getLines) {
      val res = TinyBasicLineParser.parseAll(TinyBasicLineParser.line, line)
      assertTrue(res.toString, res.successful)
    }
  }

  @Test
  def testBattleShip(): Unit = {
    val inputProgramPath = "tinybasic/battleship.txt"
    for (line <- Source.fromFile(inputProgramPath).getLines) {
      val res = TinyBasicLineParser.parseAll(TinyBasicLineParser.line, line)
      assertTrue(res.toString, res.successful)
    }
  }

  @Test
  def testTTT(): Unit = {
    val inputProgramPath = "tinybasic/ttt.txt"
    for (line <- Source.fromFile(inputProgramPath).getLines) {
      val res = TinyBasicLineParser.parseAll(TinyBasicLineParser.line, line)
      assertTrue(res.toString, res.successful)
    }
  }

  @Test
  def testParseComment(): Unit = {
    val comment = "110 REM: This is a test of comment"
    val res = TinyBasicLineParser.parseAll(TinyBasicLineParser.line, comment)
    val expected =
    assertTrue(res.toString, res.successful)
  }


}
