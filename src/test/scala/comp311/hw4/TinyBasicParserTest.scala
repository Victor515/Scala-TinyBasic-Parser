package comp311.hw4

import org.junit.Test
import org.junit.Assert._
import scala.io.Source

class TinyBasicParserTest {

  /**
    * Test for the given program, only check for acceptance or not
    */

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

  /**
    *  Test for each productions
    */
  @Test
  def testParseComment(): Unit = {
    val comment = "110 REM: This is a test of comment"
    val res = TinyBasicLineParser.parseAll(TinyBasicLineParser.line, comment)
    val expected = Line(110, Removed(": This is a test of comment"))
    assertEquals(expected, res.get)
  }

  @Test
  def testPrint(): Unit = {
    val printLine =
      """
        160 PRINT "ENTER DIRECTION RIGHT (R) DOWN (D)" ;
      """
    val res = TinyBasicLineParser.parseAll(TinyBasicLineParser.line, printLine)
    val expected = Line(160, Print(List(PrintArg(Str("\"ENTER DIRECTION RIGHT (R) DOWN (D)\""), false)), false))
    assertEquals(expected, res.get)

    val invalidPrint =
      """
        160 PRINT "ENTER DIRECTION RIGHT (R) DOWN (D) ;
      """
    val res2 = TinyBasicLineParser.parseAll(TinyBasicLineParser.line, invalidPrint)
    assertTrue("Invalid input is not accecpted", !res2.successful)

  }

  @Test
  def testIfAndGoto(): Unit = {
    val ifLine = "380 IF Z=N GOTO 20"
    val res = TinyBasicLineParser.parseAll(TinyBasicLineParser.line, ifLine)
    val expected = Line(380, If(Var("Z"), Eq, Var("N"), Goto(Num(20))))
    assertEquals(expected, res.get)
  }

  @Test
  def testInput(): Unit = {
    val inputLine = "100 INPUT X,Y,Z"
    val res = TinyBasicLineParser.parseAll(TinyBasicLineParser.line, inputLine)
    val expected = Line(100, Input(List(Var("X"), Var("Y"), Var("Z"))))
    assertEquals(expected, res.get)

    val invalidInput = "100 INPUT XY"
    val res2 = TinyBasicLineParser.parseAll(TinyBasicLineParser.line, invalidInput)
    assertTrue("Invalid INPUT is not accepted", !res2.successful)
  }

  @Test
  def testAssignment(): Unit = {
    val assignmentLine = "100 LETX=10"
    val assignmentLine2 = "101 Y=X+Z"

    val expected = Line(100, Let(Var("X"), Num(10)))
    val res = TinyBasicLineParser.parseAll(TinyBasicLineParser.line, assignmentLine)
    assertEquals(expected, res.get)

    val expected2 = Line(101, Let(Var("Y"), TermAdd(Var("X"), PlusOp, Var("Z"))))
    val res2 = TinyBasicLineParser.parseAll(TinyBasicLineParser.line, assignmentLine2)
    assertEquals(expected2, res2.get)
  }


}
