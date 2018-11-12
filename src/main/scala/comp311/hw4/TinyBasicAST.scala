package comp311.hw4

case class Line(num: Int, statement: Statement)

sealed trait Statement

case class Print(values: List[PrintArg], endLine: Boolean) extends Statement

case class PrintArg(value: Printable, aligned: Boolean)

case class If(x: Expr, op: RelOp, y: Expr, statement: Statement) extends Statement

case class Goto(line: Expr) extends Statement

case class Input(vars: List[Var]) extends Statement

case class Let(x: Var, value: Expr) extends Statement

case class GoSub(line: Expr) extends Statement

case object Return extends Statement

case object End extends Statement

case class Removed(comment: String) extends Statement

sealed trait Printable

case class Str(value: String) extends Printable

sealed trait Expr extends Printable

sealed trait Term extends Expr

case class TermAdd(x: Term, op: AdditiveOp, y: Term) extends Term

sealed trait Factor extends Term

case class FactorMult(x: Term, op: MultiplicativeOp, y: Term) extends Factor

case class Var(name: String) extends Factor

case class Num(value: Int) extends Factor

case class SubExpression(expr: Expr) extends Factor

sealed trait Builtin extends Factor with Statement

case class Random(max: Expr) extends Builtin
case class Plot(ascii: Expr) extends Builtin
case class Peek(address: Expr) extends Builtin
case class Poke(address: Expr, value: Expr) extends Builtin

trait RelOp {
  def apply(x: Int, y: Int): Boolean
}

case object Less extends RelOp {
  def apply(x: Int, y: Int): Boolean = x < y
}

case object LessEq extends RelOp {
  def apply(x: Int, y: Int): Boolean = x <= y
}

case object Greater extends RelOp {
  def apply(x: Int, y: Int): Boolean = x > y
}

case object GreaterEq extends RelOp {
  def apply(x: Int, y: Int): Boolean = x >= y
}

case object NotEq extends RelOp {
  def apply(x: Int, y: Int): Boolean = x != y
}

case object Eq extends RelOp {
  def apply(x: Int, y: Int): Boolean = x == y
}

object RelOp {
  def apply(str: String): RelOp = str match {
    case "<" => Less
    case "<>" => NotEq
    case "<=" => LessEq
    case ">" => Greater
    case ">=" => GreaterEq
    case "><" => NotEq
    case "=" => Eq
  }
}

sealed trait AdditiveOp {
  def apply(x: Int, y: Int): Int
}

case object PlusOp extends AdditiveOp {
  def apply(x: Int, y: Int): Int = x + y
}

case object MinusOp extends AdditiveOp {
  def apply(x: Int, y: Int): Int = x - y
}

object AdditiveOp {
  def apply(str: String): AdditiveOp = str match {
    case "+" => PlusOp
    case "-" => MinusOp
  }
}

sealed trait MultiplicativeOp {
  def apply(x: Int, y: Int): Int
}

case object TimesOp extends MultiplicativeOp {
  def apply(x: Int, y: Int): Int = x * y
}

case object DivideOp extends MultiplicativeOp {
  def apply(x: Int, y: Int): Int = x / y
}

object MultiplicativeOp {
  def apply(str: String): MultiplicativeOp = str match {
    case "*" => TimesOp
    case "/" => DivideOp
  }
}