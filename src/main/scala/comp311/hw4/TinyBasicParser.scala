package comp311.hw4

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Grammar based on the Tiny Basic Language
  *
  * Please refer to the Tiny Basic manual for more information:
  * http://www.ittybittycomputers.com/IttyBitty/TinyBasic/TBuserMan.htm
  *
  *  line ::= number statement
  *
  * statement ::= PRINT expr-list
  *             | PR expr-list
  *             | IF expression relop expression [ THEN ] statement
  *             | GOTO expression
  *             | INPUT var-list
  *             | [ LET ] var '=' expression
  *             | GOSUB expression
  *             | RETURN
  *             | END
  *             | REM comment
  *             | builtin
  *
  * expr-list ::= { expr-list-item ',' | expr-list-item ';' } [ expr-list-item ]
  *
  * expr-list-item ::= string | expression
  *
  * var-list ::= var { ',' var }
  *
  * expression ::= [ '+' | '-' ] term { '+' term | '-' term }
  *
  * term ::= factor { '*' factor | '/' factor }
  *
  * factor ::= builtin | var | number | '(' expression ')'
  *
  * var ::= A | B | C ... | Y | Z
  *
  * number ::= digit digit*
  *
  * builtin ::= RND '(' expression ')'
  *           | PLOT '(' expression ')'
  *           | PEEK '(' expression ')'
  *           | POKE '(' expression ',' expression ')'
  *
  * relop ::= '<' [ '>' | '=' ] | '>' [ '<' | '=' ] | '='
  *
  * ### NOTE: We assume that Java-style string literals are legal ###
  * string ::= "..."
  */
object TinyBasicLineParser extends JavaTokenParsers {
  def line: Parser[Line] = number~statement^^{case n~s => Line(n.value,s)}

  def statement: Parser[Statement] =
      "PRINT"~>expr_list^^{
        case (items, false) => Print(items, false)
        case (items, true) => Print(items, true)
      }|
      "PR"~>expr_list^^{
        case (items, false) => Print(items, false)
        case (items, true) => Print(items, true)
      }|
      "IF" ~> expression~relop~expression ~ opt("THEN") ~ statement^^{
        case expr1~relop~expr2~_~statement => If(expr1, relop, expr2, statement)
      }|
      "GOTO" ~> expression^^(expr => Goto(expr))|
      "INPUT" ~> var_list^^(var_list => Input(var_list))|
      opt("LET") ~> variable ~ "=" ~ expression^^{
        case variable~"="~expression => Let(variable, expression)
      }|
      "GOSUB" ~> expression^^(expr => GoSub(expr))|
      "RETURN"^^(x => Return)|
      "END"^^(x => End)|
      "REM" ~> stringLiteral^^(x => Removed(x.toString))|
      builtin


  def expr_list: Parser[(List[PrintArg],Boolean)] = rep(expr_list_item~","|expr_list_item~";")~opt(expr_list_item)^^{
    case items~None => (generatePrintArg(items), true)
    case items~Some(item) => (generatePrintArg(items).+:(PrintArg(item, false)), false)
  }

  def generatePrintArg(list: List[TinyBasicLineParser.~[Printable, String]], accum: List[PrintArg] = List()): List[PrintArg] = {
    list match {
      case Nil => accum
      case x::xs => {
        x match {
          case expr~"," => generatePrintArg(xs, accum.+:(PrintArg(expr, true)))
          case expr~";" => generatePrintArg(xs, accum.+:(PrintArg(expr, false)))
        }
      }
    }
  }

  def expr_list_item: Parser[Printable] = stringLiteral^^(Str(_)) | expression

  def var_list: Parser[List[Var]] = repsep(variable, ",")

  def expression: Parser[Expr] = opt("+"|"-")~term~rep("+"~term|"-"~term)^^ {
    case None~term~terms => generateExpr(term, terms)
    case Some("+")~term~terms => generateExpr(term, terms)
    case Some("-")~term~terms => generateExpr(TermAdd(Num(0), MinusOp, term), terms)
  }

  def generateExpr(term: Term, list: List[TinyBasicLineParser.~[String, Term]]): Expr ={
    list match {
      case Nil => term
      case x::xs => {
        x match {
          case "+"~other => generateExpr(TermAdd(term, PlusOp, other), xs)
          case "-"~other => generateExpr(TermAdd(term, MinusOp, other), xs)
        }
      }
    }
  }



  def term: Parser[Term] = factor~rep("*"~factor|"/"~factor)^^{
    case factor~factors => generateTerm(factor, factors)
  }

  // helper function to generate a term
  def generateTerm(term: Term, list: List[TinyBasicLineParser.~[String, Factor]]): Term = {
    list match {
      case Nil => term
      case x::xs => {
        x match {
          case "*"~factor => generateTerm(FactorMult(term, TimesOp, factor), xs)
          case "/"~factor => generateTerm(FactorMult(term, DivideOp, factor), xs)
        }
      }
    }
  }


  def factor: Parser[Factor] = builtin|variable|number|
    "("~expression~")"^^{case _~e~_ => SubExpression(e)}

  def variable: Parser[Var] =
    "A"^^(x => Var(x))|
      "B"^^(x => Var(x))|
      "C"^^(x => Var(x))|
      "D"^^(x => Var(x))|
      "E"^^(x => Var(x))|
      "F"^^(x => Var(x))|
      "H"^^(x => Var(x))|
      "I"^^(x => Var(x))|
      "J"^^(x => Var(x))|
      "K"^^(x => Var(x))|
      "L"^^(x => Var(x))|
      "M"^^(x => Var(x))|
      "N"^^(x => Var(x))|
      "O"^^(x => Var(x))|
      "P"^^(x => Var(x))|
      "Q"^^(x => Var(x))|
      "R"^^(x => Var(x))|
      "S"^^(x => Var(x))|
      "T"^^(x => Var(x))|
      "U"^^(x => Var(x))|
      "V"^^(x => Var(x))|
      "W"^^(x => Var(x))|
      "X"^^(x => Var(x))|
      "Y"^^(x => Var(x))|
      "Z"^^(x => Var(x))

  def number: Parser[Num] = wholeNumber^^{x => Num(x.toInt)}

  def builtin: Parser[Builtin] =
    "RND("~expression~")"^^{case _~e~_ => Random(e)}|
      "PLOT("~expression~")"^^{case _~e~_ => Plot(e)}|
      "PEEK("~expression~")"^^{case _~e~_ => Peek(e)}|
      "POKE("~expression~","~expression~")"^^{case _~e1~_~e2~_ => Poke(e1, e2)}

  def relop: Parser[RelOp] =
    "<"~opt(">"|"=")^^{
      case "<"~None => Less
      case "<"~Some(">") => NotEq
      case "<"~Some("=") => LessEq}|
      ">"~opt("<"|"=")^^{
        case ">"~None => Greater
        case ">"~Some("<") => NotEq
        case ">"~Some("=") => GreaterEq
      }|
      "="^^(x => Eq)

}
