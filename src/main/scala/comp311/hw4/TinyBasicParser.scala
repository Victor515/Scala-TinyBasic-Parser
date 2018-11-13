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
    "PRINT" ~ expr_list|
    "PR" ~ expr_list|
    "IF" ~ expression~relop~expression ~ opt("THEN") ~ statement|
    "GOTO" ~ expression|
    "INPUT" ~ var_list|
    opt("LET") ~ variable ~ "=" ~ expression|
    "GOSUB" ~ expression|
    "RETURN"|
    "END"|
    "REM" ~ stringLiteral|
    builtin

  def expr_list: Parser[Any] = rep(expr_list_item~","|expr_list_item~";")~opt(expr_list_item)

  def expr_list_item: Parser[Any] = stringLiteral | expression

  def var_list: Parser[Any] = variable ~ rep(","~variable)

  def expression: Parser[Any] = opt("+"|"-")~term~rep("+"~term|"-"~term)

  def term: Parser[Term] = factor~rep("*"~factor|"/"~factor)^^{
    case factor~factors => generateTerm(factor, factors)
  }

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
