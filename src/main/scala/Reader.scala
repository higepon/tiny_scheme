package tiny_scheme

import util.parsing.combinator.JavaTokenParsers

/**
 *   Copyright (c) 2012 Higepon(Taro Minowa) <higepon@users.sourceforge.jp>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

abstract class Expr

class Null extends Expr {
  override def equals(other: Any): Boolean = isInstanceOf[Null]
}

case class Cons(car: Any, cdr: Any) extends Expr {
}

case class Number(value: Int) extends Expr {
  override def toString: java.lang.String = "[" + value.toString + "]"
}

case class String(value: java.lang.String) extends Expr {
  override def toString: java.lang.String = "[" + value + "]"
}

case class Symbol(value: java.lang.String) extends Expr {
  override def toString: java.lang.String = "[" + value + "]"
}

class Reader extends JavaTokenParsers {
  def read(s: java.lang.String): Expr = {
    parseAll(sexp, s) match {
      case x:NoSuccess => throw(new Exception(x.toString))
      case p => p.get
    }
  }

  lazy val sexp: Parser[Expr] = number | scheme_null | list | string | symbol
  lazy val number: Parser[Expr] = """[0-9]+""".r ^^ { x => Number(x.toInt) }
  lazy val string: Parser[Expr] = """\"([a-zA-Z0-9]*)\"""".r ^^ {
    x => tiny_scheme.String(x.substring(1, x.length() - 1))
  }
  lazy val symbol: Parser[Expr] = """([a-zA-Z+][a-zA-Z0-9]*)""".r ^^ { x => Symbol(x) }
  lazy val list: Parser[Expr] = "(" ~> rep(sexp) <~ ")" ^^ { x => Scheme.toCell(x) }
  lazy val scheme_null: Parser[Null] = "()" ^^ {
    s => new Null
  }

}