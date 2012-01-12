package test.scala

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

import org.scalatest.FunSuite
import tiny_scheme.{Cons, Number, Reader, Nil}

class ReaderTestSuite extends FunSuite {

  test("A Reader read integer number") {
    val reader = new Reader
    assert(reader.read("1") === Number(1))
    assert(reader.read("2") === Number(2))
  }

  test("A Reader read empty list") {
    val reader = new Reader
    assert(reader.read("()") === Nil)
  }

  test("A Reader read one element list") {
    val reader = new Reader
    assert(reader.read("(1)") === Cons(1, Nil))
  }

  test("A Reader read multiple element list") {
    val reader = new Reader
    assert(reader.read("(1 2 3)") === Cons(1, Cons(2, Cons(3, Nil))))
  }

  test("A Reader read string") {
    val reader = new Reader
    assert(reader.read("\"abc\"") === tiny_scheme.String("abc"))
  }

  test("A Reader read symbol") {
    val reader = new Reader
    assert(reader.read("abc") === tiny_scheme.Symbol("abc"))
  }
  
  test("A reader read quote syntax") {
    val reader = new Reader
    assert(reader.read("'(abc)") === Cons(tiny_scheme.Symbol("abc"), Nil))
  }
}