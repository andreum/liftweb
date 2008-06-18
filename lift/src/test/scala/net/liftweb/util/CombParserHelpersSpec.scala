/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.util

import org.specs._
import scala.util.parsing.input._
import org.specs.runner._
import scala.util.parsing.combinator.Parsers
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.specs.matcher.ScalacheckParameters._

class CombParserHelpersSpecTest extends Runner(CombParserHelpersSpec) with JUnit with Console
object CombParserHelpersSpec extends Specification {
  import ParserHelpers._

  "The parser helpers" should {
  /*
    "provide an isEol function returning true iff a char is end of line or end of file" in {
      isEol('\n') must beTrue
      isEol('\r') must beTrue
      isEol('\032') must beTrue
    }
    "provide an notEol function returning true iff a char is not end of line nor end of file" in {
      notEol('\n') must beFalse
      notEol('\r') must beFalse
      notEol('\032') must beFalse
    }
    */
    "provide an isEof function returning true iff a char is end of file" in {
      isEof('\032') must beTrue
    }
    "provide an notEof function returning true iff a char is not end of file" in {
      notEof('\032') must beFalse
    }
    "provide an isNum function returning true iff a char is a digit" in {
      isNum('0') must beTrue
    }
    "provide an notNum function returning true iff a char is not a digit" in {
      notNum('0') must beFalse
    }
    "provide an wsc function returning true iff a char is a space character" in {
      List(' ', '\t', '\r', '\n') foreach { wsc(_) must beTrue }
      wsc('a') must beFalse
    }
    "provide a whitespace parser: white. Alias: wsc" in {
      import whiteStringGen._
      val whiteParse = (s: String) => white(s) must beLike { case Success(_, _) => true }
      property(whiteParse) must pass
    }
    "provide a whiteSpace parser always succeeding and discarding its result" in {
      import stringWithWhiteGen._
      val whiteSpaceParse = (s: String) => whiteSpace(s) must beLike {
        case Success(x, y) => x.toString == "()"
        case _ => false
      }
      property(whiteSpaceParse) must pass
    }
    "provide an acceptCI parser to parse whatever string matching another string ignoring case" in {
      import abcdStringGen._
      val ignoreCaseStringParse = (s: String, s2: String) => acceptCI(s).apply(s2) match {
        case Success(x, y) => s2.toUpperCase must startWith(s.toUpperCase)
        case _ => true
      }
      property(ignoreCaseStringParse) must pass
    }
    "provide a digit parser - returning a String" in {
      val isDigit = (s: String) => digit(s) match {
        case Success(x, y) => s mustMatch("\\d")
        case _ => true
      }
      property(isDigit) must pass
    }
    "provide an aNumber parser - returning an Int if succeeding" in {
     val number = (s: String) => aNumber(s) match {
        case Success(0, y) => s.toString must startWith("0")
        case Success(x, y) => strToLst(s).dropWhile(_ == '0')(0) must_==(x.toString.head)
        case _ => true
      }
      property(number) must pass
    }
    "provide a slash parser" in {
      slash("/").get must_== '/'
      slash("x") must beLike {case Failure(_, _) => true}
    }
    /*
    "provide a dslash parser which parses the slash and discards the input" in {
      dslash("/").get.toString must_== "()"
      dslash("/").next.atEnd must beTrue
    }
    */
    
    "provide a colon parser" in {
      colon(":").get must_== ':'
      colon("x") must beLike {case Failure(_, _) => true}
    }
    
    /*
    "provide a dcolon parser which parses the colon and discards the input" in {
      dcolon(":").get.toString must_== "()"
      dcolon(":").next.atEnd must beTrue
    }
    */
    
    "provide a EOL parser which parses the any and discards any end of line character" in {
      List("\n", "\r") foreach { s =>
        val result = EOL(s)
        result.get.toString must_== "()"
        result.next.atEnd must beTrue
      }
    }
    val parserA = elem("a", (c: Char) => c == 'a')
    val parserB = elem("b", (c: Char) => c == 'b')
    val parserC = elem("c", (c: Char) => c == 'c')
    val parserD = elem("d", (c: Char) => c == 'd')
    def shouldSucceed[T](r: ParseResult[T]) = r match {
      case Success(x, y) => true
      case _ => false
    }
    "provide a permute parser succeeding if any permutation of given parsers succeeds" in {
      def permuteParsers(s: String) = shouldSucceed(permute(parserA, parserB, parserC, parserD)(s))
      import abcdStringGen._
      property((s: String) => (stringWrapper(s).size == 4) ==> permuteParsers(s)) must pass
    }
    "provide a permuteAll parser succeeding if any permutation of the list given parsers, or a sublist of the given parsers succeeds" in {
      def permuteAllParsers(s: String) = shouldSucceed(permuteAll(parserA, parserB, parserC, parserD)(s))
      implicit def pick3Letters = abcdStringGen.pickN(3, List("a", "b", "c"))
      property((s: String) => (!stringWrapper(s).isEmpty) ==> permuteAllParsers(s)) must pass
    }
    "provide a repNN parser succeeding if an input can be parsed n times with a parser" in {
      def repNNParser(s: String) = shouldSucceed(repNN(3, parserA)(s))
      implicit def pick3Letters = abcdStringGen.pickN(3, List("a", "a", "a"))
      property((s: String) => (!stringWrapper(s).isEmpty) ==> repNNParser(s)) must pass
    }
  }
}
object abcdStringGen {
  implicit def abcdString = Arbitrary {
    for (len <- choose(1, 4);
         string <- pick(len, List("a", "b", "c", "d"))
         ) yield string.mkString("")
  }
  def pickN(n: Int, elems: List[String]) = Arbitrary {
    for (string <- pick(n, elems)) yield string.mkString("")
  }
}
object whiteStringGen {
  def genWhiteString: Gen[String] = for (len <- choose(1, 4);
                                         string <- vectorOf(len, frequency((1, value(" ")), (1, value("\t")), (1, value("\r")), (1, value("\n"))))
                                    ) yield string.mkString("")
  implicit def whiteString = Arbitrary(genWhiteString)
}
object stringWithWhiteGen {
  import whiteStringGen._
  implicit def genString: Arbitrary[String] = Arbitrary {
    for (len <- choose(1, 4);
         string <- vectorOf(len, frequency((1, value("a")), (2, value("b")), (1, genWhiteString)))
    ) yield string.mkString("")
  }
}
object ParserHelpers extends CombParserHelpers with Parsers
