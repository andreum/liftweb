package net.liftweb.util;

/* 
 * Copyright 2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); 
 * you may not use this file except in compliance with the License. 
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0 
 * Unless required by applicable law or agreed to in writing, software 
 * distributed under the License is distributed on an "AS IS" BASIS, 
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import scala.util.parsing.combinator.{Parsers, ImplicitConversions}
import Helpers._

object JSONParser extends SafeSeqParser with ImplicitConversions { 
  implicit def strToInput(in: String): Input = new scala.util.parsing.input.CharArrayReader(in.toCharArray)
  type Elem = Char
  
  def parse(in: String): Can[Any] = theValue(in) match {
    case Success(v, _) => Full(v)
    case _ => Empty
  }
  
  def whitespace = elem(' ') | elem('\t') | elem('\n') | elem('\r')
  
  def spaces = rep(whitespace)
  
  
  def jsonObject: Parser[Map[String, Any]] = ( spaces ~ '{' ~ spaces ~> members <~ spaces ~ '}' ~ spaces ^^ {case xs =>
    Map(xs :_*)
    } )  |
    spaces ~'{' ~ spaces ~ '}' ~ spaces  ^^ {case _ => Map.empty}
    
  
  def members = rep1sep(pair, spaces ~ ',' ~ spaces)
  
  def pair: Parser[(String, Any)] = (string | pairId) ~ spaces ~ ':' ~ spaces ~ theValue ^^ {case s ~ _ ~ _ ~ _ ~ v => (s,v)}
  
  def pairChar(in: Char): Boolean = in.isLetter || in.isDigit || in == '_'
  
  def pairId: Parser[String] = rep1(elem("pairChar", pairChar)) ^^ {case s => s.mkString}
  
  def string: Parser[String] = ('\'' ~> rep(not('\'') ~> achar) <~ '\'' ^^ {case xs => xs.mkString("")}) |
               ('"' ~> rep(not('"') ~> achar) <~ '"' ^^ {case xs => xs.mkString("")})
               
               def achar = ('\\' ~> ('"' ^^ {case _ => '"'} | 
               '\\' ^^ {case _ => '\\'} | 
               '/' ^^ {case _ => '/'} |
               'b' ^^ {case _ => '\b'} | 
               'n' ^^ {case _ => '\n'} |
               'r' ^^ {case _ => '\r'} |
               't' ^^ {case _ => '\t'} | 
               'u' ~> repN(4, hexDigit) ^^ {case dg => Integer.parseInt(dg.mkString(""), 16).toChar})) | (elem("any char", c => c != '"' && c >= ' '))
  
  def number: Parser[Double] =  intFracExp | intFrac | intExp |  (anInt ^^ {case n => n.toDouble})
  
  def exp = e ~ digits ^^ {case x ~ d => d.mkString("").toInt * x}
  
  def e = ('e' ~ '-' ^^ {case _ => -1}) | ('e' ~ '+' ^^ {case _ => 1}) | ('e' ^^ {case _ => 1}) |
  ('E' ~ '-' ^^ {case _ => -1}) | ('E' ~ '+' ^^ {case _ => 1}) | ('E' ^^ {case _ => 1})
  
  def intFracExp: Parser[Double] = anInt ~ frac ~ exp ^^ {case i ~ f ~ exp => ((i.toString+"."+f+"e"+exp).toDouble)}
  
  def intFrac = anInt ~ frac ^^ {case i ~ f => ((i.toString+"."+f).toDouble)}
  
  def intExp = anInt ~ exp ^^ {case i ~ e => ((i.toString+"e"+e).toDouble)}
  
  def anInt = (digit19 ~ digits ^^ {case x ~ xs => (x :: xs).mkString("").toLong}) | 
    (digit ^^ {case x => x.toString.toLong}) | ('-' ~ digit19 ~ digits ^^ {case x ~ xs => (x :: xs).mkString("").toLong * -1L}) | 
      ('-' ~ digit ^^ {case x => x.toString.toLong * -1L})
  
  def digit19 = elem("digit", c => c >= '1' && c <= '9')
  
  def digit = elem("digit", c => c >= '0' && c <= '9')
  
  def hexDigit = elem("hex digit", c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
  
  def digits = rep1(digit)
  
  def frac = '.' ~ digits
    
  def theValue: Parser[Any] = string | number | jsonObject | array | istrue | isfalse | isnull 
  
  def array: Parser[List[Any]] = spaces ~ '[' ~ spaces ~> elements <~ spaces ~ ']' ~ spaces ^^ {case e => e}
    
  def elements = repsep(theValue, spaces ~ ',' ~ spaces)
  
  def istrue: Parser[Boolean] = acceptSeq("true") ^^ {case _ => true}
  def isfalse: Parser[Boolean] = acceptSeq("false") ^^ {case _ => false}
  def isnull = acceptSeq("null") ^^ {case _ => Empty}
  
  // def parse(in: String) = theValue(in)
}

