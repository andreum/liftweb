package net.liftweb.util

/* 
 * Copyright 2007-2008 WorldWide Conferencing, LLC
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
import java.net.{URLDecoder, URLEncoder}
import scala.collection.mutable.{HashSet, ListBuffer}
import scala.xml.{NodeSeq, Elem, Node, Text, Group, UnprefixedAttribute, Null, Unparsed, MetaData, PrefixedAttribute}
import scala.collection.{Map}
import scala.collection.mutable.HashMap
import org.apache.commons.codec.binary.Base64
import java.io.{InputStream, ByteArrayOutputStream, ByteArrayInputStream, Reader, File, FileInputStream, BufferedReader, InputStreamReader}
import java.security.{SecureRandom, MessageDigest}
import java.util.regex._
import java.lang.Character._
import javax.crypto._
import javax.crypto.spec._

/**
 * The Helpers object provides a lot of utility functions:<ul>
 * <li>Time and date functions: 
 * <li>URL functions 
 * </ul>
 */
object Helpers extends TimeHelpers with BindHelpers {
    
  /**
   * URL decode the string.  A pass-through to Java's URL decode with UTF-8
   */
  def urlDecode(in : String) = {URLDecoder.decode(in, "UTF-8")}
  def urlEncode(in : String) = {URLEncoder.encode(in, "UTF-8")}
  
  /**
  * Take a list of name/value parse and turn them into a URL query string
  *
  * @param params the name/value pairs
  *
  * @return a valid query string
  */
  def paramsToUrlParams(params: List[(String, String)]): String = params.map{case (n,v) => urlEncode(n)+"="+urlEncode(v)}.mkString("&")
  
  
  /**
  * Append parameters to a URL
  *
  * @param url the url to append the params to
  * @param params the parameters (name/value) to append to the URL
  *
  * @return the url with the parameters appended
  */
  def appendParams(url: String, params: Seq[(String, String)]): String = params.toList match {
    case Nil => url
    case xs if url.indexOf("?") == -1 => url + "?" + paramsToUrlParams(xs)
    case xs => url + "&" + paramsToUrlParams(xs)
  }
  
  val validSuffixes = {
    val ret = new HashSet[String];
    ret += ("png", "js", "css", "jpg", "ico", "gif", "tiff", "jpeg")
    ret
  }
  
  
  def goodPath_?(path : String): Boolean = {
    if (path == null || path.length == 0 || !path.startsWith("/") || path.indexOf("/.") != -1) false
    else {
      val lastPoint = path.lastIndexOf('.')
      val lastSlash = path.lastIndexOf('/')
      if (lastPoint <= lastSlash) false else {
        validSuffixes.contains(path.substring(lastPoint + 1))
      }
    }
  }
  
  def exec(cmds: String*): Can[String] = {
    try {
      class ReadItAll(in: InputStream, done: String => Unit) extends Runnable {
        def run {
          val br = new BufferedReader(new InputStreamReader(in))
          val lines = new ListBuffer[String]
          var line = ""
          while (line != null) {
            line = br.readLine
            if (line != null) lines += line
          }
          done(lines.mkString("\n"))
        }
      }
      
      var stdOut = ""
      var stdErr = ""
      val proc = Runtime.getRuntime.exec(cmds.toArray)
      val t1 = new Thread(new ReadItAll(proc.getInputStream, stdOut = _))
      t1.start
      val t2 = new Thread(new ReadItAll(proc.getErrorStream, stdErr = _))
      val res = proc.waitFor
      t1.join
      t2.join
      if (res == 0) Full(stdOut)
      else Failure(stdErr, Empty, Nil)
    } catch {
      case e => Failure(e.getMessage, Full(e), Nil)
    }
  }
  
  def couldBeHtml(in: Map[String, String]): Boolean =
  in match {
    case null => true
    case n => {
      n.get("Content-Type") match {
        case Some(s) => { (s.toLowerCase == "text/html") ||
        (s.toLowerCase == "application/xhtml+xml") }
        case None => true
      }
    }
  }
  
  def noHtmlTag(in: NodeSeq): Boolean = (in \\ "html").length != 1
  
  def toHashMap[A,B](in : Map[A,B]) : HashMap[A,B] = {
    val ret = new HashMap[A,B];
    in.keys.foreach {
      k =>
      ret += k -> in(k)
    }
    
    ret
  }
  
  /**
  * Returns a Full can with the first element x of the list in
  * for which f(x) evaluates to true. If f(x) evaluates to false
  * for every x, then an Empty can is returned.
  *
  * @param in  a list of elements to which f can be applied
  * @param f   a function that can be applied to elements of in
  */
  def first_? [B](in: List[B])(f: => B => Boolean): Can[B] =
  in match {
    case Nil => Empty
    case x :: xs => if (f(x)) Full(x) else first_? (xs)(f)
  }
  
  /**
  * Returns the first application of f to an element of in that
  * results in a Full can. If f applied to an element of in results
  * in an Empty can, then f will be applied to the rest of the
  * elements of in until a Full can results. If the list runs out
  * then an Empty can is returned.
  * 
  * @param in  a list of elements to which f can be applied
  * @param f   a function that can be applied to elements of in
  */  
  def first[B,C](in : List[B])(f : B => Can[C]): Can[C] = {
    in match {
      case Nil => Empty
      case x :: xs => {
        f(x) match {
          case s @ Full(_) =>  s
          case _ => first(xs)(f)
        }
      }
    }
  }
  
  /**
  * Choose one of many templates from the children.  Looking for the
  * tag &lt;choose:stuff&gt; ... &lt;/choose:stuff&gt;
  *
  * @param prefix the prefix (e.g., "choose")
  * @param tag the tag to choose (e.g., "stuff")
  * @param xhtml the incoming node sequence
  *
  * @return the first matching node sequence
  */
  def chooseTemplate(prefix: String, tag: String, xhtml: NodeSeq): NodeSeq = (xhtml \\ tag).toList.filter(_.prefix == prefix) match {
    case Nil => Nil
    case x :: xs => x.child
  }
  
  /**
  * Insure all the appropriate fields are in the header
  */
  def insureField(toInsure: List[(String, String)], headers: List[(String, String)]): List[(String, String)] = {
    def insureField_inner(toInsure : List[(String, String)], field : (String, String)): List[(String, String)] =
    toInsure.ciGet(field._1) match {
      case Full(_) => toInsure
      case _ => field :: toInsure
    }
    
    headers match {
      case Nil => toInsure
      case x :: xs => insureField(insureField_inner(toInsure, x), xs)
    }
  }
  
  class ListMapish(val theList: List[(String, String)]) {
    def ciGet(swhat: String): Can[String] = {
      val what = swhat.toLowerCase
      def tGet(in: List[(String, String)]): Can[String] = 
      in match {
        case Nil => Empty
        case x :: xs if (x._1.toLowerCase == what) => Full(x._2)
        case x :: xs => tGet(xs)
      }
      tGet(theList)
    }
  }
  
  implicit def listToListMapish(in: List[(String, String)]): ListMapish = new ListMapish(in)
  
  /**
  * Convert an enum to a List[T]
  */
  def enumToList[T](enum: java.util.Enumeration[T]) : List[T] = {
    if (enum.hasMoreElements) {
      val next = enum.nextElement
      next :: enumToList(enum)
      /*
      if (next.isInstanceOf[T]) next.asInstanceOf[T] :: enumToList[T](enum)
      else enumToList[T](enum)
      */
    } else
    Nil
  }
  
  /**
  * Convert an enum to a List[String]
  */
  def enumToStringList[C](enum: java.util.Enumeration[C]) : List[String] =
  if (enum.hasMoreElements) enum.nextElement.toString :: enumToStringList(enum) else Nil
  
  
  def head[T](l : List[T], deft: => T) = l match {
    case Nil => deft
    case x :: xs => x
  }
  
  /**
  * If the incoming Elem has an 'id', return it, otherwise
  * construct a new Elem with a randomly generated id and return the pair
  *
  * @param in the element to test & add 'id' to
  *
  * @return the new element and the id
  */
  def findOrAddId(in: Elem): (Elem, String) = (in \ "@id").toList match {
    case Nil => val id = "R"+randomString(12)
    (in % ("id" -> id), id)
    case x :: xs => (in, x.text)
  } 
  
  
  
  private val random = new java.security.SecureRandom
  
  def randomLong(mod: Long): Long = Math.abs(random.nextLong) % mod
  def randomInt(mod: Int): Int = Math.abs(random.nextInt) % mod
  
  def shouldShow(percent: Int): Boolean = Math.abs(random.nextInt) % 100 < percent
  def shouldShow(percent: Double): Boolean = random.nextDouble <= percent
  
  def makeBlowfishKey: Array[Byte] = KeyGenerator.getInstance("blowfish").generateKey.getEncoded
  def blowfishKeyFromBytes(key: Array[Byte]): SecretKey = new SecretKeySpec(key, "blowfish")
  
  def blowfishDecrypt(enc: Array[Byte], key: Array[Byte]): Array[Byte] = blowfishDecrypt(enc, blowfishKeyFromBytes(key))
  def blowfishDecrypt(enc: Array[Byte], key: SecretKey): Array[Byte] = readWholeStream(decryptStream(new ByteArrayInputStream(enc), key))
  
  def blowfishEncrypt(plain: String, key: Array[Byte]): String = blowfishEncrypt(plain, blowfishKeyFromBytes(key))
  def blowfishEncrypt(plain: String, key: SecretKey): String = base64Encode(blowfishEncrypt(plain.getBytes("UTF-8"), key))
  
  def blowfishDecrypt(enc: String, key: Array[Byte]): String = blowfishDecrypt(enc, blowfishKeyFromBytes(key))
  def blowfishDecrypt(enc: String, key: SecretKey): String = new String(blowfishDecrypt(base64Decode(enc), key), "UTF-8")
  
  def blowfishEncrypt(plain: Array[Byte], key: Array[Byte]): Array[Byte] = blowfishEncrypt(plain, blowfishKeyFromBytes(key))
  def blowfishEncrypt(plain: Array[Byte], key: SecretKey): Array[Byte] = readWholeStream(encryptStream(new ByteArrayInputStream(plain), key))
  
  
  def decryptStream(in: InputStream, key: Array[Byte]): InputStream = decryptStream(in, blowfishKeyFromBytes(key))
  def decryptStream(in: InputStream, key: SecretKey): InputStream = {
    val cipher = Cipher.getInstance("blowfish")
    cipher.init(Cipher.DECRYPT_MODE, key)
    new CipherInputStream(in, cipher)
  }
  
  def encryptStream(in: InputStream, key: Array[Byte]): InputStream= encryptStream(in, blowfishKeyFromBytes(key))
  def encryptStream(in: InputStream, key: SecretKey): InputStream = {
    val cipher = Cipher.getInstance("blowfish")
    cipher.init(Cipher.ENCRYPT_MODE, key)
    new CipherInputStream(in, cipher)
  }
  
  def randomString(size: Int): String = {
    def addChar(pos: Int, lastRand: Int, sb: StringBuilder): StringBuilder = {
      if (pos >= size) sb 
      else {
        val randNum = if ((pos % 6) == 0) random.nextInt else lastRand
        sb.append((randNum & 0x1f) match {
          case n if n < 26 => ('A' + n).toChar
          case n => ('0' + (n - 26)).toChar
        })
        addChar(pos + 1, randNum >> 5, sb)
      }
    }
    addChar(0, 0, new StringBuilder(size)).toString
  }
  
  /**
  * Remove all the characters from a string exception a-z, A-Z, 0-9, and '_'
  */ 
  def clean(in : String) =  if (in == null) "" else in.replaceAll("[^a-zA-Z0-9_]", "")
  
  /**
  * Convert a 
  */
  def toBoolean(in: Any): boolean = {
    in match {
      case null => false
      case b : boolean => b
      case i: int => i != 0
      case lo: long => lo != 0
      case n : Number => n.intValue != 0
      case s : String => {
        val sl = s.toLowerCase
        if (sl.length == 0) false
        else {
          if (sl.charAt(0) == 't') true
          else toInt(s) != 0
        }
      }
      case None => false
      case Empty | Failure(_, _, _) => false
      case Full(n) => toBoolean(n)
      case Some(n) => toBoolean(n)
      case x :: xs => toBoolean(x)
      case o => toBoolean(o.toString)
    }
  }
  
  def toInt(in: Any): Int = {
    in match {
      case null => 0
      case n: Int => n
      case lo: Long => lo.toInt
      case n : Number => n.intValue
      case (n: Number) :: _ => n.intValue
      case Some(n) => toInt(n)
      case Full(n) => toInt(n)
      case None | Empty | Failure(_, _, _) => 0
      case s: String => parseNumber(s).toInt
      case d: java.util.Date => (d.getTime / 1000L).toInt
      case x :: xs => toInt(x)
      case o => toInt(o.toString)
    }
  }
  
  def toLong(in: Any): Long = {
    in match {
      case null => 0L
      case i: Int => i
      case n: Long => n
      case d: java.util.Date => d.getTime
      case n : Number => n.longValue
      case (n: Number) :: _ => n.longValue
      case Some(n) => toLong(n)
      case Full(n) => toLong(n)
      case None | Empty | Failure(_, _, _) => 0L
      case s: String => parseNumber(s)
      case x :: xs => toLong(x)
      case o => toLong(o.toString)
    }
  }
  
  def parseNumber(tin: String): Long = {
    def cToL(in: Char) = in.toLong - '0'.toLong
    def p(in: List[Char]) = in.takeWhile(isDigit).foldLeft(0L)((acc,c) => (acc * 10L) + cToL(c))
    
    if (tin eq null) 0L
    else {
      tin.trim.toList match {
        case '-' :: xs => -p(xs)
        case '+' :: xs => p(xs)
        case xs => p(xs)
      }
    }
  }
  

  
  def base64Encode(in: Array[Byte]): String = {
    new String((new Base64).encode(in))
  }
  
  def base64Decode(in: String): Array[Byte] = {
    (new Base64).decode(in.getBytes("UTF-8"))
  }
  
  def toByteArrayInputStream(in: InputStream) = {
    val ba = new Array[byte](4096)
    val bos = new ByteArrayOutputStream
    var len = 0
    while (len >= 0) {
      len = in.read(ba)
      if (len > 0) {
        bos.write(ba,0,len)
      }
    }
    
    new ByteArrayInputStream(bos.toByteArray)
  }
  
  def splitColonPair(in: String, first: String, second: String): (String, String) = {
    (in match {
      case null => List("")
      case s if s.indexOf(".") != -1 => s.roboSplit("\\.")
      case s => s.roboSplit(":")
    }) match {
      case f :: s :: _ => (f,s)
      case f :: Nil => (f, second)
      case _ => (first, second)
    }
  }
  
  
  def md5(in: Array[Byte]): Array[Byte] = (MessageDigest.getInstance("MD5")).digest(in)
  
  def md5(in: String): String = new String((new Base64) encode md5(in.getBytes("UTF-8")))
  
  def hash(in: String) : String = {
    new String((new Base64) encode (MessageDigest.getInstance("SHA")).digest(in.getBytes("UTF-8")))
  }
  
  def hash(in : Array[Byte]) : Array[byte] = {
    (MessageDigest.getInstance("SHA")).digest(in)
  }
  
  def hash256(in : Array[Byte]) : Array[byte] = {
    (MessageDigest.getInstance("SHA-256")).digest(in)
  }
  
  def hexDigest(in: Array[Byte]): String = {
    val binHash = (MessageDigest.getInstance("SHA")).digest(in)
    hexEncode(binHash)
  }
  
  
  def hash256(in : String): String = {
    new String((new Base64) encode (MessageDigest.getInstance("SHA-256")).digest(in.getBytes("UTF-8")))
  }
  
  def hexDigest256(in: Array[Byte]): String = {
    val binHash = (MessageDigest.getInstance("SHA-256")).digest(in)
    hexEncode(binHash)
  }
  
  def hexEncode(in: Array[Byte]): String = {
    val sb = new StringBuilder
    val len = in.length
    def addDigit(in: Array[byte], pos: int, len: int, sb: StringBuilder) {
      if (pos < len) {
        val b: int = in(pos)
        val msb = (b & 0xf0) >> 4
        val lsb = (b & 0x0f)
        sb.append((if (msb < 10) ('0' + msb).asInstanceOf[char] else ('a' + (msb - 10)).asInstanceOf[char]))
        sb.append((if (lsb < 10) ('0' + lsb).asInstanceOf[char] else ('a' + (lsb - 10)).asInstanceOf[char]))
        
        addDigit(in, pos + 1, len, sb)
      }
    }
    addDigit(in, 0, len, sb)
    sb.toString
  }
  
  implicit def nodeSeqToOptionString(in: NodeSeq): Can[String] = if (in.length == 0) Empty else Full(in.text)
  
  def readWholeFile(file: File): Array[Byte] = readWholeStream(new FileInputStream(file))
  
  def readWholeStream(in: InputStream): Array[Byte] = {
    val bos = new ByteArrayOutputStream
    val ba = new Array[byte](4096)
    
    def readOnce {
      val len = in.read(ba)
      if (len > 0) bos.write(ba, 0, len)
      if (len >= 0) readOnce
    }
    
    readOnce
    
    bos.toByteArray
  }
  
  def notEq(a: Array[Byte], b: Array[Byte]) = !isEq(a,b)
  def isEq(a: Array[Byte], b: Array[Byte]) = {
    def eq(a: Array[Byte], b: Array[Byte], pos: Int, len: Int): Boolean = {
      if (pos == len) true
      else if (a(pos) != b(pos)) false
      else eq(a,b, pos + 1, len)
    }
    
    val len = a.length
    len == b.length && eq(a,b,0,len)
  }
  
  def readWholeThing(in: Reader): String = {
    val bos = new StringBuilder
    val ba = new Array[char](4096)
    
    def readOnce {
      val len = in.read(ba)
      if (len < 0) return
      if (len > 0) bos.append(ba, 0, len)
      readOnce
    }
    
    readOnce
    
    bos.toString
  }
  

  /**
  * Looks for a named parameter in the XML element and return it if found
  */
  def xmlParam(in: NodeSeq, param: String): Can[String] = {
    val tmp = (in \ ("@"+param))
    if (tmp.length == 0) Empty else Full(tmp.text)
  }
  
  def processString(msg: String, subst: Map[String, String]): String = {
    val pat = Pattern.compile("\\<\\%\\=([^\\%]*)\\%\\>")
    val m = pat.matcher(msg)
    val ret = new StringBuffer
    while (m.find) {
      m.appendReplacement(ret, subst(m.group(1).trim))
    }
    m.appendTail(ret)
    ret.toString
  }
  
  private def capify(in: String, pos: Int, max: Int, lastLetter: Boolean, lastSymbol: Boolean, out: StringBuilder): Unit = if (pos >= max || pos >= in.length) return
  else {
    in.charAt(pos) match {
      case c if Character.isDigit(c) => out.append(c); capify(in, pos + 1, max, false, false, out)
      case c if Character.isLetter(c) => out.append(if (lastLetter) c else Character.toUpperCase(c)) ; capify(in, pos + 1, max, true, false, out)
      case c if (c == ' ' || c == '_') && !lastSymbol => out.append(c) ; capify(in, pos + 1, max, false, true, out)
      case _ => capify(in, pos + 1, max, false, true, out)
    }
  }
  
  def capify(in: String): String = {
    val tmp = ((in match {
      case null => ""
      case s => s
    }).trim match {
      case "" => "n/a"
      case s => s
    }).toLowerCase
    val sb = new StringBuilder
    capify(tmp, 0, 250, false, false, sb)
    sb.toString
  }  
  
  /*
  private val defaultFinder = getClass.getResource _
  private var _finder = defaultFinder
  
  def setResourceFinder(in: (String) => java.net.URL):unit = synchronized {
  _finder = in
  }
  
  def resourceFinder = synchronized {_finder}
  
  def getResource(name: String): Can[java.net.URL] = resourceFinder(name) match {case null => defaultFinder(name) match {case null => Empty; case s => Full(s)} ; case s => Full(s)} 
  def getResourceAsStream(name: String): Can[java.io.InputStream] = getResource(name).map(_.openStream)
  def loadResource(name: String): Can[Array[Byte]] = getResourceAsStream(name).map{
  stream =>
  val buffer = new Array[byte](2048)
  val out = new ByteArrayOutputStream
  def reader {
  val len = stream.read(buffer)
  if (len < 0) return
  else if (len > 0) out.write(buffer, 0, len)
  reader
  }
  reader
  stream.close
  out.toByteArray
  }
  def loadResourceAsXml(name: String): Can[NodeSeq] = loadResourceAsString(name).flatMap(s =>PCDataXmlParser(s))
  def loadResourceAsString(name: String): Can[String] = loadResource(name).map(s => new String(s, "UTF-8"))
  */
  
  /*
  def script(theScript: String): NodeSeq = (<script>
  // {Unparsed("""<![CDATA[
  """+theScript+"""
  // ]]>
  """)}</script>)
  */
  
  /**
  * Optional cons that implements the expression: expr ?> value ::: List
  */
  class OptiCons(expr: Boolean) {
    def ?>[T](f: => T): List[T] = if (expr) List(f) else Nil
  }
  
  implicit def toOptiCons(expr: Boolean): OptiCons = new OptiCons(expr)
  
  implicit def toSuperList[T](in: List[T]): SuperList[T] = new SuperList(in)
  
  def listIf[T](expr: Boolean)(f: => T): List[T] = if(expr) List(f) else Nil
  
  abstract class MyOption[+A] {
    def |[B >: A](default: => B): B
  }
  case class MySome[+A](x: A) extends MyOption[A] {
    def |[B >: A](default: => B): B  = x
  }
  case object MyNone extends MyOption[Nothing] {
    def |[B ](default: => B): B  = default
  }
  
  class Boolean2(b: Boolean) {
    def ? [A](first: A): MyOption[A] = {
      if (b) MySome(first)
      else MyNone
    }
  }
  
  implicit def boolean2(b: Boolean) = new Boolean2(b) 
  
  implicit def pairToUnprefixed(in: (String, Any)): UnprefixedAttribute = new UnprefixedAttribute(in._1, Text(in._2.toString), Null)
  
  //implicit def optionToDouble[T](in: Option[Option[T]]): DoubleOption[T] = new DoubleOption(in)
  
  implicit def stringToSuper(in: String): SuperString = new SuperString(in)
  
  /**
  * Given an incoming list, return a set of lists that is the original list rotated through all its positions
  *
  * @param in the list to rotate
  *
  * @return all the rotations of the list
  */ 
  def rotateList[T](in: List[T]): List[List[T]] = {
    def doIt(in: List[T], cnt: Int): List[List[T]] = ((in, cnt): @unchecked) match {
      case (_, 0) => Nil
      case (x :: xs, cnt) => in :: doIt(xs ::: List(x), cnt - 1)
    }
    doIt(in, in.length)
  }
  
  /**
  * Given a list, return all the permutations of the list.
  *
  * @param in -- the list
  *
  * @return all the permutations of the list
  */
  def permuteList[T](in: List[T]): List[List[T]] = (in: @unchecked) match {
    case Nil => Nil
    case x :: Nil => List(List(x))
    case xs => rotateList(xs).flatMap(x => (x: @unchecked) match{case x :: xs => permuteList(xs).map(x :: _) case _ => Nil})
  }
  
  /**
  * Given a list, return all the permutations including the removal of items (does not return a Nil list unless in is Nil).
  *
  * @param in the list to permute
  *
  * @return all the permutations of the list including sublists, sorted in longest to shortest
  */
  def permuteWithSublists[T](in: List[T]): List[List[T]] = {
    def internal(in: List[T]): List[List[T]] = in match {
      case Nil => Nil
      case x :: Nil => List(List(x))
      case xs => val rot = rotateList(xs)
      val ret = rot.flatMap(z => (z: @unchecked) match {case x :: xs => permuteList(xs).map(x :: _)})
      ret ::: rot.map(z => (z: @unchecked) match {case x :: xs => xs}).flatMap(internal(_))
    } 
    
    internal(in).removeDuplicates.sort(_.length > _.length)
    
  }
}

class SuperList[T](val what: List[T]) {
  def permute = Helpers.permuteList(what)
  def rotate = Helpers.rotateList(what)
  def permuteAll = Helpers.permuteWithSublists(what)
  def headOr(other: => T): T = if (what.isEmpty) other else what.head
  def or(other: => List[T]): List[T] = if (!what.isEmpty) what else other
  def str: String = what.mkString("")
  def comma: String = what.mkString(", ")
  def join(str: String) = what.mkString(str)
  def ? : Boolean = !what.isEmpty
  def replace(pos: Int, withWhat: T): List[T] = {
    def repl(pos: Int, withWhat: T, rest: List[T]): List[T] = rest match {
      case Nil => Nil
      case x :: xs if pos <= 0 => withWhat :: xs
      case x :: xs => x :: repl(pos - 1, withWhat, xs)
    }
    repl(pos, withWhat, what)
  }
}

class SuperString(val what: String) {
  def roboSplit(spl: String): List[String] = what match {case null => Nil case s => s.split(spl).toList.map(_.trim).filter(_.length > 0)}
  def splitAt(chr: String): List[(String, String)] = what.indexOf(chr) match {
    case -1 => Nil
    case n => List((what.substring(0, n).trim, what.substring(n + chr.length).trim))
  }
  
  def encJs: String = {
    if (what eq null) "null"
    else {
      val len = what.length
      val sb = new StringBuilder(len * 2)
      sb.append('\'')
      var pos = 0
      while (pos < len) {
        what.charAt(pos) match {
          case c @ ('\\' | '\'') => sb.append(escChar(c))
          case c if c < ' ' || c > '~' => sb.append(escChar(c))
          case c => sb.append(c) 
        }
        pos += 1
      }
      sb.append('\'')
      sb.toString
    }
  }
  
  
  def escChar(in: Char): String = {
    val ret = Integer.toString(in.toInt, 16)
    "\\u"+("0000".substring(ret.length))+ret
  }
  
  def commafy: String = {
    if (what eq null) null
    else {
      val toDo = what.toList.reverse
      
      def commaIt(in: List[Char]): List[Char] = in match {
        case Nil => in
        case x :: Nil => in
        case x1 :: x2 :: Nil => in
        case x1 :: x2 :: x3 :: Nil => in
        case x1 :: x2 :: x3 :: xs => x1 :: x2 :: x3 :: ',' :: commaIt(xs)
      }
      
      commaIt(toDo).reverse.mkString("")
    }
  }
}

// vim: set ts=2 sw=2 et:
