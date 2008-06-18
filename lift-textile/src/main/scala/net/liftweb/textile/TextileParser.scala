package net.liftweb.textile

/* 
 * Copyright 2006-2008 WorldWide Conferencing, LLC
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
import scala.xml.{Elem => XmlElem, MetaData, NodeSeq, Null, Text, TopScope, Unparsed, UnprefixedAttribute, Group, Node}
import scala.collection.mutable.HashMap

/**
 * The textile parser
 *
 * ported to the scala.util.parsing.combinator style of parsing by Adriaan Moors
 */ 
 object TextileParser {
   type RewriteFunc = WikiURLInfo => (String, NodeSeq, Option[String])
   
   case class WikiURLInfo(word: String, category: Option[String]) {
     override def toString = category match {
       case Some(cat) => cat+"/"+word
       case _ => word
     }
   }
   
   
   def parse(toParse: String, urlRewrite: Option[RewriteFunc]): Option[Lst] =
   parse(toParse, urlRewrite, false)
   
   /**
   * Take a string and return the parsed value.
   * Lst is a list of Textile parsed elements.  You can do a .toHtml on the Lst
   * to get the XHTML result to send to the browser.
   * int will be the number of characters parsed.
   */
   def parse(_toParse: String, urlRewrite: Option[RewriteFunc], disableLinks: Boolean): Option[Lst] = {
     val toParse = _toParse match {
       case null => "null\n\n"
       case s if !s.endsWith("\n\n") => s + "\n\n"
       case s => s
     }
     
     def findRefs(in : List[Textile]) : List[Pair[String,String]] = in match {
       case (s : ARef) :: rest => {Pair(s.name, s.href) :: findRefs(rest)}
       case (s : ATextile) :: rest => {findRefs(s.theElems) ::: findRefs(rest)}
       case x :: rest => {findRefs(rest)}
       case _ => Nil
     }
     
     
     def fixRefs(in : List[Textile], refs: HashMap[String, String]): Unit = in match {
       case Nil =>
       
       case (s: RefAnchor) :: rest => 
       refs.get(s.ref) match {case Some(tr) => s.href = tr case None => s.href = "javascript://"}
       fixRefs(rest, refs)
       
       case (s: ATextile) :: rest => 
       fixRefs(s.theElems, refs)
       fixRefs(rest, refs)
       
       case _ :: rest => fixRefs(rest, refs)
       
       case huh => println(huh)
     }
     
     val parser = new TextileParsers(urlRewrite, disableLinks)
     val lst = parser.document(new scala.util.parsing.input.CharArrayReader(toParse.toCharArray()))
     
     lst map { it =>
       val tr = findRefs(List(it))
	     val refs = new HashMap[String,String];
	     tr.foreach{b => refs(b._1) = b._2}
       
	     fixRefs(List(it), refs)
       
	     Some(it)
	   } getOrElse None
   }
   
   def toHtml(toParse: String, wikiUrlFunc: Option[RewriteFunc], disableLinks: Boolean): NodeSeq = {
     parse(toParse, wikiUrlFunc, disableLinks).map(_.toHtml) getOrElse Text("")
   }
   
   def toHtml(toParse: String, disableLinks: Boolean): NodeSeq = 
   toHtml(toParse, None, disableLinks)
   
   def toHtml(toParse: String, wikiUrlFunc: Option[RewriteFunc]): NodeSeq = 
   toHtml(toParse, wikiUrlFunc, false)
   
   def toHtml(toParse: String): NodeSeq = 
   toHtml(toParse, None, false)

   /**
    * Useful helper function for stripping out the surrounding <p> tags.
    */
   def paraFixer(in: NodeSeq): NodeSeq = in match {
     case g: Group => paraFixer(g.nodes)
     case e: XmlElem if e.label == "p" => e.child
     case e: XmlElem => e
     case ns: Seq[Node] if ns.length == 2 &&
     ns(1).text.trim.length == 0 => paraFixer(ns(0))
     case x => x
   }
   
    /**
   * the thing that actually does the textile parsing
   */
  class TextileParsers(wikiUrlFunc: Option[RewriteFunc], disableLinks: Boolean) extends Parsers with ImplicitConversions {
    type Elem = char

    type UnitParser=Parser[Unit]
/*
    def paraFixer(in: NodeSeq): NodeSeq = in match {
	    case g: Group => paraFixer(g.nodes)
	    case e: XmlElem if e.label == "p" => e.child
	    case e: XmlElem => e
	    case ns: Seq[Node] if ns.length == 2 &&
	    ns(1).text.trim.length == 0 => paraFixer(ns(0))
	    case x => x
	  }
  */  
/*    def elem(e: Elem): Parser[Elem] = accept(e)
    def elem(k: String, p: Elem => Boolean) = acceptIf(p) expected(k)
*/
    implicit def discard[T](p: Parser[T]): Parser[Unit] = p ^^ {x => ()}
    
    def document : Parser[Lst] = rep(paragraph) ^^ Lst
    // final val EofCh = '\032'
    private def chrExcept(cs: Char*): Parser[char] = elem("", {c => ('\032' :: cs.toList) forall (_ != c)}) //{x =>  !cs.contains(x)})
    private def mkString(cs: List[Any]) = cs.mkString("")

    implicit def str2chars(s: String): List[Char] = stringWrapper(s).toList
    
    def num = rep1(elem("digit", Character.isDigit)) ^^ mkString

    def enclosed(delim: Char, what: String, pred: Char => Boolean) = delim ~> str(what, {c => c != delim && pred(c)}) <~ delim
    def str(what: String, pred: Char => Boolean) = rep(elem(what, pred)) ^^ mkString
    def str1(what: String, pred: Char => Boolean) = rep1(elem(what, pred)) ^^ mkString
    def chrsExcept(cs: Char*): Parser[String] = rep1(chrExcept(cs : _*)) ^^ mkString
    
    
    val beginl = Parser[Unit]{ in =>
      if(in.pos.column==1) Success((), in) else Failure("at column "+in.pos.column+", not beginning of line", in)
    }

    val beginlS = beginl ~ rep(' ')
    
    /**
     * is it a blank line?  Start of line, some spaces, and an end of line
     */
    def blankLine: Parser[Textile] = beginlS ~ '\n' ^^^ BlankLine()
    

    /**
     * Line elements make up paragraphs and block elements
     */
    def lineElem : Parser[Textile] = {
      not(discard(blankLine)) ~> (endOfLine | image | footnote_def | 
                        anchor | dimension | elipsis  | 
                        copyright | trademark | registered | emDash | 
                        enDash | italic | emph | bold  | 
                        cite |  span | code | delete | insert | 
                        sup | sub | strong | html | single_quote | quote | acronym | charBlock)
    }

    /**
     * If we've got an italic (__text__), the parser doesn't do well with a single underscore, so
     * we exclude looking for _emph_
     */
    def lineElem_notEmph : Parser[Textile] = {
      not(discard(blankLine)) ~> (endOfLine | image | footnote_def | anchor | 
                          dimension | elipsis | 
                          copyright | trademark | registered | emDash | enDash | italic | 
                          bold  | 
                          cite |  span| code | delete | insert| sup | sub | strong  | 
                          html| single_quote | quote | acronym | charBlock)        
    }

    /**
    * Don't look for *strong* if we're currently in a **bold** element
    */
    def lineElem_notStrong : Parser[Textile] = {
      not(discard(blankLine)) ~> (endOfLine | image | footnote_def | anchor | 
                          dimension | elipsis | 
                          copyright | trademark | registered | emDash | enDash | italic | 
                          emph | 
                          cite |  span | code | delete | insert  | sup | 
                          sub | bold  | html| single_quote | quote | acronym  | charBlock)
    }
    

    /**
    * Look for an acronym, but don't mistake registered, copyright, and trademarks
    */
    def acronym : Parser[Acronym] =  ((chrsExcept(' ', '(', '\n') <~ not(discard(copyright | trademark | registered))) ~ ('(' ~> chrsExcept(')', '\n') <~ ')') ) ^^ flatten2(Acronym) 
    
    /**
    * is it an !image!
    */
    def image : Parser[Textile] = ('!' ~> opt('<')) ~ opt('>') ~ (rep1(not(accept('!')) ~> elem("", validUrlChar)) ^^ mkString) ~ 
       (opt('(' ~> chrsExcept(')', '\n') <~ ')') <~ '!') ~
       opt(':' ~> url ^? {case a: Anchor => a}) ^^ { case fl ~ fr ~ img_url ~ alt ~ link => 
               Image(img_url, alt getOrElse "", link.map(_.href) getOrElse null, 
                    if (!fl.isEmpty) List(AnyAttribute("style", "float:left"))
                    else if (!fr.isEmpty) List(AnyAttribute("style", "float:right"))
                    else Nil)} 

    
    /**
    * [footnote]
    */
    def footnote_def : Parser[Textile] = '[' ~> num <~ ']' ^^ FootnoteDef 
    
    /**
    * various things that make up an anchor (a tag)
    */
    def anchor = url | quote_url | quote_ref | a_ref | escCamelUrl | camelUrl


    def upper: Parser[Char] = elem("Uppercase character", Character.isUpperCase)
    def lower: Parser[Char] = elem("Lowercase character", Character.isLowerCase)
    def lowerOrNumber: Parser[Char] = elem("Lowercase character or digit", c => Character.isLowerCase(c) || Character.isDigit(c))

    /**
    * Don't use the CamelCase thing for a wikiword if it's prefixed by
    * a backslash
    */
    def escCamelUrl : Parser[CharBlock] = ('\\'  ~ upper ~ lower ~ rep(lowerOrNumber) ~ 
        upper ~ lower ~ rep(lowerOrNumber) ~
        rep(upper ~ lower ~ rep(lowerOrNumber) ^^ {case c1 ~ c2 ~ s => mkString(List(c1, c2))+ mkString(s)})) ^^ 
          {case c1 ~ c2 ~ c3 ~ s4 ~ c5 ~ c6 ~ s7 ~ s8 => CharBlock(mkString(List(c1, c2, c3)) + mkString(s4) + mkString(List(c5, c6)) + mkString(s7) + s8.mkString(""))}
    
      // combine chars to string
      /*
      // the ~~ combinator together with these conversions would work nicely, were it not for the covariance of Parser's first type param :-(
      implicit def c2s(c: Char): String = c + ""
      implicit def cs2s(cs: List[Char]): String = cs.mkString("")
      implicit def ss2s(ss: List[String]): String = ss.mkString("")
      implicit def combine[C <% String, D <% String](c: C, d: D): String = c + d*/
      
   
    
    /**
    * is the work camelized?
    */
    def camelizedWord : Parser[CharBlock] = upper ~ lower ~ rep(lowerOrNumber) ^^ { case u ~ l ~ cs => CharBlock(mkString(u :: (l :: cs))) }
    
    /**
    * a WikiWord
    */
    // def camelUrl : Parser[Textile] = camelizedWord ~ rep1(camelizedWord) ^^ { case c ~ cs => val ss = mkString((c :: cs).map(_.s)); WikiAnchor(Nil, ss, ss, Nil, wikiUrlFunc)}
    
     def lowerWord: Parser[String] = lower ~ rep1(lowerOrNumber) ^^ {case x ~ xs => (x :: xs).mkString("")}
     

     /**
     * a WikiWord
     */
     def camelUrl: Parser[Textile] = noCatCamelUrl | catCamelUrl | tickUrl | catTickUrl
     
     def catTickUrl: Parser[WikiAnchor] = lowerWord ~ ':' ~ tickUrl ^^ 
     {case cat ~ colon ~ rest => WikiAnchor(rest.word, Some(cat), wikiUrlFunc)}
     
     def tickUrl: Parser[WikiAnchor] = '`' ~ rep1(tickUrlChar) ~ '`' ^^ 
     {case tick ~ c ~ tick2 => WikiAnchor(c.mkString(""), None, wikiUrlFunc)}
     
     def isTickUrlChar(c: Char): Boolean = c.isLetter || c.isDigit ||
     c == ' ' || c == '@'
     
     def tickUrlChar: Parser[Char] = elem("tick url char", isTickUrlChar)
     
     def noCatCamelUrl: Parser[Textile] = camelizedWord ~ rep1(camelizedWord) ^^ { case c ~ cs => val ss = mkString((c :: cs).map(_.s)); WikiAnchor(ss, None, wikiUrlFunc)}
     
     def catCamelUrl: Parser[Textile] = lowerWord ~ ':' ~ camelizedWord ~ rep1(camelizedWord) ^^ {
     case cat ~ colon ~ c ~ cs => val ss = mkString((c :: cs).map(_.s)); WikiAnchor(ss, Some(cat), wikiUrlFunc)}
         
    
    def urlStr = (rep1(elem("", validUrlChar))^^ mkString)
    
    /**
    * "reference":reference
    */
    def quote_ref : Parser[Textile] = ('"' ~> chrsExcept('"', '\n') <~ '"') ~ (':' ~> urlStr) ^^
      {case fs ~ rc => RefAnchor(Nil, rc, fs, Nil)}
    
    
    def httpStr = (accept("https://") ^^ mkString | accept("http://") ^^ mkString) ~ urlStr ^^ { case protocol ~ rc => protocol + rc }
    
    /**
    * "google":http://google.com
    */
    def quote_url : Parser[Textile] = ('"' ~> chrsExcept('"', '\n')) ~ ('"' ~> ':' ~> httpStr) ^^
      { case fs ~ url => Anchor(Nil, url, fs, Nil, disableLinks) }
    
    /**
    * [reference]:http://reference.com
    */

    def a_ref : Parser[Textile] = ('[' ~> urlStr <~ ']') ~ url ^^ {case  fr ~ (url: Anchor) => ARef(fr, url.href) }

    /**
    * http://google.com
    */
    def url : Parser[Textile] = httpStr ^^ { u => Anchor(Nil, u, u, Nil, disableLinks) }
    
    /**
    * a valid character in a URL
    */
    def validUrlChar(c : char) : boolean = {
      Character.isLetterOrDigit(c) || c == '/' || c == '%' || c == '&' || c == '?' || c == '#' || 
      c == '$' || c == '.' ||
      c == '-' || c == ':' || c == '_'
    }
    
    
    /**
    * an EOL character
    */
    def endOfLine : Parser[Textile] = '\n' ^^^ EOL()
    
    // replaced by checking the position of the current input (not that beginl does not consume any input, 
    // so repeatedly calling beginningOfLine is not a good idea -- that will either loop forever or fail immediately)
    //def beginningOfLine : Parser[Textile] = beginl ^^ BOL
    
    
    /**
    * if we're in a &lt;pre&gt; block, an end of line is just an end of line.  We 
    * pass the '\n' on though.
    */
    def preEndOfLine : Parser[Textile] = '\n' ^^^ CharBlock("\n")
    
    
    /**
    * a &lt;pre&gt; block.  Just send text of through, unmolested.
    */
    def preBlock : Parser[Textile] = beginlS ~> accept("<pre") ~> rep(' ') ~> '>' ~>
      rep(not(discard(accept("</pre"))) ~> (preEndOfLine | charBlock )) <~ accept("</pre") <~ rep(' ') <~ '>' <~ rep(' ') <~ '\n' ^^ { 
        case elms => Pre(reduceCharBlocks(elms), Nil) }
    
    /**
    * (c)
    */
    def copyright : Parser[Textile] = '(' ~ (accept('c') | 'C') ~ ')' ^^^ Copyright()  // accept necesary because of clash with | on char's
    
    
    def validTagChar(c : char) = Character.isDigit(c) || Character.isLetter(c) || c == '_'
    def validStyleChar(c : char) = Character.isDigit(c) || Character.isLetter(c) || c == '.' || c == ' ' || c == ';' || c == '#'
    def validClassChar(c : char) = Character.isDigit(c) || Character.isLetter(c) || c == '.'

    def validTag = rep1(elem("valid tag character", validTagChar)) ^^ mkString ^? {case s if isValidTag(s) => s}
    
    /**
    * An HTML block is made up of single HTML tag (e.g., &lt;br /&gt;) and 
    * tags that contain other stuff
    */
    def html = single_html | multi_html
    
    def closingTag(tag: String) = accept("</") ~ accept(tag) ~ rep(' ') ~ '>'
    
    /**
    * an HTML tag that contains other stuff
    */
    def multi_html : Parser[Textile] = ('<' ~> validTag) >> { tag => 
      rep(tag_attr) ~ 
      ('>' ~> rep(not(discard(closingTag(tag))) ~> (lineElem | paragraph)) <~ closingTag(tag)) ^^ {
           case attrs ~ body => HTML(tag, reduceCharBlocks(body), attrs)}}
      
    /**
    * A stand-alone tag
    */
    def single_html : Parser[Textile] = ('<' ~> validTag) ~ (rep(tag_attr) <~ accept("/>")) ^^ { case tag ~ attrs => HTML(tag, Nil, attrs) }

    def tag_attr = single_quote_attr | double_quote_attr
    
    def attr_name(c : char) = Character.isLetterOrDigit(c) || c == '_' || c == '-'

    def attr_value(c : char) = {c >= ' '}

    /**
    * an attribute with single quotes
    */
    def single_quote_attr =  (rep(' ') ~> str1("attribute name", attr_name)) ~ 
      ('=' ~> enclosed('\'', "attribute value", attr_value(_))) ^^ flatten2(AnyAttribute)
    
    /**
    * an attribute with double quotes
    */
    def double_quote_attr : Parser[Attribute] = (rep(' ') ~> str1("attribute name", attr_name) <~ '=') ~ 
      enclosed('"', "attribute value", attr_value(_)) ^^ flatten2(AnyAttribute)
    
    /**
    * is it a valid HTML tag?  This list should be expanded
    */
    def isValidTag(in : String) = in == "b" || in == "em" || in == "code" || in == "div" || in == "span"


    /**
    * A "dimension" pretty printer
    */
    def dimension : Parser[Textile] = accept(" x ") ^^^ Dimension()

    def registered : Parser[Textile] = '(' ~ (accept('r') | 'R') ~ ')' ^^^ Register()

    def trademark : Parser[Textile] = '(' ~ (accept('t') | 'T') ~ (accept('m') | 'M') ~ ')' ^^^ Trademark()
    
    def elipsis : Parser[Textile] = accept("...") ^^^ Elipsis()

    def emDash : Parser[Textile] = accept(" -- ") ^^^ EmDash()

    def enDash : Parser[Textile] = accept(" - ") ^^^ EnDash()

    def single_quote : Parser[Textile] = '\'' ^^^ SingleQuote()

    
    def bullet(depth : int, numbered : boolean) : Parser[Textile] = {
      val oneBullet = if (numbered) accept('#') else accept('*')
        
      def bullet_line(depth : int, numbered : boolean) : Parser[Textile] =  beginlS ~> repN(depth+1, oneBullet) ~> rep(not(discard('\n')) ~> lineElem) <~ '\n' ^^ {
        case elms => BulletLine(reduceCharBlocks(elms), Nil) }
    
      bullet_line(depth, numbered) ~
        (rep1(bullet(depth + 1, numbered) | bullet_line(depth, numbered))) ^^ {case fbl ~ abl => Bullet(fbl :: abl, numbered)}
    }
    
    def formattedLineElem[Q <% Parser[Any]](m: Q): Parser[List[Textile] ~ List[Attribute]] = formattedLineElem(m, rep(attribute))
    def formattedLineElem[Q <% Parser[Any]](m: Q, p: Parser[List[Attribute]]): Parser[List[Textile] ~ List[Attribute]]
      = (spaceOrStart ~> m ~> p) ~ (rep1(not(discard(m)) ~> lineElem) <~ m) <~ spaceOrEnd ^^ { case attrs ~ ln => new ~(reduceCharBlocks(ln), attrs) }

    def spaceOrStart: UnitParser = beginl | accept(' ')
    def spaceOrEnd: UnitParser = accept(' ') | discard(endOfLine)

    // TODO: generalize formattedLineElem some more
    def bold : Parser[Textile] = (accept("**") ~> rep(attribute)) ~ (rep1(not(discard(accept("**"))) ~> lineElem_notStrong) <~ accept("**")) ^^ { 
      case attrs ~ ln => Bold(reduceCharBlocks(ln), attrs) }
    
    def strong : Parser[Textile] = formattedLineElem('*') ^^ flatten2(Strong)
    
    def cite : Parser[Textile] = formattedLineElem(accept("??")) ^^ flatten2(Cite)
    
    def code : Parser[Textile] = formattedLineElem('@') ^^ flatten2(Code)
    
    def styleElem : Parser[StyleElem] = str1("style", validStyleChar) ~ (':' ~> str1("style", validStyleChar)) ^^ flatten2(StyleElem)
    
    def attribute = style | class_id | the_class | the_id | the_lang 
    
    def para_attribute = attribute | fill_align | left_align | right_align |
    center_align | top_align | bottom_align | em_left | em_right;
    
    def left_align : Parser[Attribute] = elem('<') ^^ Align
    
    def right_align : Parser[Attribute] = elem('>') ^^ Align
    
    def center_align : Parser[Attribute] = elem('=') ^^ Align
    
    def top_align : Parser[Attribute] = elem('^') ^^ Align
    
    def bottom_align : Parser[Attribute] = elem('~') ^^ Align
    
    def fill_align : Parser[Attribute] = accept("<>") ^^^ Align('f')
    
    def em_left : Parser[Attribute] = rep1(elem('(')) ^^ {xs => Em(xs.length)}

    def em_right : Parser[Attribute] = rep1(elem(')')) ^^ {xs => Em(-xs.length)}
    
    def class_id : Parser[Attribute] = ('(' ~> str1("class", validClassChar)) ~ ('#' ~> str1("class", validClassChar) <~ ')') ^^ flatten2(ClassAndId)

    def the_id : Parser[Attribute] = '(' ~> '#' ~> str1("class", validClassChar) <~ ')' ^^ {ri => ClassAndId(null, ri)}

    def the_lang : Parser[Attribute] = '[' ~> str1("class", validClassChar) <~ ']' ^^ Lang

    def the_class : Parser[Attribute] = '(' ~> str1("class", validClassChar) <~ ')' ^^ {rc => ClassAndId(rc, null)}
    
    
    def style : Parser[Attribute] = '{' ~> repsep(styleElem, ';') <~ '}' ^^ Style
    
    def span : Parser[Textile] = formattedLineElem('%', opt(style) ^^ {s => s.toList}) ^^ flatten2(Span)
    
    def delete : Parser[Textile] = formattedLineElem('-') ^^ flatten2(Delete)

    
    def insert : Parser[Textile] = formattedLineElem('+') ^^ flatten2(Ins) 
    
    def sup : Parser[Textile] = formattedLineElem('^') ^^ flatten2(Super) 
    
    def sub : Parser[Textile] =  formattedLineElem('~') ^^ flatten2(Sub) 

    def italic : Parser[Textile] = formattedLineElem(accept("__")) ^^ flatten2(Italic) 
    
    def emph : Parser[Textile] = formattedLineElem('_') ^^ flatten2(Emph)
    
    def quote : Parser[Textile] = formattedLineElem('"', success(Nil)) ^^ flatten2((x, y) => Quoted(x))
    
    def reduceCharBlocks(in : List[Textile]) : List[Textile] = 
      (in: @unchecked) match {
        case Nil => Nil
        // this is now done (hacked) in flattenAndDropLastEOL
//        case EOL() :: BOL() :: rest => EOL :: reduceCharBlocks(rest)
//        case EOL() :: rest => reduceCharBlocks(rest)
        case CharBlock(s1) :: CharBlock(s2) :: rest => reduceCharBlocks(CharBlock(s1 + s2) :: rest)
        case x :: xs => x :: reduceCharBlocks(xs)
      }
    
    def charBlock : Parser[Textile] = chrExcept('\n') ^^ {c => CharBlock(c.toString)}

    
    
    def blankPara : Parser[Textile] = discard(rep1(blankLine)) ^^^ BlankLine()
    
    def not_blank_line : Parser[Textile] = not(discard(blankLine)) ~> lineElem
    
    def head_line : Parser[Textile] = (beginl ~> 'h' ~> elem("1, 2 or 3", {c => c == '1' | c == '2' | c == '3'})) ~ 
      rep(para_attribute) ~ 
      (accept(". ") ~> rep1(not_blank_line) <~ blankLine) ^^ {
        case n ~ attrs ~ ln => Header(n - '0', reduceCharBlocks(ln), attrs) }
    
    def blockquote : Parser[Textile] = (beginl ~> accept("bq") ~> rep(para_attribute)) ~ (accept(". ") ~> rep1(not_blank_line) <~ blankLine) ^^ {
      case attrs ~ ln => BlockQuote(reduceCharBlocks(ln), attrs)}

    
    def footnote : Parser[Textile] = (beginl ~> accept("fn") ~> num) ~ rep(para_attribute) ~ (accept(". ") ~> rep1(not_blank_line) <~ blankLine) ^^ {
      case dr ~ attrs ~ ln => Footnote(reduceCharBlocks(ln), attrs, dr)}
    
    def first_paraAttrs : Parser[TableDef] = beginlS ~> 'p' ~> rep(para_attribute) <~ '.' ^^ TableDef
    
    def normPara : Parser[Textile] = opt(first_paraAttrs) ~ rep1(not_blank_line) ~ blankLine ^^ {
      case td ~ fp ~ _ => Paragraph(reduceCharBlocks(fp), td.map(_.attrs) getOrElse(Nil))}
    
    def table_attribute = para_attribute | row_span | col_span
    
    
    
    def row_span : Parser[Attribute] = '/' ~> num ^^ {s => AnyAttribute("rowspan", s )}
    
    def col_span : Parser[Attribute] = '\\' ~> num ^^ {s => AnyAttribute("colspan", s )}
    
    def table : Parser[Textile] = opt(table_def) ~ opt(table_header) ~ rep1(table_row) ^^ {
      case td ~ th ~ tr => Table(th.toList ::: tr, td.map(_.attrs) getOrElse Nil)  }
    
    def table_def : Parser[TableDef] = (beginlS ~> accept("table") ~> rep(para_attribute) <~ '.' <~ rep(' ') <~ '\n') ^^ TableDef
    
    def row_def : Parser[TableDef] = rep1(table_attribute) <~ '.' ^^ TableDef

    def table_row : Parser[Textile] = (beginlS ~> opt(row_def)) ~ (rep(' ') ~> '|' ~> rep1(table_element(false)) <~ rep(' ') <~ '\n') ^^ {
      case td ~ re => TableRow(re, td.map(_.attrs) getOrElse Nil)}

    def table_header : Parser[Textile] = (beginlS ~> opt(row_def)) ~ (rep(' ') ~> accept("|_.") ~> rep1sep(table_element(true), accept("_.")) <~ rep(' ') <~ '\n') ^^ {
      case td ~ re => TableRow(re, td.map(_.attrs) getOrElse Nil)}
    
    def table_element(isHeader : boolean) : Parser[Textile] = opt(row_def) ~ (rep(not(discard(accept('|') | '\n')) ~> lineElem) <~ '|') ^^ {
      case td ~ el => TableElement(reduceCharBlocks(el), isHeader, td.map(_.attrs) getOrElse Nil)}

    def paragraph : Parser[Textile] =  
      preBlock | footnote | table | bullet(0, false) | bullet(0, true) | blockquote | head_line | blankPara | normPara
  }
  


  abstract class Textile {
    def toHtml : NodeSeq
  }
  
  case class Style(elms : List[StyleElem]) extends Attribute {
    def name(which : int) : String = "style"
    def value(which : int) : String = elms.mkString("",";","")
  }

  abstract class Attribute extends Textile {
    def toHtml = null
    def name(which : int) : String
    def value(which : int) : String
    def fieldCnt = 1
    def toList : List[Pair[String, String]] = {
      var ret : List[Pair[String,String]] = Nil
      var cnt = fieldCnt
      while (cnt > 0) {
        cnt = cnt -1
        val tn = name(cnt)
        val tv = value(cnt)
        if ((tn ne null) && (tv ne null)) ret = Pair(tn,tv) :: ret
      }
      ret
    }
  }

  case class AnyAttribute(name : String, value : String) extends Attribute {
    def name(which : int) : String = name
    def value(which : int) : String = value
  }

  case class ClassAndId(className : String, idName : String) extends Attribute {
    override def fieldCnt = 2
    def name(which : int) : String = {
      which match {
        case 0 => if ((className ne null) && className.length > 0) "class" else null
        case 1 => if ((idName ne null) && idName.length > 0) "id" else null
      }
    }
    def value(which : int) : String = {
      which match {
        case 0 => if ((className ne null) && className.length > 0) className else null
        case 1 => if ((idName ne null) && idName.length > 0) idName else null
      }
    }
    
  }

  case class Lang(lang : String) extends Attribute {
    def name(which : int) : String = "lang"
    def value(which : int) : String = lang
  }

  case class Align(align : char) extends Attribute {
    def name(which : int) : String = "style"
    def value(which : int) : String = {
      align match {
        case '<' => "text-align:left";
        case '>' => "text-align:right";
        case '=' => "text-align:center";
        case '^' => "vertical-align:top";
        case '~' => "vertical-align:bottom";
        case 'f' | 'j' => "text-align:justify";
        
      }
    }
  }

  case class Em(cnt : int) extends Attribute {
    def name(which : int) : String = "style"
    def value(which : int) : String = {
      if (cnt > 0) "padding-left:"+cnt+"em" else "padding-right:"+(-cnt)+"em"
    }
  }



  case class StyleElem(name : String, value : String) extends Textile {
    def toHtml = null
    override def toString = name+":"+value
  }

  abstract class ATextile(val theElems : List[Textile], attrs : List[Attribute]) extends Textile {
    def fromStyle(st : List[Attribute]) : MetaData = {
      def toList(st : List[Attribute]) : List[Pair[String, String]] = {
        st match {
          case Nil => Nil
          case x :: xs => x.toList ::: toList(xs)
        }
      }
      
      def crunchStyle(st : List[Pair[String, String]]) : List[Pair[String,String]] = {
        val p = st.partition {a => a._1 == "style"}
        if (p._1 == Nil) p._2 else
          (p._1.reduceLeft{(a : Pair[String,String],b : Pair[String,String]) => Pair("style", a._2 + ";"+ b._2)}) :: p._2
  }
      
      def fromList(st : List[Pair[String,String]]) : MetaData = {
        st match {
          case Nil => Null
          case x :: xs => {new UnprefixedAttribute(x._1, x._2, fromList(xs))}
        }
      }
      fromList(crunchStyle(toList(st)))
    }
    
    def toHtml :NodeSeq = {
      flattenAndDropLastEOL(theElems)
    }
  }
  
  case class Line(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = super.toHtml ++ Text("\n")
  }

  case class Lst(elems : List[Textile]) extends ATextile(elems, Nil) {
    /*
    def performOnWikiAnchor(f : (WikiAnchor) => Any) : unit = {
      def findWikiAnchor(in : List[Textile], f : (WikiAnchor) => Any) : unit = {
        in match {
          case (s : WikiAnchor) :: rest => {f(s) ; findWikiAnchor(rest, f)}
          case (s : ATextile) :: rest => {findWikiAnchor(s.theElems, f); findWikiAnchor(rest, f)}
          case x :: rest => {findWikiAnchor(rest, f)}
          case _ => Nil
        }
      }
      findWikiAnchor(List(this), f)
    }*/
  }
  
  // drop the last EOL to prevent needless <br />
  // TODO: find a better solution.. it's not quite clear to me where newlines are meaningful
  private def flattenAndDropLastEOL(elems : List[Textile]) = ((elems match {case Nil => Nil case x => (x.last match { case EOL() => elems.init case _ => elems})})).flatMap{e => e.toHtml.toList}
 
  case class Paragraph(elems : List[Textile], attrs: List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      XmlElem(null, "p", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*) ++ Text("\n")
    }
    
  }

  case class Acronym(thing : String, acro : String) extends ATextile(Nil, Nil) {
    override def toHtml : NodeSeq = 
      XmlElem(null, "acronym", fromStyle(AnyAttribute("title", acro) :: Nil), TopScope, Text(thing) : _*)
  }
  
  case class Image(url : String, alt : String, link : String, attrs : List[Attribute] ) extends ATextile(Nil, attrs) {
    override def toHtml : NodeSeq = {
      val img = XmlElem(null, "img", fromStyle(AnyAttribute("src", url) :: AnyAttribute("alt", alt) :: attrs), TopScope, Nil : _*)
      
      if (link ne null) XmlElem(null, "a", fromStyle(AnyAttribute("href", link) :: attrs), TopScope, img : _*)
      else img
    }
  }

  case class BlankLine extends Textile {
    def toHtml = Text("")
  }

  case class CharBlock(s : String) extends Textile {
    def toHtml = Text(s)
  }

  case class Quoted(elems : List[Textile]) extends ATextile(elems, Nil) {
    override def toHtml : NodeSeq = {
      (Unparsed("&#8220;") ++ flattenAndDropLastEOL(elems)) ++ Unparsed("&#8221;")
    }
  }

  case class EmDash extends Textile  {
    def toHtml : NodeSeq = Unparsed(" &#8212; ")
  }

/*  case class BOL extends Textile  {
    def toHtml : NodeSeq = Text("")
  }*/
  
  case class EOL extends Textile  {
    // return the characters because Scala's XML library returns <br></br> in the
    // toString for the element and this causes 2 line breaks in some browsers
    // def toHtml :NodeSeq = XmlElem(null, "br", null, TopScope, Nil : _*) concat Text("\n")
    def toHtml :NodeSeq = Unparsed("<br />\n")
  }

  case class EnDash extends Textile  {
    def toHtml : NodeSeq = Unparsed(" &#8211; ")
  }

  case class SingleQuote extends Textile  {
    def toHtml : NodeSeq = Unparsed("&#8217;")
  }

  case class Elipsis extends Textile  {
    def toHtml : NodeSeq = Unparsed("&#8230;")
  }

  case class Dimension extends Textile   {
    def toHtml : NodeSeq = Unparsed("&#215;")
  }
  
  case class Trademark extends Textile  {
    def toHtml : NodeSeq = Unparsed("&#8482;")
  }
  case class Copyright extends Textile {
    def toHtml : NodeSeq = Unparsed("&#169;")
  }
  case class Register extends Textile {
    def toHtml : NodeSeq = Unparsed("&#174;")
  }

  case class Header(what : int, elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = XmlElem(null, "h"+what, fromStyle(attrs), TopScope, super.toHtml : _*)  ++ Text("\n")
  }

  case class BlockQuote(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      val par : NodeSeq = XmlElem(null, "p", null, TopScope, flattenAndDropLastEOL(elems) : _*) ++ Text("\n")
      XmlElem(null, "blockquote", fromStyle(attrs), TopScope, par : _*)  ++ Text("\n")
    }
  }
  
  val validAttributes = List(/*"class", */ "title", /*"style", "dir",*/ "lang")
   
  case class HTML(tag : String, elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    def validAttr(in: Attribute): Boolean = in match {
      case AnyAttribute(name, _) => validAttributes.contains(name)
      case _ => false
    }

    override def toHtml : NodeSeq = {
      XmlElem(null, tag, fromStyle(attrs.filter(validAttr)), TopScope, flattenAndDropLastEOL(elems) : _*)
    }}
  case class FootnoteDef(num : String) extends ATextile(null, Nil) {
    override def toHtml : NodeSeq = {
      XmlElem(null, "sup", Null, TopScope, 
           XmlElem(null, "a", fromStyle(List(AnyAttribute("href", "#fn"+num))), TopScope, Text(num) : _*) : _*)
    }
  }

  // ) yield Footnote(reduceCharBlocks(le :: ln), attrs, (d1 :: dr).mkString(""))

  case class Footnote(elems : List[Textile], attrs : List[Attribute], num : String) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      XmlElem(null, "p", fromStyle(AnyAttribute("id", "fn"+num) :: attrs), TopScope, 
           (XmlElem(null, "sup", null, TopScope, Text(num) : _*) :: flattenAndDropLastEOL(elems)) : _*) ++ Text("\n")
    }
  }

  case class Emph(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      XmlElem(null, "em", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
    }
  }
  case class Strong(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      XmlElem(null, "strong", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
    }
  }

  case class TableDef(attrs : List[Attribute]) extends Textile {
    def toHtml = null
  }

  case class Table(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      XmlElem(null, "table", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*) ++ Text("\n")
    }
  }

  case class TableRow(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      XmlElem(null, "tr", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*) ++ Text("\n")
    }
  }

  case class TableElement(elems : List[Textile], isHeader : boolean, attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      XmlElem(null, if (isHeader) "th" else "td", fromStyle(attrs), TopScope, (if (elems == Nil) Unparsed("&nbsp;") else flattenAndDropLastEOL(elems)) : _*) ++ Text("\n")
    }
  }

  case class Bullet(elems : List[Textile], numbered : boolean) extends ATextile(elems, Nil) {
    override def toHtml : NodeSeq = {
      XmlElem(null, if (numbered) "ol" else "ul", fromStyle(Nil), TopScope, flattenAndDropLastEOL(elems) : _*) ++ Text("\n")
    }
  }

  case class BulletLine(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      XmlElem(null, "li", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*) ++ Text("\n")
    }
  }

  case class Italic(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      XmlElem(null, "i", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
    }
  }
  case class Bold(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      XmlElem(null, "b", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
    }
  }
  case class Cite(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs){
    override def toHtml : NodeSeq = XmlElem(null, "cite", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
  }  
  case class Code(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = XmlElem(null, "code", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
  }  
  case class Delete(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = XmlElem(null, "del", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
  }  
  case class Ins(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = XmlElem(null, "ins", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
  }  
  case class Super(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = XmlElem(null, "sup", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
  }  
  case class Sub(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = XmlElem(null, "sub", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
  }  
  case class Pre(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = XmlElem(null, "pre", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*) ++ Text("\n")
  }  
  case class Span(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = XmlElem(null, "span", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
  }  


  case class Anchor(elems : List[Textile], href : String, alt : String, attrs : List[Attribute], disableLinks: Boolean) extends ATextile(elems, attrs) {
    def allAttrs = AnyAttribute("href", href) :: attrs 
    override def toHtml : NodeSeq = if (disableLinks) Text(alt) else
    XmlElem(null, "a", fromStyle(allAttrs), TopScope, Text(alt) : _*)
  }

  case class RefAnchor(elems : List[Textile], ref : String, alt : String, attrs : List[Attribute]) extends ATextile(elems, attrs) {
    private var _href = ""
    def href = _href
    def href_=(i : String) {_href = i}

    def allAttrs = {AnyAttribute("href", _href) :: attrs }
    override def toHtml : NodeSeq = XmlElem(null, "a", fromStyle(allAttrs), TopScope, Text(alt) : _*)
  }

  case class ARef( name : String, href : String) extends ATextile(Nil, Nil) {
    override def toHtml : NodeSeq = Text("")
  }

     case class WikiAnchor(word: String, category: Option[String], wikiFunc: Option[RewriteFunc]) extends ATextile(Nil, Nil) {
     // var rootUrl = ""
     def allAttrs: List[AnyAttribute] = wikiFunc match {
       case Some(func) =>
       func(WikiURLInfo(word, category)) match {
         case (href, _, Some(cls)) => AnyAttribute("href", href) :: AnyAttribute("class", cls) :: Nil
         case (href, _, _) => AnyAttribute("href", href) :: Nil
       }
       
       case _ => Nil
     }
     // def allAttrs = wikiFunc.map(wf => AnyAttribute("href", wf(href)) :: attrs) getOrElse attrs 
     override def toHtml: NodeSeq = 
     wikiFunc match {
       case Some(func) =>
       func(WikiURLInfo(word, category)) match {
         case (href, text, Some(cls)) => <a href={href} class={cls}>{text}</a>
         case (href, text, _) => <a href={href}>{text}</a>
       }
       
       case _ => Text(word)
     }
     
     // wikiFunc.map(ignore => Elem(null, "a", fromStyle(allAttrs), TopScope, Text(alt) : _*)) getOrElse Text(alt)
   }
   /*
  case class WikiAnchor(elems: List[Textile], href: String, alt: String, attrs: List[Attribute], wikiFunc: Option[RewriteFunc]) extends ATextile(elems, attrs) {
    // var rootUrl = ""
    def allAttrs = wikiFunc.map(wf => AnyAttribute("href", wf(href)) :: attrs) getOrElse attrs
    override def toHtml: NodeSeq = wikiFunc.map(ignore => XmlElem(null, "a", fromStyle(allAttrs), TopScope, Text(alt) : _*)) getOrElse Text(alt)
  }*/
  
  val example = 
"""I am <em>very</em> serious

Observe -- very nice!

Observe - tiny and brief.

"Observe!"

Hello Dude

**Bold * Not Strong**


my bold line **bold**

**strong* Not Bold


*strong*

This is a single paragraph

This is another paragraph

I am <b>very</b> serious.

This
is a paragraph

<pre>
I am <b>very</b> serious.

Oh, yes I am!!
</pre>

I spoke.
And none replied.




Observe...

Observe: 2 x 2.

one(TM), two(R), three(C).  

h1. Header 1
second line of header 1

h2. Header 2

h3. Header 3

An old text

bq. A block quotation.

Any old text

This is covered elsewhere[1].

fn1. Down here, in fact.

I _believe_ every word.

And then? She *fell*!

I __know__.
I **really** __know__.

??Cat's Cradle?? by Vonnegut  

Convert with @r.to_html@

I'm -sure- not sure.

You are a +pleasant+ child.

a ^2^ + b ^2^ = c ^2^

log ~2~ x

I'm %unaware% of most soft drinks.

I'm %{color:red}unaware%
of most soft drinks.

http://hobix.com/textile/#attributes  

I searched "Google":http://google.com.

CamelCase

\\CamelCase

ThreeHumpCamel

Four4FourHumpCamel


I am crazy about "Hobix":hobix
and "it's":hobix "all":hobix I ever
"link to":hobix!

[hobix]http://hobix.com

# A first item
# A second item
# A third

# Fuel could be:
## Coal
## Gasoline
## Electricity
# Humans need only:
## Water
## Protein

* A first item
* A second item
* A third

* Fuel could be:
** Coal
** Gasoline
** Electricity
* Humans need only:
** Water
** Protein

| name | age | sex |
| joan | 24 | f |
| archie | 29 | m |
| bella | 45 | f |

|_. name |_. age |_. sex |
| joan | 24 | f |
| archie | 29 | m |
| bella | 45 | f |

|_. attribute list |
|<. align left |
|>. align right|
|=. center |
|<>. justify this block |
|^. valign top |
|~. bottom |

|\2. spans two cols |
| col 1 | col 2 |

|/3. spans 3 rows | a |
| b |
| c |

|{background:#ddd}. Grey cell|

table{border:1px solid black}.
|This|is|a|row|
|This|is|a|row|

|This|is|a|row|
{background:#ddd}. |This|is|grey|row|

p<. align left

p>. align right

p=. centered

p<>. justified

p(. left ident 1em

p((. left ident 2em

p))). right ident 3em

h2()>. Bingo.

h3()>[no]{color:red}. Bingo

<pre>
<code>
a.gsub!( /</, '' )
</code>
</pre>


<div style='float:right;'>
float right
</div>

<div style='float:right;'>

h3. Sidebar

"Hobix":http://hobix.com/
"Ruby":http://ruby-lang.org/

</div>

The main text of the
page goes here and will 
stay to the left of the 
sidebar.

!http://hobix.com/sample.jpg!

!http://hobix.com/sa.jpg(Bunny.)!

!http://hobix.com/sample.jpg!:http://hobix.com/

!>http://hobix.com/sample.jpg!

And others sat all round the small
machine and paid it to sing to them.

We use CSS(Cascading Style Sheets).


"""
  
  
  def tryit = (for (res <- parse(example, Some(DefaultRewriter("/foo")))) yield {
    // res.performOnWikiAnchor{a => a.rootUrl = "/foo/"}
    res.toHtml
  }) getOrElse ""
    
  // println(tryit)

  case class DefaultRewriter(base: String) extends RewriteFunc {
  def apply(in: WikiURLInfo) = in match {
    case WikiURLInfo(word, Some(cat)) =>
    (base+"/"+urlEncode(cat)+"/"+urlEncode(word), Text(word), None)
    case WikiURLInfo(word, _) =>
    (base+"/"+urlEncode(word), Text(word), None)
  }
  
   def urlEncode(in : String) = java.net.URLEncoder.encode(in, "UTF-8")
}
}
/*
   /**
   * the thing that actually does the textile parsing
   */
   class TextileParsers(wikiUrlFunc: Option[RewriteFunc]) extends Parsers with ImplicitConversions {
     type Elem = char
     
     def document : Parser[Lst] = rep(paragraph) ^^ Lst
     // final val EofCh = '\032'
     private def chrExcept(cs: Char*): Parser[char] = elem("", {c => ('\032' :: cs.toList) forall (_ != c)}) //{x =>  !cs.contains(x)})
     private def mkString(cs: List[Any]) = cs.mkString("")
     
     implicit def str2chars(s: String): List[Char] = stringWrapper(s).toList
     
     def num = rep1(elem("digit", Character.isDigit)) ^^ mkString
     
     def enclosed(delim: Char, what: String, pred: Char => Boolean) = delim ~ str(what, {c => c != delim && pred(c)}) ~ delim
     def str(what: String, pred: Char => Boolean) = rep(elem(what, pred)) ^^ mkString
     def str1(what: String, pred: Char => Boolean) = rep1(elem(what, pred)) ^^ mkString
     def chrsExcept(cs: Char*): Parser[String] = rep1(chrExcept(cs : _*)) ^^ mkString
     
     
     val beginl: UnitParser = new UnitParser {
       def apply(in: Input) = if(in.pos.column==1) Success((), in) else Failure("at column "+in.pos.column+", not beginning of line", in)
     }
     
     val beginlS = beginl ~ rep(' ')
     
     /**
     * is it a blank line?  Start of line, some spaces, and an end of line
     */
     def blankLine: Parser[Textile] = beginlS ~ '\n' ^^ BlankLine
     
     
     /**
     * Line elements make up paragraphs and block elements
     */
     def lineElem : Parser[Textile] = {
       not(discard(blankLine)) ~ (endOfLine | image | footnote_def | 
       anchor | dimension | elipsis  | 
       copyright | trademark | registered | emDash | 
       enDash | italic | emph | bold  | 
       cite |  span | code | delete | insert | 
       sup | sub | strong | html | single_quote | quote | acronym | charBlock)
     }
     
     /**
     * If we've got an italic (__text__), the parser doesn't do well with a single underscore, so
     * we exclude looking for _emph_
     */
     def lineElem_notEmph : Parser[Textile] = {
       not(discard(blankLine)) ~ (endOfLine | image | footnote_def | anchor | 
       dimension | elipsis | 
       copyright | trademark | registered | emDash | enDash | italic | 
       bold  | 
       cite |  span| code | delete | insert| sup | sub | strong  | 
       html| single_quote | quote | acronym | charBlock)        
     }
     
     /**
     * Don't look for *strong* if we're currently in a **bold** element
     */
     def lineElem_notStrong : Parser[Textile] = {
       not(discard(blankLine)) ~ (endOfLine | image | footnote_def | anchor | 
       dimension | elipsis | 
       copyright | trademark | registered | emDash | enDash | italic | 
       emph | 
       cite |  span | code | delete | insert  | sup | 
       sub | bold  | html| single_quote | quote | acronym  | charBlock)
     }
     
     
     /**
     * Look for an acronym, but don't mistake registered, copyright, and trademarks
     */
     def acronym : Parser[Textile] =  chrsExcept(' ', '(', '\n') ~ not(discard(copyright | trademark | registered)) ~ '(' ~ chrsExcept(')', '\n') ~ ')' ^^ flatten2(Acronym)
     
     /**
     * is it an !image!
     */
     def image : Parser[Textile] = '!' ~  opt('<') ~ opt('>') ~ (rep1(not('!') ~ elem("", validUrlChar)) ^^ mkString) ~ 
     opt('(' ~ chrsExcept(')', '\n') ~ ')') ~ '!' ~
     opt(':' ~ url ^? {case a: Anchor => a}) ^^ { case fl ~ fr ~ img_url ~ alt ~ link => 
       Image(img_url, alt getOrElse "", link.map(_.href) getOrElse null, 
       if (fl) List(AnyAttribute("style", "float:left"))
       else if (fr) List(AnyAttribute("style", "float:right"))
     else Nil)} 
     
     
     /**
     * [footnote]
     */
     def footnote_def : Parser[Textile] = '[' ~ num ~ ']' ^^ FootnoteDef 
     
     /**
     * various things that make up an anchor (a tag)
     */
     def anchor = url | quote_url | quote_ref | a_ref | escCamelUrl | camelUrl
     
     
     def upper: Parser[Char] = elem("Uppercase character", Character.isUpperCase)
     def lower: Parser[Char] = elem("Lowercase character", Character.isLowerCase)
     def lowerOrNumber: Parser[Char] = elem("Lowercase character or digit", c => Character.isLowerCase(c) || Character.isDigit(c))
     
     /**
     * Don't use the CamelCase thing for a wikiword if it's prefixed by
     * a backslash
     */
     def escCamelUrl : Parser[CharBlock] = (('\\' ^^ '\\') ~ upper ~ lower ~ rep(lowerOrNumber) ~ 
     upper ~ lower ~ rep(lowerOrNumber) ~
     rep(upper ~ lower ~ rep(lowerOrNumber) ^^ {case c1 ~ c2 ~ s => mkString(List(c1, c2))+ mkString(s)})) ^^ 
     {case c1 ~ c2 ~ c3 ~ s4 ~ c5 ~ c6 ~ s7 ~ s8 => CharBlock(mkString(List(c1, c2, c3)) + mkString(s4) + mkString(List(c5, c6)) + mkString(s7) + s8.mkString(""))}
     
     // combine chars to string
     /*
     // the ~~ combinator together with these conversions would work nicely, were it not for the covariance of Parser's first type param :-(
     implicit def c2s(c: Char): String = c + ""
     implicit def cs2s(cs: List[Char]): String = cs.mkString("")
     implicit def ss2s(ss: List[String]): String = ss.mkString("")
     implicit def combine[C <% String, D <% String](c: C, d: D): String = c + d*/
     
     
     
     /**
     * is the work camelized?
     */
     def camelizedWord : Parser[CharBlock] = upper ~ lower ~ rep(lowerOrNumber) ^^ { case u ~ l ~ cs => CharBlock(mkString(u :: (l :: cs))) }
     
     def lowerWord: Parser[String] = lower ~ rep1(lowerOrNumber) ^^ {case x ~ xs => (x :: xs).mkString("")}
     
     /**
     * a WikiWord
     */
     def camelUrl: Parser[Textile] = noCatCamelUrl | catCamelUrl | tickUrl | catTickUrl
     
     def catTickUrl: Parser[WikiAnchor] = lowerWord ~ ':' ~ tickUrl ^^ 
     {case cat ~ rest => WikiAnchor(rest.word, Some(cat), wikiUrlFunc)}
     
     def tickUrl: Parser[WikiAnchor] = '`' ~ rep1(tickUrlChar) ~ '`' ^^ 
     {case c => WikiAnchor(c.mkString(""), None, wikiUrlFunc)}
     
     def isTickUrlChar(c: Char): Boolean = c.isLetter || c.isDigit ||
     c == ' ' || c == '@'
     
     def tickUrlChar: Parser[Char] = elem("tick url char", isTickUrlChar)
     
     def noCatCamelUrl: Parser[Textile] = camelizedWord ~ rep1(camelizedWord) ^^ { case c ~ cs => val ss = mkString((c :: cs).map(_.s)); WikiAnchor(ss, None, wikiUrlFunc)}
     
     def catCamelUrl: Parser[Textile] = lowerWord ~ ':' ~ camelizedWord ~ rep1(camelizedWord) ^^ { case cat ~ c ~ cs => val ss = mkString((c :: cs).map(_.s)); WikiAnchor(ss, Some(cat), wikiUrlFunc)}
     
     
     def urlStr = (rep1(elem("", validUrlChar))^^ mkString)
     
     /**
     * "reference":reference
     */
     def quote_ref : Parser[Textile] = '"' ~ chrsExcept('"', '\n') ~ '"' ~ ':' ~ urlStr ^^
     {case fs ~ rc => RefAnchor(Nil, rc, fs, Nil)}
     
     
     def httpStr = accept("http") ~ opt('s') ~ accept("://") ~ urlStr ^^ { case secure ~ rc => (if(secure) "https://" else "http://") + rc }
     
     /**
     * "google":http://google.com
     */
     def quote_url : Parser[Textile] = '"' ~ chrsExcept('"', '\n') ~ '"' ~ ':' ~ httpStr ^^
     { case fs ~ url => Anchor(Nil, url, fs, Nil) }
     
     /**
     * [reference]:http://reference.com
     */
     
     def a_ref : Parser[Textile] = '[' ~ urlStr ~ ']' ~ url ^^ {case  fr ~ (url: Anchor) => ARef(fr, url.href) }
     
     /**
     * http://google.com
     */
     def url : Parser[Textile] = httpStr ^^ { u => Anchor(Nil, u, u, Nil) }
     
     /**
     * a valid character in a URL
     */
     def validUrlChar(c : char) : boolean = {
       Character.isLetterOrDigit(c) || c == '/' || c == '%' || c == '&' || c == '?' || c == '#' || 
       c == '$' || c == '.' || c == '=' ||
       c == '-' || c == ':' || c == '_'
     }
     
     
     /**
     * an EOL character
     */
     def endOfLine : Parser[Textile] = '\n' ^^ EOL
     
     // replaced by checking the position of the current input (not that beginl does not consume any input, 
     // so repeatedly calling beginningOfLine is not a good idea -- that will either loop forever or fail immediately)
     //def beginningOfLine : Parser[Textile] = beginl ^^ BOL
     
     
     /**
     * if we're in a &lt;pre&gt; block, an end of line is just an end of line.  We 
     * pass the '\n' on though.
     */
     def preEndOfLine : Parser[Textile] = '\n' ^^ CharBlock("\n")
     
     
     /**
     * a &lt;pre&gt; block.  Just send text of through, unmolested.
     */
     def preBlock : Parser[Textile] = beginlS ~ accept("<pre") ~ rep(' ') ~ '>' ~
     rep(not(accept("</pre")) ~ (preEndOfLine | charBlock )) ~ accept("</pre") ~ rep(' ') ~ '>' ~ rep(' ') ~ '\n' ^^ { 
     case elms => Pre(reduceCharBlocks(elms), Nil) }
     
     /**
     * (c)
     */
     def copyright : Parser[Textile] = '(' ~ (accept('c') | 'C') ~ ')' ^^ Copyright  // accept necesary because of clash with | on char's
     
     
     def validTagChar(c : char) = Character.isDigit(c) || Character.isLetter(c) || c == '_'
     def validStyleChar(c : char) = Character.isDigit(c) || Character.isLetter(c) || c == '.' || c == ' ' || c == ';' || c == '#'
     def validClassChar(c : char) = Character.isDigit(c) || Character.isLetter(c) || c == '.'
     
     def validTag = rep1(elem("valid tag character", validTagChar)) ^^ mkString ^? {case s if isValidTag(s) => s}
     
     /**
     * An HTML block is made up of single HTML tag (e.g., &lt;br /&gt;) and 
     * tags that contain other stuff
     */
     def html = single_html | multi_html
     
     def closingTag(tag: String) = accept("</") ~ accept(tag) ~ rep(' ') ~ '>'
     
     /**
     * an HTML tag that contains other stuff
     */
     def multi_html : Parser[Textile] = '<' ~ validTag >> { tag => success(tag) ~ 
       rep(tag_attr) ~ '>' ~ 
       rep(not(closingTag(tag)) ~ (lineElem | paragraph)) ~ 
      closingTag(tag) } ^^ {case tag ~ attrs ~ body => HTML(tag, reduceCharBlocks(body), attrs)}
      
      /**
      * A stand-alone tag
      */
      def single_html : Parser[Textile] = '<' ~ validTag ~ rep(tag_attr) ~ accept("/>") ^^ { case tag ~ attrs => HTML(tag, Nil, attrs) }
      
      def tag_attr = single_quote_attr | double_quote_attr
      
      def attr_name(c : char) = Character.isLetterOrDigit(c) || c == '_' || c == '-'
      
      def attr_value(c : char) = {c >= ' '}
      
      
      /**
      * an attribute with single quotes
      */
      def single_quote_attr : Parser[Attribute] =  rep(' ') ~ str1("attribute name", attr_name) ~ '=' ~ 
      enclosed('\'', "attribute value", attr_value _) ^^ flatten2(AnyAttribute)
      
      /**
      * an attribute with double quotes
      */
      def double_quote_attr : Parser[Attribute] = rep(' ') ~ str1("attribute name", attr_name) ~ '=' ~ 
      enclosed('"', "attribute value", attr_value(_)) ^^ flatten2(AnyAttribute)
      
      /**
      * is it a valid HTML tag?  This list should be expanded
      */
      def isValidTag(in : String) = in == "b" || in == "em" || in == "code" || in == "div" || in == "span"
      
      
      /**
      * A "dimension" pretty printer
      */
      def dimension : Parser[Textile] = accept(" x ") ^^ Dimension
      
      def registered : Parser[Textile] = '(' ~ (accept('r') | 'R') ~ ')' ^^ Register
      
      def trademark : Parser[Textile] = '(' ~ (accept('t') | 'T') ~ (accept('m') | 'M') ~ ')' ^^ Trademark
      
      def elipsis : Parser[Textile] = accept("...") ^^ Elipsis
      
      def emDash : Parser[Textile] = accept(" -- ") ^^ EmDash
      
      def enDash : Parser[Textile] = accept(" - ") ^^ EnDash
      
      def single_quote : Parser[Textile] = '\'' ^^ SingleQuote
      
      
      def bullet(depth : int, numbered : boolean) : Parser[Textile] = {
        val oneBullet: UnitParser = if (numbered) '#' else '*'
        
        def bullet_line(depth : int, numbered : boolean) : Parser[Textile] =  beginlS ~ repN(depth+1, oneBullet) ~ rep(not('\n') ~ lineElem) ~ '\n' ^^ {
        case elms => BulletLine(reduceCharBlocks(elms), Nil) }
        
        bullet_line(depth, numbered) ~
        (rep1(bullet(depth + 1, numbered) | bullet_line(depth, numbered))) ^^ {case fbl ~ abl => Bullet(fbl :: abl, numbered)}
      }
      
      def formattedLineElem[Q <% UnitParser](m: Q): Parser[List[Textile] ~ List[Attribute]] = formattedLineElem(m, rep(attribute))
      def formattedLineElem[Q <% UnitParser](m: Q, p: Parser[List[Attribute]]): Parser[List[Textile] ~ List[Attribute]] =  m ~ p ~  rep1(not(m) ~ lineElem) ~ m ^^ { 
      case attrs ~ ln => mkTilde(reduceCharBlocks(ln), attrs) }
      
      def spaceOrStart: UnitParser = beginl | ' '
      def spaceOrEnd: UnitParser = accept(' ') | discard(endOfLine)
      
      // TODO: generalize formattedLineElem some more
      def bold : Parser[Textile] = accept("**") ~ rep(attribute) ~ rep1(not(accept("**")) ~ lineElem_notStrong) ~ accept("**") ^^ { 
      case attrs ~ ln => Bold(reduceCharBlocks(ln), attrs) }
      
      def strong : Parser[Textile] = formattedLineElem('*') ^^ flatten2(Strong)
      
      def cite : Parser[Textile] = formattedLineElem(accept("??")) ^^ flatten2(Cite)
      
      def code : Parser[Textile] = formattedLineElem('@') ^^ flatten2(Code)
      
      def styleElem : Parser[StyleElem] = str1("style", validStyleChar) ~ ':' ~ str1("style", validStyleChar) ^^ flatten2(StyleElem)
      
      def attribute = style | class_id | the_class | the_id | the_lang 
      
      def para_attribute = attribute | fill_align | left_align | right_align |
      center_align | top_align | bottom_align | em_left | em_right;
      
      def left_align : Parser[Attribute] = elem('<') ^^ Align
      
      def right_align : Parser[Attribute] = elem('>') ^^ Align
      
      def center_align : Parser[Attribute] = elem('=') ^^ Align
      
      def top_align : Parser[Attribute] = elem('^') ^^ Align
      
      def bottom_align : Parser[Attribute] = elem('~') ^^ Align
      
      def fill_align : Parser[Attribute] = accept("<>") ^^ Align('f')
      
      def em_left : Parser[Attribute] = rep1(elem('(')) ^^ {xs => Em(xs.length)}
      
      def em_right : Parser[Attribute] = rep1(elem(')')) ^^ {xs => Em(-xs.length)}
      
      def class_id : Parser[Attribute] = '(' ~ str1("class", validClassChar) ~ '#' ~ str1("class", validClassChar) ~ ')' ^^ flatten2(ClassAndId)
      
      def the_id : Parser[Attribute] = '(' ~ '#' ~ str1("class", validClassChar) ~ ')' ^^ {ri => ClassAndId(null, ri)}
      
      def the_lang : Parser[Attribute] = '[' ~ str1("class", validClassChar) ~ ']' ^^ Lang
      
      def the_class : Parser[Attribute] = '(' ~ str1("class", validClassChar) ~ ')' ^^ {rc => ClassAndId(rc, null)}
      
      
      def style : Parser[Attribute] = '{' ~ repsep(styleElem, ';') ~ '}' ^^ Style
      
      def span : Parser[Textile] = formattedLineElem('%', opt(style) ^^ {s => s.toList}) ^^ flatten2(Span)
      
      def delete : Parser[Textile] = formattedLineElem('-') ^^ flatten2(Delete)
      
      
      def insert : Parser[Textile] = formattedLineElem('+') ^^ flatten2(Ins) 
      
      def sup : Parser[Textile] = formattedLineElem('^') ^^ flatten2(Super) 
      
      def sub : Parser[Textile] =  formattedLineElem('~') ^^ flatten2(Sub) 
      
      def italic : Parser[Textile] = formattedLineElem(accept("__")) ^^ flatten2(Italic) 
      
      def emph : Parser[Textile] = formattedLineElem('_') ^^ flatten2(Emph)
      
      def quote : Parser[Textile] = formattedLineElem('"', success(Nil)) ^^ flatten2((x, y) => Quoted(x))
      
      def reduceCharBlocks(in : List[Textile]) : List[Textile] = 
      (in: @unchecked) match {
        case Nil => Nil
        // this is now done (hacked) in flattenAndDropLastEOL
        //        case EOL() :: BOL() :: rest => EOL :: reduceCharBlocks(rest)
        //        case EOL() :: rest => reduceCharBlocks(rest)
        case CharBlock(s1) :: CharBlock(s2) :: rest => reduceCharBlocks(CharBlock(s1 + s2) :: rest)
        case x :: xs => x :: reduceCharBlocks(xs)
      }
      
      def charBlock : Parser[Textile] = chrExcept('\n') ^^ {c => CharBlock(c.toString)}
      
      
      
      def blankPara : Parser[Textile] = discard(rep1(blankLine)) ^^ BlankLine
      
      def not_blank_line : Parser[Textile] = not(discard(blankLine)) ~ lineElem
      
      def head_line : Parser[Textile] = beginl ~ 'h' ~ elem("1, 2 or 3", {c => c == '1' | c == '2' | c == '3'}) ~ rep(para_attribute) ~ accept(". ") ~ rep1(not_blank_line) ~ blankLine ^^ {
      case n ~ attrs ~ ln ~ _ => Header(n - '0', reduceCharBlocks(ln), attrs) }
      
      def blockquote : Parser[Textile] = beginl ~ accept("bq") ~ rep(para_attribute) ~ accept(". ") ~ rep1(not_blank_line) ~ blankLine ^^ {
      case attrs ~ ln ~ _ => BlockQuote(reduceCharBlocks(ln), attrs)}
      
      
      def footnote : Parser[Textile] = beginl ~ accept("fn") ~ num ~ rep(para_attribute) ~ accept(". ") ~ rep1(not_blank_line) ~ blankLine ^^ {
      case dr ~ attrs ~ ln ~ _ => Footnote(reduceCharBlocks(ln), attrs, dr)}
      
      def first_paraAttrs : Parser[TableDef] = beginlS ~ 'p' ~ rep(para_attribute) ~ '.' ^^ TableDef
      
      def normPara : Parser[Textile] = opt(first_paraAttrs) ~ rep1(not_blank_line) ~ blankLine ^^ {
      case td ~ fp ~ _ => Paragraph(reduceCharBlocks(fp), td.map(_.attrs) getOrElse(Nil))}
      
      def table_attribute = para_attribute | row_span | col_span
      
      
      
      def row_span : Parser[Attribute] = '/' ~ num ^^ {s => AnyAttribute("rowspan", s )}
      
      def col_span : Parser[Attribute] = '\\' ~ num ^^ {s => AnyAttribute("colspan", s )}
      
      def table : Parser[Textile] = opt(table_def) ~ opt(table_header) ~ rep1(table_row) ^^ {
      case td ~ th ~ tr => Table(th.toList ::: tr, td.map(_.attrs) getOrElse Nil)  }
      
      def table_def : Parser[TableDef] = beginlS ~ accept("table") ~ rep(para_attribute) ~ '.' ~ rep(' ') ~ '\n' ^^ TableDef
      
      def row_def : Parser[TableDef] = rep1(table_attribute) ~ '.' ^^ TableDef
      
      def table_row : Parser[Textile] = beginlS ~ opt(row_def) ~ rep(' ') ~ '|' ~ rep1(table_element(false)) ~ rep(' ') ~ '\n' ^^ {
      case td ~ re => TableRow(re, td.map(_.attrs) getOrElse Nil)}
      
      def table_header : Parser[Textile] = beginlS ~ opt(row_def) ~ rep(' ') ~ accept("|_.") ~ rep1sep(table_element(true), accept("_.")) ~ rep(' ') ~ '\n' ^^ {
      case td ~ re => TableRow(re, td.map(_.attrs) getOrElse Nil)}
      
      def table_element(isHeader : boolean) : Parser[Textile] = opt(row_def) ~ rep(not(accept('|') | '\n') ~ lineElem) ~ '|' ^^ {
      case td ~ el => TableElement(reduceCharBlocks(el), isHeader, td.map(_.attrs) getOrElse Nil)}
      
      def paragraph : Parser[Textile] = 
      preBlock | footnote | table | bullet(0, false) | bullet(0, true) | blockquote | head_line | blankPara | normPara
   }
   
   
   
   abstract class Textile {
     def toHtml : NodeSeq
   }
   
   case class Style(elms : List[StyleElem]) extends Attribute {
     def name(which : int) : String = "style"
     def value(which : int) : String = elms.mkString("",";","")
   }
   
   abstract class Attribute extends Textile {
     def toHtml = null
     def name(which : int) : String
     def value(which : int) : String
     def fieldCnt = 1
     def toList : List[Pair[String, String]] = {
       var ret : List[Pair[String,String]] = Nil
       var cnt = fieldCnt
       while (cnt > 0) {
         cnt = cnt -1
         val tn = name(cnt)
         val tv = value(cnt)
         if ((tn ne null) && (tv ne null)) ret = Pair(tn,tv) :: ret
       }
       ret
     }
   }
   
   case class AnyAttribute(name : String, value : String) extends Attribute {
     def name(which : int) : String = name
     def value(which : int) : String = value
   }
   
   case class ClassAndId(className : String, idName : String) extends Attribute {
     override def fieldCnt = 2
     def name(which : int) : String = {
       which match {
         case 0 => if ((className ne null) && className.length > 0) "class" else null
         case 1 => if ((idName ne null) && idName.length > 0) "id" else null
       }
     }
     def value(which : int) : String = {
       which match {
         case 0 => if ((className ne null) && className.length > 0) className else null
         case 1 => if ((idName ne null) && idName.length > 0) idName else null
       }
     }
     
   }
   
   case class Lang(lang : String) extends Attribute {
     def name(which : int) : String = "lang"
     def value(which : int) : String = lang
   }
   
   case class Align(align : char) extends Attribute {
     def name(which : int) : String = "style"
     def value(which : int) : String = {
       align match {
         case '<' => "text-align:left";
         case '>' => "text-align:right";
         case '=' => "text-align:center";
         case '^' => "vertical-align:top";
         case '~' => "vertical-align:bottom";
         case 'f' | 'j' => "text-align:justify";
         
       }
     }
   }
   
   case class Em(cnt : int) extends Attribute {
     def name(which : int) : String = "style"
     def value(which : int) : String = {
       if (cnt > 0) "padding-left:"+cnt+"em" else "padding-right:"+(-cnt)+"em"
     }
   }
   
   
   
   case class StyleElem(name : String, value : String) extends Textile {
     def toHtml = null
     override def toString = name+":"+value
   }
   
   abstract class ATextile(val theElems : List[Textile], attrs : List[Attribute]) extends Textile {
     def fromStyle(st : List[Attribute]) : MetaData = {
       def toList(st : List[Attribute]) : List[Pair[String, String]] = {
         st match {
           case Nil => Nil
           case x :: xs => x.toList ::: toList(xs)
         }
       }
       
       def crunchStyle(st : List[Pair[String, String]]) : List[Pair[String,String]] = {
         val p = st.partition {a => a._1 == "style"}
         if (p._1 == Nil) p._2 else
         (p._1.reduceLeft{(a : Pair[String,String],b : Pair[String,String]) => Pair("style", a._2 + ";"+ b._2)}) :: p._2
       }
       
       def fromList(st : List[Pair[String,String]]) : MetaData = {
         st match {
           case Nil => Null
           case x :: xs => {new UnprefixedAttribute(x._1, Text(x._2), fromList(xs))}
         }
       }
       fromList(crunchStyle(toList(st)))
     }
     
     def toHtml :NodeSeq = {
       flattenAndDropLastEOL(theElems)
     }
   }
   
   case class Line(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = super.toHtml ++ Text("\n")
   }
   
   case class Lst(elems : List[Textile]) extends ATextile(elems, Nil) {
     /*
     def performOnWikiAnchor(f : (WikiAnchor) => Any) : unit = {
     def findWikiAnchor(in : List[Textile], f : (WikiAnchor) => Any) : unit = {
     in match {
     case (s : WikiAnchor) :: rest => {f(s) ; findWikiAnchor(rest, f)}
     case (s : ATextile) :: rest => {findWikiAnchor(s.theElems, f); findWikiAnchor(rest, f)}
     case x :: rest => {findWikiAnchor(rest, f)}
     case _ => Nil
     }
     }
     findWikiAnchor(List(this), f)
     }*/
   }
   
   // drop the last EOL to prevent needless <br />
   // TODO: find a better solution.. it's not quite clear to me where newlines are meaningful
   private def flattenAndDropLastEOL(elems : List[Textile]) = ((elems match {case Nil => Nil case x => (x.last match { case EOL => elems.init case _ => elems})})).flatMap{e => e.toHtml.toList}
   
   case class Paragraph(elems : List[Textile], attrs: List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = {
       Elem(null, "p", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*) ++ Text("\n")
     }
     
   }
   
   case class Acronym(thing : String, acro : String) extends ATextile(Nil, Nil) {
     override def toHtml : NodeSeq = 
     Elem(null, "acronym", fromStyle(AnyAttribute("title", acro) :: Nil), TopScope, Text(thing) : _*)
   }
   
   case class Image(url : String, alt : String, link : String, attrs : List[Attribute] ) extends ATextile(Nil, attrs) {
     override def toHtml : NodeSeq = {
       val img = Elem(null, "img", fromStyle(AnyAttribute("src", url) :: AnyAttribute("alt", alt) :: attrs), TopScope, Nil : _*)
       
       if (link ne null) Elem(null, "a", fromStyle(AnyAttribute("href", link) :: attrs), TopScope, img : _*)
       else img
     }
   }
   
   case object BlankLine extends Textile {
     def toHtml = Text("")
   }
   
   case class CharBlock(s : String) extends Textile {
     def toHtml = Text(s)
   }
   
   case class Quoted(elems : List[Textile]) extends ATextile(elems, Nil) {
     override def toHtml : NodeSeq = {
       (Unparsed("&#8220;") ++ flattenAndDropLastEOL(elems)) ++ Unparsed("&#8221;")
     }
   }
   
   case object EmDash extends Textile  {
     def toHtml : NodeSeq = Unparsed(" &#8212; ")
   }
   
   /*  case class BOL extends Textile  {
   def toHtml : NodeSeq = Text("")
   }*/
   
   case object EOL extends Textile  {
     // return the characters because Scala's XML library returns <br></br> in the
     // toString for the element and this causes 2 line breaks in some browsers
     // def toHtml :NodeSeq = Elem(null, "br", null, TopScope, Nil : _*) concat Text("\n")
     def toHtml :NodeSeq = Unparsed("<br />\n")
   }
   
   case object EnDash extends Textile  {
     def toHtml : NodeSeq = Unparsed(" &#8211; ")
   }
   
   case object SingleQuote extends Textile  {
     def toHtml : NodeSeq = Unparsed("&#8217;")
   }
   
   case object Elipsis extends Textile  {
     def toHtml : NodeSeq = Unparsed("&#8230;")
   }
   
   case object Dimension extends Textile   {
     def toHtml : NodeSeq = Unparsed("&#215;")
   }
   
   case object Trademark extends Textile  {
     def toHtml : NodeSeq = Unparsed("&#8482;")
   }
   case object Copyright extends Textile {
     def toHtml : NodeSeq = Unparsed("&#169;")
   }
   case object Register extends Textile {
     def toHtml : NodeSeq = Unparsed("&#174;")
   }
   
   case class Header(what : int, elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = Elem(null, "h"+what, fromStyle(attrs), TopScope, super.toHtml : _*)  ++ Text("\n")
   }
   
   case class BlockQuote(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = {
       val par : NodeSeq = Elem(null, "p", null, TopScope, flattenAndDropLastEOL(elems) : _*) ++ Text("\n")
       Elem(null, "blockquote", fromStyle(attrs), TopScope, par : _*)  ++ Text("\n")
     }
   }
   
   val validAttributes = List(/*"class", */ "title", /*"style", "dir",*/ "lang")
   
   case class HTML(tag : String, elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     def validAttr(in: Attribute): Boolean = in match {
       case AnyAttribute(name, _) => validAttributes.contains(name)
       case _ => false
     }
     
     override def toHtml : NodeSeq = {
       Elem(null, tag, fromStyle(attrs.filter(validAttr)), TopScope, flattenAndDropLastEOL(elems) : _*)
   }}
   case class FootnoteDef(num : String) extends ATextile(null, Nil) {
     override def toHtml : NodeSeq = {
       Elem(null, "sup", Null, TopScope, 
       Elem(null, "a", fromStyle(List(AnyAttribute("href", "#fn"+num))), TopScope, Text(num) : _*) : _*)
     }
   }
   
   // ) yield Footnote(reduceCharBlocks(le :: ln), attrs, (d1 :: dr).mkString(""))
   
   case class Footnote(elems : List[Textile], attrs : List[Attribute], num : String) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = {
       Elem(null, "p", fromStyle(AnyAttribute("id", "fn"+num) :: attrs), TopScope, 
       (Elem(null, "sup", null, TopScope, Text(num) : _*) :: flattenAndDropLastEOL(elems)) : _*) ++ Text("\n")
     }
   }
   
   case class Emph(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = {
       Elem(null, "em", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
     }
   }
   case class Strong(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = {
       Elem(null, "strong", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
     }
   }
   
   case class TableDef(attrs : List[Attribute]) extends Textile {
     def toHtml = null
   }
   
   case class Table(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = {
       Elem(null, "table", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*) ++ Text("\n")
     }
   }
   
   case class TableRow(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = {
       Elem(null, "tr", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*) ++ Text("\n")
     }
   }
   
   case class TableElement(elems : List[Textile], isHeader : boolean, attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = {
       Elem(null, if (isHeader) "th" else "td", fromStyle(attrs), TopScope, (if (elems == Nil) Unparsed("&nbsp;") else flattenAndDropLastEOL(elems)) : _*) ++ Text("\n")
     }
   }
   
   case class Bullet(elems : List[Textile], numbered : boolean) extends ATextile(elems, Nil) {
     override def toHtml : NodeSeq = {
       Elem(null, if (numbered) "ol" else "ul", fromStyle(Nil), TopScope, flattenAndDropLastEOL(elems) : _*) ++ Text("\n")
     }
   }
   
   case class BulletLine(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = {
       Elem(null, "li", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*) ++ Text("\n")
     }
   }
   
   case class Italic(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = {
       Elem(null, "i", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
     }
   }
   case class Bold(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = {
       Elem(null, "b", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
     }
   }
   case class Cite(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs){
     override def toHtml : NodeSeq = Elem(null, "cite", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
   }  
   case class Code(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = Elem(null, "code", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
   }  
   case class Delete(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = Elem(null, "del", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
   }  
   case class Ins(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = Elem(null, "ins", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
   }  
   case class Super(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = Elem(null, "sup", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
   }  
   case class Sub(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = Elem(null, "sub", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
   }  
   case class Pre(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = Elem(null, "pre", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*) ++ Text("\n")
   }  
   case class Span(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
     override def toHtml : NodeSeq = Elem(null, "span", fromStyle(attrs), TopScope, flattenAndDropLastEOL(elems) : _*)
   }  
   
   
   case class Anchor(elems : List[Textile], href : String, alt : String, attrs : List[Attribute]) extends ATextile(elems, attrs) {
     def allAttrs = AnyAttribute("href", href) :: attrs 
     override def toHtml : NodeSeq = Elem(null, "a", fromStyle(allAttrs), TopScope, Text(alt) : _*)
   }
   
   case class RefAnchor(elems : List[Textile], ref : String, alt : String, attrs : List[Attribute]) extends ATextile(elems, attrs) {
     private var _href = ""
     def href = _href
     def href_=(i : String) {_href = i}
     
     def allAttrs = {AnyAttribute("href", _href) :: attrs }
     override def toHtml : NodeSeq = Elem(null, "a", fromStyle(allAttrs), TopScope, Text(alt) : _*)
   }
   
   case class ARef( name : String, href : String) extends ATextile(Nil, Nil) {
     override def toHtml : NodeSeq = Text("")
   }
   
   case class WikiAnchor(word: String, category: Option[String], wikiFunc: Option[RewriteFunc]) extends ATextile(Nil, Nil) {
     // var rootUrl = ""
     def allAttrs: List[AnyAttribute] = wikiFunc match {
       case Some(func) =>
       func(WikiURLInfo(word, category)) match {
         case (href, _, Some(cls)) => AnyAttribute("href", href) :: AnyAttribute("class", cls) :: Nil
         case (href, _, _) => AnyAttribute("href", href) :: Nil
       }
       
       case _ => Nil
     }
     // def allAttrs = wikiFunc.map(wf => AnyAttribute("href", wf(href)) :: attrs) getOrElse attrs 
     override def toHtml: NodeSeq = 
     wikiFunc match {
       case Some(func) =>
       func(WikiURLInfo(word, category)) match {
         case (href, text, Some(cls)) => <a href={href} class={cls}>{text}</a>
         case (href, text, _) => <a href={href}>{text}</a>
       }
       
       case _ => Text(word)
     }
     
     // wikiFunc.map(ignore => Elem(null, "a", fromStyle(allAttrs), TopScope, Text(alt) : _*)) getOrElse Text(alt)
   }
  
  val example = 
"""I am <em>very</em> serious

Observe -- very nice!

Observe - tiny and brief.

"Observe!"

Hello Dude

**Bold * Not Strong**


my bold line **bold**

**strong* Not Bold


*strong*

This is a single paragraph

This is another paragraph

I am <b>very</b> serious.

This
is a paragraph

<pre>
I am <b>very</b> serious.

Oh, yes I am!!
</pre>

I spoke.
And none replied.




Observe...

Observe: 2 x 2.

one(TM), two(R), three(C).  

h1. Header 1
second line of header 1

h2. Header 2

h3. Header 3

An old text

bq. A block quotation.

Any old text

This is covered elsewhere[1].

fn1. Down here, in fact.

I _believe_ every word.

And then? She *fell*!

I __know__.
I **really** __know__.

??Cat's Cradle?? by Vonnegut  

Convert with @r.to_html@

I'm -sure- not sure.

You are a +pleasant+ child.

a ^2^ + b ^2^ = c ^2^

log ~2~ x

I'm %unaware% of most soft drinks.

I'm %{color:red}unaware%
of most soft drinks.

http://hobix.com/textile/#attributes  

I searched "Google":http://google.com.

CamelCase

\\CamelCase

ThreeHumpCamel

Four4FourHumpCamel


I am crazy about "Hobix":hobix
and "it's":hobix "all":hobix I ever
"link to":hobix!

[hobix]http://hobix.com

# A first item
# A second item
# A third

# Fuel could be:
## Coal
## Gasoline
## Electricity
# Humans need only:
## Water
## Protein

* A first item
* A second item
* A third

* Fuel could be:
** Coal
** Gasoline
** Electricity
* Humans need only:
** Water
** Protein

| name | age | sex |
| joan | 24 | f |
| archie | 29 | m |
| bella | 45 | f |

|_. name |_. age |_. sex |
| joan | 24 | f |
| archie | 29 | m |
| bella | 45 | f |

|_. attribute list |
|<. align left |
|>. align right|
|=. center |
|<>. justify this block |
|^. valign top |
|~. bottom |

|\2. spans two cols |
| col 1 | col 2 |

|/3. spans 3 rows | a |
| b |
| c |

|{background:#ddd}. Grey cell|

table{border:1px solid black}.
|This|is|a|row|
|This|is|a|row|

|This|is|a|row|
{background:#ddd}. |This|is|grey|row|

p<. align left

p>. align right

p=. centered

p<>. justified

p(. left ident 1em

p((. left ident 2em

p))). right ident 3em

h2()>. Bingo.

h3()>[no]{color:red}. Bingo

<pre>
<code>
a.gsub!( /</, '' )
</code>
</pre>


<div style='float:right;'>
float right
</div>

<div style='float:right;'>

h3. Sidebar

"Hobix":http://hobix.com/
"Ruby":http://ruby-lang.org/

</div>

The main text of the
page goes here and will 
stay to the left of the 
sidebar.

!http://hobix.com/sample.jpg!

!http://hobix.com/sa.jpg(Bunny.)!

!http://hobix.com/sample.jpg!:http://hobix.com/

!>http://hobix.com/sample.jpg!

And others sat all round the small
machine and paid it to sing to them.

We use CSS(Cascading Style Sheets).

Hello *I am
a multi-line* bold thing... or am I(tm)?

"""

def main(arg: Array[String]) {
  try {
    def tryit = (for (res <- parse(example, Some(DefaultRewriter("/foo")))) yield {
      // res.performOnWikiAnchor{a => a.rootUrl = "/foo/"}
      res.toHtml
    }) getOrElse ""
    
    
    println("res "+tryit)
  } catch {
    case e => e.printStackTrace
  }
}

case class DefaultRewriter(base: String) extends RewriteFunc {
  def apply(in: WikiURLInfo) = in match {
    case WikiURLInfo(word, Some(cat)) =>
    (base+"/"+urlEncode(cat)+"/"+urlEncode(word), Text(word), None)
    case WikiURLInfo(word, _) =>
    (base+"/"+urlEncode(word), Text(word), None)
  }
  
   def urlEncode(in : String) = java.net.URLEncoder.encode(in, "UTF-8")
}


}*/
