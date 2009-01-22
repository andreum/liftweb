/*
 * Copyright 2008 WorldWide Conferencing, LLC
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
package net.liftweb.jpademo.snippet

import scala.xml.{NodeSeq,Text}

import net.liftweb.http.{RequestVar,S,SHtml}
import net.liftweb.util.{Box,Empty,Full,Helpers}
import S._
import Helpers._

import net.liftweb.jpademo.model._
import Model._

// Make an object so that other pages can access (ie Authors)
object BookOps {
  // Object to hold search results
  object resultVar extends RequestVar[List[Book]](Nil)
}

class BookOps {
  val formatter = new java.text.SimpleDateFormat("yyyyMMdd")

  def list (xhtml : NodeSeq) : NodeSeq = {
    val books = Model.createNamedQuery[Book]("findAllBooks").getResultList()

    books.flatMap(book =>
      bind("book", xhtml,
	   "title" -> Text(book.title),
	   "published" -> Text(formatter.format(book.published)),
	   "genre" -> Text(if (book.genre != null) book.genre.toString else ""),
	   "author" -> Text(book.author.name),
	   "edit" -> SHtml.link("add.html", () => bookVar(book), Text(?("Edit")))))
  }

  // Set up a requestVar to track the book object for edits and adds
  object bookVar extends RequestVar(new Book())
  def book = bookVar.is

  def add (xhtml : NodeSeq) : NodeSeq = {
    def doAdd () = {
      Model.merge(book)
      redirectTo("list.html")
    }

    // Hold a val here so that the "id" closure holds it when we re-enter this method
    val currentId = book.id

    val authors = Model.createNamedQuery[Author]("findAllAuthors").getResultList()
    val choices = authors.map(author => (author.id.toString -> author.name)).toList
    val default = if (book.author != null) { Full(book.author.id.toString) } else { Empty }

    bind("book", xhtml,
	 "id" -> SHtml.hidden(() => book.id = currentId),
	 "title" -> SHtml.text(book.title, book.title = _),
	 "published" -> SHtml.text(formatter.format(book.published), {id : String => book.published = formatter.parse(id)}) % ("id" -> "published"),
	 "genre" -> SHtml.select(Genre.getNameDescriptionList, (Box.legacyNullTest(book.genre).map(_.toString) or Full("")), choice => book.genre = Genre.valueOf(choice).getOrElse(null)),
	 "author" -> SHtml.select(choices, default, {authId : String => book.author = Model.getReference(classOf[Author], authId.toLong)}),
	 "save" -> SHtml.submit(?("Save"), doAdd))
  }

  def searchResults (xhtml : NodeSeq) : NodeSeq = BookOps.resultVar.is.flatMap(result =>
    bind("result", xhtml, "title" -> Text(result.title), "author" -> Text(result.author.name)))

  def search (xhtml : NodeSeq) : NodeSeq = {
    var title = ""

    def doSearch () = {
      BookOps.resultVar(Model.createNamedQuery[Book]("findBooksByTitle", "title" -> ("%" + title.toLowerCase + "%")).getResultList().toList)
    }

    bind("search", xhtml,
	 "title" -> SHtml.text(title, title = _),
	 "run" -> SHtml.submit(?("Search"), doSearch _))
  }
}
