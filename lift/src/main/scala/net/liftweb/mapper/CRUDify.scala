package net.liftweb.mapper

import net.liftweb._
import util._
import Helpers._
import http._

import scala.xml.{Node, NodeSeq}
import scala.xml.transform._

trait CRUDify[KeyType, T <: KeyedMapper[KeyType, T]] extends ScreenWrapper { 
  self: KeyedMetaMapper[KeyType, T] =>
  
  lazy val BasePath: String = urlEncode(self.dbTableName)
  
  val ListItems = "index"
  
  val UpdateItem = "update"
  
  val CreateItem = "create"
  
  val ReadItem = "read"
  
  val DeleteItem = "delete"
  
  def thePath(end: String) = "/" + BasePath + "/" + end
  
  def templates: LiftRules.TemplatePf = {
    case RequestMatcher(RequestState(BasePath :: ListItems :: Nil, _), _) =>
    () => listItems()
    
    case RequestMatcher(RequestState(BasePath :: UpdateItem :: itemId :: Nil, _), _) =>
    () => updateItem(itemId)

    case RequestMatcher(RequestState(BasePath :: CreateItem :: Nil, _), _) =>
    () => createItem()

    case RequestMatcher(RequestState(BasePath :: ReadItem :: itemId :: Nil, _), _) =>
    () => readItem(itemId)

    case RequestMatcher(RequestState(BasePath :: DeleteItem :: itemId :: Nil, _), _) =>
    () => deleteItem(itemId)
  }
  
  
  def createItem() = <b>Create</b>
  
  def readItem(itemId: String) = <b>Read {itemId}</b>
  
  def deleteItem(itemId: String) = <b>Delete {itemId}</b>
  
  def updateItem(id: String) = <b>Foo</b>
  
  def listItems() = <b>Foo</b>
  
  /*
  def index = {
    S("model_list") = model.findAll
    
    () => index_render
  }
  
  def index_render : Seq[Node] = {
    <div><table>
    <tr>{model.htmlHeaders}</tr>
    {S[Array[T]]("model_list").get.map {t => <tr>{t.htmlLine}<td><a href={
      path+"/edit?"+t.a{ av => S("model") = t}+"=na"
        }>Edit</a></td><td><a href={
          path+"/delete?"+t.a{ av =>  S("model") = t}+"=na"
    }>Delete</a></td></tr>}}
    </table>
    <div><a href={path+"/add"}>Add</a></div>
    </div>
  }
  
  def add = {
    
    if (S.post_?) {
      S[Mapper[T]]("model") match {
        case model @ Some(_) if (model.sws_validate.isEmpty) => {model.get.save; S.redirect(path)}
        case _ => {() => add_render}
      }
     } else {S("model") = model.createInstance; () => add_render}
  }
  
  def add_render : Seq[Node] = {
    S[T]("model") match {
      case smodel @ Some(_) => {
        val model = smodel.get
        <form action={path+"/add"} method="POST">
        <table>
        {model.generateInputTable}
        <tr><td><a href={path}>Cancel</a></td><td><input value="Add" type="submit"/></td></tr>
        {model.i {s => S("model") = model}}
        </table>
        </form>
      }
      case _ => {throw new RedirectException("Model Not Found",path)}
    }
  }
  
  def edit = {
    if (S.post_?) {
      S[Mapper[T]]("model") match {
        case model @ Some(_) if (model.sws_validate.isEmpty) => {model.get.save; S.redirect(path)}
        case _ => {() => edit_render}
      }
     } else {() => edit_render}
  }
  
  def edit_render : Seq[Node] = {
    Console.println("Model is "+S[T]("model"))
    
    S[T]("model") match {
      case smodel @ Some(_) => {
        val model = smodel.get
        <form action={path+"/edit"} method="POST">
        <table>
        {model.generateInputTable}
        <tr><td><a href={path}>Cancel</a></td><td><input value="Edit" type="submit"/></td></tr>
        {model.i {s => S("model") = model}}
        </table>
        </form>
      }
      case _ => {throw new RedirectException("Model Not Found",path)}
    }
  }
  
  def delete = {
    if (S.post_?) {
      S[T]("model") match {
        case model @ Some(_) => {model.get.delete_!; S.redirect(path)}
        case _ => {() => delete_render}
      }
     } else {() => delete_render}
  }
  
  def delete_render : Seq[Node] = {
    S[T]("model") match {
      case smodel @ Some(_) => {
        val model = smodel.get
        <form action={path+"/delete"} method="POST">
        <table>
        <tr><td colspan='2'>Really Delete {model.asHtml}?</td></tr>
        <tr><td><a href={path}>Cancel</a></td><td><input value="Delete" type="submit"/></td></tr>
        {model.i {s => S("model") = model}}
        </table>
        </form>
      }
      case _ => {throw new RedirectException("Model Not Found",path)}
      
    }
  }
  */
}

