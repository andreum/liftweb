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

 import scala.xml.{NodeSeq, Text, Elem, Group, MetaData, Null, UnprefixedAttribute, PrefixedAttribute}

/**
 * The helpers assocated with bindings
 */
trait BindHelpers {
  
  sealed abstract class BindParam {
    def name: String
    // def value: NodeSeq
    def calcValue(in: NodeSeq): NodeSeq
  }
  
  implicit def toTheBindParam(in: Product2[String, NodeSeq]): TheBindParam =
  TheBindParam(in._1, in._2)
  
  case class TheBindParam(name: String, value: NodeSeq) extends BindParam {
    def calcValue(in: NodeSeq): NodeSeq = value
  }
  
  case class AttrBindParam(name: String, value: NodeSeq, newAttr: String) extends BindParam {
    def calcValue(in: NodeSeq): NodeSeq = value
  }  
  
  case class FuncBindParam(name: String, value: NodeSeq => NodeSeq) extends BindParam {
    def calcValue(in: NodeSeq): NodeSeq = value(in)
  }
  case class FuncAttrBindParam(name: String, value: NodeSeq => NodeSeq, newAttr: String) extends BindParam {
    def calcValue(in: NodeSeq): NodeSeq = value(in)
  }
  
  
  object BindParamAssoc {
    implicit def canStrCanNodeSeq(in: Can[Any]): Can[NodeSeq] = in.map(_ match {
      case null => Text("null")
      case v => Text(v.toString)
    })
  }
  
  class BindParamAssoc(val name: String) {
    def -->(value: String): BindParam = TheBindParam(name, Text(value))
    def -->(value: NodeSeq): BindParam = TheBindParam(name, value)
    def -->(value: Symbol): BindParam = TheBindParam(name, Text(value.name))
    def -->(value: Any): BindParam = TheBindParam(name, Text(if (value == null) "null" else value.toString))
    def -->(func: NodeSeq => NodeSeq): BindParam = FuncBindParam(name, func)
    def -->(value: Can[NodeSeq]): BindParam = TheBindParam(name, value.openOr(Text("Empty")))    
  }
  
  implicit def strToBPAssoc(in: String): BindParamAssoc = new BindParamAssoc(in)
  implicit def symToBPAssoc(in: Symbol): BindParamAssoc = new BindParamAssoc(in.name)
  
  def renum[T](in: java.util.Enumeration[T]): List[T] = if (!in.hasMoreElements()) Nil else in.nextElement.asInstanceOf[T] :: renum(in)
  
  /**
  * Experimental extension to bind which passes in an additional "parameter" from the XHTML to the transform 
  * function, which can be used to format the returned NodeSeq.
  */
  def xbind(namespace: String, xml: NodeSeq)(transform: PartialFunction[String, NodeSeq => NodeSeq]): NodeSeq = {
    def rec_xbind(xml: NodeSeq): NodeSeq = {
      xml.flatMap {
        node => node match {
          case s: Elem if (node.prefix == namespace) =>
          if (transform.isDefinedAt(node.label))
          transform(node.label)(node)
          else
          Text("FIX"+"ME failed to bind <"+namespace+":"+node.label+" />")
          case Group(nodes) => Group(rec_xbind(nodes))
          case s: Elem => Elem(node.prefix, node.label, node.attributes, node.scope, rec_xbind(node.child) : _*)
          case n => node
        }
      }
    }
    
    rec_xbind(xml)
  }
  
  /**
  * Bind a set of values to parameters and attributes in a block of XML 
  */
  def bind(namespace: String, xml: NodeSeq, params: BindParam*): NodeSeq = {
    val map: scala.collection.immutable.Map[String, BindParam] = scala.collection.immutable.HashMap.empty ++ params.map(p => (p.name, p))
    
    def attrBind(attr: MetaData): MetaData = attr match {
      case Null => Null
      case upa: UnprefixedAttribute => new UnprefixedAttribute(upa.key, upa.value, attrBind(upa.next))
      case pa: PrefixedAttribute if pa.pre == namespace => map.get(pa.key) match {
        case None => new PrefixedAttribute(pa.pre, pa.key, Text("FIX"+"ME find to bind attribute"), attrBind(pa.next))
        case Some(abp @ AttrBindParam(_, _, newAttr)) => new UnprefixedAttribute(newAttr, abp.calcValue(pa.value), attrBind(pa.next))
        case Some(abp @ FuncAttrBindParam(_, _, newAttr)) => new UnprefixedAttribute(newAttr, abp.calcValue(pa.value), attrBind(pa.next))
        case Some(bp: TheBindParam) => new PrefixedAttribute(pa.pre, pa.key, bp.calcValue(pa.value), attrBind(pa.next))
        case Some(bp: FuncBindParam) => new PrefixedAttribute(pa.pre, pa.key, bp.calcValue(pa.value), attrBind(pa.next))
      }
      case pa: PrefixedAttribute => new PrefixedAttribute(pa.pre, pa.key, pa.value, attrBind(pa.next))
    }
    
    def in_bind(xml:NodeSeq): NodeSeq = {
      xml.flatMap {
        node =>
        node match {
          case s : Elem if (node.prefix == namespace) => {
            map.get(node.label) match {
              case None => Text("FIX"+"ME failed to bind <"+namespace+":"+node.label+" />")
              case Some(ns) => ns.calcValue(s.child)
            }
          }
          case Group(nodes) => Group(in_bind(nodes))
          case s : Elem => Elem(node.prefix, node.label, attrBind(node.attributes), node.scope, in_bind(node.child) : _*)
          case n => node
        }
      }
    }
    in_bind(xml)
  }
  
  def bind(vals: Map[String, NodeSeq], xml: NodeSeq): NodeSeq = {
    xml.flatMap {
      node =>
      node match {
        case s : Elem if (node.prefix == "lift" && node.label == "bind") => {
          node.attributes.get("name") match {
            case None => bind(vals, node.child)
            case Some(ns) => {
              vals.get(ns.text) match {
                case None => bind(vals, node.child)
                case Some(nodes) => nodes
              }
            }
          }
        }
        case Group(nodes) => Group(bind(vals, nodes))
        case s : Elem => Elem(node.prefix, node.label, node.attributes,node.scope, bind(vals, node.child) : _*)
        case n => node
      }
    }
  }
  
  def bindlist(listvals: List[Map[String, NodeSeq]], xml: NodeSeq): Can[NodeSeq] = {
    def build (listvals: List[Map[String, NodeSeq]], ret: NodeSeq): NodeSeq = listvals match {
      case Nil => ret
      case vals :: rest => build(rest, ret ++ bind(vals, xml))
    }
    if (listvals.length > 0) Full(build(listvals.drop(1), bind(listvals.head, xml)))
    else Empty
  }
  
  /**
  * Bind parameters to XML.
  * @param around XML with lift:bind elements
  * @param atWhat data to bind
  */
  def processBind(around: NodeSeq, atWhat: Map[String, NodeSeq]) : NodeSeq = {
    
    /** Find element matched predicate f(x).isDefined, and return f(x) if found or None otherwise. */
    def findMap[A, B](s: Iterable[A])(f: A => Option[B]): Option[B] =
    s.projection.map(f).find(_.isDefined).getOrElse(None)
    
    around.flatMap {
      v =>
      v match {
        case Group(nodes) => Group(processBind(nodes, atWhat))
        case Elem("lift", "bind", attr @ _, _, kids @ _*) =>
        findMap(atWhat) {
          case (at, what) if attr("name").text == at => Some({what})
          case _ => None
        }.getOrElse(processBind(v.asInstanceOf[Elem].child, atWhat))
        
        case e: Elem => {Elem(e.prefix, e.label, e.attributes, e.scope, processBind(e.child, atWhat): _*)}
        case _ => {v}
      }
      
    }
  }
}


// vim: set ts=2 sw=2 et:
