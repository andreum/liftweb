package net.liftweb.http.auth

import net.liftweb.util.{Can, Full, Empty}

/**
 * A Role may be assingned to a resource denominated by a path. A subject
 * that is assigned to the same role or to a role higher into the roles hierarchy
 * will have access to requested resource.
 */
case class Role(name: String) {

  private var childs: List[Role] = Nil

  override def toString = name

  /**
   * Add child Role(s) to this role. Node name is ensured to be unique (by name)
   * in the tree.
   */
  def addRoles(roles: Role*) = {
    for (val role <- roles) {
      getRoleByName(role.name) match {
        case Empty => childs = role :: childs
        case _ =>
      }
    }
    this
  }

  def getChildRoles = childs

  /**
   * Search for a child Role with this name
   */
  def getRoleByName(roleName : String) : Can[Role] = (this.name == roleName) match {
    case false => childs.find(role => role.getRoleByName(roleName) match {
        case Empty => false
        case theRole @ _ => return theRole
      })
      Empty
    case _ => Full(this)
    }

  /**
   * Verifies if this Roe is a child of provided role
   */
  def isChildOf(role: Role) : Boolean = !role.getRoleByName(name).isEmpty

}
