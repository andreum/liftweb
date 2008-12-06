package net.liftweb.http.auth

import net.liftweb.util.{Can, Full, Empty}

object AuthRole {
  def apply(roleName: String) = new Role {
    def name = roleName
  }
}

/**
 * A Role may be assingned to a resource denominated by a path. A subject
 * that is assigned to the same role or to a role higher into the roles hierarchy
 * will have access to requested resource.
 */
trait Role {

  private var parent: Can[Role] = Empty
  private var childs: List[Role] = Nil

  /**
   * The name ofthe role
   */
  def name: String

  /**
   * Add child Role(s) to this role. Node name is ensured to be unique (by name)
   * in the tree.
   */
  def addRoles(roles: Role*) = {
    for (val role <- roles) {
      getRoleByName(role.name) match {
        case Empty =>
          childs = role :: childs
          role.parent = Full(this)
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
   * Removes the child Role
   */
  def removeRoleByName(roleName : String) : Can[Role] = {
    getRoleByName(roleName).map(_.detach) openOr Empty
  }

  /**
   * Removes this Role from its parent
   */
  def detach : Can[Role] = {
    this.parent.map {
      case p =>
        p.childs = p.childs.filter(role => role.name != this.name)
        this.parent = Empty
        this
    }
  }

  /**
   * Verifies if this Roe is a child of provided role
   */
  def isChildOf(role: Role) : Boolean = !role.getRoleByName(name).isEmpty

  override def toString = {
    var str = "Role(" + name;
    for (val role <- childs) {
      str = str + ", " + role.toString
    }
    str + ")"
  }
}
