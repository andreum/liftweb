package net.liftweb.mapper

/*                                                *\
(c) 2006-2008 WorldWide Conferencing, LLC
Distributed under an Apache License
http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import net.liftweb.mapper._
import net.liftweb.http._
import js._
import JsCmds._
import scala.xml.{NodeSeq, Node, Group}
import scala.xml.transform._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.util.Mailer._
import S._

trait ProtoUser[T <: ProtoUser[T]] extends KeyedMapper[Long, T] { 
  self: T =>
  // the primary key for the database
  object id extends MappedLongIndex(this)
  
  // First Name
  object firstName extends MappedString(this, 32)
  
  // Last Name
  object lastName extends MappedString(this, 32)
  
  // Email
  object email extends MappedEmail(this, 48) {
    override def dbIndexed_? = true
    override def validations = valUnique(S.??("unique.email.address")) _ :: super.validations
  }
  
  // Password
  object password extends MappedPassword[T](this)
  
  
  object superUser extends MappedBoolean(this) {
    override def defaultValue = false
  }
  
  def niceName: String = (firstName.is, lastName.is, email.is) match {
    case (f, l, e) if f.length > 1 && l.length > 1 => f+" "+l+" ("+e+")"
    case (f, _, e) if f.length > 1 => f+" ("+e+")"
    case (_, l, e) if l.length > 1 => l+" ("+e+")"
    case (_, _, e) => e
  }
  
  def shortName: String = (firstName.is, lastName.is) match {
    case (f, l) if f.length > 1 && l.length > 1 => f+" "+l
    case (f, _) if f.length > 1 => f
    case (_, l) if l.length > 1 => l
    case _ => email.is
  }  
  
  def niceNameWEmailLink = <a href={"mailto:"+email.is}>{niceName}</a>  
}

trait MetaMegaProtoUser[ModelType <: MegaProtoUser[ModelType], MyType <: ModelType] extends KeyedMetaMapper[Long, ModelType] with ScreenWrapper { 
  self: MyType =>
  
  def signupFields: List[BaseOwnedMappedField[ModelType]] = firstName :: lastName :: email :: locale :: timezone :: password :: Nil
  
  override def fieldOrder: List[BaseOwnedMappedField[ModelType]] = firstName :: lastName :: email :: locale :: timezone :: password :: Nil  
  
  
  val BasePath = "user_mgt"
  val SignUp = "sign_up"
  val Login = "login"
  val LostPassword = "lost_password"
  val PasswordReset = "reset_password"
  val ChangePassword = "change_password"
  val Logout = "logout"
  val Edit = "edit"
  val ValidateUser = "validate_user"
  
  val HomePage = "/"
  
  case class MenuItem(name: String, endOfPath: String, loggedIn: Boolean) {
    def path = "/"+BasePath+"/"+endOfPath
    def display = name match {
      case null | "" => false
      case _ => true
    }
  }
  
  def thePath(end: String) = "/" + BasePath + "/" + end
  
  /**
  * Return the URL of the "login" page
  */
  def loginPageURL = "/"+BasePath+"/"+Login
  
  def notLoggedIn_? = !loggedIn_? ;
  
  lazy val testLogginIn = If(loggedIn_? _, S.??("must.be.logged.in")) ;
  
  /**
  * The menu item for login (make this "Empty" to disable)
  */
  def loginMenuLoc: Can[Menu] = {
    Full(Menu(Loc("Login", thePath(Login), S.??("login"),
    If(notLoggedIn_? _, S.??("already.logged.in")))))
  }
  
  
  /**
  * The menu item for logout (make this "Empty" to disable)
  */
  def logoutMenuLoc: Can[Menu] = 
  Full(Menu(Loc("Logout", thePath(Logout), S.??("logout"), testLogginIn)))
  
  /**
  * The menu item for creating the user/sign up (make this "Empty" to disable)
  */
  def createUserMenuLoc: Can[Menu] = 
  Full(Menu(Loc("CreateUser", thePath(SignUp),
  S.??("sign.up"), If(notLoggedIn_? _, S.??("logout.first")))))
  
  /**
  * The menu item for lost password (make this "Empty" to disable)
  */
  def lostPasswordMenuLoc: Can[Menu] = 
  Full(Menu(Loc("LostPassword", thePath(LostPassword), 
  S.??("lost.password"), 
  If(notLoggedIn_? _, S.??("logout.first"))))) // not logged in
  
  /**
  * The menu item for resetting the password (make this "Empty" to disable)
  */
  def resetPasswordMenuLoc: Can[Menu] = 
  Full(Menu(Loc("ResetPassword", (thePath(PasswordReset), true),
  S.??("reset.password"), Hidden, 
  If(notLoggedIn_? _,
  S.??("logout.first"))))) //not Logged in
  
  /**
  * The menu item for editing the user (make this "Empty" to disable)
  */
  def editUserMenuLoc: Can[Menu] = 
  Full(Menu(Loc("EditUser", thePath(Edit), S.??("edit.user"), testLogginIn)))
  
  /**
  * The menu item for changing password (make this "Empty" to disable)
  */
  def changePasswordMenuLoc: Can[Menu] = 
  Full(Menu(Loc("ChangePassword", thePath(ChangePassword), 
  S.??("change.password"), testLogginIn)))
  
  /**
  * The menu item for validating a user (make this "Empty" to disable)
  */
  def validateUserMenuLoc: Can[Menu] = 
  Full(Menu(Loc("ValidateUser", (thePath(ValidateUser), true),
  S.??("validate.user"), Hidden,
  If(notLoggedIn_? _, S.??("logout.first")))))     
  
  lazy val sitemap : List[Menu] = 
  List(loginMenuLoc, logoutMenuLoc, createUserMenuLoc, 
  lostPasswordMenuLoc, resetPasswordMenuLoc,
  editUserMenuLoc, changePasswordMenuLoc, 
  validateUserMenuLoc).flatten(a => a)
  
  
  def skipEmailValidation = false
  
  def userMenu: List[Node] = {
    val li = loggedIn_?
    ItemList.
    filter(i => i.display && i.loggedIn == li).
    map(i => (<a href={i.path}>{i.name}</a>))
  }
  
  val ItemList: List[MenuItem] = MenuItem(S.??("sign.up"), SignUp, false) ::
  MenuItem(S.??("log.in"), Login, false) :: 
  MenuItem(S.??("lost.password"), LostPassword, false) ::
  MenuItem("", PasswordReset, false) ::
  MenuItem(S.??("change.password"), ChangePassword, true) :: 
  MenuItem(S.??("log.out"), Logout, true) ::
  MenuItem(S.??("edit.profile"), Edit, true) ::
  MenuItem("", ValidateUser, false) :: Nil
  
  def templates: LiftRules.TemplatePf = {
    case RequestMatcher(RequestState(BasePath :: (w @ SignUp) :: _, _), _)
    if testLoggedIn(w) => () => signup
    
    case RequestMatcher(RequestState(BasePath :: (w @ Login) :: _, _), _)
    if testLoggedIn(w) => () => login
    
    case RequestMatcher(RequestState(BasePath :: (w @ LostPassword) :: _, _), _)
    if testLoggedIn(w) => () => lostPassword
    
    case RequestMatcher(RequestState(BasePath :: (w @ PasswordReset) :: id :: _, _), _) 
    if testLoggedIn(w) => () => passwordReset(id)
    
    case RequestMatcher(RequestState(BasePath :: (w @ ChangePassword) :: _, _), _) 
    if testLoggedIn(w) => () => changePassword
    
    case RequestMatcher(RequestState(BasePath :: (w @ Logout) :: _, _), _)
    if testLoggedIn(w) => () => logout
    
    case RequestMatcher(RequestState(BasePath :: (w @ Edit) :: _, _), _)
    if testLoggedIn(w) => () => edit
    
    case RequestMatcher(RequestState(BasePath :: (w @ ValidateUser) :: id :: _, _), _) 
    if testLoggedIn(w) => () => validateUser(id)
  }
  
  def requestLoans: List[LoanWrapper] = Nil // List(curUser)
  
  // val LoggedInUserIdentifier = "$_proto_user_current_user"
  
  private object _currentUserId extends SessionVar[Can[String]](Empty)
  
  def loggedIn_? : Boolean = currentUserId.isDefined
  
  def logUserIn(who: ModelType) {
    _currentUserId(Full(who.id.toString))
    // S.set(LoggedInUserIdentifier, who.id.toString)
    curUser.remove()
  }
  
  def logoutCurrentUser = logUserOut()
  
  def logUserOut() {
    _currentUserId.remove()
    // S.unset(LoggedInUserIdentifier)
    curUser.remove()
    S.request.foreach(_.request.getSession.invalidate)
  }
  
  def currentUserId: Can[String] = _currentUserId.is // S.get(LoggedInUserIdentifier)
  
  /*
  private val curUser: ThreadLazy[Can[ModelType]] = 
  ThreadLazy(currentUserId.flatMap(id => getSingleton.find(id)))
  */
  
  private object curUser extends RequestVar[Can[ModelType]](currentUserId.flatMap(id => getSingleton.find(id)))
  
  
  def currentUser: Can[ModelType] = curUser.is
  
  def signupXhtml(user: ModelType) = {
    (<form method="POST" action={S.uri}><table><tr><td
    colspan="2">Sign Up</td></tr>
    {localForm(user, false)}
    <tr><td>&nbsp;</td><td><user:submit/></td></tr>
    </table></form>)
  }
  
  
  def signupMailBody(user: ModelType, validationLink: String) = {
    (<html>
    <head>
    <title>Sign Up Confirmation</title>
    </head>
    <body>
    <p>Dear {user.firstName},
    <br/>
    <br/>
    Click on this link to complete signup
    <br/><a href={validationLink}>{validationLink}</a>
    <br/>
    <br/>
    Thanks
    </p>
    </body>
    </html>)
  }
  
  def signupMailSubject = S.??("sign.up.confirmation")
  
  def sendValidationEmail(user: ModelType) {
    val resetLink = S.hostAndPath+"/"+BasePath+
    "/"+ValidateUser+"/"+user.uniqueId
    
    val email: String = user.email
    
    val msgXml = signupMailBody(user, resetLink)
    
    Mailer.sendMail(From(emailFrom),Subject(signupMailSubject),
    (To(user.email) :: xmlToMailBodyType(msgXml) ::
    (bccEmail.toList.map(BCC(_)))) :_* )  
  }
  
  def signup = {
    val theUser: ModelType = create
    val theName = BasePath + SignUp
    
    def testSignup(ignore: String) {
      theUser.validate match {
        case Nil => S.removeSessionTemplater(theName)
        theUser.validated(skipEmailValidation).uniqueId.reset()
        theUser.save
        if (!skipEmailValidation) {
          sendValidationEmail(theUser)              
          S.notice(S.??("sign.up.message"))
        } else {
          S.notice(S.??("welcome"))
          logUserIn(theUser)
        }
        
        S.redirectTo(HomePage)
        
        case xs => S.error(xs)
      }
    }
    
    def innerSignup = bind("user", 
    signupXhtml(theUser), 
    "submit" --> SHtml.submit(S.??("sign.up"), testSignup))
    
    S.addSessionTemplater(theName, {
      case RequestMatcher(RequestState(BasePath :: (w @ SignUp) :: _,
      _), _)
      if testLoggedIn(w) =>  () => innerSignup
    })
    innerSignup
  }
  
  def emailFrom = "noreply@"+S.hostName
  
  def bccEmail: Can[String] = Empty
  
  def testLoggedIn(page: String): Boolean =
  ItemList.filter(_.endOfPath == page) match {
    case x :: xs if x.loggedIn == loggedIn_? => true
    case _ => false
  }
  
  
  def validateUser(id: String): NodeSeq = getSingleton.find(By(uniqueId, id)) match {
    case Full(user) if !user.validated => 
    user.validated(true).uniqueId.reset().save
    S.notice(S.??("account.validated"))
    logUserIn(user)
    S.redirectTo(HomePage)  
    
    case _ => S.error(S.??("invalid.validation.link")); S.redirectTo(HomePage)
  }
  
  def loginXhtml = {
    (<form method="POST" action={S.uri}><table><tr><td
    colspan="2">{S.??("log.in")}</td></tr>
    <tr><td>{S.??("email.address")}</td><td><user:email /></td></tr>
    <tr><td>{S.??("password")}</td><td><user:password /></td></tr>
    <tr><td><a href={"/"+BasePath+"/"+LostPassword}>{S.??("recover.password")}</a></td><td><user:submit /></td></tr></table>
    </form>)
  }
  
  def login = {
    if (S.post_?) {
      S.param("username").
      flatMap(username => getSingleton.find(By(email, username))) match {
        case Full(user) if user.validated && 
        user.password.match_?(S.param("password").openOr("*")) =>
        logUserIn(user); S.notice(S.??("logged.in")); S.redirectTo(HomePage)
        
        case Full(user) if !user.validated => 
        S.error(S.??("account.validation.error"))        
        
        case _ => S.error(S.??("invalid.credentials"))
      }
    }
    
    bind("user", loginXhtml,
    "email" --> (FocusOnLoad(<input type="text" name="username"/>)),
    "password" --> (<input type="password" name="password"/>),
    "submit" --> (<input type="submit" value={S.??("log.in")}/>))
  }
  
  def lostPasswordXhtml = {
    (<form method="POST" action={S.uri}>
    <table><tr><td 
    colspan="2">{S.??("enter.email")}</td></tr>
    <tr><td>{S.??("email.address")}</td><td><user:email /></td></tr>
    <tr><td>&nbsp;</td><td><user:submit /></td></tr>
    </table>
    </form>)
  }
  
  def passwordResetMailBody(user: ModelType, resetLink: String) = {
    (<html>
    <head>
    <title>{S.??("reset.password.confirmation")}</title>
    </head>
    <body>
    <p>{S.??("dear")} {user.firstName},
    <br/>
    <br/>
    {S.??("click.reset.link")}
    <br/><a href={resetLink}>{resetLink}</a>
    <br/>
    <br/>
    {S.??("thank.you")}
    </p>
    </body>
    </html>)
  }
  
  def passwordResetEmailSubject = S.??("reset.password.request") 
  
  def sendPasswordReset(email: String) {
    getSingleton.find(By(this.email, email)) match {
      case Full(user) if user.validated =>
      user.uniqueId.reset().save
      val resetLink = S.hostAndPath+"/"+
      BasePath+"/"+PasswordReset+"/"+user.uniqueId
      
      val email: String = user.email
      
      val msgXml = passwordResetMailBody(user, resetLink)
      Mailer.sendMail(From(emailFrom),Subject(passwordResetEmailSubject),  
      (To(user.email) :: xmlToMailBodyType(msgXml) ::
      (bccEmail.toList.map(BCC(_)))) :_*)
      
      S.notice(S.??("password.reset.email.sent")) 
      S.redirectTo(HomePage)
      
      case Full(user) => 
      sendValidationEmail(user)
      S.notice(S.??("account.validation.resent"))
      S.redirectTo(HomePage)
      
      case _ => S.error(S.??("email.address.not.found"))
    }
  }      
  
  def lostPassword = {
    bind("user", lostPasswordXhtml, 
    "email" --> SHtml.text("", sendPasswordReset _), 
    "submit" --> <input type="Submit" value={S.??("send.it")} />)
  }
  
  def passwordResetXhtml = {
    (<form method="POST" action={S.uri}>
    <table><tr><td colspan="2">{S.??("reset.your.password")}</td></tr>
    <tr><td>{S.??("enter.your.new.password")}</td><td><user:pwd/></td></tr>
    <tr><td>{S.??("repeat.your.new.password")}</td><td><user:pwd/></td></tr>
    <tr><td>&nbsp;</td><td><user:submit/></td></tr>
    </table>
    </form>)
  }
  
  def passwordReset(id: String) = 
  getSingleton.find(By(uniqueId, id)) match {
    case Full(user) => 
    def finishSet(ignore: String) {
      user.validate match {
        case Nil => S.notice(S.??("password.changed"))
        user.save
        logUserIn(user); S.redirectTo(HomePage)
        
        case xs => S.error(xs)
      }
    }
    user.uniqueId.reset().save
    
    bind("user", passwordResetXhtml,
    "pwd" --> SHtml.password_*("",(p: List[String]) => 
    user.password.setList(p)),
    "submit" --> SHtml.submit(S.??("set.password"), finishSet))
    case _ => S.error(S.??("pasword.link.invalid")); S.redirectTo(HomePage)
  }
  
  def changePasswordXhtml = {
    (<form method="POST" action={S.uri}>
    <table><tr><td colspan="2">{S.??("change.password")}</td></tr>
    <tr><td>{S.??("old.password")}</td><td><user:old_pwd /></td></tr>
    <tr><td>{S.??("new.password")}</td><td><user:new_pwd /></td></tr>
    <tr><td>{S.??("repeat.password")}</td><td><user:new_pwd /></td></tr>
    <tr><td>&nbsp;</td><td><user:submit /></td></tr>
    </table>
    </form>)
  }
  
  def changePassword = {
    val user = currentUser.open_! // we can do this because the logged in test has happened
    var oldPassword = ""
    var newPassword: List[String] = Nil
    
    def testAndSet(ignore: String) {
      if (!user.password.match_?(oldPassword)) S.error(S.??("wrong.old.password"))
      else {
        user.password.setFromAny(newPassword)
        user.validate match {
          case Nil => user.save; S.notice(S.??("pasword.changed")); S.redirectTo(HomePage)
          case xs => S.error(xs)
        }
      }
    }
    
    bind("user", changePasswordXhtml,
    "old_pwd" --> SHtml.password("", oldPassword = _), 
    "new_pwd" --> SHtml.password_*("", LFuncHolder(newPassword = _)), 
    "submit" --> SHtml.submit(S.??("change"), testAndSet _))
  }
  
  def editXhtml(user: ModelType) = {
    (<form method="POST" action={S.uri}>
    <table><tr><td colspan="2">{S.??("edit")}</td></tr>
    {localForm(user, true)}
    <tr><td>&nbsp;</td><td><user:submit/></td></tr>
    </table>
    </form>)
  }
  
  def edit = {
    val theUser: ModelType = currentUser.open_! // we know we're logged in
    val theName = BasePath + Edit
    
    def testEdit(ignore: String) {
      theUser.validate match {
        case Nil => S.removeSessionTemplater(theName)
        theUser.save
        S.notice(S.??("profle.updated"))
        S.redirectTo(HomePage)
        
        case xs => S.error(xs)
      }
    }
    
    def innerEdit = bind("user", editXhtml(theUser), 
    "submit" --> SHtml.submit(S.??("edit"), testEdit))
    
    S.addSessionTemplater(theName, {
      case RequestMatcher(RequestState(BasePath :: (w @ Edit) :: _, _), _)
      if testLoggedIn(w) => 
    () => innerEdit})
    innerEdit
  }
  
  def logout = {
    logoutCurrentUser
    S.redirectTo(HomePage)
  }
  
  private def localForm(user: ModelType, ignorePassword: Boolean): NodeSeq = {
    signupFields.
    map(fi => getSingleton.getActualBaseField(user, fi)).
    filter(f => !ignorePassword || (f match {
      case f: MappedPassword[ModelType] => false
      case _ => true
    })).
    flatMap(f => 
    f.toForm.toList.map(form => 
    (<tr><td>{f.displayName}</td><td>{form}</td></tr>) ) )
  }
  
  
}

trait ScreenWrapper {
  implicit def nodeSeqToOption(in: NodeSeq): Can[NodeSeq] = 
  screenWrap.map{
    theDoc => 
    val rw = new RewriteRule {
      override def transform(n: Node) = n match {
        case e @ (<bind />) if "lift" == e.prefix => in
        case _ => n
      }
    }
    (new RuleTransformer(rw))(theDoc)
  } or Full(in)  
  
  /**
  * If the 
  */
  def screenWrap: Can[Node] //  = Empty
}

trait MegaProtoUser[T <: MegaProtoUser[T]] extends ProtoUser[T] { 
  self: T =>
  object uniqueId extends MappedUniqueId(this, 32) {
    override def dbIndexed_? = true
    override def writePermission_?  = true
  }
  
  object validated extends MappedBoolean[T](this) {
    override def defaultValue = false
  }
  
  object locale extends MappedLocale[T](this)
  
  object timezone extends MappedTimeZone[T](this)
  
}
