package com.readclosely.model
/*
 Copyright 2009-2010 Karl Pichotta

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

  
import _root_.net.liftweb.mapper._  
import _root_.net.liftweb.http._  
import js._  
import JsCmds._  
import _root_.scala.xml.{NodeSeq, Node, Group, Text, Elem}  
import _root_.scala.xml.transform._  
import _root_.net.liftweb.sitemap._  
import _root_.net.liftweb.sitemap.Loc._  
import _root_.net.liftweb.util.Helpers._  
import _root_.net.liftweb.util._  
import _root_.net.liftweb.util.Mailer._  
import S._  
  
/**
 * An O-R mapped "User" class that includes first name, last name, password 
 */
class User extends MegaReadCloselyProtoUser[User] {
    def getSingleton = User // what's the "meta" server
}

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with MetaMegaReadCloselyProtoUser[User] {
    override def screenWrap = Full(<lift:surround with="single" at="content">
                                   <lift:bind /></lift:surround>)

}




/*
 *Everything below here is copied/pased and the modified from ProtoUser.scala, from the actual
 *lift source. It was originally under the apache 2 license, so if this gets open-sourced this code
 *is also under that license.
*/

trait ReadCloselyProtoUser[T <: ReadCloselyProtoUser[T]] extends KeyedMapper[Long, T] with UserIdAsString {  
  self: T =>  
  
  override def primaryKeyField = id  
  
  // the primary key for the database  
  object id extends MappedLongIndex(this)  
  
  def userIdAsString: String = id.is.toString  

  //username:
  object name extends MappedString(this, 32) {
      override def displayName = "User Name"
      override def dbIndexed_? = true
      override def validations = valUnique(S.?("unique.username")) _ :: super.validations  
      //override def validations = valUnique(S.??("unique.username")) _ :: super.validations  
  }
  
  /*
  // First Name  
  object firstName extends MappedString(this, 32) {  
    override def displayName = fieldOwner.firstNameDisplayName  
    override val fieldId = Some(Text("txtFirstName"))  
  }  
  
  def firstNameDisplayName = ??("First Name")  
  
  // Last Name  
  object lastName extends MappedString(this, 32) {  
    override def displayName = fieldOwner.lastNameDisplayName  
    override val fieldId = Some(Text("txtLastName"))  
  }  
  
  def lastNameDisplayName = ??("Last Name")  
  */
  
  // Email  
  object email extends MappedEmail(this, 48) {  
    override def dbIndexed_? = true  
    override def validations = valUnique(S.??("unique.email.address")) _ :: super.validations  
    override def displayName = fieldOwner.emailDisplayName  
    override val fieldId = Some(Text("txtEmail"))  
  }  
  
  def emailDisplayName = ??("Email")  
  // Password  
  object password extends MappedPassword[T](this) {  
    override def displayName = fieldOwner.passwordDisplayName  
  }  
  
  def passwordDisplayName = ??("Password")  
  
  object superUser extends MappedBoolean(this) {  
    override def defaultValue = false  
  }  

  
  /*
  def niceName: String = (firstName.is, lastName.is, email.is) match {  
    case (f, l, e) if f.length > 1 && l.length > 1 => f+" "+l+" ("+e+")"  
    case (f, _, e) if f.length > 1 => f+" ("+e+")"  
    case (_, l, e) if l.length > 1 => l+" ("+e+")"  
    case (_, _, e) => e  
  }  
  */
  
  /*
  def shortName: String = (firstName.is, lastName.is) match {  
    case (f, l) if f.length > 1 && l.length > 1 => f+" "+l  
    case (f, _) if f.length > 1 => f  
    case (_, l) if l.length > 1 => l  
    case _ => email.is  
  }  
  */
  
  //def niceNameWEmailLink = <a href={"mailto:"+email.is}>{niceName}</a>  
}  
  
trait MetaMegaReadCloselyProtoUser[ModelType <: MegaReadCloselyProtoUser[ModelType]] extends KeyedMetaMapper[Long, ModelType] {  
  self: ModelType =>  
  
  def signupFields: List[BaseOwnedMappedField[ModelType]] = name :: email :: password :: Nil  
  //def signupFields: List[BaseOwnedMappedField[ModelType]] = firstName :: lastName :: email :: locale :: timezone :: password :: Nil  
  
  override def fieldOrder: List[BaseOwnedMappedField[ModelType]] = name :: email :: password :: Nil  
  //override def fieldOrder: List[BaseOwnedMappedField[ModelType]] = firstName :: lastName :: email :: locale :: timezone :: password :: Nil  
  
  /** 
   * If the 
   */  
  def screenWrap: Box[Node] = Empty  
  
  val basePath: List[String] = "user_mgt" :: Nil  
  def signUpSuffix = "sign_up"  
  lazy val signUpPath = thePath(signUpSuffix)  
  def loginSuffix = "login"  
  lazy val loginPath = thePath(loginSuffix)  
  def lostPasswordSuffix = "lost_password"  
  lazy val lostPasswordPath = thePath(lostPasswordSuffix)  
  def passwordResetSuffix = "reset_password"  
  lazy val passwordResetPath = thePath(passwordResetSuffix)  
  def changePasswordSuffix = "change_password"  
  lazy val changePasswordPath = thePath(changePasswordSuffix)  
  def logoutSuffix = "logout"  
  lazy val logoutPath = thePath(logoutSuffix)  
  def editSuffix = "edit"  
  lazy val editPath = thePath(editSuffix)  
  def validateUserSuffix = "validate_user"  
  lazy val validateUserPath = thePath(validateUserSuffix)  
  
  def homePage = "/"  
  
  
  
  case class MenuItem(name: String, path: List[String],  
                      loggedIn: Boolean) {  
    lazy val endOfPath = path.last  
    lazy val pathStr: String = path.mkString("/", "/", "")  
    lazy val display = name match {  
      case null | "" => false  
      case _ => true  
    }  
  }  
  
  def thePath(end: String): List[String] = basePath ::: List(end)  
  
  /** 
   * Return the URL of the "login" page 
   */  
  def loginPageURL = loginPath.mkString("/","/", "")  
  
  def notLoggedIn_? = !loggedIn_?  

  lazy val testLogginIn = If(loggedIn_? _, S.??("must.be.logged.in")) ;  
  
  lazy val testSuperUser = If(superUser_? _, S.??("must.be.super.user"))  
  
  def superUser_? : Boolean = currentUser.map(_.superUser.is) openOr false  
  
  /** 
   * The menu item for login (make this "Empty" to disable) 
   */  
  def loginMenuLoc: Box[Menu] = {  
    Full(Menu(Loc("Login", loginPath, "Login To Comment",  
                  If(notLoggedIn_? _, S.??("already.logged.in")),  
                  Template(() => wrapIt(login)))))  
  }  
  
  /** 
   * The menu item for logout (make this "Empty" to disable) 
   */  
  def logoutMenuLoc: Box[Menu] =  
  Full(Menu(Loc("Logout", logoutPath, S.??("logout"),  
                Template(() => wrapIt(logout)),  
                testLogginIn)))  
  
  /** 
   * The menu item for creating the user/sign up (make this "Empty" to disable) 
   */  
  def createUserMenuLoc: Box[Menu] =  
  Full(Menu(Loc("CreateUser", signUpPath,  
                S.??("sign.up"),  
                Template(() => wrapIt(signupFunc.map(_()) openOr signup)),  
                If(notLoggedIn_? _, S.??("logout.first")))))  
  
  /** 
   * The menu item for lost password (make this "Empty" to disable) 
   */  
  def lostPasswordMenuLoc: Box[Menu] =  
  Full(Menu(Loc("LostPassword", lostPasswordPath,  
                S.??("lost.password"),  
                Template(() => wrapIt(lostPassword)),  
                If(notLoggedIn_? _, S.??("logout.first"))))) // not logged in  
  
  /** 
   * The menu item for resetting the password (make this "Empty" to disable) 
   */  
  def resetPasswordMenuLoc: Box[Menu] =  
  Full(Menu(Loc("ResetPassword", (passwordResetPath, true),  
                S.??("reset.password"), Hidden,  
                Template(() => wrapIt(passwordReset(snarfLastItem))),  
                If(notLoggedIn_? _,  
                   S.??("logout.first"))))) //not Logged in  
  
  /** 
   * The menu item for editing the user (make this "Empty" to disable) 
   */  
  def editUserMenuLoc: Box[Menu] =  
  Full(Menu(Loc("EditUser", editPath, S.??("edit.user"),  
                Template(() => wrapIt(editFunc.map(_()) openOr edit)),  
                testLogginIn)))  
  
  /** 
   * The menu item for changing password (make this "Empty" to disable) 
   */  
  def changePasswordMenuLoc: Box[Menu] =  
  Full(Menu(Loc("ChangePassword", changePasswordPath,  
                S.??("change.password"),  
                Template(() => wrapIt(changePassword)),  
                testLogginIn)))  
  
  /** 
   * The menu item for validating a user (make this "Empty" to disable) 
   */  
  def validateUserMenuLoc: Box[Menu] =  
  Full(Menu(Loc("ValidateUser", (validateUserPath, true),  
                S.??("validate.user"), Hidden,  
                Template(() => wrapIt(validateUser(snarfLastItem))),  
                If(notLoggedIn_? _, S.??("logout.first")))))  
  
  lazy val sitemap: List[Menu] =  
  List(loginMenuLoc, logoutMenuLoc, createUserMenuLoc,  
       lostPasswordMenuLoc, resetPasswordMenuLoc,  
       editUserMenuLoc, changePasswordMenuLoc,  
       validateUserMenuLoc).flatten(a => a)  
  
  
  def skipEmailValidation = false  
  
  def userMenu: List[Node] = {  
    val li = loggedIn_?  
    ItemList.  
    filter(i => i.display && i.loggedIn == li).  
    map(i => (<a href={i.pathStr}>{i.name}</a>))  
  }  
  
  protected def snarfLastItem: String =  
  (for (r <- S.request) yield r.path.wholePath.last) openOr ""  
  
  lazy val ItemList: List[MenuItem] =  
  List(MenuItem(S.??("sign.up"), signUpPath, false),  
       MenuItem(S.??("log.in"), loginPath, false),  
       MenuItem(S.??("lost.password"), lostPasswordPath, false),  
       MenuItem("", passwordResetPath, false),  
       MenuItem(S.??("change.password"), changePasswordPath, true),  
       MenuItem(S.??("log.out"), logoutPath, true),  
       MenuItem(S.??("edit.profile"), editPath, true),  
       MenuItem("", validateUserPath, false))  
  
  // def requestLoans: List[LoanWrapper] = Nil // List(curUser)  
  
  var onLogIn: List[ModelType => Unit] = Nil  
  
  var onLogOut: List[Box[ModelType] => Unit] = Nil  
  
  def loggedIn_? : Boolean = currentUserId.isDefined  

  def logUserIdIn(id: String) {  
    curUser.remove()  
    curUserId(Full(id))  
  }  
  def logUserIn(who: ModelType) {  
    curUser.remove()  
    curUserId(Full(who.id.toString))  
    onLogIn.foreach(_(who))  
  }  
  
  def logoutCurrentUser = logUserOut()  
  
  def logUserOut() {  
    onLogOut.foreach(_(curUser))  
    curUserId.remove()  
    curUser.remove()  
    S.request.foreach(_.request.getSession.invalidate)  
  }  
  
  private object curUserId extends SessionVar[Box[String]](Empty)  
  
  def currentUserId: Box[String] = curUserId.is  
  
  private object curUser extends RequestVar[Box[ModelType]](currentUserId.flatMap(id => getSingleton.find(id)))  
  
  
  def currentUser: Box[ModelType] = curUser.is  
  
  def signupXhtml(user: ModelType) = {  
    (<form method="post" action={S.uri}>
        <lift:Msgs>
            <lift:error_msg></lift:error_msg> 
            <lift:error_class>signuperr</lift:error_class> 
            <lift:notice_msg></lift:notice_msg>
            <lift:notice_class>signupnotice</lift:notice_class>
        </lift:Msgs>
        <table><tr><td  
              colspan="2">Sign Up</td></tr>  
          {localForm(user, false)}  
          <tr><td> </td><td><user:submit/></td></tr>  
                                        </table></form>)  
  }  
  
  
  def signupMailBody(user: ModelType, validationLink: String) = {  
    (<html>  
        <head>  
          <title>ReadClosely Sign Up Confirmation</title>  
        </head>  
        <body>  
          <p>Dear {user.name},  
            <br/>  
            <br/>  
            Click on this link to complete signup:
            <br/><a href={validationLink}>{validationLink}</a>  
            <br/>  
            <br/>  
            Thanks.
          </p>  
        </body>  
     </html>)  
  }  
  
  def signupMailSubject = S.??("sign.up.confirmation")  
  
  def sendValidationEmail(user: ModelType) {  
    val resetLink = S.hostAndPath+"/"+validateUserPath.mkString("/")+  
    "/"+user.uniqueId  
  
    val email: String = user.email  
  
    val msgXml = signupMailBody(user, resetLink)  
  
    Mailer.sendMail(From(emailFrom),Subject(signupMailSubject),  
                    (To(user.email) :: xmlToMailBodyType(msgXml) ::  
                     (bccEmail.toList.map(BCC(_)))) :_* )  
  }  
  
  protected object signupFunc extends RequestVar[Box[() => NodeSeq]](Empty)  
  
  def signup = {  
    val theUser: ModelType = create  
    val theName = signUpPath.mkString("")  
  
    def testSignup() {  
      theUser.validate match {  
        case Nil =>  
          theUser.validated(skipEmailValidation).uniqueId.reset()  
          theUser.save  
          if (!skipEmailValidation) {  
            sendValidationEmail(theUser)  
            S.notice(S.??("sign.up.message"))  
          } else {  
            S.notice(S.??("welcome"))  
            logUserIn(theUser)  
          }  
  
          S.redirectTo(homePage)  
  
        case xs => S.error(xs) ; signupFunc(Full(innerSignup _))  
      }  
    }  
  
    def innerSignup = bind("user",  
                           signupXhtml(theUser),  
                           "submit" -> SHtml.submit(S.??("sign.up"), testSignup _))  
  
    innerSignup  
  }  
  
  def emailFrom = "noreply@"+S.hostName  
  
  def bccEmail: Box[String] = Empty  
  
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
      S.redirectTo(homePage)  
  
    case _ => S.error(S.??("invalid.validation.link")); S.redirectTo(homePage)  
  }  
  
  def loginXhtml = {  
    (<form method="post" action={S.uri}>
        <lift:Msgs>
            <lift:error_msg></lift:error_msg> 
            <lift:error_class>signuperr</lift:error_class> 
            <lift:notice_msg></lift:notice_msg>
            <lift:notice_class>signupnotice</lift:notice_class>
        </lift:Msgs>
        <table><tr><td  
              colspan="2">{S.??("log.in")}</td></tr>  
          <tr><td>{S.?("Username")}</td><td><user:name /></td></tr>  
          <tr><td>{S.?("Password")}</td><td><user:password /></td></tr>  
          <tr><td><a href={lostPasswordPath.mkString("/", "/", "")}  
                >{S.?("Recover Password")}</a></td><td><user:submit /></td></tr>
        </table>  
     </form>)  
  }  
  //<tr><td>{S.??("email.address")}</td><td><user:email /></td></tr>  
  
  def login = {  
    if (S.post_?) {  
      S.param("name").  
      flatMap(username => getSingleton.find(By(name, username))) match {  
        case Full(user) if user.validated &&  
          user.password.match_?(S.param("password").openOr("*")) =>  
          logUserIn(user); S.notice(S.??("logged.in")); S.redirectTo(homePage)  
  
        case Full(user) if !user.validated =>  
          S.error(S.??("account.validation.error"))  
  
        case _ => S.error(S.??("invalid.credentials"))  
      }  
    }  
  
    bind("user", loginXhtml,  
         //"email" -> (FocusOnLoad(<input type="text" name="username"/>)),  
         "name" -> (FocusOnLoad(<input type="text" name="name"/>)),  
         "password" -> (<input type="password" name="password"/>),  
         "submit" -> (<input type="submit" value={S.??("log.in")}/>))  
  }  
  
  def lostPasswordXhtml = {  
    (<form method="post" action={S.uri}>  
        <lift:Msgs>
            <lift:error_msg></lift:error_msg> 
            <lift:error_class>signuperr</lift:error_class> 
            <lift:notice_msg></lift:notice_msg>
            <lift:notice_class>signupnotice</lift:notice_class>
        </lift:Msgs>
        <table><tr><td  
              colspan="2">{S.??("enter.email")}</td></tr>  
          <tr><td>{S.??("email.address")}</td><td><user:email /></td></tr>  
          <tr><td> </td><td><user:submit /></td></tr>  
        </table>  
     </form>)  
  }  
  
  def passwordResetMailBody(user: ModelType, resetLink: String) = {  
    (<html>  
        <head>  
          <title>{S.??("reset.password.confirmation")}</title>  
        </head>  
        <body>  
          <p>{S.??("dear")} {user.name},  
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
        val resetLink = S.hostAndPath+  
        passwordResetPath.mkString("/", "/", "/")+user.uniqueId  
  
        val email: String = user.email  
  
        val msgXml = passwordResetMailBody(user, resetLink)  
        Mailer.sendMail(From(emailFrom),Subject(passwordResetEmailSubject),  
                        (To(user.email) :: xmlToMailBodyType(msgXml) ::  
                         (bccEmail.toList.map(BCC(_)))) :_*)  
  
        S.notice(S.??("password.reset.email.sent"))  
        S.redirectTo(homePage)  
  
      case Full(user) =>  
        sendValidationEmail(user)  
        S.notice(S.??("account.validation.resent"))  
        S.redirectTo(homePage)  
  
      case _ => S.error(S.??("email.address.not.found"))  
    }  
  }  
  
  def lostPassword = {  
    bind("user", lostPasswordXhtml,  
         "email" -> SHtml.text("", sendPasswordReset _),  
         "submit" -> <input type="Submit" value={S.??("send.it")} />)  
  }  
  
  def passwordResetXhtml = {  
    (<form method="post" action={S.uri}>  
        <lift:Msgs>
            <lift:error_msg></lift:error_msg> 
            <lift:error_class>signuperr</lift:error_class> 
            <lift:notice_msg></lift:notice_msg>
            <lift:notice_class>signupnotice</lift:notice_class>
        </lift:Msgs>
        <table><tr><td colspan="2">{S.??("reset.your.password")}</td></tr>  
          <tr><td>{S.??("enter.your.new.password")}</td><td><user:pwd/></td></tr>  
          <tr><td>{S.??("repeat.your.new.password")}</td><td><user:pwd/></td></tr>  
          <tr><td> </td><td><user:submit/></td></tr>  
        </table>  
     </form>)  
  }  
  
  def passwordReset(id: String) =  
  getSingleton.find(By(uniqueId, id)) match {  
    case Full(user) =>  
      def finishSet() {  
        user.validate match {  
          case Nil => S.notice(S.??("password.changed"))  
            user.save  
            logUserIn(user); S.redirectTo(homePage)  
  
          case xs => S.error(xs)  
        }  
      }  
      user.uniqueId.reset().save  
  
      bind("user", passwordResetXhtml,  
           "pwd" -> SHtml.password_*("",(p: List[String]) =>  
          user.password.setList(p)),  
           "submit" -> SHtml.submit(S.??("set.password"), finishSet _))  
    case _ => S.error(S.??("pasword.link.invalid")); S.redirectTo(homePage)  
  }  
  
  def changePasswordXhtml = {  
    (<form method="post" action={S.uri}>  
        <lift:Msgs>
            <lift:error_msg></lift:error_msg> 
            <lift:error_class>signuperr</lift:error_class> 
            <lift:notice_msg></lift:notice_msg>
            <lift:notice_class>signupnotice</lift:notice_class>
        </lift:Msgs>
        <table><tr><td colspan="2">{S.??("change.password")}</td></tr>  
          <tr><td>{S.??("old.password")}</td><td><user:old_pwd /></td></tr>  
          <tr><td>{S.??("new.password")}</td><td><user:new_pwd /></td></tr>  
          <tr><td>{S.??("repeat.password")}</td><td><user:new_pwd /></td></tr>  
          <tr><td> </td><td><user:submit /></td></tr>  
        </table>  
     </form>)  
  }  
  
  def changePassword = {  
    val user = currentUser.open_! // we can do this because the logged in test has happened  
    var oldPassword = ""  
    var newPassword: List[String] = Nil  
  
    def testAndSet() {  
      if (!user.password.match_?(oldPassword)) S.error(S.??("wrong.old.password"))  
      else {  
        user.password.setFromAny(newPassword)  
        user.validate match {  
          case Nil => user.save; S.notice(S.??("pasword.changed")); S.redirectTo(homePage)  
          case xs => S.error(xs)  
        }  
      }  
    }  
  
    bind("user", changePasswordXhtml,  
         "old_pwd" -> SHtml.password("", oldPassword = _),  
         "new_pwd" -> SHtml.password_*("", LFuncHolder(newPassword = _)),  
         "submit" -> SHtml.submit(S.??("change"), testAndSet _))  
  }  
  
  def editXhtml(user: ModelType) = {  
    (<form method="post" action={S.uri}>  
        <lift:Msgs>
            <lift:error_msg></lift:error_msg> 
            <lift:error_class>signuperr</lift:error_class> 
            <lift:notice_msg></lift:notice_msg>
            <lift:notice_class>signupnotice</lift:notice_class>
        </lift:Msgs>
        <table><tr><td colspan="2">{S.??("edit")}</td></tr>  
          {localForm(user, true)}  
          <tr><td> </td><td><user:submit/></td></tr>  
        </table>  
     </form>)  
  }  
  
  object editFunc extends RequestVar[Box[() => NodeSeq]](Empty)  
  
  def edit = {  
    val theUser: ModelType = currentUser.open_! // we know we're logged in  
    val theName = editPath.mkString("")  
  
    def testEdit() {  
      theUser.validate match {  
        case Nil =>  
          theUser.save  
          S.notice(S.??("profle.updated"))  
          S.redirectTo(homePage)  
  
        case xs => S.error(xs) ; editFunc(Full(innerEdit _))  
      }  
    }  
  
    def innerEdit = bind("user", editXhtml(theUser),  
                         "submit" -> SHtml.submit(S.??("edit"), testEdit _))  
  
    innerEdit  
  }  
  
  def logout = {  
    logoutCurrentUser  
    S.redirectTo(homePage)  
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
  
  protected def wrapIt(in: NodeSeq): NodeSeq =  
  screenWrap.map(new RuleTransformer(new RewriteRule {  
        override def transform(n: Node) = n match {  
          case e: Elem if "bind" == e.label && "lift" == e.prefix => in  
          case _ => n  
        }  
      })) openOr in  
}  
  
trait MegaReadCloselyProtoUser[T <: MegaReadCloselyProtoUser[T]] extends ReadCloselyProtoUser[T] {  
  self: T =>  
  object uniqueId extends MappedUniqueId(this, 32) {  
    override def dbIndexed_? = true  
    override def writePermission_?  = true  
  }  
  
  object validated extends MappedBoolean[T](this) {  
    override def defaultValue = false  
    override val fieldId = Some(Text("txtValidated"))  
  }  
  
  object locale extends MappedLocale[T](this) {  
    override def displayName = fieldOwner.localeDisplayName  
    override val fieldId = Some(Text("txtLocale"))  
  }  
  
  object timezone extends MappedTimeZone[T](this) {  
    override def displayName = fieldOwner.timezoneDisplayName  
    override val fieldId = Some(Text("txtTimeZone"))  
  }  
  
  def timezoneDisplayName = ??("Time Zone")  
  
  def localeDisplayName = ??("Locale")  
  
}  

