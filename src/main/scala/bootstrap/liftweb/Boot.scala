package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import _root_.java.sql.{Connection, DriverManager}
import _root_.com.readclosely.model._
import _root_.javax.servlet.http.{HttpServletRequest}
import com.readclosely.util.{SectionUtil}

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?)
      DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)

    // where to search snippet
    LiftRules.addToPackages("com.readclosely")
    Schemifier.schemify(true, Log.infoF _, User,
                                           Passage,
                                           Annotation,
                                           Mod,
                                           Doc,
                                           Section,
                                           DocSubTitle,
                                           DeletedAnnotation,
                                           DeletedMod)

    // Build SiteMap
    LiftRules.setSiteMap(SiteMap(MenuInfo.menu :_*))

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    /*
     * Resource bundles. Added kbp
     */
    LiftRules.resourceNames = "UserMessages" :: Nil

    S.addAround(DB.buildLoanWrapper)

    /**
    * URL Rewriting.
    */
    LiftRules.rewrite.append {
      case RewriteRequest(ParsePath(List("passage", psgID), _, _, _), _, _) =>
        RewriteResponse("viewPassage" :: Nil, Map("id" -> urlDecode(psgID)))
      case RewriteRequest(ParsePath(List("user", username), _, _, _), _, _) =>
        RewriteResponse("viewUser" :: Nil, Map("username" -> urlDecode(username)))
      case RewriteRequest(ParsePath(List("doc", docID, secNum), _, _, _), _, _) =>
        RewriteResponse("viewPassage" :: Nil, Map("id" -> SectionUtil.getPsgID(urlDecode(docID), urlDecode(secNum))))
      case RewriteRequest(ParsePath(List("doc", docID), _, _, _), _, _) => {
	val psgID: String = SectionUtil.getPsgIDOrNull(urlDecode(docID))
	if(psgID == null) RewriteResponse("viewDoc" :: Nil, Map("id" -> urlDecode(docID)))
	else RewriteResponse("viewPassage" :: Nil, Map("id" -> urlDecode(psgID)))
      }
    }

  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HttpServletRequest) {
    req.setCharacterEncoding("UTF-8")
  }

} //Boot


object MenuInfo {
    import Loc._
    val IfLoggedIn = If(() => User.currentUser.isDefined, "You must be logged in")
    val IfSU = If( () => User.superUser_?, "You must have administrative privileges.")
    def menu: List[Menu] =  Menu(Loc("home", List("index"), "Home")) ::
        Menu(Loc("viewPassage", List("viewPassage"), "View Passage", Hidden)) ::
        //Menu(Loc("newPassage", List("newPassage"), "Add New Passage")) ::
        Menu(Loc("viewDoc", List("viewDoc"), "View Document", Hidden)) ::
        //Menu(Loc("newDoc", List("newDoc"), "Add New Document", IfAdmin)) ::
        Menu(Loc("newDocFromFile", List("newDocFromFile"), "Upload New Document", IfSU)) ::
        Menu(Loc("viewUser", List("viewUser"), "View User", Hidden)) ::
        Menu(Loc("about", List("about"), "About")) ::
        Menu(Loc("terms", List("terms"), "Terms of use", Hidden)) ::
        Menu(Loc("search", List("search"), "Search", Hidden)) ::
        Menu(Loc("docs", List("docs"), "All Texts", Hidden)) ::
        Menu(Loc("adminAnns", List("adminAnns"), "Admin Anns", IfSU)) ::
        User.sitemap
}




/**
* Database connection calculation
*/
object DBVendor extends ConnectionManager {
 def newConnection(name: ConnectionIdentifier): Box[Connection] = {
   try {
     Class.forName("com.mysql.jdbc.Driver")
     val dm = DriverManager.getConnection("jdbc:mysql://localhost/rcdb?user=root&password=[ROOTPASSWD]")
     Full(dm)
   } catch {
     case e : Exception => e.printStackTrace; Empty
   }
 }
 def releaseConnection(conn: Connection) {conn.close}
}

/*
object DBVendor extends ConnectionManager {
  private var pool: List[Connection] = Nil
  private var poolSize = 0
  private val maxPoolSize = 4

  private def createOne: Box[Connection] = try {
    val driverName: String = Props.get("db.driver") openOr
    "org.apache.derby.jdbc.EmbeddedDriver"

    val dbUrl: String = Props.get("db.url") openOr
    "jdbc:derby:meta50DB;create=true"

    Class.forName(driverName)

    val dm = (Props.get("db.user"), Props.get("db.password")) match {
      case (Full(user), Full(pwd)) =>
	DriverManager.getConnection(dbUrl, user, pwd)

      case _ => DriverManager.getConnection(dbUrl)
    }

    Full(dm)
  } catch {
    case e: Exception => e.printStackTrace; Empty
  }

  def newConnection(name: ConnectionIdentifier): Box[Connection] =
    synchronized {
      pool match {
	case Nil if poolSize < maxPoolSize =>
	  val ret = createOne
        poolSize = poolSize + 1
        ret.foreach(c => pool = c :: pool)
        ret

	case Nil => wait(1000L); newConnection(name)
	case x :: xs => try {
          x.setAutoCommit(false)
          Full(x)
        } catch {
          case e => try {
            pool = xs
            poolSize = poolSize - 1
            x.close
            newConnection(name)
          } catch {
            case e => newConnection(name)
          }
        }
      }
    }

  def releaseConnection(conn: Connection): Unit = synchronized {
    pool = conn :: pool
    notify
  }
}
*/


