package net.liftweb.mapper

/*                                                *\
(c) 2006-2008 WorldWide Conferencing, LLC
Distributed under an Apache License
http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import java.sql.{Connection, ResultSet, Statement, PreparedStatement, Types, ResultSetMetaData}
import javax.sql.{ DataSource}
import javax.naming.{Context, InitialContext}
import scala.collection.mutable._
import net.liftweb.util._
// import net.liftweb.util.Lazy._

object DB {
  private val threadStore = new ThreadLocal[HashMap[ConnectionIdentifier, ConnectionHolder]]
  private val envContext = FatLazy((new InitialContext).lookup("java:/comp/env").asInstanceOf[Context])
  val logger = LogBoot.loggerByClass(DB.getClass.asInstanceOf[Class[AnyRef]])
  
  var queryTimeout: Can[Int] = Empty
  
  private var logFuncs: List[(String, Long) => Any] = Nil
  
  def addLogFunc(f: (String, Long) => Any): List[(String, Long) => Any] = {
    logFuncs = logFuncs ::: List(f)
    logFuncs
  }
  
  /**
  * can we get a JDBC connection from JNDI?
  */
  def jndiJdbcConnAvailable_? : Boolean = {
    val touchedEnv = envContext.calculated_?
    
    val ret = try {
      (envContext.get.lookup(DefaultConnectionIdentifier.jndiName).asInstanceOf[DataSource].getConnection) != null
    } catch {
      case e => false
    }
    
    if (!touchedEnv) envContext.reset
    ret
  }
  
  // var connectionManager: Can[ConnectionManager] = Empty
  private val connectionManagers = new HashMap[ConnectionIdentifier, ConnectionManager];
  
  def defineConnectionManager(name: ConnectionIdentifier, mgr: ConnectionManager) {
    connectionManagers(name) = mgr
  }
  
  case class ConnectionHolder(conn: SuperConnection, cnt: Int, postCommit: List[() => Unit]) 
  
  private def info : HashMap[ConnectionIdentifier, ConnectionHolder] = {
    threadStore.get match {
      case null =>
      val tinfo = new HashMap[ConnectionIdentifier, ConnectionHolder]
      threadStore.set(tinfo)
      tinfo
      
      case v => v
    }
  }
  
  // remove thread-local association
  def clearThread: Unit = { threadStore.remove }  
  
  private def newConnection(name : ConnectionIdentifier) : SuperConnection = {
    val ret = (Can(connectionManagers.get(name)).flatMap(cm => cm.newConnection(name).map(c => new SuperConnection(c, () => cm.releaseConnection(c))))) openOr {
      Helpers.tryo {
        val conn = envContext.get.lookup(name.jndiName).asInstanceOf[DataSource].getConnection
        new SuperConnection(conn, () => conn.close)
      } openOr {throw new NullPointerException("Looking for Connection Identifier "+name+" but failed to find either a JNDI data source "+
      "with the name "+name.jndiName+" or a lift connection manager with the correct name")}
    }
    ret.setAutoCommit(false)
    ret
  }
  
  
  
  private def releaseConnection(conn : SuperConnection) : unit = conn.close
  
  private def getConnection(name : ConnectionIdentifier): SuperConnection =  {
    var ret = info.get(name) match {
      case None => ConnectionHolder(newConnection(name), 1, Nil)
      case Some(ConnectionHolder(conn, cnt, post)) => ConnectionHolder(conn, cnt + 1, post)
    }
    info(name) = ret
    ret.conn
  }
  
  private def releaseConnectionNamed(name: ConnectionIdentifier) {
    info.get(name) match {
      case Some(ConnectionHolder(c, 1, post)) => c.commit; c.releaseFunc(); info -= name; post.reverse.foreach(_()); logger.trace("Released connection "+name)
      case Some(ConnectionHolder(c, n, post)) => info(name) = ConnectionHolder(c, n - 1, post)
      case _ =>
    }
  }
  
  /**
  *  Append a function to be invoked after the commit has taken place for the given connection identifier
  */
  def appendPostFunc(name: ConnectionIdentifier, func: () => Unit) {
    info.get(name) match {
      case Some(ConnectionHolder(c, n, post)) => info(name) = ConnectionHolder(c, n, func :: post)
      case _ =>
    }
  }
  
  private def runLogger(query: String, time: Long) {
    logFuncs.foreach(_(query, time))
  }
  
  def statement[T](db : SuperConnection)(f : (Statement) => T) : T =  {
    Helpers.calcTime {
      val st = db.createStatement
      queryTimeout.foreach(to => st.setQueryTimeout(to))
      try {
        (st.toString, f(st))
      } finally {
        st.close
      }
    } match {case (time, (query, res)) => runLogger(query, time); res}
  }
  
  def exec[T](db : SuperConnection, query : String)(f : (ResultSet) => T) : T = {
    Helpers.calcTime(
    statement(db) {st => 
      f(st.executeQuery(query))
      }) match {case (time, res) => runLogger(query, time); res}
  }
  
     
    
  private def asString(pos: Int, rs: ResultSet, md: ResultSetMetaData): String = {
     import java.sql.Types._
     md.getColumnType(pos) match {
      case ARRAY | BINARY | BLOB | DATALINK | DISTINCT | JAVA_OBJECT | LONGVARBINARY | NULL | OTHER | REF | STRUCT | VARBINARY  => rs.getObject(pos) match {
        case null => null
        case s => s.toString
      }
      case BIGINT |  INTEGER | DECIMAL | NUMERIC | SMALLINT | TINYINT => rs.getLong(pos).toString
      case BIT | BOOLEAN => rs.getBoolean(pos).toString
      
      case VARCHAR | CHAR | CLOB | LONGVARCHAR => rs.getString(pos)
      
      case DATE | TIME | TIMESTAMP => rs.getTimestamp(pos).toString
      
      case DOUBLE | FLOAT | REAL  => rs.getDouble(pos).toString
    }
  }
  
  def resultSetTo(rs: ResultSet): (List[String], List[List[String]]) = {
    val md = rs.getMetaData
      val cnt = md.getColumnCount
      val cntList = (1 to cnt).toList
      val colNames = cntList.map(i => md.getColumnName(i))
      
      val lb = new ListBuffer[List[String]]()
      
      while(rs.next) {
        lb += cntList.map(i => asString(i, rs, md))
      }
      
      (colNames, lb.toList)
  }
  
  def runQuery(query: String, params: List[Any]): (List[String], List[List[String]]) = {
    use(DefaultConnectionIdentifier)(conn => prepareStatement(query, conn) {
      ps =>
      params.zipWithIndex.foreach {
        case (null, idx) => ps.setNull(idx + 1, Types.VARCHAR)
        case (i: Int, idx) => ps.setInt(idx +1, i)
        case (l: Long, idx) => ps.setLong(idx + 1, l)
        case (d: Double, idx) => ps.setDouble(idx + 1, d)
        case (f: Float, idx) => ps.setFloat(idx + 1, f)
        case (d: java.util.Date, idx) => ps.setDate(idx + 1, new java.sql.Date(d.getTime))
        case (b: Boolean, idx) => ps.setBoolean(idx + 1, b)
        case (s: String, idx) => ps.setString(idx + 1, s)
        case (bn: java.math.BigDecimal, idx) => ps.setBigDecimal(idx + 1, bn)
        case (obj, idx) => ps.setObject(idx + 1, obj)
      }
      
      resultSetTo(ps.executeQuery)
    })
  }
  
  def runQuery(query: String): (List[String], List[List[String]]) = {

    
    use(DefaultConnectionIdentifier)(conn => exec(conn, query)(resultSetTo)) 
  }
  
  def rollback(name: ConnectionIdentifier) = use(name)(conn => conn.rollback)
  
  /**
  * Executes {@code statement} and converts the {@code ResultSet} to model 
  * instance {@code T} using {@code f} 
  */
  def exec[T](statement : PreparedStatement)(f : (ResultSet) => T) : T = {
    queryTimeout.foreach(to => statement.setQueryTimeout(to))
    Helpers.calcTime {
      val rs = statement.executeQuery
      try {
        (statement.toString, f(rs))
      } finally {
        statement.close
        rs.close
      }} match {case (time, (query, res)) => runLogger(query, time); res}
  }
  
  def prepareStatement[T](statement : String, conn: SuperConnection)(f : (PreparedStatement) => T) : T = {
    Helpers.calcTime {
      val st = conn.prepareStatement(statement)
      queryTimeout.foreach(to => st.setQueryTimeout(to))
      try {
        (st.toString, f(st))
      } finally {
        st.close
      }} match {case (time, (query, res)) => runLogger(query, time); res}
  }
  
  def prepareStatement[T](statement : String, keys: int, conn: SuperConnection)(f : (PreparedStatement) => T) : T = {
    Helpers.calcTime{
      val st = conn.prepareStatement(statement, keys)
      queryTimeout.foreach(to => st.setQueryTimeout(to))
      try {
        (st.toString, f(st))
      } finally {
        st.close
      }} match {case (time, (query, res)) => runLogger(query, time); res}
  }
  
  /**
  * Executes function {@code f} with the connection named {@code name}. Releases the connection
  * before returning.
  */
  def use[T](name : ConnectionIdentifier)(f : (SuperConnection) => T) : T = {
    val conn = getConnection(name)
    try {
      f(conn)
    } finally {
      releaseConnectionNamed(name)
    }
  }
  
  
  val reservedWords = scala.collection.immutable.HashSet.empty ++ List("abort" , 
  "accept" , 
  "access" , 
  "add" , 
  "admin" , 
  "after" , 
  "all" , 
  "allocate" , 
  "alter" , 
  "analyze" , 
  "and" , 
  "any" , 
  "archive" , 
  "archivelog" , 
  "array" , 
  "arraylen" , 
  "as" , 
  "asc" , 
  "assert" , 
  "assign" , 
  "at" , 
  "audit" , 
  "authorization" , 
  "avg" , 
  "backup" , 
  "base_table" , 
  "become" , 
  "before" , 
  "begin" , 
  "between" , 
  "binary_integer" , 
  "block" , 
  "body" , 
  "boolean" , 
  "by" , 
  "cache" , 
  "cancel" , 
  "cascade" , 
  "case" , 
  "change" , 
  "char" , 
  "character" , 
  "char_base" , 
  "check" , 
  "checkpoint" , 
  "close" , 
  "cluster" , 
  "clusters" , 
  "cobol" , 
  "colauth" , 
  "column" , 
  "columns" , 
  "comment" , 
  "commit" , 
  "compile" , 
  "compress" , 
  "connect" , 
  "constant" , 
  "constraint" , 
  "constraints" , 
  "contents" , 
  "continue" , 
  "controlfile" , 
  "count" , 
  "crash" , 
  "create" , 
  "current" , 
  "currval" , 
  "cursor" , 
  "cycle" , 
  "database" , 
  "data_base" , 
  "datafile" , 
  "date" , 
  "dba" , 
  "debugoff" , 
  "debugon" , 
  "dec" , 
  "decimal" , 
  "declare" , 
  "default" , 
  "definition" , 
  "delay" , 
  "delete" , 
  "delta" , 
  "desc" , 
  "digits" , 
  "disable" , 
  "dismount" , 
  "dispose" , 
  "distinct" , 
  "do" , 
  "double" , 
  "drop" , 
  "dump" , 
  "each" , 
  "else" , 
  "elsif" , 
  "enable" , 
  "end" , 
  "entry" , 
  "escape" , 
  "events" , 
  "except" , 
  "exception" , 
  "exception_init" , 
  "exceptions" , 
  "exclusive" , 
  "exec" , 
  "execute" , 
  "exists" , 
  "exit" , 
  "explain" , 
  "extent" , 
  "externally" , 
  "false" , 
  "fetch" , 
  "file" , 
  "float" , 
  "flush" , 
  "for" , 
  "force" , 
  "foreign" , 
  "form" , 
  "fortran" , 
  "found" , 
  "freelist" , 
  "freelists" , 
  "from" , 
  "function" , 
  "generic" , 
  "go" , 
  "goto" , 
  "grant" , 
  "group" , 
  "having" , 
  "identified" , 
  "if" , 
  "immediate" , 
  "in" , 
  "including" , 
  "increment" , 
  "index" , 
  "indexes" , 
  "indicator" , 
  "initial" , 
  "initrans" , 
  "insert" , 
  "instance" , 
  "int" , 
  "integer" , 
  "intersect" , 
  "into" , 
  "is" , 
  "key" , 
  "language" , 
  "layer" , 
  "level" , 
  "like" , 
  "limited" , 
  "link" , 
  "lists" , 
  "lock" , 
  "logfile" , 
  "long" , 
  "loop" , 
  "manage" , 
  "manual" , 
  "max" , 
  "maxdatafiles" , 
  "maxextents" , 
  "maxinstances" , 
  "maxlogfiles" , 
  "maxloghistory" , 
  "maxlogmembers" , 
  "maxtrans" , 
  "maxvalue" , 
  "min" , 
  "minextents" , 
  "minus" , 
  "minvalue" , 
  "mlslabel" , 
  "mod" , 
  "mode" , 
  "modify" , 
  "module" , 
  "mount" , 
  "natural" , 
  "new" , 
  "next" , 
  "nextval" , 
  "noarchivelog" , 
  "noaudit" , 
  "nocache" , 
  "nocompress" , 
  "nocycle" , 
  "nomaxvalue" , 
  "nominvalue" , 
  "none" , 
  "noorder" , 
  "noresetlogs" , 
  "normal" , 
  "nosort" , 
  "not" , 
  "notfound" , 
  "nowait" , 
  "null" , 
  "number" , 
  "number_base" , 
  "numeric" , 
  "of" , 
  "off" , 
  "offline" , 
  "old" , 
  "on" , 
  "online" , 
  "only" , 
  "open" , 
  "optimal" , 
  "option" , 
  "or" , 
  "order" , 
  "others" , 
  "out" , 
  "own" , 
  "package" , 
  "parallel" , 
  "partition" , 
  "pctfree" , 
  "pctincrease" , 
  "pctused" , 
  "plan" , 
  "pli" , 
  "positive" , 
  "pragma" , 
  "precision" , 
  "primary" , 
  "prior" , 
  "private" , 
  "privileges" , 
  "procedure" , 
  "profile" , 
  "public" , 
  "quota" , 
  "raise" , 
  "range" , 
  "raw" , 
  "read" , 
  "real" , 
  "record" , 
  "recover" , 
  "references" , 
  "referencing" , 
  "release" , 
  "remr" , 
  "rename" , 
  "resetlogs" , 
  "resource" , 
  "restricted" , 
  "return" , 
  "reuse" , 
  "reverse" , 
  "revoke" , 
  "role" , 
  "roles" , 
  "rollback" , 
  "row" , 
  "rowid" , 
  "rowlabel" , 
  "rownum" , 
  "rows" , 
  "rowtype" , 
  "run" , 
  "savepoint" , 
  "schema" , 
  "scn" , 
  "section" , 
  "segment" , 
  "select" , 
  "separate" , 
  "sequence" , 
  "session" , 
  "set" , 
  "share" , 
  "shared" , 
  "size" , 
  "smallint" , 
  "snapshot" , 
  "some" , 
  "sort" , 
  "space" , 
  "sql" , 
  "sqlbuf" , 
  "sqlcode" , 
  "sqlerrm" , 
  "sqlerror" , 
  "sqlstate" , 
  "start" , 
  "statement" , 
  "statement_id" , 
  "statistics" , 
  "stddev" , 
  "stop" , 
  "storage" , 
  "subtype" , 
  "successful" , 
  "sum" , 
  "switch" , 
  "synonym" , 
  "sysdate" , 
  "system" , 
  "tabauth" , 
  "table" , 
  "tables" , 
  "tablespace" , 
  "task" , 
  "temporary" , 
  "terminate" , 
  "then" , 
  "thread" , 
  "time" , 
  "to" , 
  "tracing" , 
  "transaction" , 
  "trigger" , 
  "triggers" , 
  "true" , 
  "truncate" , 
  "type" , 
  "uid" , 
  "under" , 
  "union" , 
  "unique" , 
  "unlimited" , 
  "until" , 
  "update" , 
  "use" , 
  "user" , 
  "using" , 
  "validate" , 
  "values" , 
  "varchar" , 
  "varchar2" , 
  "variance" , 
  "view" , 
  "views" , 
  "when" , 
  "whenever" , 
  "where" , 
  "while" , 
  "with" , 
  "work" , 
  "write" , 
  "xor")  
}

class SuperConnection(val connection: Connection,val releaseFunc: () => Any) {
  lazy val brokenLimit_? = driverType.brokenLimit_?
  lazy val brokenAutogeneratedKeys_? = driverType.brokenAutogeneratedKeys_?
  lazy val wickedBrokenAutogeneratedKeys_? = driverType.wickedBrokenAutogeneratedKeys_?
  lazy val returningIntoClause_? = driverType.returningIntoClause_?

  def createTablePostpend: String = driverType.createTablePostpend
  def supportsForeignKeys_? : Boolean = driverType.supportsForeignKeys_?
  
  lazy val driverType = (calcDriver(connection.getMetaData.getDatabaseProductName))
  
  def calcDriver(name: String): DriverType = {
    name match {
      case DerbyDriver.name => DerbyDriver
      case MySqlDriver.name => MySqlDriver
      case PostgreSqlDriver.name => PostgreSqlDriver
      case H2Driver.name => H2Driver
      case OracleDriver.name => OracleDriver
    }
  }
}

object SuperConnection {
  implicit def superToConn(in: SuperConnection): Connection = in.connection
}

trait ConnectionIdentifier {
  def jndiName: String
}

case object DefaultConnectionIdentifier extends ConnectionIdentifier {
  var jndiName = "lift"
}
