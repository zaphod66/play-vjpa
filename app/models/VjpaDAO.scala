package models

import play.api.Logger

import scala.util.{Try, Success, Failure}
import scala.language.postfixOps

import com.versant.jpa._

import javax.persistence._

case class VjpaDAO(emf: EntityManagerFactory, em: EntityManager, url: String)

object VjpaDAO {
  import scala.collection.JavaConverters._
  
  def open(connectionURL: String): Try[Long] = {
    val GEN_PERSISTENCE_XML = 
          """<persistence version="2.0">
                 <persistence-unit name="genericUnit" transaction-type="RESOURCE_LOCAL">
                   <properties>
                     <property name="versant.connectionURL" value="""" + connectionURL + """"/>
                     <property name="versant.genericAccess" value="true" />
                   </properties>
                 </persistence-unit>
             </persistence>"""

    
    val props = Map("versant.persistence.xml" -> GEN_PERSISTENCE_XML)

    try {
      val emf = Some(Persistence.createEntityManagerFactory("genericUnit", props.asJava))
      val em  = emf map { e => e.createEntityManager }
      val url = Some(connectionURL)
      
      val dao = VjpaDAO(emf.get, em.get, url.get)
      
      val sessionId = Global.addSession(dao)
      Logger.logger.info(s"Opened session $sessionId.")
      
      Success(sessionId)
    } catch {
      case e: Exception => Failure(e)
    }
  }
  
  def close(id: Long): Boolean = {
    Logger.logger.info(s"closing session $id")
    
    val dao = Global.getSession(id)
    
    dao foreach { d => d.em.close(); d.emf.close() }
    
    Global.removeSession(id)
    
    true
  }
  
  import com.versant.jpa.generic._
  
  def allClassNames(id: Long) = {
    val dao = Global.getSession(id)
    
    val classes = dao map { d => DatabaseClass.getAllClasses(d.emf).toSeq }
    val names   = classes map { s => s map { c => c.getName } }
    val snames  = names map { _.sorted }
    
    snames
  }
  
  def allDBNames(id: Long) = {
    val dao = Global.getSession(id)
    
    dao map { d =>
      val vemf = d.emf.asInstanceOf[VersantEntityManagerFactory]
      val dbs  = vemf.getAllDatabases
      val dbNames = dbs map { _.getName }
      
      dbNames.toSeq
    }
  }
  
  def getClass(id: Long, clsName: String) = {
    val dao = Global.getSession(id)
    
    dao map { d => DatabaseClass.forName(clsName, d.emf) }
  }
  
  def fields(clazz: Option[DatabaseClass]): Option[Seq[DatabaseField]] = {
    def helper(clazz: DatabaseClass): Seq[DatabaseField] = {
      if (clazz == null)
        Seq[DatabaseField]()
      else
        clazz.getDeclaredFields ++ helper(clazz.getSuperclass)
    }
    
    clazz map { c => helper(c) }
  }

  def getFields(clazz: DatabaseClass): Option[Seq[DatabaseField]] = {
    def helper(clazz: DatabaseClass): Seq[DatabaseField] = {
      if (clazz == null)
        Seq[DatabaseField]()
      else
        clazz.getDeclaredFields ++ helper(clazz.getSuperclass)
    }
    
    if (clazz == null) None
    else Some(helper(clazz))
  }
  
  def getAllInstances(id: Long, clsName: String): Seq[Long] = {
    val dao = Global.getSession(id)
    
    val dbClass = getClass(id, clsName)
    
    var query: Option[Query] = None
    
    try {
      query  = dao map { d => d.em.createQuery(s"SELECT x FROM $clsName x") }
    } catch {
      case e: IllegalArgumentException => {
        val simpleName = clsName
        
        query = dao map { d => d.em.createQuery(s"SELECT x FROM $simpleName x") }
      }
    }
    
    val result = query  map { _.getResultList }
    val loids  = result map { LoidUtil.getLoids(_) }
    val buffer = loids  map { _.asScala }
    val seqany = buffer map { _.toSeq }
    val slong  = seqany map { sa => sa map { _.asInstanceOf[Long] } }

    slong.getOrElse(Seq[Long]())
  } 
  
  def getInstance(id: Long, loid: Long): Option[DatabaseObject] = {
    val dao = Global.getSession(id)
    val vem = dao map { d => d.em.asInstanceOf[VersantEntityManager] }
    val lst = vem map { _.find(List(loid).asJava) }
    val obj = lst map { l => if (l.size >= 1) l.get(0) }
    val dbo = obj map { _.asInstanceOf[DatabaseObject] }
    val res = dbo flatMap { o => if (o == null) None else Some(o) }
    
    res
  }
  
  def excuteQuery(id: Long, jpql: String): Try[Seq[Long]] = {
    val dao = Global.getSession(id)
    
    var query: Option[Query] = None
    
    try {
      query = dao map { d => d.em.createQuery(jpql) }
    
      val oloids = for {
        q <- query
        result = q.getResultList
        loids  = LoidUtil.getLoids(result)
        buffer = loids.asScala.toSeq
        seqlong = buffer map { _.asInstanceOf[Long] }
      } yield seqlong
    
      Success(oloids.getOrElse(Seq[Long]()))
    } catch {
      case e: Exception => {
        Logger.logger.warn(s"Error executing query: $jpql. With ${e.getMessage}")
        Failure(e)
      }
    }
  }
  
  def fieldNamesforClass(id: Long, clsName: String) = {
    val clazz    = getClass(id, clsName)
    
    for {
      flds <- fields(clazz)
      names = flds map { _.getName }
    } yield names
  }
  
  def addLoids(id: Long, loids: Seq[Long]) = {
    Logger.logger.info(s"Caching loids (${loids.size}) for session $id")
    Global.addLoids(id, loids)
    loids
  }
  
  def getLoids(id: Long) = {
    Logger.logger.info(s"Getting loids for session $id")
    Global.getLoids(id)
  }
  
  def removeLoids(id: Long) = {
    Logger.logger.info(s"Removing loids for session $id")
    Global.removeLoids(id)
  }
}
