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
      Logger.logger.info(s"opened Session $sessionId")
      
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
    
    val classes = dao map { d => DatabaseClass.getAllClasses(d.emf) }
    val names   = classes map { arr => arr map { c => c.getName } }
    val snames  = names map { _.sorted }
    
    snames
  }
  
  def allDBNames(id: Long) = {
    val dao = Global.getSession(id)
    
    dao match {
      case Some(d) => {
        val vemf = d.emf.asInstanceOf[VersantEntityManagerFactory]
        val dbs  = vemf.getAllDatabases
        val dbNames = dbs.map { db => db.getName }
        
        dbNames.toSeq
      }
      case None => Seq[String]()
    }
  }
  
  def getClass(id: Long, clsName: String): Option[DatabaseClass] = {
    val dao = Global.getSession(id)
    
    val clazz = dao map { d => DatabaseClass.forName(clsName, d.emf) }
    
    clazz
  }
  
  def fields(clazz: Option[DatabaseClass]): Seq[DatabaseField] = {
    clazz match {
      case Some(c) => {
        if (c == null)
          Seq[DatabaseField]()
        else
          c.getDeclaredFields ++ fields(Some(c.getSuperclass))
      }
      case None    => Seq[DatabaseField]()
    }
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
    
    dbo
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
    val flds     = fields(clazz)
    val fldNames = flds map { fld => fld.getName }
    
    fldNames
  }
}
