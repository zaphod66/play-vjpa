package models

import play.api.Logger

import scala.util.{Try, Success, Failure}
import scala.language.postfixOps

import com.versant.jpa._

import javax.persistence._

case class VjpaDAO(emf: EntityManagerFactory, em: EntityManager, url: String)

object VjpaDAO {
  import scala.collection.JavaConverters._
  
  var emf: Option[EntityManagerFactory] = None
  var em:  Option[EntityManager]        = None
  var url: Option[String]               = None

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
      emf = Some(Persistence.createEntityManagerFactory("genericUnit", props.asJava))
      em  = emf map { e => e.createEntityManager }
      url = Some(connectionURL)
      
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
    
    val dao = Global.closeSession(id)
    
    dao foreach { d => d.em.close(); d.emf.close() }
    
    true
  }
  
  import com.versant.jpa.generic._
  
  def allClassNames = {
    val classes = emf map { e => DatabaseClass.getAllClasses(e) }
    val names   = classes map { arr => arr map { c => c.getName } }
    val snames  = names map { _.sorted }
    
    snames
  }
  
  def allDBNames = {
    emf match {
      case Some(e) => {
        val vemf = e.asInstanceOf[VersantEntityManagerFactory]
        val dbs  = vemf.getAllDatabases
        val dbNames = dbs.map { db => db.getName }
        
        dbNames.toSeq
      }
      case None => Seq[String]()
    }
  }
  
  def getClass(clsName: String): Option[DatabaseClass] = {
    val clazz = emf map { e => DatabaseClass.forName(clsName, e) }
    
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

  def getAllInstances(clsName: String): Seq[Long] = {
    val dbClass = getClass(clsName)
    
    var query: Option[Query] = None
    
    try {
      query  = em map { _.createQuery(s"SELECT x FROM $clsName x") }
    } catch {
      case e: IllegalArgumentException => {
        val simpleName = clsName
        
        query = em map { _.createQuery(s"SELECT x FROM $simpleName x") }
      }
    }
    
    val result = query  map { _.getResultList }
    val loids  = result map { LoidUtil.getLoids(_) }
    val buffer = loids  map { _.asScala }
    val seqany = buffer map { _.toSeq }
    val slong  = seqany map { sa => sa map { _.asInstanceOf[Long] } }

    slong.getOrElse(Seq[Long]())
  } 
  
  def getInstance(loid: Long): Option[DatabaseObject] = {
    val vem = em  map { _.asInstanceOf[VersantEntityManager] }
    val lst = vem map { _.find(List(loid).asJava) }
    val obj = lst map { l => if (l.size >= 1) l.get(0) }
    val dbo = obj map { _.asInstanceOf[DatabaseObject] }
    
    dbo
  }
  
  def fieldNamesforClass(clsName: String) = {
    val clazz    = getClass(clsName)
    val flds     = fields(clazz)
    val fldNames = flds map { fld => fld.getName }
    
    fldNames
  }
}
