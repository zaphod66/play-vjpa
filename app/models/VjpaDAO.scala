package models

import scala.language.postfixOps
import com.versant.jpa._

import javax.persistence._

object VjpaDAO {
  import scala.collection.JavaConverters._
  
  var emf: Option[EntityManagerFactory] = None
  var em:  Option[EntityManager]        = None
  var url: Option[String]               = None

  def open(connectionURL: String): Boolean = {
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

    close

    emf = Some(Persistence.createEntityManagerFactory("genericUnit", props.asJava))
    em  = emf map { e => e.createEntityManager }
    url = Some(connectionURL)
    
    val names = allClassNames

    emf match {
      case Some(_) => true
      case None    => false
    }
  }
  
  def close(): Boolean = {
    em map { e => e.close }
    emf map { e => { e.close; println("closing...") } }

    em  = None
    emf = None
    url = None
    
    true
  }
  
  import com.versant.jpa.generic._
  
  def allClassNames = {
    val classes = emf map { e => DatabaseClass.getAllClasses(e) }
    val names   = classes map { arr => arr map { c => c.getFullyQualifiedName } }
    
    names
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
  
  def getSimpleName(fullyQualifiedName: String): String = {
    val sname = (fullyQualifiedName split('.')).lastOption
    sname.getOrElse("")
  }
  
  def getClass(clsName: String): Option[DatabaseClass] = {
    val clazz = emf map { e => DatabaseClass.forName(clsName, e) }
    
    clazz match {
      case Some(c) => {
        if (c == null) {
          val simpleName = getSimpleName(clsName)
          emf map { e => DatabaseClass.forName(simpleName, e) }
        } else {
          clazz
        }
      }
      case None => None
    }
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
        val simpleName = getSimpleName(clsName)
        
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
    
    val tmp1 = flds
    val tmp2 = tmp1 headOption
    val tmp3 = tmp2 map { f => f.getName }
    val tmp4 = tmp2 map { f => f.getTypeName }
    
    tmp3 match {
      case Some(n) => println(s"Type of field $n is $tmp4")
      case None    => println("f. getName failed")
    }
    
    fldNames
  }
  
  def isInUse: Boolean = {
    emf.isDefined
  }
}
