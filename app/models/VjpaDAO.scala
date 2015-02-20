package models

import com.versant.jpa._

import javax.persistence._

object VJPAClasses {
  import scala.collection.JavaConverters._
  
  var emf: Option[EntityManagerFactory] = None
  var em:  Option[EntityManager]        = None
    
  def open(connectionURL: String): Boolean = {
    val GEN_PERSISTENCE_XML = 
          """<persistence version="2.0">
                 <persistence-unit name="genericUnit" transaction-type="RESOURCE_LOCAL">
                   <properties>
                     <property name="versant.connectionURL" value="jpatest1@towel-ubvm|jpatest2@towel-ubvm|jpatest3@towel-ubvm"/>
                     <property name="versant.genericAccess" value="true" />
                   </properties>
                 </persistence-unit>
             </persistence>"""

    
    val props = Map("versant.persistence.xml" -> GEN_PERSISTENCE_XML)
    
    emf = Some(Persistence.createEntityManagerFactory("genericUnit", props.asJava))
    em  = emf map { e => e.createEntityManager }
    
    val names = allClassNames

//  names map { arr => arr foreach { n => println(n) }}
    
    true
  }
  
  def close(): Boolean = {
    em map { e => e.close }
    emf map { e => e.close }

    em  = None
    emf = None
    
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
      case None    => Seq[String]()
    }
  }
}