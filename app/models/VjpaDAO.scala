package models

import com.versant.jpa._

import javax.persistence._

object VjpaDAO {
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
      case None => Seq[String]()
    }
  }
  
  def fields(clazz: DatabaseClass): Seq[DatabaseField] = {
    if (clazz == null)
      List[DatabaseField]()
    else
      clazz.getDeclaredFields ++ fields(clazz.getSuperclass)
  }
  
  def fieldNamesforClass(clsName: String) = {
    val simpleClsName = clsName split('.') lastOption
    val clazz = emf map { e => DatabaseClass.forName(simpleClsName.get, e) }
    
    clazz match {
      case Some(c) => if (c != null) println("=>" + c.getName) else println("=> c is null")
      case None    => println("NoClass found")
    }
    val flds   = clazz map { c => fields(c) }
    val fldNames = flds map { flds => flds map { fld => fld.getName }}
    
    val tmp1 = flds.get
    val tmp2 = tmp1 headOption
    val tmp3 = tmp2 map { f => f.getName }
    val tmp4 = tmp2 map { f => f.getTypeName }
    
    tmp3 match {
      case Some(n) => println(s"Type of field $n is $tmp4")
      case None    => println("f. getName failed")
    }
    
    fldNames match {
      case Some(fn) => fn.toSeq
      case None     => Seq[String]()
    }
  }
}