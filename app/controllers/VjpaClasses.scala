package controllers

import play.api.mvc.{Action, Controller, Flash}
import play.api.i18n.Messages
import models.VjpaDAO

import com.versant.jpa.LoidUtil
import com.versant.jpa.spi.PersistenceCapable
import com.versant.jpa.core.tracked.TrackedCollection
import com.versant.jpa.core.tracked._
import com.versant.jpa.metadata.AttributeMetaData.Category._

import scala.collection.JavaConverters._

object Classes extends Controller {
  def listNames(clsNames: Option[Array[String]]) = Action { implicit request =>
    val dbsNames = Seq[String]() // VjpaDAO.allDBNames
    
    clsNames match {
      case Some(names) => Ok(views.html.classes.classnames(dbsNames,names))
      case None        => Ok("No Classes found")
    }
  }
  
  def showClass(clsName: String) = Action { implicit request =>
    val session = request.session.get("sessionId")
    
    val clazz = (session map { id => VjpaDAO.getClass(id.toLong, clsName) }).getOrElse(None)
    val flds = VjpaDAO.fields(clazz)
    
    clazz match {
      case Some(cls) => Ok(views.html.classes.classdetails(cls, flds))
      case None      => Ok("No Class found")
    }
    
  }
  
  def listAllNames = Action { implicit request =>
    val session = request.session.get("sessionId")
    
    val clsNames = (session map { id => VjpaDAO.allClassNames(id.toLong) }).getOrElse(None)
    val dbsNames = (session map { id => VjpaDAO.allDBNames(id.toLong) }).getOrElse(Seq[String]())
    
    clsNames match {
      case Some(names) => Ok(views.html.classes.classnames(dbsNames,names))
      case None        => Ok("No Classes found")
    }
  }
  
  def allInstances(clsName: String) = Action { implicit request =>
    val session = request.session.get("sessionId")
    
    val instances = session map { id => VjpaDAO.getAllInstances(id.toLong,clsName) }
    val sort      = instances map { _.sorted }

    Ok(views.html.classes.classinstances(clsName, sort.getOrElse(Seq[Long]())))
  }
  
  def showInstance(loid: Long) = Action { implicit request =>
    val session = request.session.get("sessionId")

    val obj = (session map { id => VjpaDAO.getInstance(id.toLong,loid) }).getOrElse(None)

    obj match {
      case Some(o) => {
        val flds = VjpaDAO.fields(Some(o.getType))  // fields
        val vals = flds map { fld => fld.get(o) }   // value of fields
        val ss   = vals map { value => val2String(value) }
        val fss  = flds zip ss

        Ok(views.html.classes.classinstance(o, fss))
      }
      case None    => NotFound
    }
  }

  def val2String(v: Object): String = {
    if (v == null) {
      "#Null"
    } else {
      if (v.isInstanceOf[PersistenceCapable]) {
        val pc = v.asInstanceOf[PersistenceCapable]

        "[ " + loid2Anchor(pc._vjpaGetId) + " ]"
      } else if (v.isInstanceOf[TrackedCollection]) {
        val tc = v.asInstanceOf[TrackedCollection]
        val cat = tc.getCategory

        cat match {
          case ARRAY_PRIMITIVES | COLLECTION_PRIMITIVES => v.toString
          case ARRAY_REFERENCES | COLLECTION_REFERENCES => {
            val al = tc.asInstanceOf[TrackedArrayList]
            val ls = al.getLoidValues
            val ss = ls map { l => loid2Anchor(l) }
            val s = ss.mkString("[", ", ", "]")

            "#" + al.size + " " + s
          }
          case MAP_PRIMITIVE_PRIMITIVE => {
            val mpp = tc.asInstanceOf[TrackedHashMap]
            val mks = mpp.getPrimitiveKeys.toList
            val mvs = mpp.getPrimitiveValues.toList
            val mkv = mks.zip(mvs)
            val mss = mkv map { case (k,v) => "(" + k.toString + " => " + v.toString + ")" }
            val s   = mss mkString("{", ", ", "}")

            "#" + mss.size + " " + s
          }
          case MAP_PRIMITIVE_REFERENCE => {
                val mpr = tc.asInstanceOf[TrackedHashMap]
                val mks = mpr.getPrimitiveKeys.toList
                val mvs = LoidUtil.getValuesLoids(mpr).asScala
                val mkv = mks.zip(mvs)
                val mss = mkv map { case (k,v) => "(" + k.toString + " => " + loid2Anchor(v) + ")" }
                val s   = mss mkString("{", ", ", "}")

                "#" + mss.size + " " + s
              }
          case MAP_REFERENCE_PRIMITIVE => {
            val mrp = tc.asInstanceOf[TrackedHashMap]
            val mks = LoidUtil.getKeysLoids(mrp).asScala
            val mvs = mrp.getPrimitiveValues.toList
            val mkv = mks.zip(mvs)
            val mss = mkv map { case (k,v) => "(" + loid2Anchor(k) + " => " + v.toString + ")" }
            val s   = mss mkString("{", ", ", "}")

            "#" + mss.size + " " + s
            }
          case MAP_REFERENCE_REFERENCE => {
            val mrr = tc.asInstanceOf[TrackedHashMap]
            val mks = LoidUtil.getKeysLoids(mrr).asScala
            val mvs = LoidUtil.getValuesLoids(mrr).asScala
            val mkv = mks.zip(mvs)
            val mss = mkv map { case (k,v) => "(" + loid2Anchor(k) + " => " + loid2Anchor(v) + ")" }
            val s   = mss mkString("{", ", ", "}")

            " #" + mss.size + " " + s
            }

          case _ => "??: " + v.toString
        }
      } else {
        v.toString
      }
    }
  }
  
  def loid2Anchor(loid: Long): String = {
    s"""<a href="${controllers.routes.Classes.showInstance(loid)}">${LoidUtil.convertLongToString(loid)}</a>"""
  }
}
