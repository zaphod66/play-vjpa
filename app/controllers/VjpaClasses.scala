package controllers

import play.api.mvc.{Action, Controller, Flash}
import play.api.i18n.Messages
import models.VjpaDAO

object Classes extends Controller {
  def listNames(clsNames: Option[Array[String]]) = Action { implicit request =>
    val dbsNames = VjpaDAO.allDBNames
    
    clsNames match {
      case Some(names) => Ok(views.html.classes.classnames(dbsNames,names))
      case None        => Ok("No Classes found")
    }
  }
  
  def showClass(clsName: String) = Action { implicit request =>
    val clazz = VjpaDAO.getClass(clsName)
    val flds = VjpaDAO.fields(clazz)
    
    Ok(views.html.classes.classdetails(clazz.get, flds))
  }
  
  def listAllNames = {
    val clsNames = VjpaDAO.allClassNames
    
    listNames(clsNames)
  }
  
  def allInstances(clsName: String) = Action { implicit request =>
    val instances = VjpaDAO.getAllInstances(clsName)

    Ok(views.html.classes.classinstances(clsName, instances))
  }
  
  def showInstance(loid: Long) = Action { implicit request =>
    val obj = VjpaDAO.getInstance(loid)
    
    obj match {
      case Some(o) => {
        val flds = VjpaDAO.fields(Some(o.getType))
        Ok(views.html.classes.classinstance(o, flds))
      }
      case None    => NotFound
    }
  }
}
