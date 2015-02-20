package controllers

import play.api.mvc.{Action, Controller, Flash}
import play.api.i18n.Messages
import models.VjpaDAO

object Classes extends Controller {
  def listNames = Action { implicit request =>
    val clsNames = VjpaDAO.allClassNames
    val dbsNames = VjpaDAO.allDBNames
    
    clsNames match {
      case Some(names) => Ok(views.html.classes.classnames(dbsNames,names))
      case None        => Ok("No Classes found")
    }
  }
  
  def showClass(clsName: String) = Action { implicit request =>
    val fldNames = VjpaDAO.fieldNamesforClass(clsName)
    
    Ok(views.html.classes.classfields(clsName,fldNames))
  }
}
