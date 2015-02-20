package controllers

import play.api.mvc.{Action, Controller, Flash}
import play.api.i18n.Messages
import models.VJPAClasses

object Classes extends Controller {
  def listNames = Action { implicit request =>
    val clsNames = VJPAClasses.allClassNames
    val dbsNames = VJPAClasses.allDBNames
    
    clsNames match {
      case Some(names) => Ok(views.html.classes.classnames(dbsNames,names))
      case None        => Ok("No Classes found")
    }
  }
  
  def showClass(name: String) = Action { request =>
    Ok("showClass: " + name)
  }
}
