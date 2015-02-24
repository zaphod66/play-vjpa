package controllers

import play.api.mvc.{Controller, Action}
import play.api.data.Form
import play.api.data.Forms
import play.api.data.Forms._

import models.{ ConnectionURL, VjpaDAO }

object VjpaDatabase extends Controller {
  def requestURL = Action { implicit request =>
    val form = connectionForm.fillAndValidate(ConnectionURL("jpatest1@towel-ubvm"))

    Ok(views.html.openConnection(form))
  }
  
  def open = Action { implicit request => 
    val urlForm = connectionForm.bindFromRequest()
    
    var connectionURL = ""
    
    urlForm.fold(
        hasErrors = { form => Redirect(routes.VjpaDatabase.requestURL) },
        success   = { c => connectionURL = c.url }
    )

    val connected = VjpaDAO.open(connectionURL)
    if (connected) {
      println("connected to:" + connectionURL)
      Redirect(routes.Classes.listNames())
    } else {
      Redirect(routes.Application.index())
    }
  }
  
  def close = Action { implicit request =>
    VjpaDAO.close()
    
    Redirect(routes.Application.index())
  }
  
  val connectionMapping = mapping(
    "url" -> nonEmptyText
  )(ConnectionURL.apply)(ConnectionURL.unapply)
  
  val connectionForm = Form(connectionMapping)
}
