package controllers

import play.api.mvc.{Controller, Action, Flash}
import play.api.data.{Form, Forms}
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger

import scala.util.{Try, Success, Failure}

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
        hasErrors = { form => Redirect(routes.VjpaDatabase.requestURL).flashing(Flash(urlForm.data) + ("error" -> Messages("validation.errors"))) },
        success   = { c => connectionURL = c.url }
    )

    Logger.logger.info(s"Connecting to $connectionURL ...")
    val connected = VjpaDAO.open(connectionURL)
    Logger.logger.info(connected.toString)
    
    connected match {
      case Success(s) => Redirect(routes.VjpaDatabase.connected).flashing("success" -> s)
      case Failure(e) => Redirect(routes.VjpaDatabase.requestURL).flashing("error" -> e.getMessage)
    }
  }
  
  def close = Action { implicit request =>
    VjpaDAO.close()
    
    Redirect(routes.Application.index())
  }
  
  def connected = Action { implicit request =>
    val dbsNames = VjpaDAO.allDBNames
    
    Ok(views.html.openDatabase(dbsNames))
  }
  
  val connectionMapping = mapping(
    "url" -> nonEmptyText
  )(ConnectionURL.apply)(ConnectionURL.unapply)
  
  val connectionForm = Form(connectionMapping)
}
