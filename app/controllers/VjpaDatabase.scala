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
    
    connected match {
      case Success(i) => {
        val msg = s"Connected to $connectionURL"
        Logger.logger.info(msg + s" with sessionId $i.")
        Redirect(routes.VjpaDatabase.connected).flashing("success" -> msg).withSession(request.session + ("sessionId" -> i.toString))
      }
      case Failure(e) => {
        Logger.logger.info(s"connection to $connectionURL failed." + e.getMessage)
        Redirect(routes.VjpaDatabase.requestURL).flashing("error" -> e.getMessage)
      }
    }
  }
  
  def close = Action { implicit request =>
    val session = request.session.get("sessionId")
    
    val r = session map { s => VjpaDAO.close(s.toLong) }
    
    Redirect(routes.Application.index())
  }
  
  def connected = Action { implicit request =>
    val session = request.session.get("sessionId")
    
    val dbsNames = session map { s => VjpaDAO.allDBNames(s.toLong) }
    
    Ok(views.html.openDatabase(dbsNames.getOrElse(Seq[String]())))
  }
  
  val connectionMapping = mapping(
    "url" -> nonEmptyText
  )(ConnectionURL.apply)(ConnectionURL.unapply)
  
  val connectionForm = Form(connectionMapping)
}
