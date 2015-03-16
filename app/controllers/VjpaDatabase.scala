package controllers

import play.api.mvc.{Controller, Action, Flash}
import play.api.data.Form
import play.api.data.Forms
import play.api.data.Forms._
import play.api.i18n.Messages

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

    println(s"connect to $connectionURL ...")
    val connected = VjpaDAO.open(connectionURL)
    println(connected)
    
    connected match {
      case Success(s) => Redirect(routes.VjpaDatabase.connected).flashing("success" -> s)
      case Failure(e) => Redirect(routes.Application.index()).flashing("failure" -> e.getMessage)
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
