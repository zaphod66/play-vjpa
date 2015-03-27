package controllers

import play.api.mvc.{Action, Controller}
import models.VjpaDAO

object Application extends Controller {

  def index = Action { implicit request =>
    val session = request.session.get("sessionId")
    val inUse = session map { s => models.Global.sessionInUse(s.toLong) }
    val flag  = inUse.getOrElse(false)

    if (flag)
      Redirect(routes.VjpaDatabase.connected)
    else
      Redirect(routes.VjpaDatabase.requestURL)
  }
}
