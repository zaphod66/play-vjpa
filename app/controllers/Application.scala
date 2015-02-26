package controllers

import play.api.mvc.{Action, Controller}
import models.VjpaDAO

object Application extends Controller {

  def index = Action {
    if (VjpaDAO.isInUse)
      Redirect(routes.VjpaDatabase.connected)
    else
      Redirect(routes.VjpaDatabase.requestURL)
  }

}
