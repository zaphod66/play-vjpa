import play.api.{GlobalSettings, Application, Logger}

import models.VjpaDAO

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    Logger.logger.info("Application Play for V/JPA started.")
  }
  
  override def onStop(app: Application) {
    Logger.logger.info("Application Play for V/JPA stopped.")
    
    VjpaDAO.close()
  }
}

object IdGen {
  var current = 0L

  def next() = {
    current = current + 1
    current
  }
}
