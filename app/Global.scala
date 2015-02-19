import play.api.{GlobalSettings, Application, Logger}

import models._

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    Logger.logger.info("Application Play for V/JPA started.")
    
    VJPAClasses.open()
  }
  
  override def onStop(app: Application) {
    Logger.logger.info("Application Play for V/JPA stopped.")
    
    VJPAClasses.close()
  }
}
