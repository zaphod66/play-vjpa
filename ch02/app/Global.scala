import play.api.{GlobalSettings, Application, Logger}

import models._

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    Logger.logger.info("Application ch02 started.")
    
    VJPAClasses.open()
  }
  
  override def onStop(app: Application) {
    Logger.logger.info("Application ch02 stopped.")
    
    VJPAClasses.close()
  }
}
