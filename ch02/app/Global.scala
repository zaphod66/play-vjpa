import play.api.{GlobalSettings, Application, Logger}

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    Logger.logger.info("Application ch02 started.")
  }
  
  override def onStop(app: Application) {
    Logger.logger.info("Application ch02 stopped.")
  }
}
