import play.api.{GlobalSettings, Application, Logger}

import models._

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    Logger.logger.info("Application Play for V/JPA started.")
    
    VJPAClasses.open("jpatest1@towel-ubvm|jpatest2@towel-ubvm|jpatest3@towel-ubvm")
  }
  
  override def onStop(app: Application) {
    Logger.logger.info("Application Play for V/JPA stopped.")
    
    VJPAClasses.close()
  }
}
