package models

import play.api.{GlobalSettings, Application, Logger}
import play.api.mvc.RequestHeader
import play.api.mvc.Results._

import scala.collection.mutable.Map
import scala.concurrent.Future

object Global extends GlobalSettings {

  private val sessionMap = Map[Long,VjpaDAO]()
  private val loidsCache = Map[Long,Seq[Long]]()
  
  override def onStart(app: Application) {
    Logger.logger.info("Application Play for V/JPA started.")
  }

  override def onStop(app: Application) {
    Logger.logger.info("Application Play for V/JPA stopped.")

    sessionMap foreach { s => VjpaDAO.close(s._1) }
  }
  
//  override def onError(request: RequestHeader, ex: Throwable) = {
//    Future.successful(InternalServerError(request.path + " - " + ex.getMessage))
//  }
  
  def addSession(dao: VjpaDAO): Long = {
    val id = IdGen.next
    sessionMap.put(id, dao)
    
    id
  }
  
  def removeSession(id: Long): Option[VjpaDAO] = {
    removeLoids(id)
    sessionMap.remove(id)
  }
  
  def getSession(id: Long): Option[VjpaDAO] = {
    sessionMap.get(id)
  }
  
  def sessionInUse(id: Long): Boolean = {
    sessionMap.contains(id)
  }
  
  def addLoids(id: Long, loids: Seq[Long]) = {
    loidsCache.put(id, loids)
  }
  
  def getLoids(id: Long) = {
    loidsCache.get(id)
  }
  
  def removeLoids(id: Long) = {
    loidsCache.remove(id)
  }
}

object IdGen {
  var current = 0L

  def next() = {
    current = current + 1
    current
  }
}
