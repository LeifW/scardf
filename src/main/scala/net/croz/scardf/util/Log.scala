package net.croz.scardf.util

trait Logging {
  protected[this] val log = new Log( getClass.getName )
}

import org.apache.commons.logging.LogFactory

class Log( name: String ) {
  private[this] val jLog = LogFactory.getLog( name )
  
  def trace( msg: => Any ) = if ( jLog.isTraceEnabled ) jLog trace msg
  
  def debug( msg: => Any ) = if ( jLog.isDebugEnabled ) jLog debug msg
  
  def info( msg: => Any ) = if ( jLog.isInfoEnabled ) jLog info msg

  def warn( msg: => Any ): Unit = warn( msg, null )
  def warn( msg: => Any, t: Throwable ) = if ( jLog.isWarnEnabled ) jLog.warn( msg, t )
  
  def error( msg: => Any ): Unit = error( msg, null )
  def error( msg: => Any, t: Throwable ) = if ( jLog.isErrorEnabled ) jLog.error( msg, t )
}
