package util

trait Logging {
  protected[this] val log = new Log( getClass.getName )
}

import org.apache.log4j.Logger
import org.apache.log4j.Level._

class Log( name: String ) {
  private[this] val jLogger = Logger.getLogger( name )
  
  def trace( msg: => Any ) = if ( jLogger.isTraceEnabled ) jLogger trace msg
  def debug( msg: => Any ) = if ( jLogger.isDebugEnabled ) jLogger debug msg
  def info( msg: => Any ) = if ( jLogger.isInfoEnabled ) jLogger info msg

  def warn( msg: => Any ): Unit = warn( msg, null )
  def warn( msg: => Any, e: Exception ) = if ( jLogger isEnabledFor WARN ) jLogger.warn( msg, e )
  
  def error( msg: => Any ): Unit = error( msg, null )
  def error( msg: => Any, e: Exception ) = if ( jLogger isEnabledFor ERROR ) jLogger.error( msg, e )
}
