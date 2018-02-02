package com.sagebear.bigrussianboss.bot

import com.sagebear.bigrussianboss.Script.Action

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NoStackTrace
/**
  * @author vadim
  * @since 30.01.2018
  */
trait SensorsAndActuators {
  def observe(text: String)(a: Action)(implicit ec: ExecutionContext): Future[SensorsAndActuators]
  def act(a: Action)(implicit ec: ExecutionContext): Future[String]
}

object SensorsAndActuators {
  case object CanNotDoThis extends RuntimeException with NoStackTrace
  case object DoNotUnderstand extends RuntimeException with NoStackTrace


}