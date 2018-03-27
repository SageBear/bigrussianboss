package com.sagebear.bigrussianboss.bot

import com.sagebear.Phrase
import com.sagebear.bigrussianboss.Script.Action
import com.typesafe.config.Config

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random
import scala.util.control.NoStackTrace
/**
  * @author vadim
  * @since 30.01.2018
  */
trait SensorsAndActuators {
  def act(a: Action)(implicit ec: ExecutionContext, rnd: Random): Future[Phrase]
  def observe(text: String)(a: Action)(implicit ec: ExecutionContext): Future[SensorsAndActuators]
}

object SensorsAndActuators {
  case object CanNotDoThis extends RuntimeException with NoStackTrace
  case object DoNotUnderstand extends RuntimeException with NoStackTrace
}