package com.sagebear.bigrussianboss.ScriptTest.bot

import com.sagebear.Bio
import com.sagebear.bigrussianboss.Script.Action

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random
import scala.util.control.NoStackTrace
import scala.util.matching.Regex

/**
  * @author vadim
  * @since 30.01.2018
  */
trait SensorsAndActuators {
  def act(action: Action)(implicit ec: ExecutionContext, rnd: Random): Future[String]

  def actWithBio(action: Action, tokenizer: Regex = Bio.defaultTokenizer)
                (implicit ec: ExecutionContext, rnd: Random): Future[(String, Bio)]
  def observe(text: String)(a: Action)(implicit ec: ExecutionContext): Future[SensorsAndActuators]
}

object SensorsAndActuators {
  case object CanNotDoThis extends RuntimeException with NoStackTrace
  case object DoNotUnderstand extends RuntimeException with NoStackTrace
}