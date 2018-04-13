package com.sagebear.bigrussianboss.ScriptTest.bot

import com.sagebear.Bio
import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.ScriptTest.bot.SensorsAndActuators.CanNotDoThis

import scala.concurrent.{ExecutionContext, Future}
import scala.util.matching.Regex
import scala.util.{Random, Try}

class ObedientBot(private val phrases: List[String]) extends SensorsAndActuators {
  override def observe(text: String)(a: Script.Action)(implicit ec: ExecutionContext): Future[SensorsAndActuators] = {
    Future(new ObedientBot(phrases.tail))
  }

  override def act(action: Script.Action)(implicit ec: ExecutionContext, rnd: Random): Future[String] = {
    this.phrases match {
      case head :: _ =>
        Future(head)
      case _ =>
        Future.failed(CanNotDoThis)
    }
  }

  override def actWithBio(action: Script.Action, tokenizer: Regex)
                         (implicit ec: ExecutionContext, rnd: Random): Future[(String, Bio)] =
    act(action).map((text) => (text, Bio(text, tokenizer)))
}

object ObedientBot {
  def client(phrases: String*): Try[ObedientBot] = Try(new ObedientBot(phrases.toList))
  def operator(phrases: String*): Try[ObedientBot] = Try(new ObedientBot(phrases.toList))
}