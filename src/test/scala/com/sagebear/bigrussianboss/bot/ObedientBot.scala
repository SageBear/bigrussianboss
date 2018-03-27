package com.sagebear.bigrussianboss.bot

import com.sagebear.{Bio, Phrase}
import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.bot.SensorsAndActuators.CanNotDoThis

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Random, Try}

class ObedientBot(private val phrases: List[String]) extends SensorsAndActuators {
  override def observe(text: String)(a: Script.Action)(implicit ec: ExecutionContext): Future[SensorsAndActuators] = {
    Future(new ObedientBot(phrases.tail))
  }

  override def act(a: Script.Action)(implicit ec: ExecutionContext, rnd: Random): Future[Phrase] = {
    this.phrases match {
      case head :: _ =>
        Future(Phrase(a, head, Bio(head, "O", single=false)))
      case _ =>
        Future.failed(CanNotDoThis)
    }
  }
}

object ObedientBot {
  def client(phrases: String*): Try[ObedientBot] = Try(new ObedientBot(phrases.toList))
  def operator(phrases: String*): Try[ObedientBot] = Try(new ObedientBot(phrases.toList))
}