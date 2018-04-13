package com.sagebear.bigrussianboss.ScriptTest.bot

import com.sagebear.Bio
import com.sagebear.bigrussianboss.Script

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn
import scala.util.Random
import scala.util.matching.Regex

class Cli extends SensorsAndActuators {
  override def observe(text: String)(a: Script.Action)
                      (implicit ec: ExecutionContext): Future[SensorsAndActuators] = Future {
    System.out.println(text)
    this
  }

  override def act(action: Script.Action)
                  (implicit ec: ExecutionContext, rnd: Random): Future[String] = Future(StdIn.readLine()).flatMap {
    txt => if (txt == "(_+_)") Future.failed(SensorsAndActuators.CanNotDoThis) else Future(txt)
  }

  override def actWithBio(action: Script.Action, tokenizer: Regex)
                         (implicit ec: ExecutionContext, rnd: Random): Future[(String, Bio)] =
    act(action).map((text) => (text, Bio(text, tokenizer)))
}

