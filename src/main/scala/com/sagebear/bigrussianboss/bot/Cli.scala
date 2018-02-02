package com.sagebear.bigrussianboss.bot
import com.sagebear.bigrussianboss.Script

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn

class Cli extends SensorsAndActuators {
  override def observe(text: String)(a: Script.Action)(implicit ec: ExecutionContext): Future[SensorsAndActuators] = Future {
    System.out.println(text)
    this
  }

  override def act(a: Script.Action)(implicit ec: ExecutionContext): Future[String] = Future(StdIn.readLine()).flatMap { txt =>
    if (txt == "(_+_)") Future.failed(SensorsAndActuators.CanNotDoThis)
    else Future(txt)
  }
}

