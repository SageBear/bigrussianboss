package com.sagebear.bigrussianboss.bot
import com.sagebear.Bio
import com.sagebear.Extensions._
import com.sagebear.{Interpolation, Phrase}
import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.bot.SensorsAndActuators.{CanNotDoThis, DoNotUnderstand}
import com.sagebear.bigrussianboss.intent.Intents._
import com.typesafe.config.Config

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

/**
  * @author vadim
  * @since 01.02.2018
  */
abstract class RuleBased(val config: Config) extends SensorsAndActuators {
  protected def context: Map[String, String]

  protected def reflex(action: Script.Action): Seq[Interpolation]
  protected def instance(context: Map[String, String]): RuleBased

  private def collect(text: String)(a: Script.Action)(implicit ec: ExecutionContext): Future[Map[String, String]] = {
    a match {
      case And(q1, q2) =>
        for {
          b1 <- collect(text)(q1)
          b2 <- collect(text)(q2)
        } yield b1 ++ b2

      case _ =>
        Future({
          reflex(a).map(_.get(text)).collectFirst {
            case Some(args) => args
          }.getOrElse(throw DoNotUnderstand)
        })
    }
  }

  override def observe(text: String)(a: Script.Action)(implicit ec: ExecutionContext): Future[RuleBased] = {
    for {
      args <- collect(text)(a)
    } yield instance(context ++ args)
  }

  override def act(a: Script.Action)(implicit ec: ExecutionContext, rnd: Random): Future[Phrase] = a match {
    case And(q1, q2) =>
      for {
        q1u <- act(q1)
        q2u <- act(q2)
      } yield Phrase(
        a,
        q1u.content + " и " + q2u.content,
        q1u.bio +: Bio(" и ", "O", single=false) +: q2u.bio
      )

    case _ =>
      Future({
        val alternatives = reflex(a).map(_.put(context)).collect {
          case Some(text) => text
        }
        alternatives.length match {
          case len if len > 0 =>
            val alternative = alternatives(rnd.nextInt(len))
            Phrase(
              a,
              alternative.content,
              alternative.bio()
            )
          case _ => throw CanNotDoThis
        }
      })
  }
}