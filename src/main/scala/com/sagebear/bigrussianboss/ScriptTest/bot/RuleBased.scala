package com.sagebear.bigrussianboss.ScriptTest.bot

import com.sagebear.Bio
import com.sagebear.Extensions._
import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.ScriptTest.bot.SensorsAndActuators.{CanNotDoThis, DoNotUnderstand}
import com.sagebear.bigrussianboss.intent.Intents._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.matching.Regex
import scala.util.{Random, Try}

/**
  * @author vadim
  * @since 01.02.2018
  */
trait RuleBased extends SensorsAndActuators {
  protected def context: Map[String, String]

  protected def reflex(action: Script.Action): Seq[String]

  protected def instance(context: Map[String, String]): RuleBased

  override def observe(text: String)(action: Script.Action)(implicit ec: ExecutionContext): Future[RuleBased] = {
    Future(instance(
      (action match {
        case And(action1, action2) =>
          for {
            first <- reflex(action1)
            second <- reflex(action2)
          } yield (first + " и " + second).parse(text)
        case _ =>
          reflex(action).map(_.parse(text))
      }).collectFirst {
        case Some(args) => args
      }.getOrElse(throw DoNotUnderstand)))
  }

  private def act_[T](action: Script.Action,
                      substitute: (String, Map[String, String]) => T, addition: (T, T) => T)
                     (implicit ec: ExecutionContext, rnd: Random): Future[T] = action match {
    case And(action1, action2) =>
      for {
        p1 <- act_(action1, substitute, addition)
        p2 <- act_(action2, substitute, addition)
      } yield addition(p1, p2)

    case _ =>
      Future({
        val possibleAlternatives = reflex(action).map((alternative) =>
          Try(Some(substitute(alternative, context))).getOrElse(None)).collect {
          case Some(text) => text
        }
        possibleAlternatives.length match {
          case len if len > 0 =>
            possibleAlternatives(rnd.nextInt(len))
          case _ => throw CanNotDoThis
        }
      })
  }

  override def act(action: Script.Action)
                  (implicit ec: ExecutionContext, rnd: Random): Future[String] =
    act_(action,
      (alternative: String, context: Map[String, String]) => alternative.substitute(context),
      (p1: String, p2: String) => p1 + " и " + p2)

  override def actWithBio(action: Script.Action, tokenizer: Regex)
                         (implicit ec: ExecutionContext, rnd: Random): Future[(String, Bio)] =
    act_(action,
      (alternative: String, context: Map[String, String]) => alternative.substituteWithBio(context, tokenizer),
      (p1: (String, Bio), p2: (String, Bio)) => (p1._1 + " и " + p2._1, p1._2 ::: Bio(" и ", tokenizer) ::: p2._2))
}