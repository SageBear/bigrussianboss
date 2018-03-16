package com.sagebear.bigrussianboss.bot
import com.sagebear.Extensions._
import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.bot.SensorsAndActuators.{CanNotDoThis, DoNotUnderstand}
import com.sagebear.bigrussianboss.intent.Intents._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

/**
  * @author vadim
  * @since 01.02.2018
  */
trait RuleBased extends SensorsAndActuators {
  protected def context: Map[String, String]

  protected def reflex[T](action: Script.Action, subs: (Set[String], Seq[String]) => T): T

  protected def instance(context: Map[String, String]): RuleBased

  override def observe(text: String)(a: Script.Action)(implicit ec: ExecutionContext): Future[RuleBased] = {
    a match {
      case And(q1, q2) =>
        for {
          b1 <- observe(text)(q1)
          b2 <- observe(text)(q2)
        } yield this.instance(context)

      case _ =>
        val txt = text.toLowerCase

        def subs(texts: Set[String], v: Seq[String]): Option[Map[String, String]] = {
          val patterns = texts.map(_.toLowerCase().replace("%s", "(.+)").r(v: _*))
          patterns.map(_.findFirstMatchIn(txt)).collectFirst {
            case Some(res) => v.map(arg => arg -> res.group(arg)).toMap
          }
        }

        reflex(a, subs).fold[Future[RuleBased]](Future.failed(DoNotUnderstand)) {
          args => Future(instance(context ++ args))
        }
    }
  }

  override def act(a: Script.Action)(implicit ec: ExecutionContext, rnd: Random): Future[String] = a match {
    case And(q1, q2) =>
      for {
        q1u <- act(q1)
        q2u <- act(q2)
      } yield s"$q1u и $q2u"

    case _ =>
      def subs(texts: Set[String], args: Seq[String]): Set[String] = if (args.forall(context.contains)) {
        val values = args.map(context)
        texts.map(_.format(values: _*))
      } else Set.empty

      reflex(a, subs).choose(rnd).fold[Future[String]](Future.failed(CanNotDoThis))(s => Future(s))
  }
}