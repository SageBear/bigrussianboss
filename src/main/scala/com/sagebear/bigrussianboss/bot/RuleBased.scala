package com.sagebear.bigrussianboss.bot
import com.sagebear.Extensions._
import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.bot.SensorsAndActuators.{CanNotDoThis, DoNotUnderstand}
import com.sagebear.bigrussianboss.intent.Intents._
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Random, Try}

/**
  * @author vadim
  * @since 01.02.2018
  */
class RuleBased(private val context: Map[String, String], conf: Config, rnd: Random) extends SensorsAndActuators {

  private def reflex[T](action: Script.Action, subs: (Set[String], Seq[String]) => T): T = PartialFunction[Script.Action, T] {
    case Вопрос_про_адрес => subs(Set("где ты живешь?", "скажи свой адрес", "ты с каого района, епта?"), Seq.empty)
    case Вопрос_про_телефон => subs(Set("твой телефон?", "цифры телефона скажи, епта", "твоя мобила?"), Seq.empty)
    case Информацию_про_свой_адрес => subs(Set("я живу на %s"), Seq("address"))
    case Информацию_про_свой_телефон => subs(Set("%s моя мобила"), Seq("phone"))
    case Информацию_где_купить_пиво => subs(Set("иди в ближайший к %s ларек"), Seq("address"))
    case Вопрос_про_покупку_пива => subs(Set("пивчан хочу!", "где мне попить пива?", "где найти пива?"), Seq.empty)
    case Hello => subs(Set("здравствуй","здравствуйте","мое почтение","здарова","приветствую","привет", "доброго времени суток",
      "как дела","хай","чо как","здаров"), Seq.empty)
    case Bye => subs(Set("пока", "до свидания", "прощай", "бб", "бай","бывай","до новых встреч","покедова","будь"), Seq.empty)
    case Глупости => subs(Set("слоны идут на север", "епта", "коза"), Seq.empty)
  }.applyOrElse(action, (_: Script.Action) => subs(Set.empty, Seq.empty))

  override def observe(text: String)(a: Script.Action)(implicit ec: ExecutionContext): Future[RuleBased] =
    a match {
      case And(q1, q2) =>
        for {
          b1 <- observe(text)(q1)
          b2 <- observe(text)(q2)
        } yield new RuleBased(b1.context ++ b2.context, conf, rnd)

      case _ =>
        val txt = text.toLowerCase

        def subs(texts: Set[String], v: Seq[String]): Option[Map[String, String]] = {
          val patterns = texts.map(_.replace("%s","(.+)").r(v: _*))
          patterns.map(_.findFirstMatchIn(txt)).collectFirst {
            case Some(res) => v.map(arg => arg -> res.group(arg)).toMap
          }
        }
        reflex(a, subs).fold[Future[RuleBased]](Future.failed(DoNotUnderstand)) { args => Future(new RuleBased(context ++ args, conf, rnd)) }
    }

  override def act(a: Script.Action)(implicit ec: ExecutionContext): Future[String] = a match {
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
      reflex(a, subs).choose(rnd).fold[Future[String]](Future.failed(CanNotDoThis))(s => Future(s.toLowerCase))
  }
}

object RuleBased {
  def client(address: String, phone: String)(implicit rnd: Random = Random): Try[RuleBased] = Try(
    new RuleBased(Map("address" -> address, "phone" -> phone), ConfigFactory.load(), rnd))
  def operator(implicit rnd: Random = Random): Try[RuleBased] = Try(new RuleBased(Map.empty, ConfigFactory.load(), rnd))
}
