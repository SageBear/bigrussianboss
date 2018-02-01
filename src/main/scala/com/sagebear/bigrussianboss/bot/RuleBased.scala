package com.sagebear.bigrussianboss.bot
import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.intent.Intents._
import com.sagebear.Extensions._
import com.sagebear.bigrussianboss.bot.SensorsAndActuators.CanNotDoThis

import scala.concurrent.{ExecutionContext, Future}

/**
  * @author vadim
  * @since 01.02.2018
  */
class RuleBased(context: Map[String, String] = Map.empty) extends SensorsAndActuators {

  protected def reflex[T](action: Script.Action, subs: (Set[String], Seq[String]) => Set[T]): Set[T] = Map[Script.Action, Set[T]](
    Адрес -> subs(Set("где ты живешь?", "скажи свой адрес", "ты с каого района, епта?")),
    Телефон -> subs(Set("твой телефон?", "цифры телефона скажи, епта", "твоя мобила?")),
    Свой_адрес -> subs(Set("я живу на %s", "address")),
    Свой_телефон -> subs(Set("%s моя мобила", "phone")),
    Где_купить_пиво -> subs(Set("Иди в ближайший к %s ларек. а еще ответь на смску, я ее послал на %s", "address", "phone")),
    Про_покупку_пива -> subs(Set("пивчан хочу!", "где мне попить пива?", "где найти пива?")),
    Hello -> subs(Set("привет, епта", "здарова, отец", "чо как?")),
    Bye -> subs(Set("бывай", "будь", "покеда, епта")),
    Глупости -> subs(Set("слоны идут на север", "епта", "коза"))
  ).getOrElse(action, Set.empty)

  override def observe(text: String)(a: Script.Action)(implicit ec: ExecutionContext): Future[RuleBased] =
    a match {
      case And(q1, q2) =>
        for {
          b1 <- observe(text)(q1)
          b2 <- observe(text)(q2)
        } yield new RuleBased(b1.context ++ b2.context)

      case _ =>
        def subs(texts: Set[String], v: Seq[String]): Set[String] = texts.map(_.replace("%s","(.+)"))
        //reflex(a, subs)
        ???
    }

  override def act(a: Script.Action)(implicit ec: ExecutionContext): Future[String] = a match {
    case And(q1, q2) =>
      for {
        q1u <- act(q1)
        q2u <- act(q2)
      } yield s"$q1u и $q2u"

    case _ =>
      def subs(texts: Set[String], args: Seq[String]): Set[String] = if (args.forall(context.contains)) texts.map(_.format(args)) else Set.empty
      reflex(a, subs).choose.fold[Future[String]](Future.failed(CanNotDoThis))(Future(_))
  }
}
