package com.sagebear.bigrussianboss.bot
import com.sagebear.bigrussianboss.intent.Intents._
import com.sagebear.Extensions._
import com.sagebear.bigrussianboss.Script.{Action, Subject}
import com.sagebear.bigrussianboss.bot.BotIO._
import com.sagebear.bigrussianboss.ner.NerMarkup

import scala.concurrent.{ExecutionContext, Future}

/**
  * @author vadim
  * @since 30.01.2018
  */
case class TestBot(role: Subject) extends BotIO {
  override def encode(a: Action, context: Context)(implicit ec: ExecutionContext): Future[EncodedUtterance] =
    a match {
      case Адрес => toUtterance(Set("где ты живешь?", "скажи свой адрес", "ты с каого района, епта?").choose.head, Адрес)
      case Телефон => toUtterance(Set("твой телефон?", "цифры телефона скажи, епта", "твоя мобила?").choose.head, Телефон)
      case Свой_адрес if context.contains("address") => Future {
        bio2utterance(Свой_адрес, wrap2tag("я живу на ") ++ wrap2tag(context("address"), NerMarkup.Address))
      }
      case Свой_телефон if context.contains("phone") => Future {
        bio2utterance(Свой_телефон, wrap2tag(context("phone"), NerMarkup.Phone) ++ wrap2tag("моя мобила"))
      }
      case Где_купить_пиво if context.contains("address") && context.contains("phone") => Future {
        bio2utterance(Где_купить_пиво, wrap2tag("Иди в ближайший к") ++
          wrap2tag(context("address"), NerMarkup.Address) ++
          wrap2tag("ларек. а еще ответь на смску, я ее послал на") ++
          wrap2tag(context("phone"), NerMarkup.Phone))
      }
      case Про_покупку_пива => toUtterance(Set("пивчан хочу!", "где мне попить пива?", "где найти пива?").choose.head, Про_покупку_пива)

      case And(q1, q2) =>
        for {
          q1u <- encode(q1, context)
          q2u <- encode(q2, context)
          bio = (q1u.bioMarkup :+ "и" -> NerMarkup.Other.OtherT) ++ q2u.bioMarkup
        } yield EncodedUtterance(role, q1u.text + " и " + q2u.text, bio, q1u.intentName ++ q2u.intentName)

      case Hello => Future(bio2utterance(Hello, wrap2tag(Set("привет, епта", "здарова, отец", "чо как?").choose.head)))
      case Bye => Future(bio2utterance(Hello, wrap2tag(Set("бывай", "будь", "покеда, епта").choose.head)))
      case Глупости => Future(bio2utterance(Hello, wrap2tag(Set("слоны идут на север", "епта", "коза").choose.head)))
    }

  private def toUtterance(str: String, action: Action)(implicit ec: ExecutionContext): Future[EncodedUtterance] = Future {
    EncodedUtterance(role, str, str.split(" ").map(v => (v, NerMarkup.Other.OtherT)), Set(action))
  }

  private def wrap2tag(entity: String, tag: NerMarkup.NerClass = NerMarkup.Other): Seq[(String, NerMarkup.NerTag)] = {
    val tokens = tokinize(entity)
    (tokens.head -> tag.beginTag) +: tokens.tail.map(_ -> tag.inTag)
  }

  private def tokinize(text: String): Seq[String] = text.split(" ")

  //TODO: may lost information if tokenization clenup data
  private def bio2utterance(intent: Action, bio: Seq[(String, NerMarkup.NerTag)]): EncodedUtterance = EncodedUtterance(role,
    bio.map(_._1).mkString(" "),
    bio,
    Set(intent)
  )

  override def decode(text: String)(implicit ec: ExecutionContext): Future[(Action, Context)] = ???
}
