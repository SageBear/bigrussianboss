package com.sagebear.bigrussianboss.domain

import com.sagebear.bigrussianboss.Syntax
import com.sagebear.bigrussianboss.Extensions._

object Beer extends Syntax {
  case object Клиент extends Subject
  case object Оператор extends Subject

  case object Про_покупку_пива extends Utterance
  case object Адрес extends Utterance
  case object Телефон extends Utterance

  case object Свой_адрес extends Utterance
  case object Свой_телефон extends Utterance
  case object Где_купить_пиво extends Utterance

  override def utterance2text(context: Map[String, String]): Beer.CompileRule =  {
    case Адрес => Set("где ты живешь?", "скажи свой адрес", "ты с каого района, епта?").choose.head
    case Телефон => Set("твой телефон?", "цифры телефона скажи, епта", "твоя мобила?").choose.head
    case Свой_адрес if context.contains("address") => s"я живу на ${context("address")}"
    case Свой_телефон if context.contains("phone") => s"${context("phone")} моя мобила"
    case Где_купить_пиво if context.contains("address") && context.contains("phone") =>
      s"Иди в ближайший к ${context("address")} ларек. а еще ответь на смску, я ее послал на ${context("phone")}"
    case Про_покупку_пива => Set("пивчан хочу!", "где мне попить пива?", "где найти пива?").choose.head
  }
}
