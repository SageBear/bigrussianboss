package com.sagebear.bigrussianboss.bot

import com.sagebear.Interpolation
import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.intent.Intents._
import com.typesafe.config.{Config, ConfigFactory}

import scala.util.{Random, Try}

class BeerBot(val context: Map[String, String]) extends RuleBased(ConfigFactory.empty()) {
  override protected def reflex(action: Script.Action): Seq[Interpolation] = PartialFunction[Script.Action, Seq[Interpolation]] {
    case Вопрос_про_адрес => Seq("где ты живешь?", "скажи свой адрес", "ты с каого района, епта?")
    case Вопрос_про_телефон => Seq("твой телефон?", "цифры телефона скажи, епта", "твоя мобила?")
    case Информацию_про_свой_адрес => Seq("я живу на &{address}")
    case Информацию_про_свой_телефон => Seq("&{phone} моя мобила")
    case Информацию_где_купить_пиво => Seq("иди в ближайший к &{address} ларек")
    case Вопрос_про_покупку_пива => Seq("пивчан хочу!", "где мне попить пива?", "где найти пива?")
    case Hello => Seq("здравствуй", "здравствуйте", "мое почтение", "здарова", "приветствую", "привет", "доброго времени суток",
      "как дела", "хай", "чо как", "здаров")
    case Bye => Seq("пока", "до свидания", "прощай", "бб", "бай", "бывай", "до новых встреч", "покедова", "будь")
    case Глупости => Seq("слоны идут на север", "епта", "коза")
  }.applyOrElse(action, (_: Script.Action) => Seq.empty)

  override protected def instance(args: Map[String, String]): RuleBased = {
    new BeerBot(context ++ args)
  }
}

object BeerBot {
  def client(address: String, phone: String)(implicit rnd: Random = Random): Try[BeerBot] = Try(
    new BeerBot(Map("address" -> address, "phone" -> phone)))

  def operator(implicit rnd: Random = Random): Try[BeerBot] = Try(new BeerBot(Map.empty))
}
