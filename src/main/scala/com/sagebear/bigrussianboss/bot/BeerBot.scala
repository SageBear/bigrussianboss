package com.sagebear.bigrussianboss.bot

import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.intent.Intents._

import scala.util.{Random, Try}

class BeerBot(val context: Map[String, String]) extends RuleBased {
  override protected def reflex[T](action: Script.Action, subs: (Set[String], Seq[String]) => T): T = PartialFunction[Script.Action, T] {
    case Вопрос_про_адрес => subs(Set("где ты живешь?", "скажи свой адрес", "ты с каого района, епта?"), Seq.empty)
    case Вопрос_про_телефон => subs(Set("твой телефон?", "цифры телефона скажи, епта", "твоя мобила?"), Seq.empty)
    case Информацию_про_свой_адрес => subs(Set("я живу на %s"), Seq("address"))
    case Информацию_про_свой_телефон => subs(Set("%s моя мобила"), Seq("phone"))
    case Информацию_где_купить_пиво => subs(Set("иди в ближайший к %s ларек"), Seq("address"))
    case Вопрос_про_покупку_пива => subs(Set("пивчан хочу!", "где мне попить пива?", "где найти пива?"), Seq.empty)
    case Hello => subs(Set("здравствуй", "здравствуйте", "мое почтение", "здарова", "приветствую", "привет", "доброго времени суток",
      "как дела", "хай", "чо как", "здаров"), Seq.empty)
    case Bye => subs(Set("пока", "до свидания", "прощай", "бб", "бай", "бывай", "до новых встреч", "покедова", "будь"), Seq.empty)
    case Глупости => subs(Set("слоны идут на север", "епта", "коза"), Seq.empty)
  }.applyOrElse(action, (_: Script.Action) => subs(Set.empty, Seq.empty))

  override protected def instance(context: Map[String, String]): RuleBased = {
    new BeerBot(context)
  }
}

object BeerBot {
  def client(address: String, phone: String)(implicit rnd: Random = Random): Try[BeerBot] = Try(
    new BeerBot(Map("address" -> address, "phone" -> phone)))

  def operator(implicit rnd: Random = Random): Try[BeerBot] = Try(new BeerBot(Map.empty))
}
