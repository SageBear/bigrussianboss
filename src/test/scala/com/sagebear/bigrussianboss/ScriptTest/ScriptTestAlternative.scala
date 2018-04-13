package com.sagebear.bigrussianboss.ScriptTest

import com.sagebear.bigrussianboss.Script._
import com.sagebear.bigrussianboss.ScriptTest.bot.{ObedientBot, _}
import com.sagebear.bigrussianboss.intent.Intents._
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.FlatSpec

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random


class ScriptTestAlternative extends FlatSpec {
  private val script_alternativeFirst = примеры(
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент прощается,
      Оператор прощается,
    ),
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент говорит Цель_своего_визита,
      Оператор говорит Глупости,
      Клиент прощается,
      Оператор прощается,
    ),
  )

  private val script_alternativeSecond = примеры(
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент говорит Цель_своего_визита,
      Оператор говорит Глупости,
      Клиент прощается,
      Оператор прощается,
    ),
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент прощается,
      Оператор прощается,
    ),
  )

  private implicit val rnd: Random = new Random(0)
  private implicit val config: Config = ConfigFactory.load("LegalBotSingleAlternative")

  private val operator = LegalBot.operator.get
  private val obedientClient = ObedientBot.client("Здравствуйте",
    "Недавно у вас покупал товар, и меня он не устраивает как его вернуть?", "Всего доброго, до свидания").get

  it should "understand alternative" in {
    assert(Await.result(script_alternativeFirst.run(obedientClient, operator), 1.hour) ===
      Await.result(script_alternativeSecond.run(obedientClient, operator), 1.hour))
  }
}