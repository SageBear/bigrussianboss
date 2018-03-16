package com.sagebear.bigrussianboss

import java.util.Locale

import com.github.javafaker.Faker
import com.sagebear.bigrussianboss.bot.ObedientBot
import com.sagebear.bigrussianboss.Script._
import com.sagebear.bigrussianboss.bot._
import com.sagebear.bigrussianboss.intent.Intents._
import org.scalatest.FlatSpec

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps


class BeerBotTestAlternative extends FlatSpec {
  private val script_alternativeFirst = примеры(
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент спрашивает Вопрос_про_покупку_пива,
      Оператор прощается,
      Клиент прощается
    ),
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент прощается,
      Оператор прощается,
    ),
  )

  private val script_alternativeSecond = примеры(
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент прощается,
      Оператор прощается,
    ),
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент спрашивает Вопрос_про_покупку_пива,
      Оператор прощается,
      Клиент прощается
    )
  )

  private val operator = BeerBot.operator.get
  private val clientAlternative = ObedientBot.client("чо как", "где мне попить пива?", "прощай").get

  it should "understand alternative (in first Пример) chosen from cli" in {
    assert(Await.result(script_alternativeFirst.execute(clientAlternative, operator), 1.hour) ===
      s""">> чо как
         |:: чо как
         |>> где мне попить пива?
         |:: прощай
         |>> прощай
         |""".stripMargin)
  }

  it should "understand alternative (in second Пример) chosen from cli" in {
    assert(Await.result(script_alternativeSecond.execute(clientAlternative, operator), 1.hour) ===
      s""">> чо как
         |:: чо как
         |>> где мне попить пива?
         |:: прощай
         |>> прощай
         |""".stripMargin)
  }
}