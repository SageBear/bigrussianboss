package com.sagebear.bigrussianboss

import java.util.Locale

import com.github.javafaker.Faker
import com.sagebear.bigrussianboss.Script._
import com.sagebear.bigrussianboss.bot.{Cli, RuleBased}
import com.sagebear.bigrussianboss.intent.Intents._
import org.scalatest.FlatSpec

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random

class AlternativesTest extends FlatSpec {
  private val script1 = примеры(Пример(
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

  private val script2 = примеры(
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

  private implicit val rnd: Random = new Random(0)
  private val faker = new Faker(new Locale("ru"))

  private val clientAddress = faker.address().streetAddress()
  private val clientPhone = faker.phoneNumber().cellPhone()

  private val client = RuleBased.client(clientAddress, clientPhone).get
  private val clientH = new Cli//RuleBased.client(clientAddress, clientPhone).get
  private val operator = RuleBased.operator.get
  private val operatorH = new Cli

  it should "Client cli simple alternative" in {
    assert(Await.result(script1.execute(clientH, operator), 1.hour) ===
      s""">> чо как
         |:: чо как
         |>> где мне попить пива?
         |:: прощай
         |>> прощай
         |""".stripMargin)
  }

  it should "Client cli simple alternative reorder" in {
    assert(Await.result(script2.execute(clientH, operator), 1.hour) ===
      s""">> чо как
         |:: чо как
         |>> где мне попить пива?
         |:: прощай
         |>> прощай
         |""".stripMargin)
  }
}