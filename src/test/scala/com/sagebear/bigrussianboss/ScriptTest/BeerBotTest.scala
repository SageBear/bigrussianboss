package com.sagebear.bigrussianboss.ScriptTest

import java.util.Locale

import com.github.javafaker.Faker
import com.sagebear.bigrussianboss.Script._
import com.sagebear.bigrussianboss.ScriptTest.bot._
import com.sagebear.bigrussianboss.intent.Intents._
import org.scalatest.FlatSpec

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random

class BeerBotTest extends FlatSpec {
  private val script_forRobots = примеры(
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент спрашивает Вопрос_про_покупку_пива,
      Оператор спрашивает Вопрос_про_адрес,
      Клиент говорит Информацию_про_свой_адрес,
      Оператор говорит Информацию_где_купить_пиво,
      Оператор прощается,
      Клиент прощается
    ),
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент спрашивает Вопрос_про_покупку_пива,
      Оператор спрашивает (Вопрос_про_телефон и Вопрос_про_адрес),
      Клиент говорит Информацию_про_свой_адрес,
      Оператор спрашивает Вопрос_про_телефон,
      Клиент говорит Глупости,
      Оператор спрашивает Вопрос_про_телефон,
      Клиент говорит Информацию_про_свой_телефон,
      Оператор говорит Информацию_где_купить_пиво,
      Оператор прощается,
      Клиент прощается
    )
  )

  private val script_Simple = примеры(
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент спрашивает Вопрос_про_покупку_пива,
      Оператор прощается,
      Клиент прощается
    ),
  )

  private implicit val rnd: Random = new Random(0)

  private val faker = new Faker(new Locale("ru"))

  private val clientAddress = faker.address().streetAddress()
  private val clientPhone = faker.phoneNumber().cellPhone()

  private val client = BeerBot.client(clientAddress, clientPhone).get
  private val operator = BeerBot.operator.get

  private val clientUnknown = ObedientBot.client("чо как", "где мне попить пива?", "Гав гав").get
  private val clientRegister = ObedientBot.client("чО КАК", "где мНе поПитЬ пИвА?", "ПроЩаЙ").get

  it should "fail on unknown words" in {
    assertThrows[RuntimeException](Await.result(script_Simple.run(clientUnknown, operator), 1.hour))
  }

  it should "work with wrong register" in {
    assert(Await.result(script_Simple.run(clientRegister, operator), 1.hour).nonEmpty)
  }
}