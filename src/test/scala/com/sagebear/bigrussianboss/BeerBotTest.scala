package com.sagebear.bigrussianboss

import java.util.Locale

import com.github.javafaker.Faker
import com.sagebear.bigrussianboss.Script._
import com.sagebear.bigrussianboss.bot._
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

  private val clientAlternative = ObedientBot.client("чо как", "где мне попить пива?", "прощай").get
  private val clientUnknown = ObedientBot.client("чо как", "где мне попить пива?", "Гав гав").get
  private val clientRegister = ObedientBot.client("чО КАК", "где мНе поПитЬ пИвА?", "ПроЩаЙ").get

  private val cli = new Cli

  it should "work for robots" in {
    assert(Await.result(script_forRobots.execute(client, operator), 1.hour) ===
      s""">> чо как
         |:: чо как
         |>> где найти пива?
         |:: скажи свой адрес
         |>> я живу на $clientAddress
         |:: иди в ближайший к ${clientAddress.toLowerCase()} ларек
         |:: прощай
         |>> прощай
         |""".stripMargin)
  }

  it should "bot should understand alternative (in first Пример) chosen from cli" in {
    assert(Await.result(script_alternativeFirst.execute(clientAlternative, operator), 1.hour) ===
      s""">> чо как
         |:: чо как
         |>> где мне попить пива?
         |:: прощай
         |>> прощай
         |""".stripMargin)
  }

  it should "bot should understand alternative (in second Пример) chosen from cli" in {
    assert(Await.result(script_alternativeSecond.execute(clientAlternative, operator), 1.hour) ===
      s""">> чо как
         |:: чо как
         |>> где мне попить пива?
         |:: прощай
         |>> прощай
         |""".stripMargin)
  }

  it should "bot shouldn't understand unknown" in {
    assertThrows[RuntimeException](Await.result(script_Simple.execute(clientUnknown, operator), 1.hour))
  }

  it should "bot should ignore words register" in {
    assert(Await.result(script_Simple.execute(clientRegister, operator), 1.hour) ===
      s""">> чО КАК
         |:: чо как
         |>> где мНе поПитЬ пИвА?
         |:: прощай
         |>> ПроЩаЙ
         |""".stripMargin)
  }
}