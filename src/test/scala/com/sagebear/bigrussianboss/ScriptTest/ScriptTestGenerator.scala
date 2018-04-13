package com.sagebear.bigrussianboss.ScriptTest

import java.util.Locale

import com.github.javafaker.Faker
import com.sagebear.bigrussianboss.Script._
import com.sagebear.bigrussianboss.ScriptTest.bot.LegalBot
import com.sagebear.bigrussianboss.intent.Intents._
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.FlatSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import scala.util.Random


class ScriptTestGenerator extends FlatSpec {
  private val script = примеры(
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент говорит Цель_своего_визита,
      Оператор спрашивает Место_покупки_товара,
      Клиент говорит Купил_в_магазине,
      Оператор спрашивает Устраивает_ли_качество_товара,
      Клиент говорит Не_устраивает,
      Оператор спрашивает Является_ли_товар_технически_сложным,
      Клиент говорит Является,
      Оператор спрашивает Информацию_о_возврате_технически_сложного_товара,
      Клиент прощается,
      Оператор прощается,
    ),
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент говорит Цель_своего_визита,
      Оператор спрашивает Место_покупки_товара,
      Клиент говорит Купил_в_магазине,
      Оператор спрашивает Устраивает_ли_качество_товара,
      Клиент говорит Не_устраивает,
      Оператор спрашивает Является_ли_товар_технически_сложным,
      Клиент говорит Не_является,
      Оператор спрашивает Информацию_о_возврате_технически_не_сложного_товара,
      Клиент прощается,
      Оператор прощается,
    ),
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент говорит Цель_своего_визита,
      Оператор спрашивает Место_покупки_товара,
      Клиент говорит Купил_в_магазине,
      Оператор спрашивает Устраивает_ли_качество_товара,
      Клиент говорит Устраивает,
      Оператор спрашивает Информацию_о_возврате_товара_когда_устраивает_качество,
      Клиент прощается,
      Оператор прощается,
    ),
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент говорит Цель_своего_визита,
      Оператор спрашивает Место_покупки_товара,
      Клиент говорит Купил_онлайн,
      Оператор спрашивает Информацию_о_возврате_товара_при_покупке_онлайн,
      Клиент прощается,
      Оператор прощается,
    )
  )

  private implicit val rnd: Random = new Random(0)
  private implicit val config: Config = ConfigFactory.load("LegalBotSingleAlternative")

  private val faker = new Faker(new Locale("ru"))

  private val clientAddress = faker.address().streetAddress()
  private val clientPhone = faker.phoneNumber().cellPhone()

  private val client = LegalBot.client.get
  private val operator = LegalBot.operator.get

  it should "generate different dialogs" in {
    assert(script.generate(client, operator).take(1000).toSet.size > 1)
  }

}
