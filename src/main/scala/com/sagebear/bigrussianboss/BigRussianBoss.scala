package com.sagebear.bigrussianboss

import java.util.Locale

import com.github.javafaker.Faker
import com.sagebear.bigrussianboss.Script._
import com.sagebear.bigrussianboss.bot.{BeerBot, LegalBot}
import com.sagebear.bigrussianboss.intent.Intents._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

class BigRussianBoss extends App {
  private val beerScript = примеры(
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

  private val legalScript = примеры(
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
      Оператор говорит Информацию_о_возврате_технически_сложного_товара,
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
      Оператор говорит Информацию_о_возврате_технически_не_сложного_товара,
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
      Оператор говорит Информацию_о_возврате_товара_когда_устраивает_качество,
      Клиент прощается,
      Оператор прощается,
    ),
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент говорит Цель_своего_визита,
      Оператор спрашивает Место_покупки_товара,
      Клиент говорит Купил_онлайн,
      Оператор говорит Информацию_о_возврате_товара_при_покупке_онлайн,
      Клиент прощается,
      Оператор прощается,
    )
  )

  private val faker = new Faker(new Locale("ru"))

  private val clientAddress = faker.address().streetAddress()
  private val clientPhone = faker.phoneNumber().cellPhone()
  private val beerClient = BeerBot.client(clientAddress, clientPhone).get
  private val beerOperator = BeerBot.operator.get

  private val client = LegalBot.client.get
  private val operator = LegalBot.operator.get

  println(Await.result(beerScript.execute(beerClient, beerClient), 1.hour))
}
