package com.sagebear.bigrussianboss

import com.sagebear.bigrussianboss.Script._
import com.sagebear.bigrussianboss.bot.{Cli, RuleBased}
import com.sagebear.bigrussianboss.intent.Intents._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

object BigRussianBoss extends App {
  private val script = примеры(
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

  private val client = new Cli//RuleBased.client(clientAddress, clientPhone)
  private val operator = RuleBased.operator.get

  println(Await.result(script.execute(client, operator), 1.hour))
}
