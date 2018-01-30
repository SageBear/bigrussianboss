package com.sagebear.bigrussianboss

import com.sagebear.bigrussianboss.Script._
import com.sagebear.bigrussianboss.bot.{BotIO, TestBot}
import com.sagebear.bigrussianboss.intent.Intents._
import org.scalatest.FlatSpec

import scala.language.postfixOps

class ScriptTreeMixinTest extends FlatSpec {
  val script: Script = примеры(
    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент спрашивает Про_покупку_пива,
      Оператор спрашивает Адрес,
      Клиент говорит Свой_адрес,
      Оператор отвечает Где_купить_пиво,
      Оператор прощается,
      Клиент прощается
    ),

    Пример(
      Клиент приветствует,
      Оператор приветствует,
      Клиент спрашивает Про_покупку_пива,
      Оператор спрашивает (Телефон и Адрес),
      Клиент говорит Свой_адрес,
      Оператор спрашивает Телефон,
      Клиент говорит Глупости,
      Оператор спрашивает Телефон,
      Клиент говорит Свой_телефон,
      Оператор отвечает Где_купить_пиво,
      Оператор прощается,
      Клиент прощается
    )
  )

  it should "work" in {
    println(
      BotIO.encode(TestBot(Клиент), Map("address" -> "ул. Стойкости", "phone" -> "89645091637"), TestBot(Оператор),
        Map("address" -> "ул. Стойкости", "phone" -> "89645091637"))(script)
          .map(_.mkString("\n")).mkString("\n---------\n")
    )
  }
}
