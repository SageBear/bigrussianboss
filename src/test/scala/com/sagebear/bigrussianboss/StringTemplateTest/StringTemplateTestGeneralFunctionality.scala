package com.sagebear.bigrussianboss.StringTemplateTest

import com.sagebear.Extensions._
import org.scalatest.FlatSpec

class StringTemplateTestGeneralFunctionality extends FlatSpec {
  it should "substitute and parse variables" in {
    val userContext = Map(
      "name" -> "Никита",
      "friendName" -> "Олег",
      "address" -> "Первомайская 32 к. 2",
      "phone" -> "8 (999) 231-14-24")
    var operatorContext = Map.empty[String, String]

    operatorContext ++= "Привет, меня зовут &{userName: Name}".parse(
      s"Привет, меня зовут ${userContext("name")}"
    ).get
    assert(operatorContext("userName") == userContext("name"))

    assert("Привет, &{userName: Name}! Скажи свой номер телефона и адрес.".substitute(operatorContext) ===
      s"Привет, ${operatorContext("userName")}! Скажи свой номер телефона и адрес.")

    operatorContext ++= "Мой номер &{phone: Phone} и адрес &{address: Address}".parse(
      s"Мой номер ${userContext("phone")} и адрес ${userContext("address")}"
    ).get
    assert(operatorContext("phone") === userContext("phone"))
    assert(operatorContext("address") === userContext("address"))

    assert("Также для статистики нам нужно имя друга, который позвал тебя в наш клуб.".substitute(operatorContext) ===
      "Также для статистики нам нужно имя друга, который позвал тебя в наш клуб.")

    operatorContext ++= s"Меня позвал &{friendName: Name}".parse(
      s"Меня позвал ${userContext("friendName")}"
    ).get
    assert(operatorContext("friendName") === userContext("friendName"))

    assert("Тебя зовут &{userName: Name}, твой номер &{phone: Phone}, и ты живешь по адресу &{address: Address}. В наш клуб тебя позвал &{friendName: Name}".substitute(operatorContext) ===
      s"Тебя зовут ${operatorContext("userName")}, твой номер ${operatorContext("phone")}, и ты живешь по адресу ${operatorContext("address")}. В наш клуб тебя позвал ${operatorContext("friendName")}"
    )
  }
}
