package com.sagebear.bigrussianboss.StringTemplateTest

import com.sagebear.Extensions._
import org.scalatest.FlatSpec

class StringTemplateTestParse extends FlatSpec {
  it should "parse variables" in {
    val data = "&{name: Name} &{surname: Name}".parse("Иван Иванович")
    assert(data.get("name") === "Иван")
    assert(data.get("surname") === "Иванович")
  }

  it should "work with special symbols" in {
    var data = "?&{name: Name}.*&{surname: Name}.*".parse("?Иван.*Иванович.*")
    assert(data.get("name") === "Иван")
    assert(data.get("surname") === "Иванович")

    data = "(?<name>&{name: Name})*[&{surname: Name}]*".parse("(?<name>Иван)*[Иванович]*")
    assert(data.get("name") === "Иван")
    assert(data.get("surname") === "Иванович")
  }

  it should "return None if template was not found in string" in {
    assert("Я хочуу &{product: Product}.".parse("Я хочу? хлеб.") === None)
  }

  it should "ignore cases" in {
    val data = "Меня зовут &{name: Name}".parse("меня зовут Никита")
    assert(data.get("name") === "Никита")
  }

  it should "recognise only complete coincidence" in {
    assert("Меня зовут &{name: Name}.".parse("меня зовут Никита. что-то еще") === None)
  }
}
