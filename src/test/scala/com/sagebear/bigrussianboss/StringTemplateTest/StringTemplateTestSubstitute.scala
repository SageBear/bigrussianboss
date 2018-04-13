package com.sagebear.bigrussianboss.StringTemplateTest

import java.util.MissingFormatArgumentException

import com.sagebear.Extensions._
import org.scalatest.FlatSpec

import scala.language.postfixOps

class StringTemplateTestSubstitute extends FlatSpec {
  it should "substitute variables" in {
    assert(
      "&{var1: Type} &{var2: Type} &{var3: Type}".substitute(
        Map("var1" -> "1", "var2" -> "2", "var3" -> "3")) ===
        "1 2 3")
  }

  it should "substitute variable multiple times" in {
    assert(
      "&{var1: Type} &{var2: Type} &{var1: Type} &{var1: Type} &{var1: Type} &{var1: Type}".substitute(
        Map("var1" -> "1", "var2" -> "2")) ===
        "1 2 1 1 1 1")
  }

  it should "work with special symbols" in {
    assert(
      "*.? (.*) (?<var1>.*&{var1: Type})*".substitute(
        Map("var1" -> "1")) ===
        "*.? (.*) (?<var1>.*1)*")
  }

  it should "throw MissingFormatArgumentException when argument doesn't present in variables" in {
    assertThrows[MissingFormatArgumentException](
      "Мой любимый фрукт &{bestFruit: Fruit}".substitute(Map("worseFruit" -> "apple")))
  }
}
