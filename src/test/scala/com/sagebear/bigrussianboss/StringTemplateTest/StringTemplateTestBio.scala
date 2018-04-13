package com.sagebear.bigrussianboss.StringTemplateTest

import java.util.MissingFormatArgumentException

import com.sagebear.Extensions._
import org.scalatest.FlatSpec

class StringTemplateTestBio extends FlatSpec {
  it should "generate bio" in {
    val context = Map(
      "myName" -> "Никита Воронцов",
      "friendName" -> "Топильский Олег Евгеньевич",
      "myHome" -> "Первомайская 32 к. 2",
      "myPhone" -> "8 (999) 231-14-24")

    assert(
      "Меня зовут &{myName: Name}, моего друга зовут &{friendName: Name}. Можете позвонить мне на телефон &{myPhone: Phone} или навестить по адресу &{myHome: Address}.".bio(context).toString ===
        s"""Меня O
           |зовут O
           |Никита B-Name
           |Воронцов I-Name
           |, O
           |моего O
           |друга O
           |зовут O
           |Топильский B-Name
           |Олег I-Name
           |Евгеньевич I-Name
           |. O
           |Можете O
           |позвонить O
           |мне O
           |на O
           |телефон O
           |8 B-Phone
           |( I-Phone
           |999 I-Phone
           |) I-Phone
           |231 I-Phone
           |- I-Phone
           |14 I-Phone
           |- I-Phone
           |24 I-Phone
           |или O
           |навестить O
           |по O
           |адресу O
           |Первомайская B-Address
           |32 I-Address
           |к I-Address
           |. I-Address
           |2 I-Address
           |. O""".stripMargin)
  }

  it should "throw MissingFormatArgumentException when argument doesn't present in variables" in {
    assertThrows[MissingFormatArgumentException](
      "Мой любимый фрукт &{bestFruit: Fruit}".bio(Map("worseFruit" -> "apple")))
  }
}
