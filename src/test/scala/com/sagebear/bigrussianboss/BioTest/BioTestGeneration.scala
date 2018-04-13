package com.sagebear.bigrussianboss.BioTest

import com.sagebear.Bio
import org.scalatest.FlatSpec

class BioTestGeneration extends FlatSpec {
  it should "put tag `O` if parameter doesn't set" in {
    assert(Bio("Просто текст без тэга").toString ===
      s"""Просто O
         |текст O
         |без O
         |тэга O""".stripMargin
    )
  }

  it should "put 'B-' prefix to the first entry and 'I-' to the rest" in {
    assert(Bio("8 (800) 555-35-35", "Phone").toString ===
      s"""8 B-Phone
         |( I-Phone
         |800 I-Phone
         |) I-Phone
         |555 I-Phone
         |- I-Phone
         |35 I-Phone
         |- I-Phone
         |35 I-Phone""".stripMargin
    )
  }

  it should "work with custom tokenizer" in {
    assert(Bio("*привет?. ", """(?U)\w""".r).toString ===
      s"""п O
         |р O
         |и O
         |в O
         |е O
         |т O""".stripMargin
    )
  }

  it should "concatenate multiple Bio" in {
    assert((Bio("Просто текст без тэга") ::: Bio("8 (800) 555-35-35", "Phone")).toString ===
      s"""Просто O
         |текст O
         |без O
         |тэга O
         |8 B-Phone
         |( I-Phone
         |800 I-Phone
         |) I-Phone
         |555 I-Phone
         |- I-Phone
         |35 I-Phone
         |- I-Phone
         |35 I-Phone""".stripMargin
    )
  }
}
