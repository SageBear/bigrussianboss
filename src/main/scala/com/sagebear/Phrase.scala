package com.sagebear

import java.util.regex.Pattern

import com.sagebear.Bio.Bio
import com.sagebear.bigrussianboss.Script.Action

object Phrase {
  def apply(content: String) = new Phrase(content)
  def apply(content: String, bio: Bio) = new Phrase(content, bio)
}

class Phrase(val content: String, val bio: Bio) {
  def this(content: String) {
    this(content, Bio(content, "O", single = false))
  }

  def +(that: Phrase) = new Phrase(content + that.content, bio +: that.bio)

  override def toString: String = content
}
