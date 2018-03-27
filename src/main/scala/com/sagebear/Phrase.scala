package com.sagebear

import java.util.regex.Pattern

import com.sagebear.Bio.Bio
import com.sagebear.bigrussianboss.Script.Action

case class Phrase(action: Action, content: String, bio: Bio) {
  override def toString: String = content
}