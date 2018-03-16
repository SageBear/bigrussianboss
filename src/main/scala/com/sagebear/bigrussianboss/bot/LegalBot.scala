package com.sagebear.bigrussianboss.bot

import java.util

import scala.collection.JavaConversions._

import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.Script.{Action, ConfigAction}
import com.sagebear.bigrussianboss.intent.Intents._
import com.typesafe.config.ConfigFactory

import scala.util.{Random, Try}

class LegalBot(val context: Map[String, String], private var intents: Map[Action, Set[String]]) extends RuleBased {
  def this(context: Map[String, String]) {
    this(context, Map.empty)

    intents += (Bye -> Set.empty[String])

    val config = ConfigFactory.load("LegalBot")
    for (items <- config.getList("intents").unwrapped().toArray()) {
      val map = items match {
        case map: java.util.HashMap[String, Any] => map
      }
      val action = map.get("intent") match {
        case name: String => ConfigAction(name)
      }
      if (!intents.contains(action)) {
        map.get("templates") match {
          case list: java.util.ArrayList[String] =>
            intents += (action -> list.toSet)
        }
      }
    }
  }

  override protected def reflex[T](action: Script.Action, subs: (Set[String], Seq[String]) => T): T = {
    subs(intents.getOrElse(action, Set.empty), Seq.empty)
  }

  override protected def instance(context: Map[String, String]): RuleBased = new LegalBot(context, intents)
}

object LegalBot {
  def client(implicit rnd: Random = Random): Try[LegalBot] = Try(new LegalBot(Map.empty))

  def operator(implicit rnd: Random = Random): Try[LegalBot] = Try(new LegalBot(Map.empty))
}
