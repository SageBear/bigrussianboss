package com.sagebear.bigrussianboss.bot

import java.util

import scala.collection.JavaConversions._
import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.Script.{Action, ConfigAction}
import com.sagebear.bigrussianboss.intent.Intents._
import com.typesafe.config.{Config, ConfigFactory}

import scala.util.{Random, Try}

class LegalBot(val context: Map[String, String]) extends RuleBased {
  override protected def reflex[T](action: Script.Action, subs: (Set[String], Seq[String]) => T): T = {
    val config = ConfigFactory.load("LegalBot")

    val alternatives = config.getConfigList("intents").filter {
      (c: Config) => c.getString("intent") == action.getClass.getCanonicalName
    }.map {
      (c: Config) => c.getStringList("templates").toSet
    }.fold(Set.empty) {
      (all, part) => all ++ part
    }

    subs(alternatives, Seq.empty)
  }

  override protected def instance(context: Map[String, String]): RuleBased = new LegalBot(context)
}

object LegalBot {
  def client(implicit rnd: Random = Random): Try[LegalBot] = Try(new LegalBot(Map.empty))

  def operator(implicit rnd: Random = Random): Try[LegalBot] = Try(new LegalBot(Map.empty))
}
