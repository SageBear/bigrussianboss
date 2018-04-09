package com.sagebear.bigrussianboss.bot

import scala.collection.JavaConverters._
import com.sagebear.bigrussianboss.Script
import com.typesafe.config.Config

import scala.util.{Random, Try}

class LegalBot(val context: Map[String, String])(implicit config: Config) extends RuleBased {
  override protected def reflex[T](action: Script.Action, subs: (Set[String], Seq[String]) => T): T = {
    val alternatives = asScalaIterator(config.getConfigList("intents").iterator()).filter {
      (c: Config) => c.getString("intent") + "$" == action.getClass.getSimpleName
    }.map {
      (c: Config) => asScalaBuffer(c.getStringList("templates")).toSet
    }.fold(Set.empty) {
      (acc, part) => acc ++ part
    }

    subs(alternatives, Seq.empty)
  }

  override protected def instance(context: Map[String, String]): RuleBased = new LegalBot(context)
}

object LegalBot {
  def client()(implicit rnd: Random = Random, config: Config): Try[LegalBot] = Try(new LegalBot(Map.empty))
  def operator()(implicit rnd: Random = Random, config: Config): Try[LegalBot] = Try(new LegalBot(Map.empty))
}
