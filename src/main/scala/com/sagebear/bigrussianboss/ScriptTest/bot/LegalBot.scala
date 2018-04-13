package com.sagebear.bigrussianboss.ScriptTest.bot


import com.sagebear.bigrussianboss.Script
import com.typesafe.config.Config

import scala.collection.JavaConverters._
import scala.util.{Random, Try}

class LegalBot(val context: Map[String, String])(implicit config: Config) extends RuleBased {
  override protected def instance(args: Map[String, String]): RuleBased = new LegalBot(context ++ args)

  override protected def reflex(action: Script.Action): Seq[String] = {
    val alternatives = asScalaIterator(config.getConfigList("intents").iterator()).filter {
      (c: Config) => c.getString("intent") + "$" == action.getClass.getSimpleName
    }.map {
      (c: Config) => asScalaBuffer(c.getStringList("templates")).toSet
    }.fold(Set.empty) {
      (acc, part) => acc ++ part
    }
    alternatives.toSeq
  }
}

object LegalBot {
  def client(implicit rnd: Random = Random, config: Config): Try[LegalBot] = Try(new LegalBot(Map.empty))

  def operator(implicit rnd: Random = Random, config: Config): Try[LegalBot] = Try(new LegalBot(Map.empty))
}
