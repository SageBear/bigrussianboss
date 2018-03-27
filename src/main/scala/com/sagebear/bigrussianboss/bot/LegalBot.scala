package com.sagebear.bigrussianboss.bot

import java.util

import com.sagebear.Interpolation

import scala.collection.JavaConverters._
import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.Script.Action
import com.sagebear.bigrussianboss.intent.Intents._
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Random, Try}

class LegalBot(val context: Map[String, String], config: Config) extends RuleBased(config) {
  override protected def instance(args: Map[String, String]): RuleBased = new LegalBot(context ++ args, config)

  override protected def reflex(action: Script.Action): Seq[Interpolation] = {
    val alternatives = asScalaIterator(config.getConfigList("intents").iterator()).filter {
      (c: Config) => c.getString("intent") + "$" == action.getClass.getSimpleName
    }.map {
      (c: Config) => asScalaBuffer(c.getStringList("templates")).toSet
    }.fold(Set.empty) {
      (acc, part) => acc ++ part
    }

    alternatives.map(Interpolation.apply).toSeq
  }
}

object LegalBot {
  def client(config: Config)(implicit rnd: Random = Random): Try[LegalBot] = Try(new LegalBot(Map.empty, config))
  def operator(config: Config)(implicit rnd: Random = Random): Try[LegalBot] = Try(new LegalBot(Map.empty, config))
}
