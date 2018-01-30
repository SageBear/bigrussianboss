package com.sagebear.bigrussianboss.bot

import com.sagebear.Tree
import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.Script.{Action, Subject, Клиент}
import com.sagebear.bigrussianboss.ner.NerMarkup

/**
  * @author vadim
  * @since 30.01.2018
  */
trait BotIO {
  def encode(a: Action, context: BotIO.Context): BotIO.EncodedUtterance
  def decode(text: String): (Action, BotIO.Context)
}

object BotIO {
  case class EncodedUtterance(owner: Subject, text: String, bioMarkup: Seq[(String, NerMarkup.NerTag)], intentName: Set[Action]) {
    override def toString: String = s"$owner: $text"
  }
  type EncodedDialog = Seq[EncodedUtterance]
  type Context = Map[String, String]

  def encode(client: BotIO, clientContext: Context, operator: BotIO, operatorContext: Context)(script: Script): Stream[EncodedDialog] = {
    def encodeNode(owner: Subject, action: Action) =
      if (owner == Клиент) client.encode(action, clientContext) else operator.encode(action, operatorContext)
    def dts(node: Tree[Script.Step]): Stream[EncodedDialog] =
      if(node.children.isEmpty) Seq(encodeNode(node.value.owner, node.value.action)) #:: Stream.empty[EncodedDialog]
      else node.children.toStream.flatMap(dts(_).map(encodeNode(node.value.owner, node.value.action) +: _))
    script.children.toStream.flatMap(dts)
  }
}