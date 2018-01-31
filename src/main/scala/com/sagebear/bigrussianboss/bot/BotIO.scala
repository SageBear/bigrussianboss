package com.sagebear.bigrussianboss.bot

import com.sagebear.Tree
import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.Script.{Action, Subject, Клиент}
import com.sagebear.bigrussianboss.ner.NerMarkup

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.control.NoStackTrace

import scala.concurrent.ExecutionContext.Implicits.global
/**
  * @author vadim
  * @since 30.01.2018
  */
trait BotIO {
  def encode(a: Action, context: BotIO.Context): Future[BotIO.EncodedUtterance]
  def decode(text: String): Future[(Action, BotIO.Context)]
}

object BotIO {
  case object EncodeImpossible extends RuntimeException with NoStackTrace

  case class EncodedUtterance(owner: Subject, text: String, bioMarkup: Seq[(String, NerMarkup.NerTag)], intentName: Set[Action]) {
    override def toString: String = s"$owner: $text"
  }
  type EncodedDialog = Seq[EncodedUtterance]
  type Context = Map[String, String]

  private def dts[T](node: Tree[Script.Step], process: Tree[Script.Step] => EncodedUtterance): Stream[EncodedDialog] =
    if(node.children.isEmpty) Seq(process(node)) #:: Stream.empty[EncodedDialog]
    else node.children.toStream.flatMap(dts(_, process).map(process(node) +: _))

  def encode(client: BotIO, clientContext: Context, operator: BotIO, operatorContext: Context)(script: Script): Stream[EncodedDialog] = {
    def encodeNode(node: Tree[Script.Step]) =
      if (node.value.owner == Клиент) Await.result(client.encode(node.value.action, clientContext), 1.second)
      else Await.result(operator.encode(node.value.action, operatorContext), 1.second)

    script.children.toStream.flatMap(dts(_, encodeNode))
  }

  /*def run(client: BotIO, operator: BotIO)(script: Script): Future[String] = {

  }*/

  private type Node = Tree[Script.Step]

  def run(client: BotIO, operator: BotIO)(script: Script): Future[String] = {
    case class Ctx(client: Context, operator: Context)

    def step(alternatives: Seq[Node], ctx: Ctx, cancelCallback: => Future[String]): Future[String] = {
      if (alternatives.isEmpty) Future("")
      else {
        val node = alternatives.head
        val currentSubject = if (node.value.owner == Клиент) client else operator
        val context: Context = if (node.value.owner == Клиент) ctx.client else ctx.operator
        (for {
            EncodedUtterance(_, text, _, _) <- currentSubject.encode(node.value.action, context)
            (action, decodedContext) <- currentSubject.decode(text)
            if action == node.value.action
            newContexts = if (node.value.owner == Клиент) ctx.copy(client = context ++ decodedContext)
                          else ctx.copy(operator = context ++ decodedContext)
            res <-  if (node.isLeaf) Future("")
                    else if (alternatives.tail.nonEmpty)  step(node.children, newContexts,
                                                            step(alternatives.tail, newContexts, cancelCallback)
                                                          ).map(text + "\n" + _)
                    else step(node.children, newContexts, cancelCallback).map(text + "\n" + _)
        } yield res)
        .recoverWith { case EncodeImpossible => cancelCallback }
      }
    }

    step(script.children, Ctx(Map.empty, Map.empty), Future.failed(EncodeImpossible))
  }
}