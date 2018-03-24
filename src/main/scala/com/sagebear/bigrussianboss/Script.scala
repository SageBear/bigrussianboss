package com.sagebear.bigrussianboss

import com.sagebear.Tree
import com.sagebear.bigrussianboss.bot.SensorsAndActuators
import com.sagebear.bigrussianboss.bot.SensorsAndActuators.{CanNotDoThis, DoNotUnderstand}
import com.sagebear.bigrussianboss.intent.Intents.And
import com.typesafe.config.Config

import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.concurrent.duration._
import scala.util.Random

/**
  * @author vadim
  * @since 30.01.2018
  */
class Script(children: Seq[Tree[Script.Step]]) {
  import Script._

  private def insert(node: Tree[Step]): Script = {
    children.zipWithIndex.find { case (n, _) => n.value == node.value } match {
      case None => new Script(children :+ node)
      case Some((ch, i)) =>
        val newChildren = node.children.foldLeft(ch) { case (acc, n) => acc.insert(n) }
        new Script(children.updated(i, newChildren))
    }
  }

  def execute(client: SensorsAndActuators, operator: SensorsAndActuators)(implicit ec: ExecutionContext, rnd: Random): Future[String] = {
    def findAppropriateListener(observer: (Action) => Future[SensorsAndActuators], alternatives: Seq[Node]): Future[(SensorsAndActuators, Node, Seq[Node])] = {
      if (alternatives.isEmpty) Future.failed(DoNotUnderstand)
      else {
        (for {
          newListener <- observer(alternatives.head.value.action)
        } yield (newListener, alternatives.head, alternatives.tail)).recoverWith {
          case DoNotUnderstand => findAppropriateListener(observer, alternatives.tail)
        }
      }
    }

    def step(alternatives: Seq[Node],
             client: SensorsAndActuators,
             operator: SensorsAndActuators,
             rollupCallback: () => Future[String]): Future[String] =
      if (alternatives.isEmpty) Future("")
      else {
        val (left, right) = alternatives.splitAt(rnd.nextInt(alternatives.length))
        val (node, newRight) = (right.head, right.tail)
        val newAlternatives = left ++ newRight

        val (speaker, listener, prompt) = if (node.value.speaker == Клиент) (client, operator, ">> ") else (operator, client, ":: ")

        val text = speaker.act(node.value.action)
        val communicate =
          for {
            text <- text
            (newListener, correctNode, restAlternatives) <- findAppropriateListener(listener.observe(text), node +: newAlternatives)
            (newClient, newOperator) = if (node.value.speaker == Клиент) (client, newListener) else (newListener, operator)
          } yield (text, correctNode, restAlternatives, newClient, newOperator)

        (for {
          (text, correctNode, restAlternatives, nextClient, nextOperator) <- communicate

          nextRollupCallback = if (restAlternatives.nonEmpty) {
            () => step(restAlternatives, client, operator, rollupCallback)
          } else rollupCallback

          utterance <- step(correctNode.children, nextClient, nextOperator, nextRollupCallback).map(prompt + text + "\n" + _)
        } yield utterance).recoverWith {
          case CanNotDoThis => rollupCallback()
          case DoNotUnderstand => rollupCallback().map(prompt + text.value.get.get + "\n" + _)
        }
      }

    step(children, client, operator, () => Future.failed(DoNotUnderstand))
  }

  def generate(client: SensorsAndActuators, operator: SensorsAndActuators)
              (implicit ec: ExecutionContext, rnd: Random): Stream[String] =
    Await.result(execute(client, operator), Duration.Inf) #:: generate(client, operator)
}

object Script {
  private type Node = Tree[Step]

  trait Subject {
    def приветствует: Step = Step(this, intent.Intents.Hello)
    def прощается: Step = Step(this, intent.Intents.Bye)
    def спрашивает(question: Action): Step = Step(this, question)
    def говорит(answer: Action): Step = Step(this, answer)
  }

  case object Клиент extends Subject
  case object Оператор extends Subject

  case class Step(speaker: Subject, action: Action) {
    override def toString: String = s"$speaker: $action"
  }

  trait Action {
    def и(q: Action): Action = if (q.hashCode() > this.hashCode()) And(q, this) else And(this, q)
  }

  case class Пример(items: Script.Step*)
  def примеры(items: Пример*): Script = {
    val examples: Seq[Tree[Script.Step]] = items.map(_.items).map { items =>
      items.dropRight(1).foldRight(Tree(items.last, Seq.empty)) { case (rule, totalN) =>
        Tree(rule, Seq.empty).insert(totalN)
      }
    }
    examples.foldLeft(new Script(Seq.empty)) { case (res, example) => res.insert(example) }
  }
}
