package com.sagebear.bigrussianboss

import com.sagebear.Tree
import com.sagebear.bigrussianboss.bot.SensorsAndActuators
import com.sagebear.bigrussianboss.bot.SensorsAndActuators.{CanNotDoThis, DoNotUnderstand}
import com.sagebear.bigrussianboss.intent.Intents.And

import scala.concurrent.{ExecutionContext, Future}
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
    def step(alternatives: Seq[Node],
             client: SensorsAndActuators,
             operator: SensorsAndActuators,
             rollupCallback: () => Future[String]): Future[String] =
      if (alternatives.isEmpty) Future("")
      else {
        val node = alternatives.head
        val (speaker, listener, prompt) = if (node.value.speaker == Клиент) (client, operator, ">> ") else (operator, client, ":: ")
        val communicate =
          for {
            text <- speaker.act(node.value.action)
            newListener <- listener.observe(text)(node.value.action)
            (newClient, newOperator) = if (node.value.speaker == Клиент) (client, newListener) else (newListener, operator)
          } yield (text, newClient, newOperator)

        (for {
          (text, nextClient, nextOperator) <- communicate
          nextRollupCallback = if (alternatives.tail.nonEmpty) () => step(alternatives.tail, client, operator, rollupCallback) else rollupCallback
          utterance <- step(node.children, nextClient, nextOperator, nextRollupCallback).map(prompt + text + "\n" + _)
        } yield utterance).recoverWith {
          case CanNotDoThis => rollupCallback()
          case DoNotUnderstand => rollupCallback()
        }
      }

    step(children, client, operator, () => Future.failed(DoNotUnderstand))
  }
}

object Script {
  private type Node = Tree[Step]

  trait Subject {
    def приветствует: Step = Step(this, intent.Intents.Hello)
    def прощается: Step = Step(this, intent.Intents.Bye)
    def спрашивает(question: Action): Step = Step(this, question)
    def говорит(answer: Action): Step = Step(this, answer)
    def -(action: Action): Step = Step(this, action)
  }

  case object Клиент extends Subject
  case object Оператор extends Subject

  case class Step(speaker: Subject, action: Action) {
    override def toString: String = s"$speaker: $action"
  }

  trait Action {
    def и(q: Action): Action = if (q.hashCode() > this.hashCode()) And(q, this) else And(this, q)
  }
  class NamedAction(name: String) extends Action {
    override def toString: String = name
  }

  object ConfigAction {
    private var intentsTable: Map[String, Action] = Map.empty

    def apply(name: String): Action = intentsTable.get(name) match {
      case Some(action) => action
      case None => {
        val action = new NamedAction(name)
        intentsTable += (name -> action)
        action
      }
    }
  }
  implicit def strToAction(name: String): Action = {
    ConfigAction(name)
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
