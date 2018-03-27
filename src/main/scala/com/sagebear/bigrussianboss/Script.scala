package com.sagebear.bigrussianboss

import com.sagebear.{Interpolation, Phrase, Tree}
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

  def execute(client: SensorsAndActuators, operator: SensorsAndActuators)(implicit ec: ExecutionContext, rnd: Random): Future[Seq[Phrase]] = {
    def step(alternatives: Seq[Node],
             client: SensorsAndActuators,
             operator: SensorsAndActuators,
             rollupCallback: () => Future[Seq[Phrase]]): Future[Seq[Phrase]] =
      if (alternatives.isEmpty) Future(Seq.empty[Phrase])
      else {
        val node = alternatives(rnd.nextInt(alternatives.length))
        val (speaker, listener, prompt) = if (node.value.speaker == Клиент) (client, operator, ">> ") else (operator, client, ":: ")
        val communicate =
          for {
            phrase <- speaker.act(node.value.action)
            newListener <- listener.observe(phrase.toString)(node.value.action)
            (newClient, newOperator) = if (node.value.speaker == Клиент) (client, newListener) else (newListener, operator)
          } yield (phrase, newClient, newOperator)
        (for {
          (phrase, nextClient, nextOperator) <- communicate
          nextRollupCallback = if (alternatives.tail.nonEmpty) () => step(alternatives.tail, client, operator, rollupCallback) else rollupCallback
          utterance <- step(node.children, nextClient, nextOperator, nextRollupCallback).map(phrase +: _)
        } yield utterance).recoverWith {
          case CanNotDoThis => rollupCallback()
          case DoNotUnderstand => rollupCallback()
        }
      }

    step(children, client, operator, () => Future.failed(DoNotUnderstand))
  }

  def generate(client: SensorsAndActuators, operator: SensorsAndActuators)
              (implicit ec: ExecutionContext, rnd: Random): Stream[Seq[Phrase]] =
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
