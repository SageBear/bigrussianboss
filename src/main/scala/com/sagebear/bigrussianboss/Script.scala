package com.sagebear.bigrussianboss

import com.sagebear.Dialog.Phrase
import com.sagebear.bigrussianboss.ScriptTest.bot.SensorsAndActuators
import com.sagebear.bigrussianboss.ScriptTest.bot.SensorsAndActuators.{CanNotDoThis, DoNotUnderstand}
import com.sagebear.bigrussianboss.intent.Intents.And
import com.sagebear.{Bio, Dialog, Tree}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
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

  private def execute[A, W, T](client: SensorsAndActuators, operator: SensorsAndActuators,
                               act: (SensorsAndActuators, Action) => Future[A],
                               getText: (A) => String, wrap: (Step, A) => W, addition: (W, T) => T,
                               acc: T)
                              (implicit ec: ExecutionContext, rnd: Random): Future[T] = {

    def findAppropriateListener(observer: (Action) => Future[SensorsAndActuators],
                                alternatives: Seq[Node]): Future[(SensorsAndActuators, Node, Seq[Node])] =
      alternatives match {
        case Nil => Future.failed(DoNotUnderstand)
        case _ =>
          (for {
            updatedListener <- observer(alternatives.head.value.action)
          } yield (updatedListener, alternatives.head, alternatives.tail)).recoverWith {
            case _ => findAppropriateListener(observer, alternatives.tail)
          }
      }

    def step(alternatives: Seq[Node], client: SensorsAndActuators, operator: SensorsAndActuators,
             rollupCallback: () => Future[T]): Future[T] = alternatives match {
      case Nil => Future(acc)
      case _ =>
        val shuffledAlternatives = rnd.shuffle(alternatives)
        val currentStep = shuffledAlternatives.head.value
        val (speaker, listener) = if (currentStep.speaker == Клиент) (client, operator) else (operator, client)

        val communicator = for {
          acted <- act(speaker, currentStep.action)
          (updatedListener, node, restAlternatives) <-
            findAppropriateListener(listener.observe(getText(acted)), shuffledAlternatives)
          (newClient, newOperator) = if (node.value.speaker == Клиент) {
            (client, updatedListener)
          } else {
            (updatedListener, operator)
          }
        } yield (newClient, newOperator, wrap(node.value, acted), node, restAlternatives)

        (for {
          (newClient, newOperator, wrapped, node, restAlternatives) <- communicator
          nextRollupCallback = if (restAlternatives.nonEmpty) {
            () => step(restAlternatives, client, operator, rollupCallback)
          } else rollupCallback

          utterance <- step(node.children, newClient, newOperator, nextRollupCallback).map(addition(wrapped, _))
        } yield utterance).recoverWith {
          case CanNotDoThis => rollupCallback()
          case DoNotUnderstand => if (communicator.isCompleted) {
            rollupCallback().map(addition(communicator.value.get.get._3, _))
          } else {
            rollupCallback()
          }
        }
    }

    step(children, client, operator, () => Future.failed(DoNotUnderstand))
  }

  def run(client: SensorsAndActuators, operator: SensorsAndActuators)
         (implicit ec: ExecutionContext, rnd: Random): Future[String] =
    execute(client, operator,
      (bot: SensorsAndActuators, action: Action) => bot.act(action),
      (text: String) => text, (step: Step, text: String) => (if (step.speaker == Клиент) ">> " else ":: ") + text,
      (text: String, dialog: String) => text + "\n" + dialog,
      "")

  def generate(client: SensorsAndActuators, operator: SensorsAndActuators)
              (implicit ec: ExecutionContext, rnd: Random): Stream[Dialog] =
    Await.result(
      execute(client, operator,
        (bot: SensorsAndActuators, action: Action) => bot.actWithBio(action),
        (acted: (String, Bio)) => acted._1, (step: Step, acted: (String, Bio)) => Phrase(step, acted._1, acted._2),
        (phrase: Phrase, dialog: Dialog) => phrase :: dialog,
        Dialog.empty),
      Duration.Inf) #:: generate(client, operator)
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
