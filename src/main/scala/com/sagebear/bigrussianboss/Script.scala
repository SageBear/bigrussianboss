package com.sagebear.bigrussianboss

import com.sagebear.Tree
import com.sagebear.bigrussianboss.intent.Intents.And

/**
  * @author vadim
  * @since 30.01.2018
  */
case class Script(children: Seq[Tree[Script.Step]]) {
  def insert(node: Tree[Script.Step]): Script = {
    children.zipWithIndex.find { case (n, _) => n.value == node.value } match {
      case None => new Script(children :+ node)
      case Some((ch, i)) =>
        val newChildren = node.children.foldLeft(ch) { case (acc, n) => acc.insert(n) }
        new Script(children.updated(i, newChildren))
    }
  }
}

object Script {
  trait Subject {
    def приветствует: Step = Step(this, intent.Intents.Hello)
    def прощается: Step = Step(this, intent.Intents.Bye)
    def спрашивает(question: Action): Step = Step(this, question)
    def говорит(answer: Action): Step = Step(this, answer)
    def отвечает(answer: Action): Step = говорит(answer)
  }

  case object Клиент extends Subject
  case object Оператор extends Subject

  case class Step(owner: Subject, action: Action) {
    override def toString: String = s"$owner: $action"
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
    examples.foldLeft(Script(Seq.empty)) { case (res, example) => res.insert(example) }
  }
}
