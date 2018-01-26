package com.sagebear.bigrussianboss

import Extensions._

trait Syntax {
  trait Subject {
    def приветствует: Rule = Rule(this, Hello)
    def прощается: Rule = Rule(this, Bye)
    def спрашивает(question: Utterance): Rule = Rule(this, question)
    def говорит(answer: Utterance): Rule = Rule(this, answer)
    def отвечает(answer: Utterance): Rule = говорит(answer)
  }

  case class Rule(subject: Subject, utterance: Utterance) {
    override def toString: String = s"$subject: $utterance"
  }

  protected type CompileRule = PartialFunction[Utterance, String]
  protected def utterance2text(context: Map[String, String]): CompileRule

  trait Utterance {
    def и(q: Utterance): Utterance = if (q.hashCode() > this.hashCode()) And(q, this) else And(this, q)

    def text(context: Map[String, String]): String = utterance2text(context).orElse[Utterance, String] {
      case And(q1, q2) => q1.text(context) + " и " + q2.text(context)

      case Bye => Set("бывай", "будь", "покеда, епта").choose.head
      case Hello => Set("привет, епта", "здарова, отец", "чо как?").choose.head
      case Глупости => Set("слоны идут на север", "епта", "коза").choose.head
    }.applyOrElse[Utterance, String](this, _ => "хз")
  }

  case class And(q1: Utterance, q2: Utterance) extends Utterance

  case object Hello extends Utterance
  case object Bye extends Utterance
  case object Глупости extends Utterance

  case class Пример(items: Rule*)
  def примеры(items: Пример*): Script = {
    items.map(_.items).map { items =>
      items.dropRight(1).foldRight(Script.Node(items.last, Seq.empty)) { case (rule, totalN) => Script.Node(rule, Seq.empty).insert(totalN) }
    }.foldLeft(new Script(Seq.empty)) { case (res, example) => res.insert(example) }
  }

  class Script(protected val children: Seq[Script.Node]) extends Script.TreeMixin[Script] {
    protected def children(children: Seq[Script.Node]) = new Script(children)

    private def sampleNode(node: Script.Node, context: Map[Subject, Map[String, String]]) = context.get(node.value.subject).map { vars =>
      node.value.utterance.text(vars)
    }.getOrElse("...can't compute...")

    def examples(context: (Subject, Map[String, String])*): Stream[String] = {
      val cx = context.toMap[Subject, Map[String, String]]
      def dts(node: Script.Node): Stream[String] =
        if (node.children.isEmpty) sampleNode(node, cx) #:: Stream.empty[String]
        else node.children.toStream.flatMap(dts(_).map(sampleNode(node, cx) + "\n" + _))
      this.children.toStream.flatMap(dts)
    }

    override def toString: String = {
      import Extensions._
      val CAPTION_LEN = 30
      def level(captions: Seq[(String, Int)], acc: String): String =
        if (captions.isEmpty) acc
        else level(captions.tail,
          acc + " " * ((captions.head._2 - 1) * CAPTION_LEN / 2) +
            captions.head._1.wrap(CAPTION_LEN) +
            " " * ((3 * CAPTION_LEN + 2) / 2 - captions.head._1.length)
        )
      def levels(nodes: Seq[Option[Script.Node]], acc: Seq[String]): Seq[String] =
        if (nodes.forall(_.isEmpty)) acc
        else {
          val procNodes = nodes.map {
            case Some(n) => ((n.value.toString, n.nodes.count(_.children.isEmpty)),
              if (n.children.isEmpty) Seq(None) else n.children.map(Some(_)))
            case None => ((" " * CAPTION_LEN, 1), Seq(None))
          }
          levels(procNodes.flatMap(_._2), acc :+ level(procNodes.map(_._1), ""))
        }
      levels(children.map(Some(_)), Seq.empty).mkString("\n")
    }
  }

  object Script {
    trait TreeMixin[T <: TreeMixin[_]] {
      protected val children: Seq[Node]
      //TODO: ordering
      def insert(node: Node): T = {
        children.zipWithIndex.find { case (n, _) => n.value == node.value } match {
          case None => children(children :+ node)
          case Some((ch, i)) =>
            val newChildren = node.children.foldLeft(ch) { case (acc, n) => acc.insert(n) }
            children(children.updated(i, newChildren))
        }
      }

      protected def children(child: Seq[Node]): T
    }

    case class Node(value: Rule, children: Seq[Node]) extends TreeMixin[Node] {
      protected def children(children: Seq[Node]) = Node(value, children)

      def nodes: Seq[Node] = {
        def expandNode(node: Node, frontier: Seq[Node], acc: Seq[Node]): Seq[Node] = {
          val newFrontier = frontier ++ node.children
          if (newFrontier.isEmpty) node +: acc
          else expandNode(newFrontier.head, newFrontier.tail, node +: acc)
        }
        expandNode(this, Seq.empty, Seq.empty)
      }
    }
  }
}

