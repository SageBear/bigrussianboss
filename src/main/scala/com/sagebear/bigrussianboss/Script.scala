package com.sagebear.bigrussianboss

import com.sagebear.bigrussianboss.Syntax.{Rule, Пример}

class Script(protected val children: Seq[Script.Node]) extends Script.TreeMixin[Script] {
  protected def children(children: Seq[Script.Node]) = new Script(children)

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

  case class Node(value: Rule[_], children: Seq[Node]) extends TreeMixin[Node] {
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

  def apply(items: Seq[Пример]): Script = {
    items.map(_.items).map { items =>
      items.dropRight(1).foldRight(Node(items.last, Seq.empty)) { case (rule, totalN) => Node(rule, Seq.empty).insert(totalN) }
    }.foldLeft(new Script(Seq.empty)) { case (res, example) => res.insert(example) }
  }
}
