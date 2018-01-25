package com.sagebear.bigrussianboss

import com.sagebear.bigrussianboss.Syntax.{Rule, Пример}

class Script(protected val children: Seq[Script.Node]) extends Script.TreeMixin[Script] {
  protected def children(children: Seq[Script.Node]) = new Script(children)

  override def toString: String = {
    val SPACE_WIDTH = 4
    def center(node: Script.Node): Int = node.nodes.count(_.children.isEmpty) / 2 * SPACE_WIDTH
    def line(nodes: Seq[Script.Node], acc: String = ""): String =
      if (nodes.isEmpty) acc
      else {
        val halfWidth = center(nodes.head)
        line(nodes.tail, acc + " " * halfWidth + nodes.head.value.action + " " * halfWidth)
      }
    def levels(nodes: Seq[Script.Node], acc: Seq[Seq[Script.Node]]): Seq[Seq[Script.Node]] =
      if (nodes.isEmpty) acc
      else levels(nodes.flatMap(_.children), acc :+ nodes)

    levels(children, Seq.empty).foldLeft("") { case (acc, level) => acc + "\n" + line(level) }
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
