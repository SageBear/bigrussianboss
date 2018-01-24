package com.sagebear.bigrussianboss

import com.sagebear.bigrussianboss.Syntax.{Rule, Пример}

trait DialogTree {
  val children: Seq[DialogTree]
  //TODO: ordering
  def insert(node: DialogTree): DialogTree = node match {
    case n: Node => insertChild(n)
    case StartNode(ch) => ch.foldLeft[DialogTree](this) { case (acc, elem) => acc.insert(elem) }
  }

  protected def insertChild(child: DialogTree): DialogTree
}
case class StartNode(children: Seq[DialogTree]) extends DialogTree {
  override protected def insertChild(child: DialogTree): DialogTree = StartNode(children :+ child)
}

case class Node(children: Seq[DialogTree], value: Rule[_]) extends DialogTree {
  override protected def insertChild(child: DialogTree): DialogTree = Node(children :+ child, value)
}

object DialogTree {
  def apply(items: Seq[Пример]): DialogTree = ???

  def parse(items: Seq[Пример]) = {
    for {
      i <- items
    } yield (i.items.head, i.items.tail)
  }
}
