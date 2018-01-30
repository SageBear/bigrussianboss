package com.sagebear

import com.sagebear.Extensions._

/**
  * @author vadim
  * @since 30.01.2018
  */
case class Tree[T](value: T, children: Seq[Tree[T]]) {
  type Node = Tree[T]

  //TODO: ordering
  def insert(node: Node): Node = {
    children.zipWithIndex.find { case (n, _) => n.value == node.value } match {
      case None => Tree(value, children :+ node)
      case Some((ch, i)) =>
        val newChildren = node.children.foldLeft(ch) { case (acc, n) => acc.insert(n) }
        Tree(value, children.updated(i, newChildren))
    }
  }

  private def nodes: Seq[Node] = {
    def expandNode(node: Node, frontier: Seq[Node], acc: Seq[Node]): Seq[Node] = {
      val newFrontier = frontier ++ node.children
      if (newFrontier.isEmpty) node +: acc
      else expandNode(newFrontier.head, newFrontier.tail, node +: acc)
    }
    expandNode(this, Seq.empty, Seq.empty)
  }

  def inspect: String = {
    val CAPTION_LEN = 30
    def level(captions: Seq[(String, Int)], acc: String): String =
      if (captions.isEmpty) acc
      else level(captions.tail,
        acc + " " * ((captions.head._2 - 1) * CAPTION_LEN / 2) +
          captions.head._1.wrap(CAPTION_LEN) +
          " " * ((3 * CAPTION_LEN + 2) / 2 - captions.head._1.length)
      )
    def levels(nodes: Seq[Option[Node]], acc: Seq[String]): Seq[String] =
      if (nodes.forall(_.isEmpty)) acc
      else {
        val procNodes = nodes.map {
          case Some(n) =>
            val nextLevelNodes = if (n.children.isEmpty) Seq(None) else n.children.map(Some(_))
            val nodeCaption = n.value.toString
            val leafsCount = n.nodes.count(_.children.isEmpty)
            ((nodeCaption, leafsCount), nextLevelNodes)
          case None => ((" " * CAPTION_LEN, 1), Seq(None))
        }
        levels(procNodes.flatMap(_._2), acc :+ level(procNodes.map(_._1), ""))
      }
    levels(children.map(Some(_)), Seq.empty).mkString("\n")
  }
}
