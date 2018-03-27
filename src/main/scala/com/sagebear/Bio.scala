package com.sagebear

object Bio {
  private val tokenizer = """(?U)[\w]+|[‑–—“”€№…’\"#$%&\'()+,-.\/:;<>?]""".r

  case class Elementary(str: String, tag: String) {
    override def toString: String = str + " " + tag
  }

  case class Bio(elems: List[Elementary]) {
    override def toString: String = elems.mkString("\n")

    def +:(that: Bio): Bio = Bio(that.elems ::: this.elems)
  }

  def empty = Bio(List.empty[Elementary])

  def apply(str: String, name: String, single: Boolean = true): Bio = {
    val tokens = tokenizer.findAllIn(str).filter(_ != "").toList

    if (single) {
      Bio(Elementary(tokens.head, "B-" + name) :: tokens.tail.map(Elementary(_, "I-" + name)))
    } else {
      Bio(tokens.map(Elementary(_, name)))
    }
  }
}