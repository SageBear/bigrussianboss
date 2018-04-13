package com.sagebear

import scala.util.matching.Regex

class Bio(val items: List[Bio.Elementary]) {
  def ::(that: Bio.Elementary): Bio = new Bio(that :: this.items)

  def :::(that: Bio): Bio = new Bio(that.items ::: this.items)

  override def toString: String = items.mkString("\n")
}

object Bio {
  /**
    * Tokenizer to detect bio-parts in text
    */
  val defaultTokenizer: Regex =
    """(?U)[\w]+|[‑–—“”€№…’\"#$%&\'()+,-.\/:;<>?]""".r

  /**
    * Elementary part of Bio
    *
    * @param content - string
    * @param tag     - bioTag to the content
    */
  case class Elementary(content: String, tag: String) {
    override def toString: String = content + " " + tag
  }

  /**
    * Creates empty Bio
    *
    * @return Bio
    */
  def empty = new Bio(List.empty[Elementary])

  def apply(bios: Iterable[Bio]): Bio = bios.foldRight(empty) {
    (bio, acc) => bio ::: acc
  }

  /**
    * Creates bio with "O" tag and [[defaultTokenizer]]
    *
    * @param string - text to be bio-tagged
    * @return Bio
    */
  def apply(string: String): Bio = {
    apply(string, defaultTokenizer)
  }

  /**
    * Creates bio with "O" tag with custom tokenizer
    *
    * @param string    - text to be bio-tagged
    * @param tokenizer - tokenizer to recognise parts of text
    * @return Bio
    */
  def apply(string: String, tokenizer: Regex): Bio = {
    new Bio(tokenizer.findAllIn(string).map(Elementary(_, "O")).toList)
  }

  /**
    * Creates bio with tag and tokenizer
    *
    * @param string    - text to be bio-tagged
    * @param tag       - bioTag of the string
    * @param tokenizer - tokenizer to recognise parts of text
    * @return Bio
    * @note The first part will have "B-`tag`" bioTag and the rest will have "I-`tag`" bioTag
    */
  def apply(string: String, tag: String, tokenizer: Regex = defaultTokenizer): Bio = {
    val tokens = tokenizer.findAllIn(string).toList
    Elementary(tokens.head, "B-" + tag) :: new Bio(tokens.tail.map(Elementary(_, "I-" + tag)))
  }
}