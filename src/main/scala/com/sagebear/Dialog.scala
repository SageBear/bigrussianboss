package com.sagebear

import com.sagebear.bigrussianboss.Script.{Step, Клиент}


class Dialog(private val list: List[Dialog.Phrase] = List.empty[Dialog.Phrase]) {
  def ::(that: Dialog.Phrase) = new Dialog(that :: list)

  def phrases: List[Dialog.Phrase] = list

  def bio: Bio = Bio(list.map(_.bio))

  override def toString: String = list.mkString("\n")
}

object Dialog {

  case class Phrase(step: Step, content: String, bio: Bio) {
    override def toString: String = (if (step.speaker == Клиент) ">> " else ":: ") + content
  }

  def empty: Dialog = new Dialog(List.empty[Phrase])
}

