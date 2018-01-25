package com.sagebear.bigrussianboss

object Syntax {
  import Extensions._

  sealed trait Subject {
    def приветствует: Rule[Unit] = Rule(this, Hello)
    def прощается: Rule[Unit] = Rule(this, Bye)
    def спрашивает(question: Utterance[Unit]): Rule[Unit] = Rule(this, question)
    def говорит[T](answer: Utterance[T]): Rule[T] = Rule(this, answer)
    def отвечает[T](answer: Utterance[T]): Rule[T] = говорит(answer)
  }

  case object Клиент extends Subject
  case object Оператор extends Subject

  sealed trait Utterance[T] {
    def generate(params: T): String
    def parse(candidate: String): Option[T]
  }

  trait SimpleUtterance extends Utterance[Unit] {
    val utterances: Set[String]

    def и(q: SimpleUtterance): SimpleUtterance = if (q.hashCode() > this.hashCode()) QuestionAnd(q, this) else QuestionAnd(this, q)

    def generate(void: Unit): String = utterances.choose.head
    def parse(candidate: String): Option[Unit] = if (utterances.contains(candidate)) Some(()) else None
  }

  case object Про_покупку_пива extends SimpleUtterance {
    val utterances: Set[String] = Set("пивчан хочу!", "где мне попить пива?", "где найти пива?")
  }
  case object Адрес extends SimpleUtterance {
    val utterances: Set[String] = Set("где ты живешь?", "скажи свой адрес", "ты с каого района, епта?")
  }
  case object Телефон extends SimpleUtterance {
    val utterances: Set[String] = Set("твой телефон?", "цифры телефона скажи, епта", "твоя мобила?")
  }

  case class QuestionAnd(q1: SimpleUtterance, q2: SimpleUtterance) extends SimpleUtterance {
    val utterances: Set[String] = q1.utterances ++ q2.utterances
  }

  case object Свой_адрес extends Utterance[String] {
    def generate(address: String): String = address
    def parse(utterance: String): Option[String] = ???
  }
  case object Свой_телефон extends Utterance[String] {
    def generate(phone: String): String = phone
    def parse(utterance: String): Option[String] = ???
  }
  case object Где_купить_пиво extends Utterance[(String, String)] {
    def generate(addressAndPhone: (String, String)): String = s"Иди в ближайший к ${addressAndPhone._1} ларек. а еще ответь на смску, я ее послали на ${addressAndPhone._2}"
    def parse(utterance: String): Option[(String, String)] = ???
  }

  case object Hello extends SimpleUtterance {
    val utterances: Set[String] = Set("привет, епта", "здарова, отец", "чо как?")
  }
  case object Bye extends SimpleUtterance {
    val utterances: Set[String] = Set("бывай", "будь", "покеда, епта")
  }
  case object Глупости extends SimpleUtterance {
    val utterances: Set[String] = Set("слоны идут на север", "епта", "коза")
  }

  case class Rule[T](context: Subject, action: Utterance[T])
  case class Пример(items: Rule[_]*)
  def примеры(items: Пример*) = Script(items)
}
