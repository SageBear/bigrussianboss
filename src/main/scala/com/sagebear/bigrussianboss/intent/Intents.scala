package com.sagebear.bigrussianboss.intent

import com.sagebear.bigrussianboss.Script.Action

/**
  * @author vadim
  * @since 30.01.2018
  */
object Intents {
  case class And(q1: Action, q2: Action) extends Action

  case object Hello extends Action
  case object Bye extends Action
  case object Глупости extends Action

  case object Вопрос_про_покупку_пива extends Action
  case object Вопрос_про_адрес extends Action
  case object Вопрос_про_телефон extends Action

  case object Информацию_про_свой_адрес extends Action
  case object Информацию_про_свой_телефон extends Action
  case object Информацию_где_купить_пиво extends Action
}
