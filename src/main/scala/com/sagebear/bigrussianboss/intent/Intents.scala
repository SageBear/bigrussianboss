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


  case object Да extends Action
  case object Нет extends Action

  case object Информацию_про_цель_визита extends Action
  case object Информацию_место_покупки_магазин extends Action
  case object Информацию_о_возврате_технически_сложного_товара extends Action
  case object Информацию_о_возврате_технически_не_сложного_товара extends Action
  case object Информацию_о_возврате_товара_когда_устраивает_качество extends Action
  case object Информацию_место_покупки_онлайн extends Action
  case object Информацию_о_возврате_товара_при_покупке_онлайн extends Action

  case object Вопрос_про_место_покупки_товара extends Action
  case object Вопрос_устраивает_ли_качество_товара extends Action
  case object Вопрос_является_ли_технически_сложным_товаром extends Action
}
