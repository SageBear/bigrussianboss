package com.sagebear.bigrussianboss.bot
import com.sagebear.Extensions._
import com.sagebear.bigrussianboss.Script
import com.sagebear.bigrussianboss.bot.SensorsAndActuators.{CanNotDoThis, DoNotUnderstand}
import com.sagebear.bigrussianboss.intent.Intents._
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Random, Try}

/**
  * @author vadim
  * @since 01.02.2018
  */
class ReturnRuleBased(private val context: Map[String, String], conf: Config, rnd: Random) extends SensorsAndActuators {

  private def reflex[T](action: Script.Action, subs: (Set[String], Seq[String]) => T): T = PartialFunction[Script.Action, T] {
    case Hello => subs(Set("Здравствуйте", "Добрый день", "Привет", "Доброго времени суток"), Seq.empty)
    case Bye => subs(Set("До свидания", "Пока"), Seq.empty)

    case Нет => subs(Set("Нет", "Ни в коем случае", "Неа"), Seq.empty)
    case Да => subs(Set("Да", "Конечно", "Так точно", "А как же"), Seq.empty)

    case Информацию_про_цель_визита => subs(
      Set("Недавно у вас покупал товар, и меня он не устраивает как его вернуть?",
          "На днях купил товар, и хочу вернуть"), Seq.empty)

    case Информацию_место_покупки_магазин => subs(
      Set("Покупал у вас в магазине",
          "В магазине",
          "Да вот возле дома"), Seq.empty)

    case Информацию_о_возврате_технически_сложного_товара => subs(
      Set("Технически сложный товар можно вернуть оставив заявку в магазине",
          "Вам придется прийти в магазин и оставить заявку",
          "В вашем случае пишется заявление в магазине",
          "Необходимо написать заявление в магазине"), Seq.empty)

    case Информацию_о_возврате_технически_не_сложного_товара => subs(
      Set("Просто приходите в магазин и обмениваете товар",
          "Тогда можно вернуть там же, где и купили без каких либо проблем",
          "Приходите в любой магазин, там вам смогут обменять"), Seq.empty)

    case Информацию_о_возврате_товара_когда_устраивает_качество => subs(
      Set("Подумайте, стоит ли вам менять этот товар, если все таки стоит, то можете подойти в магазин",
          "С чеком приходите в магазин"), Seq.empty)

    case Информацию_место_покупки_онлайн => subs(
      Set("На сайте заказывал",
          "В онлайне брал",
          "Выбирал на вашем сайте, забирал в магазине"), Seq.empty)

    case Информацию_о_возврате_товара_при_покупке_онлайн => subs(
      Set("Вы можете остаить заявку возврата на сайте, но все равно придется прийти в магазин для обмена",
          "Найдите номар заказа и с ним приходите в магазин для совершения обмена"), Seq.empty)

    case Вопрос_про_место_покупки_товара => subs(
      Set("Подскажите где был куплен товар в магазине или онлайн?",
          "Покупали в магазине или на сайте?"), Seq.empty)

    case Вопрос_устраивает_ли_качество_товара => subs(
      Set("Вас устраивает сам товар?",
          "Качество самого товара вас устраивает?",
          "Вы удовлетворены качеством товара?"), Seq.empty)

    case Вопрос_является_ли_технически_сложным_товаром => subs(
      Set("Является ли товар технически сложным?",
          "Ваш товар технически сложный?",
          "Технически сложный товар, верно?"), Seq.empty)

  }.applyOrElse(action, (_: Script.Action) => subs(Set.empty, Seq.empty))

  override def observe(text: String)(a: Script.Action)(implicit ec: ExecutionContext): Future[ReturnRuleBased] =
    a match {
      case And(q1, q2) =>
        for {
          b1 <- observe(text)(q1)
          b2 <- observe(text)(q2)
        } yield new ReturnRuleBased(b1.context ++ b2.context, conf, rnd)

      case _ =>
        val txt = text.toLowerCase

        def subs(texts: Set[String], v: Seq[String]): Option[Map[String, String]] = {
          val patterns = texts.map(_.toLowerCase.replace("%s","(.+)").r(v: _*))
          patterns.map(_.findFirstMatchIn(txt)).collectFirst {
            case Some(res) => v.map(arg => arg -> res.group(arg)).toMap
          }
        }
        reflex(a, subs).fold[Future[ReturnRuleBased]](Future.failed(DoNotUnderstand)) { args => Future(new ReturnRuleBased(context ++ args, conf, rnd)) }
    }

  override def act(a: Script.Action)(implicit ec: ExecutionContext): Future[String] = a match {
    case And(q1, q2) =>
      for {
        q1u <- act(q1)
        q2u <- act(q2)
      } yield s"$q1u и $q2u"

    case _ =>
      def subs(texts: Set[String], args: Seq[String]): Set[String] = if (args.forall(context.contains)) {
        val values = args.map(context)
        texts.map(_.format(values: _*))
      } else Set.empty
      val t = reflex(a, subs).choose(rnd).fold[Future[String]](Future.failed(CanNotDoThis))(s => Future(s))
      t
  }
}

object ReturnRuleBased {
  def client(implicit rnd: Random = Random): Try[ReturnRuleBased] = Try(new ReturnRuleBased(Map.empty, ConfigFactory.load(), rnd))
  def operator(implicit rnd: Random = Random): Try[ReturnRuleBased] = Try(new ReturnRuleBased(Map.empty, ConfigFactory.load(), rnd))
}
