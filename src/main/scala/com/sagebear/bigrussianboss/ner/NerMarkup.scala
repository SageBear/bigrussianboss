package com.sagebear.bigrussianboss.ner

/**
  * @author vadim
  * @since 30.01.2018
  */
object NerMarkup {
  sealed trait NerTag

  trait NerClass {
    val inTag: NerTag
    val beginTag: NerTag
  }

  case object Address extends NerClass {
    case object BeginAddress extends NerTag
    case object InAddress extends NerTag

    override val inTag: NerTag = InAddress
    override val beginTag: NerTag = BeginAddress
  }

  case object Phone extends NerClass {
    case object BeginPhone extends NerTag
    case object InPhone extends NerTag

    override val inTag: NerTag = InPhone
    override val beginTag: NerTag = BeginPhone
  }

  case object Other extends NerClass {
    case object OtherT extends NerTag

    override val inTag: NerTag = OtherT
    override val beginTag: NerTag = OtherT
  }
}
