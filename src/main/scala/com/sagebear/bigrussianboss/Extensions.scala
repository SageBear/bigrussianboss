package com.sagebear.bigrussianboss

object Extensions {
  implicit class SetExtension[T](set: Set[T]) {
    def choose: Option[T] = scala.util.Random.shuffle(set).headOption
  }
}
