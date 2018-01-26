package com.sagebear.bigrussianboss

object Extensions {
  implicit class SetExtension[T](set: Set[T]) {
    def choose: Option[T] = scala.util.Random.shuffle(set).headOption
  }

  implicit class StringExtensions(str: String) {
    def wrap(len: Int): String = if (str.length > len) str.dropRight(3) + "..." else str
  }
}
