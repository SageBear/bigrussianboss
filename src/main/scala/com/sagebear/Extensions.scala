package com.sagebear

object Extensions {
  implicit class SetExtension[T](set: Set[T]) {
    def choose: Option[T] = scala.util.Random.shuffle(set).headOption
  }

  implicit class StringExtensions(str: String) {
    def wrap(len: Int): String = if (str.length > len) str.dropRight(3) + "..." else str

    def ngram: List[String] ={
      val trigrams = ("££" + str + "££").toCharArray.sliding(3)
      trigrams.map(_.mkString("")).toList
    }
  }

  implicit class CPMerge(sentences: Set[String]) {
    import CPMerge._

    private val words: IndexedSeq[String] = sentences.toIndexedSeq

    private val lookupTable = {
      val ngramSizeWordTriples = words.map(_.ngram).zipWithIndex.flatMap { case (ngrams, wordIndex) =>
        ngrams.map { ngram => (ngram, ngrams.size, wordIndex ) }
      }

      ngramSizeWordTriples.map {
        triple =>
          // ngram_size => wordIndex
          (triple._1 + "_" + triple._2, triple._3)
      }.groupBy(_._1)
        .mapValues(_.map(_._2).toSet)
    }

    private def overlapJoin(features: List[String], minOverlap: Int, sizeOfQuery: Int) = {
      def getMatches(ngram: String) = lookupTable.get(ngram + "_" + sizeOfQuery)

      val candidates = features.flatMap(getMatches).sortBy(_.size)

      /* Given a list counts how many times every item occurs*/
      def countCocurrances(list:List[Int]): Map[Int, Int] = list.groupBy(i => i).map(t => (t._1, t._2.length) )

      val candidatesCounts =  candidates.slice(0, features.size - minOverlap + 1).flatMap(_.toList)
      val narrowedCandidatesSet = candidatesCounts.toSet

      val extraCounts = candidates.slice(features.size - minOverlap + 1, features.size).flatMap { currentMatches =>
        narrowedCandidatesSet.toList.flatMap { word => Some(word).filter(currentMatches.contains) }
      }

      val matches = countCocurrances(candidatesCounts ++ extraCounts).filter{ t => minOverlap <= t._2 }.keySet
      matches
    }

    def search(query: String, threshold: Double = 0.8, measure: Measure = Cosine): Set[String] = {
      val features = query.ngram

      val matchesSize = Range(measure.min(threshold, features.size), measure.max(threshold, features.size) + 1)
      val matches = matchesSize.flatMap {
        l =>
          val minOverlap = measure.t(threshold, features.toSet.size, l)
          overlapJoin(features, minOverlap, l)
      }.toSet

      matches.map(words(_))
    }
  }

  object CPMerge {
    private trait Measure {
      def t(threshold:Double, sizeOfA:Int, sizeOfB:Int):Int
      def min(threshold:Double, sizeOfA:Int):Int
      def max(threshold:Double, sizeOfA:Int):Int
    }

    private object Cosine extends Measure {
      def t(threshold:Double, sizeOfFeaturesA:Int, sizeOfFeaturesB:Int): Int =
        Math.ceil(threshold * Math.sqrt(sizeOfFeaturesA * sizeOfFeaturesB)).toInt
      def max(threshold:Double, sizeOfFeaturesA:Int):Int = Math.floor(sizeOfFeaturesA/(threshold * threshold)).toInt
      def min(threshold:Double, sizeOfFeaturesA:Int):Int = Math.ceil(threshold * threshold * sizeOfFeaturesA).toInt
    }

    private object Jaccard extends Measure {
      def t(threshold:Double, sizeOfFeaturesA:Int, sizeOfFeaturesB:Int): Int =
        Math.ceil( (threshold * (sizeOfFeaturesA + sizeOfFeaturesB)) / (1 + threshold)).toInt
      def max(threshold:Double, sizeOfFeaturesA:Int): Int = Math.floor(sizeOfFeaturesA/threshold).toInt
      def min(threshold:Double, sizeOfFeaturesA:Int): Int = Math.ceil(threshold * sizeOfFeaturesA).toInt
    }

    private object Dice extends Measure {
      def t(threshold:Double, sizeOfFeaturesA:Int, sizeOfFeaturesB:Int):Int =
        Math.ceil( 0.5 *  threshold * (sizeOfFeaturesA + sizeOfFeaturesB) ).toInt
      def max(threshold:Double, sizeOfFeaturesA:Int): Int = Math.floor( ((2-threshold)/threshold) * sizeOfFeaturesA ).toInt
      def min(threshold:Double, sizeOfFeaturesA:Int): Int = Math.ceil( (threshold/(2-threshold)) * sizeOfFeaturesA ).toInt
    }
  }
}
