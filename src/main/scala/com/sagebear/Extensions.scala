package com.sagebear

import java.util.MissingFormatArgumentException

import scala.util.Random
import scala.util.matching.Regex

object Extensions {

  implicit class StringTemplate(template: String) {

    /**
      *
      * @param name   - variable name
      * @param bioTag - variable type to create bio
      */
    private case class Variable(name: String, bioTag: String)

    /**
      * Pattern to recognise variables in present [[template]]
      * Note: variable should look like "&{name: bioTag}"
      */
    private val variablePattern: Regex = new Regex("""[&][{](\w+)\s*[:]\s*(\w+)[}]""", "name", "bioTag")

    /**
      * Construct pattern to recognise [[template]] in text
      *
      * @note 1. Ignore cases
      *       2. Work with unicode
      *       3. Escape regex specials *, ?, ., etc.
      */
    private lazy val templatePattern: Regex = {
      new Regex("(?iU)^" + variablePattern.replaceAllIn(
        """[()*.?\[\]]""".r.replaceAllIn(template, """\\""" + _), "(.*)") + "$",
        variables.map(_.name): _*)
    }

    /**
      * Variables from [[template]]
      */
    private lazy val variables: List[Variable] =
      variablePattern.findAllMatchIn(template).foldRight(List.empty[Variable]) {
        (matcher, acc) => Variable(matcher.group("name"), matcher.group("bioTag")) :: acc
      }

    /**
      * Substitute variables into [[template]]
      *
      * @param vars - variables to be substitute
      * @return StringExtension
      * @throws MissingFormatArgumentException - when vars doesn't contain all of [[variables]]
      */
    def substitute(vars: Map[String, String]): String =
      variablePattern.replaceAllIn(template, (replacer) => {
        vars.getOrElse(replacer.group("name"), throw new MissingFormatArgumentException(replacer.group("name")))
      })

    /**
      *
      * @param vars      - variables to be substitute
      * @param tokenizer - tokenizer
      * @return (String, Bio) - substituted string and bio to it
      * @throws MissingFormatArgumentException - when vars doesn't contain all of [[variables]]
      */
    def substituteWithBio(vars: Map[String, String], tokenizer: Regex = Bio.defaultTokenizer): (String, Bio) =
      (substitute(vars), bio(vars, tokenizer))

    /**
      * Collect variables from first entry according to [[templatePattern]] in present string
      *
      * @param string - target text
      * @return Some(Map[String, String]) - of founded information
      *         None - if match was not found
      */
    def parse(string: String): Option[Map[String, String]] = {
      templatePattern.findFirstMatchIn(string) match {
        case Some(matcher) =>
          Some(variables.foldRight(Map.empty[String, String]) {
            (variable, acc) => acc + (variable.name -> matcher.group(variable.name))
          })
        case None => None
      }
    }

    /**
      * Create bio information according to [[template]] and vars
      *
      * @param vars      - variables to be substitute
      * @param tokenizer - bio tokenizer
      * @return Bio to the substituted [[template]]
      * @throws MissingFormatArgumentException - when vars doesn't contain all of [[variables]]
      */
    def bio(vars: Map[String, String], tokenizer: Regex = Bio.defaultTokenizer): Bio = {
      val otherBio = variablePattern.split(template).map(Bio(_, tokenizer))
      val varBio = variables.map((variable) =>
        Bio(vars.getOrElse(variable.name, throw new MissingFormatArgumentException(variable.name)), variable.bioTag, tokenizer))

      otherBio.zipAll(varBio, Bio.empty, Bio.empty).foldRight(Bio.empty) {
        case ((oBio, vBio), acc) => oBio ::: vBio ::: acc
      }
    }

    override def toString: String = template
  }

  implicit class SetExtension[T](set: Set[T]) {
    def choose(rnd: Random): Option[T] = rnd.shuffle(set).headOption
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
    trait Measure {
      def t(threshold:Double, sizeOfA:Int, sizeOfB:Int):Int
      def min(threshold:Double, sizeOfA:Int):Int
      def max(threshold:Double, sizeOfA:Int):Int
    }

    object Cosine extends Measure {
      def t(threshold:Double, sizeOfFeaturesA:Int, sizeOfFeaturesB:Int): Int =
        Math.ceil(threshold * Math.sqrt(sizeOfFeaturesA * sizeOfFeaturesB)).toInt
      def max(threshold:Double, sizeOfFeaturesA:Int):Int = Math.floor(sizeOfFeaturesA/(threshold * threshold)).toInt
      def min(threshold:Double, sizeOfFeaturesA:Int):Int = Math.ceil(threshold * threshold * sizeOfFeaturesA).toInt
    }

    object Jaccard extends Measure {
      def t(threshold:Double, sizeOfFeaturesA:Int, sizeOfFeaturesB:Int): Int =
        Math.ceil( (threshold * (sizeOfFeaturesA + sizeOfFeaturesB)) / (1 + threshold)).toInt
      def max(threshold:Double, sizeOfFeaturesA:Int): Int = Math.floor(sizeOfFeaturesA/threshold).toInt
      def min(threshold:Double, sizeOfFeaturesA:Int): Int = Math.ceil(threshold * sizeOfFeaturesA).toInt
    }

    object Dice extends Measure {
      def t(threshold:Double, sizeOfFeaturesA:Int, sizeOfFeaturesB:Int):Int =
        Math.ceil( 0.5 *  threshold * (sizeOfFeaturesA + sizeOfFeaturesB) ).toInt
      def max(threshold:Double, sizeOfFeaturesA:Int): Int = Math.floor( ((2-threshold)/threshold) * sizeOfFeaturesA ).toInt
      def min(threshold:Double, sizeOfFeaturesA:Int): Int = Math.ceil( (threshold/(2-threshold)) * sizeOfFeaturesA ).toInt
    }
  }
}
