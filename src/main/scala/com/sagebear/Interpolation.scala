package com.sagebear

import com.sagebear.Bio.{Bio, Elementary}

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object Interpolation {
  implicit def stringToInterpolationString(str: String): Interpolation = new Interpolation(str)
  def apply(str: String) = new Interpolation(str)
}

class Interpolation(interpolatedString: String) {
  private val special = """[()*.?]""".r
  private val varPattern: Regex = """\&(\w{1,})|\&\{([^\}]{1,})\}""".r

  private lazy val names = varPattern.findAllMatchIn(interpolatedString).foldRight(List.empty[String]) {
    (reg, acc) => getVariableName(reg) :: acc
  }
  private lazy val pattern: Regex = {
    ("(?iU)" + varPattern.replaceAllIn(escapeSpecials(interpolatedString), "(.*)")).r
  }

  private def escapeSpecials(text: String): String = {
    special.replaceAllIn(interpolatedString, """\\""" + _)
  }

  private def getVariableName(variable: Match): String = {
    if (variable.group(1) != null)
      variable.group(1)
    else
      variable.group(2)
  }

  def put(vars: Map[String, String] = Map.empty): Option[Interpolated] = {
    Some(Interpolated(this, varPattern.replaceAllIn(interpolatedString, (replacer) => {
      vars.getOrElse(getVariableName(replacer), return None)
    })))
  }

  def get(text: String): Option[Map[String, String]] = {
    pattern.findFirstMatchIn(text) match {
      case Some(matcher) =>
        Some(names.zipWithIndex.foldRight(Map.empty[String, String]) {
          case ((name, i), acc) => acc + (name -> matcher.group(i + 1))
        })
      case _ => None
    }
  }

  def bio(text: String): Option[Bio] = {
    pattern.findFirstMatchIn(text) match {
      case Some(matcher) =>
        val varBio: List[Bio] = names.zipWithIndex.map {
          case (name, i) => Bio(matcher.group(i + 1), name)
        }
        val otherBio: List[Bio] = varPattern.split(interpolatedString).map(Bio(_, "O", single=false)).toList

        Some(otherBio.zipAll(varBio, Bio.empty, Bio.empty).foldLeft(Bio.empty) {
          case (acc, (oBio, vBio)) => oBio +: vBio +: acc
        })
      case _ => None
    }
  }
}

case class Interpolated(interpolation: Interpolation, content: String) {
  def data(): Map[String, String] = interpolation.get(content).get
  def bio(): Bio = interpolation.bio(content).get
}