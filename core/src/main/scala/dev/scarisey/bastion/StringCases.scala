/*
 * Copyright 2020 dev.scarisey
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package dev.scarisey.bastion

/**
 * Internal methods to compute string cases.
 */
object StringCases {

  private val regexp = "(?:(_*[a-z]|_+)([a-z0-9]*)|(_*[A-Z]|_+)([a-z0-9]+|[A-Z]*))".r

  private def capitalise(str: String): String =
    if (str.isEmpty) str
    else str.take(1).toUpperCase + str.tail

  val transformations: List[Array[String] => String] = List(
    camelCase,
    capitalDotCase,
    capitalHyphenCase,
    capitalSnakeCase,
    capitalWords,
    dotCase,
    hyphenCase,
    lowerCamelCase,
    lowerCase,
    snakeCase,
    upperCase,
    upperDotCase,
    upperHyphenCase,
    upperSnakeCase,
    upperWords,
    words
  )

  def tokenize(s: String): Array[String] = {
    val res1 = regexp
      .findAllMatchIn(s)
      .map { m =>
        if (m.group(1) != null)
          s"${m.group(1)}_${m.group(2)}"
        else
          s"${m.group(3)}_${m.group(4)}"
      }
      .toArray
    val res2 = res1.map(s => s.split("_").mkString)
    res2.filter(s => s != "")
  }

  /**
  Capital_Snake_Case
   */
  def capitalSnakeCase(ss: Array[String]): String = ss.map(capitalise).mkString("_")

  /**
  Capital-Hyphen-Case
   */
  def capitalHyphenCase(ss: Array[String]): String = ss.map(capitalise).mkString("-")

  /**
  Capital.Dot.Case
   */
  def capitalDotCase(ss: Array[String]): String = ss.map(capitalise).mkString(".")

  /**
  Capital Words
   */
  def capitalWords(ss: Array[String]): String = ss.map(capitalise).mkString(" ")

  /**
  CamelCase
   */
  def camelCase(ss: Array[String]): String = ss.map(capitalise).mkString

  /**
  UPPERCASE
   */
  def upperCase(ss: Array[String]): String = ss.mkString.toUpperCase

  /**
  lowercase
   */
  def lowerCase(ss: Array[String]): String = ss.mkString.toLowerCase

  /**
  snake_case
   */
  def snakeCase(ss: Array[String]): String = ss.map(_.toLowerCase).mkString("_")

  /**
  UPPER_SNAKE_CASE
   */
  def upperSnakeCase(ss: Array[String]): String = ss.map(_.toUpperCase).mkString("_")

  /**
  hyphen-case
   */
  def hyphenCase(ss: Array[String]): String = ss.map(_.toLowerCase).mkString("-")

  /**
  UPPER-HYPHEN-CASE
   */
  def upperHyphenCase(ss: Array[String]): String = ss.map(_.toUpperCase).mkString("-")

  /**
  dot.case
   */
  def dotCase(ss: Array[String]): String = ss.map(_.toLowerCase).mkString(".")

  /**
  UPPER.DOT.CASE
   */
  def upperDotCase(ss: Array[String]): String = ss.map(_.toUpperCase).mkString(".")

  /**
  some words
   */
  def words(ss: Array[String]): String = ss.map(_.toLowerCase).mkString(" ")

  /**
  UPPER WORDS
   */
  def upperWords(ss: Array[String]): String = ss.map(_.toUpperCase).mkString(" ")

  /**
  lowerCamelCase
   */
  def lowerCamelCase(ss: Array[String]): String =
    if (ss.length > 1) {
      s"${ss.head}${ss.tail.map(capitalise).mkString}"
    } else {
      ss.map(_.toLowerCase).mkString
    }

}
