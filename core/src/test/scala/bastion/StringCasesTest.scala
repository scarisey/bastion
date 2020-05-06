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

package bastion

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import bastion.StringCases.tokenize

class StringCasesTest extends AnyFlatSpec with Matchers {

  val testValues = Map(
    "Capital0000_Snake_Case" -> Array("Capital0000", "Snake", "Case"),
    "Capital_Snake_Case"     -> Array("Capital", "Snake", "Case"),
    "Capital-Hyphen-Case"    -> Array("Capital", "Hyphen", "Case"),
    "Capital.Dot.Case"       -> Array("Capital", "Dot", "Case"),
    "Capital Words"          -> Array("Capital", "Words"),
    "Camel00Case"            -> Array("Camel00", "Case"),
    "CamelCase"              -> Array("Camel", "Case"),
    "CamelCase02"            -> Array("Camel", "Case02"),
    "UPPERCASE"              -> Array("UPPERCASE"),
    "lowercase00mixed"       -> Array("lowercase00mixed"),
    "snake_case"             -> Array("snake", "case"),
    "snake_case_2"           -> Array("snake", "case", "2"),
    "snake_3_case"           -> Array("snake", "3", "case"),
    "UPPER_SNAKE_CASE"       -> Array("UPPER", "SNAKE", "CASE"),
    "hyphen-case"            -> Array("hyphen", "case"),
    "UPPER-HYPHEN-CASE"      -> Array("UPPER", "HYPHEN", "CASE"),
    "dot.case"               -> Array("dot", "case"),
    "UPPER.DOT.CASE"         -> Array("UPPER", "DOT", "CASE"),
    "some words"             -> Array("some", "words"),
    "UPPER WORDS"            -> Array("UPPER", "WORDS"),
    "lowerCamelCase"         -> Array("lower", "Camel", "Case"),
    "aCamelCase"             -> Array("a", "Camel", "Case")
  )

  it should "tokenise string" in {
    testValues.map { case (k, v) => tokenize(k) shouldEqual v }
  }

//  it should "generate all string representation" in {
//    val res = testValues.map { case (k, v) => tokenize(k) }.flatMap { s => StringCases.transformations.map(f => f(s)) }
//    val str = res.toList.toString()
//    println(str)
//  }

}
