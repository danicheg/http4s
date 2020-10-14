/*
 * Copyright 2013-2020 http4s.org
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.http4s
package parser

import cats.implicits._
import org.http4s.internal.parboiled2._
import org.scalacheck.{Gen, Prop}

final case class BasicRulesParser(input: ParserInput) extends Parser with Rfc2616BasicRules {
  def ListSepOnly = rule(ListSep ~ EOI)
}

class Rfc2616ParserSpec extends Http4sSpec {
  "Rfc2616 parser" should {
    "Parse chars" in {
      Prop.forAll(genChar) { (l: Char) =>
        Either.fromTry(BasicRulesParser(l.toString).Char.run()) must beRight(())
      }
    }
    "Parse octet" in {
      Prop.forAll(genOctet) { (l: Char) =>
        Either.fromTry(BasicRulesParser(l.toString).Octet.run()) must beRight(())
      }
    }
    "Parse crlf" in {
      Prop.forAll(genCrLf) { (l: String) =>
        Either.fromTry(BasicRulesParser(l).CRLF.run()) must beRight(())
      }
    }
    "Parse lws" in {
      Prop.forAll(genLws) { (l: String) =>
        Either.fromTry(BasicRulesParser(l).LWS.run()) must beRight(())
      }
    }
    "Parse text" in {
      Prop.forAll(genText) { (l: String) =>
        Either.fromTry(BasicRulesParser(l).Text.run()) must beRight(())
      }
    }
    "Parse quoted pair" in {
      Prop.forAll(genQuotedPair) { (l: String) =>
        Either.fromTry(BasicRulesParser(l).QuotedPair.run()).map(Some.apply) must beRight(
          l.lastOption)
      }
    }
    "Parse quoted text" in {
      Prop.forAll(genQDText) { (l: String) =>
        Either.fromTry(BasicRulesParser(l).QDText.run()).map(Some.apply) must beRight(l.headOption)
      }
    }
    "Parse quoted string" in {
      Prop.forAll(genQuotedString) { (l: String) =>
        val withoutQuotes = l.substring(1, l.length - 1)
        Either.fromTry(BasicRulesParser(l).QuotedString.run()).map(Some.apply) must (beRight(
          Some(withoutQuotes))
          .or(beRight(withoutQuotes.lastOption.map(_.toString)))
          .or(beRight(Some(""))))
      }
    }
    "Parse list separator" in {
      Prop.forAll(genListSep) { listSep =>
        Either.fromTry(BasicRulesParser(listSep).ListSepOnly.run()) must beRight
      }
    }
    "Not parse list separator without a comma" in {
      Prop.forAll(genOptWs) { listSep =>
        Either.fromTry(BasicRulesParser(listSep).ListSepOnly.run()) must beLeft
      }
    }
    "Not parse list separator with more than one comma" in {
      val genBadListSep =
        for {
          initListSep <- genListSep
          moreCommas <- Gen.nonEmptyListOf {
            Gen.sequence[List[String], String](List(Gen.const(","), genOptWs))
          }
        } yield initListSep + moreCommas.flatten.mkString

      Prop.forAll(genBadListSep) { listSep =>
        Either.fromTry(BasicRulesParser(listSep).ListSepOnly.run()) must beLeft
      }
    }
    "Not parse a non-comma list separator" in {
      val genBadListSep = Gen.asciiPrintableChar.filterNot(_ == ',').map(_.toString)

      Prop.forAll(genBadListSep) { listSep =>
        Either.fromTry(BasicRulesParser(listSep).ListSepOnly.run()) must beLeft
      }
    }
  }
}
