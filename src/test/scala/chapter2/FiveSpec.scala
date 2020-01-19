package chapter2

import org.scalatest.{FlatSpec, Matchers}

class FiveSpec extends FlatSpec with Matchers {
 "compose" should "compose concat and then upper case" in {
   Five.compose((str: String) => str.toUpperCase, (str: String) => str.concat(" world"))("hello") shouldEqual "HELLO WORLD"
 }
  "compose" should "upper case and then concat" in {
    Five.compose((str: String) => str.concat(" world"), (str: String) => str.toUpperCase)("hello") shouldEqual "HELLO world"
  }
}
