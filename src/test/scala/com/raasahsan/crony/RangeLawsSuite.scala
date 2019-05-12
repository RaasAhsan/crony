package com.raasahsan.crony

import org.scalacheck.{Gen, Prop}
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import Range.Hours

// Range possesses a very set-like nature, so most of these laws are derived from set algebra.
class RangeLawsSuite extends FunSuite with Checkers {

  import Prop.forAll
  import Schedule._

  // TODO: Come up with a better generator here
  // TODO: Include 2-level, 3-level Ranges that have been composed
  val genRange: Gen[Hours] = Gen.oneOf((hour(5) && minute(8)).range, hour(3).range, minute(2).range, second(5).range, never.range, all.range)

  test("union associativity") {
    check {
      forAll(genRange, genRange, genRange) { (a, b, c) =>
        ((a || b) || c) == (a || (b || c))
      }
    }
  }

  test("union commutativity") {
    check {
      forAll(genRange, genRange) { (a, b) =>
        (a || b) == (b || a)
      }
    }
  }

  test("union identity") {
    check {
      forAll(genRange) { a =>
        (a || Range.empty) == a
      }
    }
  }

  test("union domination") {
    check {
      forAll(genRange) { a =>
        (a || Range.all) == Range.all
      }
    }
  }

  test("union complement") {
    check {
      forAll(genRange) { a =>
        (a || a.not) == Range.all
      }
    }
  }

  test("union distributivity over intersection") {
    check {
      forAll(genRange, genRange, genRange) { (a, b, c) =>
        (a || (b && c)) == ((a || b) && (a || c))
      }
    }
  }

  test("union idempotency") {
    check {
      forAll(genRange) { a =>
        (a || a) == a
      }
    }
  }

  test("intersection associativity") {
    check {
      forAll(genRange, genRange, genRange) { (a, b, c) =>
        ((a && b) && c) == (a && (b && c))
      }
    }
  }

  test("intersection commutativity") {
    check {
      forAll(genRange, genRange) { (a, b) =>
        (a && b) == (b && a)
      }
    }
  }

  test("intersection identity") {
    check {
      forAll(genRange) { a =>
        (a && Range.all) == a
      }
    }
  }

  test("intersection domination") {
    check {
      forAll(genRange) { a =>
        (a && Range.empty) == Range.empty
      }
    }
  }

  test("intersection complement") {
    check {
      forAll(genRange) { a =>
        (a && a.not) == Range.empty
      }
    }
  }

  test("intersection distributivity over union") {
    check {
      forAll(genRange, genRange, genRange) { (a, b, c) =>
        (a && (b || c)) == ((a && b) || (a && c))
      }
    }
  }

  test("intersection idempotency") {
    check {
      forAll(genRange) { a =>
        (a && a) == a
      }
    }
  }

}
