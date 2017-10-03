// Copyright (c) 2013-2017 Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.syntax

import cats._
import cats.data._
import cats.implicits._
import doobie._, doobie.implicits._
import shapeless.test.illTyped
import shapeless.HNil
import org.specs2.mutable.Specification

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object stringspec extends Specification {

  "sql interpolator" should {

    "support no-param queries" in {
      val q = sql"foo bar baz".query[Int]
      q.sql must_=== "foo bar baz"
    }

    "support atomic types" in {
      val a = 1
      val b = "two"
      val q = sql"foo $a bar $b baz".query[Int]
      q.sql must_=== "foo ? bar ? baz"
    }

    "handle leading params" in {
      val a = 1
      val q = sql"$a bar baz".query[Int]
      q.sql must_=== "? bar baz"
    }

    "support trailing params" in {
      val b = "two"
      val q = sql"foo bar $b".query[Int]
      q.sql must_=== "foo bar ?"
    }

    "not support product params" in {
      Composite[(Int, String)]
      illTyped(""" val a = (1, "two"); sql"foo $a bar baz".query[Int] """)
      true
    }

  }

  "Fragment interpolator" should {

    val columnsFragment = fr0"cars.car_id, cars.color, cars.manufacturer, cars.year"
    val carIds: NonEmptyList[Int] = NonEmptyList.of(1,2)
    val carColorCheck = fr0"cars.color = 'blue'"

    "substitute fragment placeholders properly" in {
      val q: Fragment = fr"""
SELECT cars.car_id, cars.color, cars.manufacturer, cars.year
FROM cars
WHERE cars.car_id IN (?, ?) AND cars.color = 'blue'"""
      val qInterpolated: Fragment = fr"""
SELECT ${columnsFragment}
FROM cars
${Fragments.whereAnd(Fragments.in(fr"cars.car_id", carIds), carColorCheck)}
"""

      // There are some whitespace differences here. to illustrate that this sort-of works ignore them for now.
      q.query[HNil].sql.replace("  ", " ").trim must_== qInterpolated.query[HNil].sql.replace("  ", " ").trim
    }
  }

}
