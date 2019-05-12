package com.raasahsan.crony

import java.time.LocalTime

import Range.Hours
import com.raasahsan.crony.internals.{RangeBetween, RangeNext}

final case class Range(hours: Hours)

object Range {

  val everySecond: Seconds =
    Seconds((0 to 59).toList)

  val everyMinute: Minutes =
    minuteOf(everySecond)

  val everyHour: Hours =
    hourOf(everyMinute)

  val empty: Hours =
    Hours(List())

  val all: Hours =
    everyHour

  def hourOf(minutes: Minutes): Hours =
    Hours((0 to 23).toList.map((_, minutes)))

  def minuteOf(seconds: Seconds): Minutes =
    Minutes((0 to 59).toList.map((_, seconds)))

  def hour(n: Int): Hours =
    Hours(List(n).map((_, everyMinute)))

  def minute(n: Int): Minutes =
    Minutes(List(n).map((_, everySecond)))

  def second(n: Int): Seconds =
    Seconds(List(n))

  private def combineMap[A, B](m: Map[A, B], n: Map[A, B]): Map[A, List[B]] = {
    val firstMap = m.foldLeft(Map[A, List[B]]()) { case (acc, (a, b)) =>
      acc + (a -> List(b))
    }

    val secondMap = n.foldLeft(firstMap) { case (acc, (a, b)) =>
      acc.get(a) match {
        case Some(v) => acc + (a -> (b :: v))
        case None => acc + (a -> List(b))
      }
    }

    secondMap
  }

  final case class Hours(array: List[(Int, Minutes)]) {

    def between(start: LocalTime, end: LocalTime): List[LocalTime] = {
      RangeBetween(this, start, end)
    }

    def next(localTime: LocalTime): Option[LocalTime] = {
      RangeNext(this, localTime)
    }

    // When both numbers exist, union the minutes.
    // When either exists, keep both.
    def ||(that: Hours): Hours = {
      val newArray = combineMap(array.toMap, that.array.toMap)
        .map { case (hour, minutes) =>
          minutes match {
            case a :: b :: Nil => (hour, a || b)
            case a :: Nil => (hour, a)
            case _ => null
          }
        }
        .toList
        .sortBy(_._1)

      Hours(newArray)
    }

    def &&(that: Hours): Hours = {
      val newArray = combineMap(array.toMap, that.array.toMap)
        .filter(_._2.length == 2)
        .map { case (hour, minutes) =>
          minutes match {
            case a :: b :: Nil => (hour, a && b)
            case _ => null
          }
        }
        .filter(_._2.nonEmpty)
        .toList
        .sortBy(_._1)

      Hours(newArray)
    }

    // Replace every non-existing hour with a full minute.
    // Replace all the minute in an existing hour with the complement of its minute
    def not: Hours = {
      val a = array
        .map { case (i, minutes) => (i, minutes.not) }
        .filter(_._2.nonEmpty)

      val b = ((0 to 23).toSet -- array.map(_._1).toSet)
        .toList
        .map(i => (i, everyMinute))

      val newArray = (a ++ b).sortBy(_._1)

      Hours(newArray)
    }

  }

  final case class Minutes(array: List[(Int, Seconds)]) {

    // TODO: Option[Int]
    def first: (Int, Seconds) =
      array.head

    def after(n: Int): List[(Int, Seconds)] =
      array.dropWhile(_._1 <= n)

    def nonEmpty: Boolean =
      array.nonEmpty

    def ||(that: Minutes): Minutes = {
      val newArray = combineMap(array.toMap, that.array.toMap)
        .map { case (minute, seconds) =>
          seconds match {
            case a :: b :: Nil => (minute, a || b)
            case a :: Nil => (minute, a)
            case _ => null
          }
        }
        .toList
        .sortBy(_._1)

      Minutes(newArray)
    }

    def &&(that: Minutes): Minutes = {
      val newArray = combineMap(array.toMap, that.array.toMap)
        .filter(_._2.length == 2)
        .map { case (minute, seconds) =>
          seconds match {
            case a :: b :: Nil => (minute, a && b)
            case _ => null
          }
        }
        .filter(_._2.nonEmpty)
        .toList
        .sortBy(_._1)

      Minutes(newArray)
    }

    // Replace every non-existing minute with a full second.
    // Replace all the second in an existing minute with the complement of its seconds
    def not: Minutes = {
      val a = array
        .map { case (i, seconds) => (i, seconds.not) }
        .filter(_._2.nonEmpty)

      val b = ((0 to 59).toSet -- array.map(_._1).toSet)
        .toList
        .map(i => (i, everySecond))

      val newArray = (a ++ b).sortBy(_._1)

      Minutes(newArray)
    }

  }

  final case class Seconds(array: List[Int]) {

    // TODO: Option[Int]
    def first: Int =
      array.head

    def after(n: Int): Option[Int] =
      array.dropWhile(_ <= n).headOption

    def nonEmpty: Boolean =
      array.nonEmpty

    def ||(that: Seconds): Seconds = {
      val newArray = array.toSet.union(that.array.toSet)
        .toList
        .sortBy(identity)

      Seconds(newArray)
    }

    def &&(that: Seconds): Seconds = {
      val newArray = array.toSet.intersect(that.array.toSet)
        .toList
        .sortBy(identity)

      Seconds(newArray)
    }

    def not: Seconds = {
      val newArray = ((0 to 59).toSet -- array).toList.sortBy(identity)

      Seconds(newArray)
    }

  }

}
