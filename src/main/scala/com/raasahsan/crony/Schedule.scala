package com.raasahsan.crony

import java.time.LocalDateTime

import com.raasahsan.crony.Range.Hours
import com.raasahsan.crony.internals._

/**
 * A `Schedule` is an abstraction that represents a subset of the set of
 * all moments in time, denoted T.
 */
sealed abstract class Schedule {

  import Schedule._

  /**
   * Returns a `Schedule` that contains all moments in time that are both in
   * this `Schedule` and `that`. Represents the intersection of both schedules.
   *
   * [[&&]] is commutative and associative.
   *
   * Forms a monoid over `Schedule` with `Schedule.all` as the identity element.
   */
  final def &&(that: Schedule): Schedule =
    Intersection(this, that)

  /**
   * Returns a `Schedule` that contains all moments in time contained in either
   * this `Schedule` or `that`. Represents the union of both schedules.
   *
   * [[||]] is commutative and associative.
   *
   * Forms a monoid over `Schedule` with `Schedule.never` as the identity element.
   */
  final def ||(that: Schedule): Schedule =
    Union(this, that)

  /**
   * Returns a `Schedule` that is the complement of this `Schedule`. Represents
   * the complement of this schedule.
   *
   * [[not]] is its own inverse.
   */
  final def not: Schedule =
    Complement(this)

  /**
   * Checks whether the `LocalDateTime` is contained in this `Schedule`.
   */
  final def in(localDateTime: LocalDateTime): Boolean =
    ScheduleIn(this, localDateTime)

  /**
   * Produces the next valid `LocalDateTime` in this `Schedule`, if it exists.
   */
  final def next(dateTime: LocalDateTime): Option[LocalDateTime] =
    ScheduleNext.naive(this, dateTime)

  /**
   * Returns a list of all valid `LocalDateTime` in this `Schedule`/
   */
  final def between(start: LocalDateTime, end: LocalDateTime): List[LocalDateTime] =
    ScheduleBetween(this, start, end)

  /**
   * Returns an equivalent, but simpler schedule that has been term reduced by applying set laws and properties.
   */
  final def rewrite: Schedule =
    ScheduleRewrite(this)

  final def range: Hours =
    ScheduleRange(this)

}

object Schedule extends ScheduleValues {

  /**
   * Returns a `Schedule` that is equivalent to the empty set.
   */
  def never: Schedule =
    Never

  /**
   * Returns a `Schedule` that is equivalent to T.
   */
  def all: Schedule =
    All

  /**
   * Returns a `Schedule` that represents the subset of T that contains all moments
   * in time where the second hand is `n`.
   */
  def second(n: Int): Schedule =
    Second(n)

  /**
   * Returns a `Schedule` that represents the subset of T that contains all moments
   * in time where the minute hand is `n`.
   */
  def minute(n: Int): Schedule =
    Minute(n)

  /**
   * Returns a `Schedule` that represents the subset of T that contains all moments
   * in time where the hour hand is `n`.
   */
  def hour(n: Int): Schedule =
    Hour(n)

  /**
   * Returns a `Schedule` that represents the subset of T that contains all moments
   * in time where the day of the week is `n`.
   */
  def dayOfWeek(n: Int): Schedule =
    DayOfWeek(n)

  /**
   * Returns a `Schedule` that represents the subset of T that contains all moments
   * in time where the day of the month is `n`.
   */
  def dayOfMonth(n: Int): Schedule =
    DayOfMonth(n)

  /**
   * Returns a `Schedule` that represents the subset of T that contains all moments
   * in time where the day of the year is `n`.
   */
  def dayOfYear(n: Int): Schedule =
    DayOfMonth(n)

  /**
   * Returns a `Schedule` that represents the subset of T that contains all moments
   * in time where the month is `n`.
   */
  def month(n: Int): Schedule =
    Month(n)

  /**
   * Returns a `Schedule` that represents the subset of T that contains all moments
   * in time where the year is `n`.
   */
  def year(n: Int): Schedule =
    Year(n)

  /**
   * Returns a `Schedule` that represents the subset of T that contains all moments
   * in time where the year is a leap year.
   */
  def leapYear: Schedule =
    LeapYear

  /**
   * Returns a `Schedule` that represents the subset of T that contains all moments
   * in time during the day passed in.
   */
  def localDate(month: Int, dayOfMonth: Int): Schedule =
    this.month(month) && this.dayOfMonth(dayOfMonth)

  private[crony] case object Never extends Schedule

  private[crony] case object All extends Schedule

  private[crony] final case class Intersection(left: Schedule, right: Schedule) extends Schedule

  private[crony] final case class Union(left: Schedule, right: Schedule) extends Schedule

  private[crony] final case class Complement(value: Schedule) extends Schedule

  private[crony] final case class Second(n: Int) extends Schedule

  private[crony] final case class Minute(n: Int) extends Schedule

  private[crony] final case class Hour(n: Int) extends Schedule

  private[crony] final case class DayOfWeek(n: Int) extends Schedule

  private[crony] final case class DayOfMonth(n: Int) extends Schedule

  private[crony] final case class DayOfYear(n: Int) extends Schedule

  private[crony] final case class Month(n: Int) extends Schedule

  private[crony] final case class Year(n: Int) extends Schedule

  private[crony] case object LeapYear extends Schedule

}

// TODO: Move all these to a separate submodule that will be published separately

private[crony] trait ScheduleValues extends DaysOfWeek with Months {

  import Schedule._

  /**
   * A `Schedule` that represents every midnight.
   */
  val midnight: Schedule =
    hour(0) && minute(0) && second(0)

  /**
   * A `Schedule` that represents every noon.
   */
  val noon: Schedule =
    hour(12) && minute(0) && second(0)

  /**
   * A `Schedule` that represents two given days of a month.
   */
  def semimonthly(first: Int, second: Int): Schedule =
    dayOfMonth(first) || dayOfMonth(second)

}

private[crony] trait DaysOfWeek {

  import Schedule._

  val Sunday: Schedule =
    dayOfWeek(0)

  val Monday: Schedule =
    dayOfWeek(1)

  val Tuesday: Schedule =
    dayOfWeek(2)

  val Wednesday: Schedule =
    dayOfWeek(3)

  val Thursday: Schedule =
    dayOfWeek(4)

  val Friday: Schedule =
    dayOfWeek(5)

  val Saturday: Schedule =
    dayOfWeek(6)
}

private[crony] trait Months {

  import Schedule._

  val January: Schedule =
    month(1)

  val February: Schedule =
    month(2)

  val March: Schedule =
    month(3)

  val April: Schedule =
    month(4)

  val May: Schedule =
    month(5)

  val June: Schedule =
    month(6)

  val July: Schedule =
    month(7)

  val August: Schedule =
    month(8)

  val September: Schedule =
    month(9)

  val October: Schedule =
    month(10)

  val November: Schedule =
    month(11)

  val December: Schedule =
    month(12)

}
