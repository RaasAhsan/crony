package com.raasahsan.crony.internals

import java.time.{LocalDateTime, ZoneId}

import com.raasahsan.crony.Schedule

private[crony] object ScheduleNext {

  import Schedule._

  /*
  Let's start with very strong assumptions about our schedule.
  We have a non-wrapping, ordered set of moments from 00:00:00 to 23:59:59.

  Can we impose any constraints on the tree of our Schedule that will make it easier to inspect?
  Do we need to introduce a new type that will let us track the result of processing?
   */

  def apply(schedule: Schedule, localDateTime: LocalDateTime): Option[LocalDateTime] = {
    /*
      Start with a single node in the tree: Second(n)
      Then a union of Second(n). The previous case can be generalized under the union.

      Can we consider each node in a union separately?
      Take the next result of each node, and return them all up?


      */

    schedule match {
      case Second(n) => {
        // If the current second is less than the specified second, set the second to that one.
        // Otherwise, increase the minute by one.

        if (localDateTime.getSecond < n)
          Some(localDateTime.withSecond(n))
        else
          Some(localDateTime.withSecond(n).plusMinutes(1))
      }

      case Union(Second(a), Second(b)) => {
        // Collect the list of seconds in ascending order. Then take the next one in ascending order.
        // If there is none, then increase the minute and reset.

        val currentSecond = localDateTime.getSecond
        val seconds = List(a, b).sortBy(identity)
        val nextSecond = seconds.dropWhile(_ < currentSecond).headOption

        nextSecond match {
          case Some(n) => Some(localDateTime.withSecond(n))
          case None => Some(localDateTime.withSecond(seconds.head).plusMinutes(1))
        }
      }

      case _ => None
    }
  }

  def apply2(schedule: Schedule, localDateTime: LocalDateTime): Option[LocalDateTime] = {
    def all(schedule: Schedule): List[LocalDateTime] =
      schedule match {
        case Second(n) =>
          if (localDateTime.getSecond < n)
            List(localDateTime.withSecond(n))
          else
            List(localDateTime.plusMinutes(1).withSecond(n))
        case Union(a, b) =>
          all(a) ++ all(b)
        case _ => List()
      }

    val ns = all(schedule)
    ns.sortBy(_.atZone(ZoneId.systemDefault()).toInstant.toEpochMilli).headOption
  }

  @annotation.tailrec
  def naive(schedule: Schedule, localDateTime: LocalDateTime): Option[LocalDateTime] = {
    val next = localDateTime.plusSeconds(1)
    if (schedule.in(next)) {
      Some(next)
    } else {
      naive(schedule, next)
    }
  }

}
