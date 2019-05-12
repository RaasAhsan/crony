package com.raasahsan.crony.internals

import java.time.LocalDateTime

import com.raasahsan.crony.Schedule

private[crony] object ScheduleIn {

  import com.raasahsan.crony.Schedule._

  def apply(schedule: Schedule, localDateTime: LocalDateTime): Boolean = {
    schedule match {
      case All =>
        true
      case Never =>
        false
      case Union(left, right) =>
        apply(left, localDateTime) || apply(right, localDateTime)
      case Intersection(left, right) =>
        apply(left, localDateTime) && apply(right, localDateTime)
      case Complement(value) =>
        !apply(value, localDateTime)
      case Second(n) =>
        n == localDateTime.getSecond
      case Minute(n) =>
        n == localDateTime.getMinute
      case Hour(n) =>
        n == localDateTime.getHour
      case DayOfWeek(n) =>
        n == localDateTime.getDayOfWeek.getValue
      case DayOfMonth(n) =>
        n == localDateTime.getDayOfMonth
      case DayOfYear(n) =>
        n == localDateTime.getDayOfYear
      case Month(n) =>
        n == localDateTime.getMonthValue
      case Year(n) =>
        n == localDateTime.getYear
      case LeapYear =>
        localDateTime.toLocalDate.isLeapYear
    }
  }

}
