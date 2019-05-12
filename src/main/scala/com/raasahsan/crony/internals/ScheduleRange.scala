package com.raasahsan.crony.internals

import com.raasahsan.crony
import com.raasahsan.crony.Schedule

private[crony] object ScheduleRange {

  import com.raasahsan.crony.Range._
  import Schedule._

  def apply(schedule: Schedule): Hours = {
    to(schedule)
  }

  private def to(schedule: Schedule): Hours = {
    schedule match {
      case Second(n) =>
        hourOf(minuteOf(crony.Range.second(n)))
      case Minute(n) =>
        hourOf(crony.Range.minute(n))
      case Hour(n) =>
        crony.Range.hour(n)
      case Union(a, b) =>
        to(a) || to(b)
      case Intersection(a, b) =>
        to(a) && to(b)
      case Complement(a) =>
        to(a).not
      case Never =>
        empty
      case All =>
        // This only remains true as long as we're working in times
        everyHour
      case _ => Hours(List())
    }
  }

}
