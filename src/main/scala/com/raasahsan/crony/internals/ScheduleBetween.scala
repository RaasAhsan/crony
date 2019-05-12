package com.raasahsan.crony.internals

import java.time.LocalDateTime

import com.raasahsan.crony.Schedule

object ScheduleBetween {

  def apply(schedule: Schedule, start: LocalDateTime, end: LocalDateTime): List[LocalDateTime] = {
    @annotation.tailrec
    def loop(dateTime: LocalDateTime, list: List[LocalDateTime]): List[LocalDateTime] = {
      if (dateTime.isAfter(end)) {
        list
      } else {
        val nl = if (schedule.in(dateTime)) {
          dateTime :: list
        } else {
          list
        }

        schedule.next(dateTime) match {
          case Some(n) => loop(n, nl)
          case None => nl
        }
      }
    }

    loop(start, Nil)
  }

}
