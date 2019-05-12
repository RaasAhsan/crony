package com.raasahsan.crony.internals

import java.time.LocalTime

import com.raasahsan.crony.Range.Hours

object RangeBetween {

  def apply(range: Hours, start: LocalTime, end: LocalTime): List[LocalTime] = {
    @annotation.tailrec
    def loop(current: LocalTime, acc: List[LocalTime]): List[LocalTime] =
      RangeNext(range, current) match {
        case Some(next) =>
          if (next.isAfter(end)) {
            acc
          } else {
            loop(next, acc :+ next)
          }
        case None => acc
      }

    loop(start, Nil)
  }

}
