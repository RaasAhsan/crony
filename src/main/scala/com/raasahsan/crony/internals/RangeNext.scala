package com.raasahsan.crony.internals

import java.time.LocalTime

import com.raasahsan.crony.Range.{Hours, Minutes, Seconds}

object RangeNext {

  def apply(range: Hours, localTime: LocalTime): Option[LocalTime] = {
    hourLoop(range.array, localTime)
  }

  @annotation.tailrec
  private def hourLoop(ns: List[(Int, Minutes)], localTime: LocalTime): Option[LocalTime] = ns match {
    case x :: xs =>
      if (x._1 > localTime.getHour) {
        val firstMinute = x._2.first
        val firstSecond = firstMinute._2.first

        Some(localTime.withHour(x._1).withMinute(firstMinute._1).withSecond(firstSecond))
      } else if (x._1 == localTime.getHour) {
        val minutes = x._2.after(localTime.getMinute)

        minuteLoop(minutes, localTime) match {
          case Some((minute, second)) => Some(localTime.withMinute(minute).withSecond(second))
          case None => hourLoop(xs, localTime)
        }
      } else {
        hourLoop(xs, localTime)
      }
    case Nil => None
  }

  private def minuteLoop(ns: List[(Int, Seconds)], localTime: LocalTime): Option[(Int, Int)] = ns match {
    case x :: xs =>
      if (x._1 > localTime.getMinute) {
        Some((x._1, x._2.first))
      } else if (x._1 == localTime.getMinute) {

        x._2.after(localTime.getSecond) match {
          case Some(nextSecond) => Some((x._1, nextSecond))
          case None => minuteLoop(xs, localTime)
        }
      } else {
        minuteLoop(xs, localTime)
      }
    case Nil => None
  }

}
