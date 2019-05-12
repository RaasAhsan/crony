package com.raasahsan.crony

import java.time.LocalTime

object Main extends App {

  import Schedule._

  val schedule = (minute(2) && second(3)) && (hour(7).not)

  val range = schedule.range

  val start = LocalTime.of(4, 6, 4)
  val end = LocalTime.of(9, 1, 3)

}
