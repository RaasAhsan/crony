package com.raasahsan.crony.internals

import com.raasahsan.crony.Schedule

private[crony] object ScheduleRewrite {

  import Schedule._

  def apply(schedule: Schedule): Schedule =
    reduce(schedule)

  // Can we make use of associativity laws to rewrite terms to be completely right-associative?
  // This lets us make the assumption that e.g. all intersections are right associated.
  
  // We can perform many of these reductions and rewrites in the union, intersection and complement operators

  def reduce(schedule: Schedule): Schedule = {
    schedule match {
      /* Intersection laws */
      case Intersection(left, right) =>
        (reduce(left), reduce(right)) match {
          case (All, x) => x
          case (x, All) => x
          case (Never, _) => Never
          case (_, Never) => Never
          case (x, y) => Intersection(x, y)
        }

      /* Union laws */
      case Union(left, right) =>
        (reduce(left), reduce(right)) match {
          case (All, _) => All
          case (_, All) => All
          case (Never, x) => x
          case (x, Never) => x
          case (x, y) => Union(x, y)
        }

      /* Complement laws */
      case Complement(value) =>
        reduce(value) match {
          case All => Never
          case Never => All
          case Complement(i) => i
          case x => Complement(x)
        }

      case _ => schedule
    }
  }

  def reassociateRight(schedule: Schedule): Schedule = {
    // reassociate intersections
    // reassociate unions

    ???
  }

}
