package u06lab.code

object Solitaire extends App:
  def render(solution: Seq[(Int, Int)], size: (Int, Int)): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until size._2
          row = for x <- 0 until size._1
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  type Position = (Int, Int)
  type Solution = List[Position]

  def placeMarks(size: (Int, Int))(init: Position): Iterable[Solution] =
    def _placeMarks(n: Int): Iterable[Solution] = n match
      case 1 => Seq(List(init)).view
      case _ =>
        for
          solution <- _placeMarks(n - 1)
          i <- 0 until size._1
          j <- 0 until size._2
          newPos = (i, j)
          if !solution.contains(newPos)
          if isSafe(newPos, solution.head)
        yield newPos :: solution

    _placeMarks(size._1 * size._2)

  def isSafe(p1: Position, p2: Position): Boolean =
    (math.abs(p1._1 - p2._1) == 2 && math.abs(p1._2 - p2._2) == 2)
      || (math.abs(p1._1 - p2._1) == 3 && math.abs(p1._2 - p2._2) == 0)
      || (math.abs(p1._1 - p2._1) == 0 && math.abs(p1._2 - p2._2) == 3)

  val size = (5, 5) //._1 width ._2 height
  val center = (size._1 / 2, size._2 / 2)
  //println(center)
  val solutions = placeMarks(size)(center)
  if (true) //
    for s <- solutions
      do println(render(s, size) + "\n")

  println(solutions.size)