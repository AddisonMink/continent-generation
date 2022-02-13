package continent

import scala.annotation.tailrec

/** A square grid
  * @param size - dimension of the square
  * @param elements
  * @tparam A
  */
sealed class Grid[A] private (
    val size: Int,
    private val elements: Vector[A]
) {
  import Grid._

  private def index(x: Int, y: Int): Int = y * size + x
  private def unindex(i: Int): Point = (i % size, i / size)

  def get(point: Point): A = elements(index(point._1, point._2))

  def map[B](f: A => B): Grid[B] = Grid(size, elements.map(f))

  def forEach(f: A => Unit): Unit = elements.foreach(f)

  def fold[B](z: B)(f: (A, B) => B): B =
    elements.foldLeft(z)((b, a) => f(a, b))

  def indexedMap[B](f: (Point, A) => B): Grid[B] =
    Grid(
      size,
      elements.zipWithIndex.map { case (a, i) =>
        f(unindex(i), a)
      }
    )

  def indexedForEach(f: (Point, A) => Unit): Unit =
    elements.zipWithIndex.foreach { case (a, i) => f(unindex(i), a) }

  def indexedFold[B](z: B)(f: (A, Point, B) => B): B =
    elements.zipWithIndex.foldLeft(z) { case (b, (a, i)) =>
      f(a, unindex(i), b)
    }

  def neighbors(point: Point): List[Point] = {
    val (x, y) = point
    List(
      (x - 1, y - 1),
      (x, y - 1),
      (x + 1, y - 1),
      (x - 1, y),
      (x + 1, y),
      (x - 1, y + 1),
      (x, y + 1),
      (x + 1, y + 1)
    ).filter(inBounds)
  }

  def inBounds(point: (Int, Int)): Boolean = {
    val (x, y) = point
    x >= 0 && x < size && y >= 0 && y < size
  }

  def search(
      start: Point,
      goal: A => Boolean,
      cost: (A, A) => Double = (a, b) => 0,
      heuristic: (A, Point) => Double = (a, p) => 0
  ): List[Point] = {
    @tailrec
    def loop(paths: List[List[Point]], visited: Set[Point]): List[Point] =
      paths match {
        case (head :: tail) :: rest if goal(get(head)) => head :: tail

        case (head :: tail) :: rest =>
          val moves = neighbors(head)
            .filterNot(visited.contains)
            .sortBy(point =>
              cost(get(head), get(point)) + heuristic(get(point), point)
            )

          val newPaths = moves.map(_ :: head :: tail)
          loop(newPaths ++ rest, visited + head)

        case _ => Nil
      }

    loop(List(List(start)), Set())
  }

  def floodFill(start: Point, predicate: A => Boolean): Set[Point] = {
    @tailrec
    def loop(queue: List[Point], visited: Set[Point]): Set[Point] =
      queue match {
        case head :: tail if predicate(get(head)) =>
          val newNodes = neighbors(head).filterNot(visited.contains)
          loop(newNodes ++ tail, visited + head)

        case head :: tail => loop(tail, visited)

        case Nil => visited
      }

    loop(List(start), Set())
  }
}

object Grid {

  type Point = (Int, Int)

  def makeGrid[A](size: Int, init: A): Grid[A] =
    new Grid(size, Vector.fill(size * size)(init))

  extension(grid: Grid[Double]) {
    def min(): Double = grid.elements.min
    def max(): Double = grid.elements.max
    def minWhere(f: Double => Boolean): Double = grid.elements.filter(f).min
    def maxWhere(f: Double => Boolean): Double = grid.elements.filter(f).max
  }
}
