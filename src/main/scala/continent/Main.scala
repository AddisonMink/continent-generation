package continent

import continent.Continent.Land

import scala.util.Random

object Main extends App {
  import Continent._
  import Grid._

  val POINT_SIZE = 5

  val CONFIG = Continent.ContinentConfig(
    size = 100,
    period = 25,
    amplitude = 1,
    octaves = 3,
    seaLevel = 0.6,
    mountainLevel = 1,
    riverFrequency = 0.01,
    greenRange = 10,
    seed = Random.nextInt()
  )

  println(CONFIG.seed)

  val drawPoint = Display.prepareCanvas(
    id = "continent",
    width = CONFIG.size,
    height = CONFIG.size,
    pointSize = POINT_SIZE
  )

  val Continent(landGrid,elevationGrid) = Continent.continent(CONFIG)
  val normalElevationGrid = normalize(elevationGrid,landGrid)

  landGrid.indexedForEach { case (point@(x, y), land) =>
    val elevation = normalElevationGrid.get(point)
    val color = land match {
      case Ocean    => scaleColor(0,0,255,elevation)
      case Mountain => scaleColor(165, 42, 42,elevation)
      case Field    => scaleColor(0,255,0,elevation)
      case River    => scaleColor(0, 191, 255,elevation)
      case Desert   => scaleColor(210, 180, 140,elevation)
    }

    drawPoint(x, y, color)
  }

  def normalize(elevationGrid: Grid[Double], landGrid: Grid[Land]): Grid[Double] = {
    val oceanMin = elevationGrid.minWhere(_ < CONFIG.seaLevel)
    val oceanMax = elevationGrid.maxWhere(_ < CONFIG.seaLevel)
    val oceanDiff = oceanMax - oceanMin

    val mountainMin = elevationGrid.minWhere(_ > CONFIG.mountainLevel)
    val mountainMax = elevationGrid.maxWhere(_ > CONFIG.mountainLevel)
    val mountainDiff = mountainMax - mountainMin

    val generalMin = elevationGrid.minWhere(e => e >= CONFIG.seaLevel && e <= CONFIG.mountainLevel)
    val generalMax = elevationGrid.maxWhere(e => e >= CONFIG.seaLevel && e <= CONFIG.mountainLevel)
    val generalDiff = generalMax - generalMin

    landGrid.indexedMap { case (point,land) =>
      val elevation = elevationGrid.get(point)

      land match {
        case Ocean =>
          val normal = (elevation - oceanMin) / oceanDiff
          normal/2 + 0.5
        case Mountain =>
          val normal = (elevation - mountainMin) / mountainDiff
          normal/2 + 0.5
        case _ =>
          val normal = (elevation - generalMin) / generalDiff
          normal/2 + 0.5
      }
    }
  }

  def scaleColor(r: Int, g: Int, b: Int, brightness: Double): String = {
    val _r = (r * brightness).round
    val _g = (g * brightness).round
    val _b = (b * brightness).round
    s"rgb($_r,$_g,$_b)"
  }
}
