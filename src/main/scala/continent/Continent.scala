package continent

import scala.util.Random

case class Continent(
    landGrid: Grid[Continent.Land],
    elevationGrid: Grid[Double]
)

object Continent {
  import Grid._

  case class ContinentConfig(
      size: Int,
      period: Double,
      amplitude: Double,
      octaves: Int,
      seaLevel: Double,
      mountainLevel: Double,
      riverFrequency: Double,
      greenRange: Double,
      seed: Int
  )

  sealed trait Land
  case object Ocean extends Land
  case object Mountain extends Land
  case object Field extends Land
  case object River extends Land
  case object Desert extends Land

  def continent(config: ContinentConfig): Continent = {
    implicit val rng = new Random(config.seed)
    implicit val implicitConfig = config

    val elevationGrid = Grid
      .makeGrid(config.size, 0)
      .indexedMap { case ((x, y), _) => mound(x, y) + noise(x, y) }

    val rivers = elevationGrid
      .indexedFold(List[Point]()) { case (z, point, acc) =>
        val isMountain = z > config.mountainLevel
        val isSource = rng.nextDouble() < config.riverFrequency
        if (isMountain && isSource) point :: acc else acc
      }
      .flatMap(source => river(source, elevationGrid))
      .toSet

    val oceans = elevationGrid.floodFill((0, 0), _ < config.seaLevel)

    val lakes = elevationGrid.indexedFold(Set[Point]()) {
      case (z, point, acc) =>
        if (z < config.seaLevel && !oceans.contains(point)) acc + point else acc
    }

    val freshWaters = rivers.union(lakes)

    val hydrationGrid = elevationGrid.indexedMap { case (point, elevation) =>
      if (freshWaters.contains(point)) 0
      else if (oceans.contains(point)) 0
      else distance(freshWaters.minBy(distance(_, point)), point)
    }

    val landGrid = elevationGrid.indexedMap { case (point, elevation) =>
      if (oceans.contains(point)) Ocean
      else if (freshWaters.contains(point)) River
      else if (elevation > config.mountainLevel) Mountain
      else if (hydrationGrid.get(point) < config.greenRange) Field
      else Desert
    }

    Continent(landGrid, elevationGrid)
  }

  private def mound(x: Double, y: Double)(implicit
      config: ContinentConfig
  ): Double = {
    val halfSize = config.size / 2
    val zx = Math.cos((x - halfSize) / halfSize * Math.PI / 2)
    val zy = Math.cos((y - halfSize) / halfSize * Math.PI / 2)
    zx * zy
  }

  private def noise(x: Double, y: Double)(implicit
      config: ContinentConfig
  ): Double =
    (1 to config.octaves)
      .foldLeft((config.period, config.amplitude, 0.0)) {
        case ((period, amplitude, total), _) =>
          val z = Perlin.noise2d(x, y, period, amplitude, config.seed)
          (period / 2, amplitude / 2, total + z)
      }
      ._3

  private def river(source: Point, elevations: Grid[Double])(implicit
      config: ContinentConfig
  ): List[Point] =
    elevations
      .search(
        start = source,
        goal = _ < config.seaLevel,
        cost = (current, move) => move - current
      )
      .tail

  private def distance(from: Point, to: Point): Double = {
    val (x0, y0) = from
    val (x1, y1) = to
    Math.sqrt(Math.pow(x1 - x0, 2) + Math.pow(y1 - y0, 2))
  }
}
