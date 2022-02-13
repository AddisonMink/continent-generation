package continent

import scala.util.Random

object Perlin {

  /** 1-dimensional Perlin noise
    * @param x - x input
    * @param period - distance between vertices
    * @param amplitude - maximum height of crest
    * @param seed - seed used to pseudo-randomly map vertices to gradients
    * @return - Perlin noise value of x.
    */
  def noise1d(
      x: Double,
      period: Double = 1,
      amplitude: Double = 1,
      seed: Int = 0
  ): Double = {
    // Pseudo-randomly map vertices to gradients.
    val gradients = Vector(-2.0, -1.0, -0.5, 0.0, 0.5, 1.0)

    def hash(vertex: Int): Int =
      Random(seed + vertex).nextInt(gradients.length)

    def gradient(vertex: Int): Double = gradients(hash(vertex))

    // Determine interpolation fractions.
    val mu = x % period / period
    val smoothMu = smooth(mu)

    // Compute vertices.
    val v0 = (x / period).toInt
    val v1 = v0 + 1

    // Extrapolate the vertex gradients to 'x' and interpolate the results.
    val y0 = gradient(v0) * mu * amplitude
    val y1 = gradient(v1) * (-1 + mu) * amplitude
    y0 * (1 - smoothMu) + y1 * smoothMu
  }

  /** 2-dimensional Perlin noise
    * @param x - x input
    * @param y - y input
    * @param period - distance between axis-aligned vertices
    * @param amplitude - maximum height of crest
    * @param seed - seed used to pseudo-randomly map vertices to gradients
    * @return - Perlin noise value of (x,y)
    */
  def noise2d(
      x: Double,
      y: Double,
      period: Double = 1,
      amplitude: Double = 1,
      seed: Int = 0
  ): Double = {
    // Pseudo-randomly map vertices to gradients.
    val gradients = Vector(
      (1.0, 0.0),
      (Math.sqrt(2) / 2, Math.sqrt(2) / 2),
      (0.0, 1.0),
      (-1 * Math.sqrt(2) / 2, Math.sqrt(2) / 2),
      (-1.0, 0.0),
      (-1 * Math.sqrt(2) / 2, -1 * Math.sqrt(2) / 2),
      (0.0, -1.0),
      (Math.sqrt(2) / 2, -1 * Math.sqrt(2) / 2)
    )

    def hash(x: Int, y: Int): Int = {
      val h = s"($x,$y)#$seed".hashCode
      (Random(h).nextDouble() * gradients.length).toInt
    }

    def gradient(vertex: (Int, Int)): (Double, Double) = gradients(
      hash(vertex._1, vertex._2)
    )

    def dot(u: (Double, Double), v: (Double, Double)): Double = {
      val (ux, uy) = u
      val (vx, vy) = v
      ux * vx + uy * vy
    }

    // Determine interpolation fractions
    val muX = x % period / period
    val smoothMuX = smooth(muX)
    val muY = y % period / period
    val smoothMuY = smooth(muY)

    // Determine vertices
    val v00 @ (v00x, v00y) = ((x / period).toInt, (y / period).toInt)
    val v10 = (v00x + 1, v00y)
    val v01 = (v00x, v00y + 1)
    val v11 = (v00x + 1, v00y + 1)

    // Extrapolate from vertex gradients.
    val z00 = dot(gradient(v00), (muX, muY)) * amplitude
    val z10 = dot(gradient(v10), (-1 + muX, muY)) * amplitude
    val z01 = dot(gradient(v01), (muX, -1 + muY)) * amplitude
    val z11 = dot(gradient(v11), (-1 + muX, -1 + muY)) * amplitude

    // Smoothly interpolate the results.
    val z00_10 = z00 * (1 - smoothMuX) + z10 * smoothMuX
    val z01_11 = z01 * (1 - smoothMuX) + z11 * smoothMuX
    z00_10 * (1 - smoothMuY) + z01_11 * smoothMuY
  }

  /** Creates a continuous S-shaped curve from [0,1] where the derivative is 0 at 0 and 1.
    * @param x - [0,1]
    * @return - [0,1]
    */
  private def smooth(x: Double): Double =
    6 * Math.pow(x, 5) - 15 * Math.pow(x, 4) + 10 * Math.pow(x, 3)

}
