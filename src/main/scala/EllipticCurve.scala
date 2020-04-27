import scala.annotation.tailrec

/** An elliptic curve modulo a prime.
 * @param b the x-coefficient
 * @param c the x-squared-coefficient
 * @param p the prime modulo of the curve
 */
case class EllipticCurve(b: BigInt, c: BigInt, p: BigInt)  {
  private def invModPrime(n: BigInt): BigInt = {
    n.modPow(p-2, p)
  }
  /** Adds two points on the elliptic curve together.
   * @param p1 The first point on the elliptic curve
   * @param p2 The second point on the elliptic curve
   * @return A new instance of Point
   */
  def add(p1: Point, p2: Point): Point = {
    p1 match {
      case PointAtInfinity => return p2
      case _ =>
    }
    p2 match {
      case PointAtInfinity => return p1
      case _ =>
    }
    if(p1.x == p2.x && p1.y != p2.y) {
      return PointAtInfinity
    } else if(p1 == p2 && p1.y == 0) {
      return PointAtInfinity
    }
    val m = if(p1 == p2) {
      (3*p1.x*p1.x + b).mod(p)*invModPrime(2*p1.y)
    } else {
      (p2.y - p1.y).mod(p)*invModPrime(p2.x - p1.x)
    }
    val x3 = m.modPow(2, p) - p1.x - p2.x
    ECPoint(x3.mod(p), (m*(p1.x - x3) - p1.y).mod(p))
  }

  def subtract(p1: Point, p2: Point): Point = {
    add(p1, -p2)
  }

  /** Adds a point on the elliptic curve to itself a given number of times by repeated doubling.
    * @param pt The point on the elliptic curve
    * @param b The quantity by which the point is being multiplied by
    * @return a new instance of Point
    */
  def multiply(pt: Point, b: BigInt): Point = {
    @tailrec
    def mult(a: Point, b: BigInt, accu: Point): Point = {
      if (b == 0) {
        accu
      } else {
        val (d, c) = if(b%2 == 1) {
          (a, (b-1)/2)
        } else {
          (PointAtInfinity, b/2)
        }
        mult(add(a, a), c, add(d, accu))
      }
    }
    mult(pt, b, PointAtInfinity)
  }

  /** Determines whether the point lies on this elliptic curve.
   * @param point The point on the elliptic curve
   * @return a boolean
   */
  def pointIsOnCurve(point: Point): Boolean = {
    (point.y*point.y).mod(p) == (point.x*point.x*point.x + b*point.x + c).mod(p)
  }
}

case object EllipticCurve {
  /** Creates an elliptic curve from the x-coefficient, the prime modulo and a point that needs to be on the curve.
   * @param b the x-coefficient of the curve
   * @param p the prime modulo
   * @param basePoint the point that is required to be on the curve
   * @return a new instance of an elliptic curve
   */
  def apply(b: BigInt, p: BigInt, basePoint: Point): EllipticCurve = {
    val c = (basePoint.y*basePoint.y - basePoint.x*basePoint.x*basePoint.x - b*basePoint.x).mod(p)
    EllipticCurve(b, c, p)
  }
}