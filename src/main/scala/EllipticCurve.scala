import scala.annotation.tailrec

case class EllipticCurve(b: BigInt, c: BigInt, p: BigInt)  {
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
    } else if(p1.x == p2.x && p1.y == p2.y && p1.y == 0) {
      return PointAtInfinity
    }
    val m = if(p1.x == p2.x && p1.y == p2.y) {
      ModInt(3*p1.x*p1.x + b, p)/(2*p1.y)
    } else {
      ModInt(p2.y - p1.y, p)/(p2.x - p1.x)
    }
    val x3 = (m**2).n - p1.x - p2.x
    ECPoint(x3.mod(p), (m*(p1.x - x3) - p1.y).n.mod(p))
  }

  def multiply(a: Point, b: BigInt): Point = {
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
    mult(a, b, PointAtInfinity)
  }

  def pointIsOnCurve(point: Point): Boolean = {
    (point.y*point.y).mod(p) == (point.x*point.x*point.x + b*point.x + c).mod(p)
  }
}

case object EllipticCurve {
  def create(b: BigInt, p: BigInt, basePoint: Point): EllipticCurve = {
    val c = (basePoint.y*basePoint.y - basePoint.x*basePoint.x*basePoint.x - b*basePoint.x).mod(p)
    EllipticCurve(b, c, p)
  }
}