/**
 * A point in space.
 */
trait Point {
  val x: BigInt
  val y: BigInt
  def unary_-(): Point
}

case class ECPoint(x: BigInt, y: BigInt) extends Point {
  override def unary_-(): Point = {
    ECPoint(x, -y)
  }
}

/**
 * represents the point-at-infinity on an elliptic curve, which acts as the additive identity for the group.
 */
case object PointAtInfinity extends Point {
  val x = BigInt(0)
  val y = BigInt(0)
  override def unary_-(): Point = {
    PointAtInfinity
  }
}