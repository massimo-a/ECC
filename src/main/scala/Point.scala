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

// The "point at infinity" acts as the additive identity of any elliptic curve
case object PointAtInfinity extends Point {
  val x = BigInt(0)
  val y = BigInt(0)
  override def unary_-(): Point = {
    ECPoint(x, -y)
  }
}