trait Point {
  val x: BigInt
  val y: BigInt
}

case class ECPoint(x: BigInt, y: BigInt) extends Point

// The "point at infinity" acts as the additive identity of the elliptic curve
case object PointAtInfinity extends Point {
  val x = BigInt(0)
  val y = BigInt(0)
}