import java.security.SecureRandom

case class ElGamal(
  private val generator: Point,
  private val b: BigInt,
  private val p: BigInt
) {
  // Algorithm layout
  /* Given an elliptic curve E mod p and a base point G
     Map a message M to a point on the elliptic curve P
       - Function should be invertible
       - Idea: convert M to a number use it as x in the curve equation to get the corresponding y
     Choose a secret key 0 < x < p and publish Y = xG
     Encryption:
       - Choose a secret 0 < k < p and compute C = kG and C' = kY
       - Map message M to point P
       - The ciphertext is (C, C' + P)
     Decryption:
       - From (C, D) compute P = D - xC
       - Reverse the point P back to message M
   */
  private val curve = EllipticCurve.create(b, p, generator)
  private val privateKey = BigInt(p.bitLength, new SecureRandom())
  val publicKey: Point = curve.multiply(generator, privateKey)

  private def messageToPoint(m: BigInt): Point = {
    generator
  }

  def encrypt(plain: BigInt, pubkey: Point): (Point, Point) = {
    val pt = messageToPoint(plain)
    val secret = BigInt(p.bitLength, new SecureRandom())
    val (c, d) = (curve.multiply(generator, secret), curve.multiply(pubkey, secret))
    (c, curve.add(d, pt))
  }

  def decrypt(cipher: (Point, Point)): Point = {
    curve.subtract(cipher._2, curve.multiply(cipher._1, privateKey))
  }
}

object ElGamalMain {
  def main(args: Array[String]): Unit = {
    val bob = ElGamal(ECPoint(1,2), 5, 19)
    val cipher = bob.encrypt(100, bob.publicKey)
    println(cipher)
    println(bob.decrypt(cipher))
  }
}