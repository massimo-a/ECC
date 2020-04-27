import java.security.SecureRandom

case class ECCrypto(
  private val generator: Point,
  private val b: BigInt,
  private val p: BigInt,
) {
  private val success: Int = 100
  private val curve = EllipticCurve(b, p, generator)

  @scala.annotation.tailrec
  private def factorOutTwo(a: BigInt, count: BigInt): (BigInt, BigInt) = {
    if(a % 2 == 1) {
      (a, count)
    } else {
      factorOutTwo(a/2, count + 1)
    }
  }

  @scala.annotation.tailrec
  private def findNonResidue(a: BigInt): BigInt = {
    if(a.modPow((p-1)/2, p) != p-1) {
      findNonResidue(a + 1)
    } else {
      a
    }
  }

  @scala.annotation.tailrec
  private def repeatedSquaring(t: BigInt, s: BigInt, i: BigInt): BigInt = {
    if(i == s) {
      i
    } else if(t.modPow(BigInt(2).modPow(i, p), p) != 1) {
      repeatedSquaring(t, s, i+1)
    } else {
      i+1
    }
  }

  @scala.annotation.tailrec
  private def recurseTonelliShanks(t: BigInt, s: BigInt, c: BigInt, r: BigInt): BigInt = {
    if(t == 0) {
      -1
    } else if(t == 1) {
      r
    } else {
      val i = repeatedSquaring(t, s, 1)
      if(i == s) return -1
      val n = c.modPow(BigInt(2).modPow(s - i - 1, p), p)
      recurseTonelliShanks((t*n*n).mod(p), i, (n*n).mod(p), (r*n).mod(p))
    }
  }

  private def tonelliShanks(b: BigInt): BigInt = {
    val a = factorOutTwo(p-1, 0)
    val q = a._1
    val s = a._2
    val z = findNonResidue(2)

    val c = z.modPow(q, p)
    val t = b.modPow(q, p)
    val r = b.modPow((q + 1)/2, p)

    recurseTonelliShanks(t, s, c, r)
  }

  private def squareRootMod(b: BigInt): BigInt = {
    if(p.mod(4) == 3) {
      val r = b.modPow((p+1)/4, p)
      if((r*r).mod(p) == b) {
        r
      } else {
        -1
      }
    } else {
      tonelliShanks(b)
    }
  }

  private def messageToPoint(m: BigInt): Point = {
    if((m + 1)*success >= p) {
      throw new Exception(f"message too large and prime is too small. Please keep the message in the range 0 to ${p/success - 1}")
    }
    for(i <- 0 until success) {
      val x = m*success + i
      val y_squared = (x*x*x + b*x + curve.c).mod(p)
      val y = squareRootMod(y_squared)
      if (y > 0) {
        return ECPoint(x, y)
      }
    }
    throw new Exception("Integer could not be mapped to point on elliptic curve")
  }

  /**
   * @return a cryptographically secure, random private key in the range [0, p-1]
   */
  def generatePrivateKey: BigInt = {
    BigInt(p.bitLength, new SecureRandom())
  }

  /** Encrypts a message, given as a number, using the ElGamal cryptosystem.
   * @param plain the plaintext message as a big integer
   * @param pubkey the public key belonging to the person who the message is going to
   * @return two new points on the elliptic curve
   */
  def encrypt(plain: BigInt, pubkey: Point): (Point, Point) = {
    val pt = messageToPoint(plain)
    val secret = BigInt(p.bitLength, new SecureRandom())
    val (c, d) = (curve.multiply(generator, secret), curve.multiply(pubkey, secret))
    (c, curve.add(d, pt))
  }

  /** Decrypt a message, given as two points.
   * @param cipher the ciphertext
   * @param pk the private key of the person who is meant to read the message
   * @return the message, as a big integer
   */
  def decrypt(cipher: (Point, Point), pk: BigInt): BigInt = {
    val pt = curve.subtract(cipher._2, curve.multiply(cipher._1, pk))
    pt.x/success
  }

  /** Gets the public key belonging to the person with the given private key.
   * @param privateKey the private key of the user
   * @return a public key belonging to the person whose private key was given
   */
  def getPublicKey(privateKey: BigInt): Point = {
    curve.multiply(generator, privateKey)
  }

  /** Uses Diffie-Hellman key agreement to generate a shared private key.
   * @param point the public key belonging to the second party
   * @param privateKey the user's private key
   * @return a shared private key known only to the two parties, as a point on the elliptic curve
   */
  def getSharedKey(point: Point, privateKey: BigInt): Point = {
    curve.multiply(point, privateKey)
  }
}