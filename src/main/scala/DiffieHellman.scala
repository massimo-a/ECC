import java.security.SecureRandom

case class DiffieHellman(
  private val generator: Point,
  private val b: BigInt,
  private val p: BigInt,
  private val privateKey: BigInt
) {
  private val curve = EllipticCurve.create(b, p, generator)

  def getPublicKey: Point = {
    curve.multiply(generator, privateKey)
  }

  def getSharedKey(point: Point): Point = {
    curve.multiply(point, privateKey)
  }
}

object Main {
  def help(): Unit = {
    println(
      """
        |--basepoint [x] [y] : Specifies the generating point on the elliptic curve.
        |                      This is public information and should be agreed upon by Alice and Bob
        |                      through any means of communication.
        |--coefficient [b]   : Any elliptic curve of the form y^2 = x^3 + bx + c can be specified by
        |                      the single x-coefficient b, and the basepoint. The coefficient c can
        |                      then be inferred. This coefficient is also public information.
        |--prime [p]         : This is the modulus to use for the elliptic curve. Note that the program
        |                      does not check if the input is actually a prime, so make sure it is.
        |--privateKey [pk]   : If you have a private key already, you can use this parameter to input
        |                      it. If you do not, a private key will be randomly generated for you. Note
        |                      the private key should be an integer in the range [1, p-1].
        |""".stripMargin)
  }
  def main(args: Array[String]): Unit = {
    help()
    val basepoint = if(args.contains("--basepoint")) {
      val i = args.indexOf("--basepoint")
      ECPoint(BigInt(args(i+1)), BigInt(args(i+2)))
    } else {
      ECPoint(102, 307)
    }
    val x = basepoint.x
    val y = basepoint.y
    val b = if(args.contains("--coefficient")) {
      val i = args.indexOf("--coefficient")
      BigInt(args(i+1))
    } else {
      BigInt(32)
    }
    val p = if(args.contains("--prime")) {
      val i = args.indexOf("--prime")
      BigInt(args(i+1))
    } else {
      BigInt("35755847116784105248336462776751227610669647609823")
    }
    val c = (y*y - x*x*x - b*x).mod(p)
    val privkey = if(args.contains("--privateKey")) {
      val i = args.indexOf("--privateKey")
      BigInt(args(i+1))
    } else {
      BigInt(p.bitLength, new SecureRandom())
    }
    val diffieHellman = DiffieHellman(ECPoint(x, y), b, p, privkey)
    val publickey = diffieHellman.getPublicKey
    println(f"The public elliptic curve to be used is y^2 = x^3 + ${b}x + $c (mod $p) with base point ($x, $y)")
    println(f"Your private key is : $privkey")
    println(f"Your public key is : (${publickey.x}, ${publickey.y})")
    val opt = scala.io.StdIn.readLine("Would you like to generate a shared key based on a public key you've received? [Y/N]")
    if(opt.toUpperCase() == "Y") {
      println("Please input the public key you received in comma-delimited format")
      val a = scala.io.StdIn.readLine().replace(" ", "").split(',')
      val sharedkey = diffieHellman.getSharedKey(ECPoint(BigInt(a(0)), BigInt(a(1))))
      println(f"Your shared key is (${sharedkey.x}, ${sharedkey.y})")
    }
  }
}