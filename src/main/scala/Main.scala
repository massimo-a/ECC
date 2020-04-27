import java.security.SecureRandom

object Main {
  def help(): Unit = {
    println(
      """
        |--basepoint [x] [y] : Specifies the generating point on the elliptic curve.
        |                      This is public information and should be agreed upon by Alice and Bob
        |                      through any means of communication.
        |--coefficient [b]   : Any elliptic curve of the form y^2 = x^3 + bx + c can be specified by
        |                      the single x-coefficient b, and the basepoint. The coefficient c can
        |                      then be inferred. The coefficient b is public information.
        |--prime [p]         : This is the modulus to use for the elliptic curve. It is public.
        |                      Note that the program does not check if the input is actually a prime,
        |                      so make sure it is.
        |--privateKey [pk]   : If you have a private key already, you can use this parameter to input
        |                      it. If you do not, a private key will be randomly generated for you. Note
        |                      the private key should be an integer in the range [1, p-1].
        |--diffie            : Option for running the Elliptic Curve Diffie-Hellman key agreement protocol.
        |--elgamal           : Option for running the Elliptic Curve ElGamal cryptosystem.
        |""".stripMargin)
  }
  def diffieHellman(diffie: ECCrypto, privkey: BigInt): Unit = {
    val opt = scala.io.StdIn.readLine("Would you like to generate a shared key based on a public key you've received? [Y/N]")
    if(opt.toUpperCase() == "Y") {
      println("Please input the public key you received in comma-delimited format")
      val a = scala.io.StdIn.readLine().replace(" ", "").split(',')
      val sharedkey = diffie.getSharedKey(ECPoint(BigInt(a(0)), BigInt(a(1))), privkey)
      println(f"Your shared key is (${sharedkey.x}, ${sharedkey.y})")
    }
  }

  def elGamal(el: ECCrypto, privkey: BigInt): Unit = {
    val opt = scala.io.StdIn.readLine("Would you like to encrypt a message? [Y/N]")
    if(opt.toUpperCase() == "Y") {
      println("Please input the public key you received in comma-delimited format")
      val a = scala.io.StdIn.readLine().replace(" ", "").split(',')
      val pt1 = ECPoint(BigInt(a(0)), BigInt(a(1)))
      println("Please input the message to be encrypted")
      val b = scala.io.StdIn.readLine()
      val enc = el.encrypt(BigInt(b), pt1)
      println(f"Your ciphertext is ${enc._1}, ${enc._2}")
    }
    val opt2 = scala.io.StdIn.readLine("Would you like to decrypt a ciphertext with your private key? [Y/N]")
    if(opt2.toUpperCase() == "Y") {
      println("Please input the first point of the ciphertext you would like to decrypt, in comma delimited format")
      val a = scala.io.StdIn.readLine().replace(" ", "").split(',')
      val pt1 = ECPoint(BigInt(a(0)), BigInt(a(1)))
      println("Please input the second point of the ciphertext you would like to decrypt, in comma delimited format")
      val b = scala.io.StdIn.readLine().replace(" ", "").split(',')
      val pt2 = ECPoint(BigInt(b(0)), BigInt(b(1)))
      println(f"The message is ${el.decrypt((pt1, pt2), privkey)}")
    }
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
      // a 256 bit prime
      BigInt("47397778346411896641319677892497457270047265800000431553410184780174114258089")
    }
    val c = (y*y - x*x*x - b*x).mod(p)
    val privkey = if(args.contains("--privateKey")) {
      val i = args.indexOf("--privateKey")
      BigInt(args(i+1))
    } else {
      BigInt(p.bitLength, new SecureRandom())
    }
    val cryptoSystem = ECCrypto(ECPoint(x, y), b, p)
    val publickey = cryptoSystem.getPublicKey(privkey)
    println(f"The public elliptic curve to be used is y^2 = x^3 + ${b}x + $c (mod $p) with base point ($x, $y)")
    println(f"Your private key is : $privkey")
    println(f"Your public key is : (${publickey.x}, ${publickey.y})")
    if(args.contains("--diffie")) {
      diffieHellman(cryptoSystem, privkey)
    } else if(args.contains("--elgamal")) {
      elGamal(cryptoSystem, privkey)
    } else {
      println("No cryptographic action selected. Please re-run with either --elgamal or --diffie")
    }
  }
}