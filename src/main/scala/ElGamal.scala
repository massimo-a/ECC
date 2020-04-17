case class ElGamal() {
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
       - From (C, D) compute C' = xC and P = D - C'
       - Reverse the point P back to message M
   */
}

object ElGamal {
  def main(args: Array[String]): Unit = {

  }
}