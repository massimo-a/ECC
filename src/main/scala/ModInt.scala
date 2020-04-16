case class ModInt(n: BigInt, mod: BigInt) {
  def +(m: BigInt): ModInt = {
    ModInt((n+m).mod(mod), mod)
  }

  def -(m: BigInt): ModInt = {
    ModInt((n-m).mod(mod), mod)
  }

  def *(m: BigInt): ModInt = {
    ModInt((n*m).mod(mod), mod)
  }

  def **(m: BigInt): ModInt = {
    if(m <= 0) {
      val a = n.modPow(-m, mod)
      ModInt(a, mod).invModPrime()
    } else {
      ModInt(n.modPow(m, mod), mod)
    }
  }

  def invModPrime(): ModInt = {
    ModInt(n.modPow(mod-2, mod), mod)
  }

  def /(m: BigInt): ModInt = {
    if(m.gcd(mod) != BigInt(1)) {
      throw new Exception(f"""In order to divide $n by $m modulo $mod the GCD of $m and $mod has to equal 1, but it actually equals ${m.gcd(mod)}""")
    }
    val a = ModInt(m, mod).invModPrime()
    ModInt((n*a.n).mod(mod), mod)
  }
}

case object ModInt {
  implicit def int2bigint(i: Int): BigInt = {
    BigInt(i)
  }
  def apply(m: Int): Int => ModInt = {
    n: Int => {
      ModInt(n, m)
    }
  }
}