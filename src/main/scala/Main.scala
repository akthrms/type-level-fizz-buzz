/**
 * entry point
 */
object Main extends App {

  // --------------------
  // natural number
  // --------------------

  trait Nat

  case class Succ(nat: Nat) extends Nat

  case object Zero extends Nat

  // --------------------
  // nat to int
  // --------------------

  implicit class NatToInt(nat: Nat) {
    def toInt: Int = nat match {
      case Succ(n) => 1 + n.toInt
      case Zero => 0
    }
  }

  // --------------------
  // int to nat
  // --------------------

  implicit class IntToNat(int: Int) {
    def toNat: Nat = int match {
      case 0 => Zero
      case n => Succ((n - 1).toNat)
    }
  }

  // --------------------
  // nat to fizz-buzz
  // --------------------

  @scala.annotation.tailrec
  def isFizz(nat: Nat): Boolean = nat match {
    case Succ(Succ(Succ(Zero))) => true
    case Succ(Succ(Succ(n))) => isFizz(n)
    case _ => false
  }

  @scala.annotation.tailrec
  def isBuzz(nat: Nat): Boolean = nat match {
    case Succ(Succ(Succ(Succ(Succ(Zero))))) => true
    case Succ(Succ(Succ(Succ(Succ(n))))) => isBuzz(n)
    case _ => false
  }

  implicit class NatToFizzBuzzString(nat: Nat) {
    def toFizzBuzzString: String = (isFizz(nat), isBuzz(nat)) match {
      case (true, true) => "FizzBuzz"
      case (true, _) => "Fizz"
      case (_, true) => "Buzz"
      case _ => nat.toInt.toString
    }
  }

  // --------------------
  // run
  // --------------------

  (1 to 100)
    .map(_.toNat.toFizzBuzzString)
    .foreach(println)

}
