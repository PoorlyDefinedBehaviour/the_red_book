case class ISODate(value: String)

case class CreditCard(number: String, validUntil: ISODate, cvv: Int)

class Coffe {
  val price = 10
}

case class Charge(cc: CreditCard, amount: Int) {
  def combine(other: Charge): Charge = {
    if (cc != other.cc) {
      throw new Exception("Can't combine charges to different cards")
    }

    Charge(cc, amount + other.amount)
  }

}

object Cafe {
  def buyCoffe(cc: CreditCard): (Coffe, Charge) = {
    val cup = new Coffe()
    (cup, Charge(cc, cup.price))
  }

  def buyCoffes(cc: CreditCard, amountOfCoffes: Int): (List[Coffe], Charge) = {
    val purchases = List.fill(amountOfCoffes)(buyCoffe(cc))
    val (coffes, charges) = purchases.unzip
    (coffes, charges.reduce((c1, c2) => c1.combine(c2)))
  }

  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
}

object Main extends App {
  println("Hello, World!")
}
