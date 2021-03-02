case class Player(name: String, score: Int)

object Main extends App {
  /*
      given an impure function f of type A => B,
      we can split f into two functions:
        - A pure function of type A => D, where D is some description of the result of f
        - An impure function of type D => B, which can be thought of as an interpreter of these descriptions

      |--------------------------------------------------------------|
      |                      imperative shell                        |
      |                    -----------------------                   |
      | imperative shell  | pure functional core |  imperative shell |
      |                   -----------------------                    |
      |                     imperative shell                         |
      ---------------------------------------------------------------
   */

  def winnerMessage(player: Option[Player]): String =
    player
      .map(p => s"${p.name} is the winner")
      .getOrElse("it's a draw")

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score)
      Some(p1)
    else if (p1.score < p2.score)
      Some(p2)
    else
      None

  def contest(p1: Player, p2: Player): Unit =
    println(winnerMessage(winner(p1, p2)))
}
