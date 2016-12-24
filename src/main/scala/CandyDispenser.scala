import scalaz._, scalaz.Scalaz._

case class CandyDispenser(coinsCnt : Int, candiesCnt : Int, isLocked : Boolean)

sealed trait Input
sealed trait Output
object Coin extends Input with Output
object Turn extends Input
object Candy extends Output

object CandyDispenser {

  type DispenserState[T] = State[CandyDispenser, T]

  def simulateOnce(input : Input) : DispenserState[Option[Output]] = {
    val id = modify(identity[CandyDispenser])
    def _sim(dispenser: CandyDispenser) : (Option[Output], DispenserState[Unit]) =
      input match {
        case Coin if dispenser.candiesCnt == 0 =>
          (Some(Coin), id)

        case Coin if dispenser.isLocked =>
          (None, modify({
            case CandyDispenser(coins, candies, _) =>
              CandyDispenser(coins + 1, candies, isLocked = false)
          }))
        case Coin =>
          (Some(Coin), id)

        case Turn if dispenser.isLocked =>
          (None, id)
        case Turn =>
          (Some(Candy), modify({
            case CandyDispenser(coins, candies, _) =>
              CandyDispenser(coins, candies - 1, isLocked = true)
          }))
      }

    for {
      dispenser <- get[CandyDispenser]
      (out, transform) = _sim(dispenser)
      _ <- transform
    } yield out
  }

  type DispenserLoggerTState[T] = WriterT[DispenserState, DList[String], T]

  private def tell(message : String) =
    WriterT[DispenserState, DList[String], Seq[Option[Output]]](State[CandyDispenser, (DList[String], Seq[Option[Output]])](x => (x, (DList(message), Seq()))))

  private def liftWithLog[T](dispenserState: DispenserState[T]) = WriterT[DispenserState, DList[String], T](
    for {
      result <- dispenserState
      dispenser <- get
    } yield (DList(s"||| $dispenser | Output = $result |||"), result)
  )

  def simulateAndLogRepeatedly(inputs : List[Input]) : DispenserLoggerTState[List[Option[Output]]] =
    inputs.traverse[DispenserLoggerTState, Option[Output]]((simulateOnce _).andThen(liftWithLog))
}
