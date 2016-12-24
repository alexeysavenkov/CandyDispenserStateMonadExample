val (disp, (logs, outs)) = CandyDispenser.simulateAndLogRepeatedly(List(
  Turn,
  Coin,
  Coin,
  Turn,
  Turn,
  Coin
)).run(CandyDispenser(0, 1, isLocked = true))

logs.toList
outs