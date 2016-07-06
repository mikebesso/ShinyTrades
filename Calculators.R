CalculatePosition <- function(
  UseTicks,
  BuyOrSell,
  TickValue,
  TicksPerPoint,
  Entry,
  MaxRisk,
  InputStopLoss,
  InputProfitTarget
){

#browser();

  # Simplifying the logic below.  By default, we do not have a trade
  Risk <- -1;
  Reward <- 0;
  RewardRiskRatio <- 0;
  PositionSize <- 0;
  StopLoss <- -1;
  StopLossTicks <- -1;
  ProfitTarget <- -1;
  ProfitTargetTicks <- -1;

  if (UseTicks) {
    StopLossTicks <- InputStopLoss;
    ProfitTargetTicks <- InputProfitTarget;
  } else {
    StopLoss <- InputStopLoss;
    ProfitTarget <- InputProfitTarget;
  }

  if (Entry > 0 && StopLoss > 0 && ProfitTarget > 0){}

  if (UseTicks) {
    if ((StopLossTicks > 0) || (ProfitTargetTicks > 0)) {
      StopLoss <- Entry - (BuyOrSell * StopLossTicks / TicksPerPoint);
      ProfitTarget <- Entry + (BuyOrSell * ProfitTargetTicks / TicksPerPoint);
      Risk <- StopLossTicks * TickValue;
    }
  } else {

    TradeParametersValid <- ifelse(
      BuyOrSell > 0,
      (Entry > StopLoss) && (Entry < ProfitTarget),
      (Entry < StopLoss) && (Entry > ProfitTarget)
    )

    if (TradeParametersValid){
      if ((StopLoss > 0) || (ProfitTarget > 0)) {
        StopLossTicks <- abs(Entry - StopLoss) * TicksPerPoint;
        ProfitTargetTicks <- abs(Entry - ProfitTarget) * TicksPerPoint;
        Risk <- abs(Entry - StopLoss) * TicksPerPoint * TickValue;
      }
    }

  }


  Reward <- ProfitTargetTicks * TickValue;

  if ((Risk < MaxRisk) && (Risk > 0)){
    PositionSize <- MaxRisk / Risk;
    RewardRiskRatio <- Reward / Risk;
  }

  return(
    list(
      UseTicks = UseTicks,
      TickValue =  TickValue,
      BuyOrSell = BuyOrSell,

      Entry = Entry,

      MaxRisk = MaxRisk,
      Risk = Risk,
      RewardRiskRatio = RewardRiskRatio,

      StopLossTicks = StopLossTicks,
      StopLoss = StopLoss,
      ProfitTargetTicks = ProfitTargetTicks,
      ProfitTarget = ProfitTarget,
      PositionSize = PositionSize
    )
  )
}

