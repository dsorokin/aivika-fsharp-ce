
(* Example: Financial Model as described in
   Vensim 5 Modeling Guide, Chapter Financial Modeling and Risk *)

#nowarn "40"

#I "../../bin"
#r "../../bin/Simulation.Aivika.dll"
#r "../../bin/Simulation.Aivika.Results.dll"

open Simulation.Aivika
open Simulation.Aivika.SD
open Simulation.Aivika.Results

/// The simulation specs
let specs = 
    { StartTime = 0.0;
      StopTime = 5.0;
      DT = 0.015625;
      Method = RungeKutta4;
      GeneratorType = StrongGenerator }

/// The model parameters.
type Parameters =
    { TaxDepreciationTime    : Parameter<float>;
      TaxRate                : Parameter<float>;
      AveragePayableDelay    : Parameter<float>;
      BillingProcessingTime  : Parameter<float>;
      BuildingTime           : Parameter<float>;
      DebtFinancingFraction  : Parameter<float>;
      DebtRetirementTime     : Parameter<float>;
      DiscountRate           : Parameter<float>;
      FractionalLossRate     : Parameter<float>;
      InterestRate           : Parameter<float>;
      Price                  : Parameter<float>;
      ProductionCapacity     : Parameter<float>;
      RequiredInvestment     : Parameter<float>;
      VariableProductionCost : Parameter<float> }

/// The default model parameters.
let defaultParams =
    { TaxDepreciationTime    = parameter.Return 10.0;
      TaxRate                = parameter.Return 0.4;
      AveragePayableDelay    = parameter.Return 0.09;
      BillingProcessingTime  = parameter.Return 0.04;
      BuildingTime           = parameter.Return 1.0;
      DebtFinancingFraction  = parameter.Return 0.6;
      DebtRetirementTime     = parameter.Return 3.0;
      DiscountRate           = parameter.Return 0.12;
      FractionalLossRate     = parameter.Return 0.06;
      InterestRate           = parameter.Return 0.12;
      Price                  = parameter.Return 1.0;
      ProductionCapacity     = parameter.Return 2400.0;
      RequiredInvestment     = parameter.Return 2000.0;
      VariableProductionCost = parameter.Return 0.6 }

/// Random parameters for the Monte-Carlo simulation.
let randomParams =
    let averagePayableDelay    = Parameter.randomUniform 0.07 0.11
    let billingProcessingTime  = Parameter.randomUniform 0.03 0.05
    let buildingTime           = Parameter.randomUniform 0.8 1.2
    let fractionalLossRate     = Parameter.randomUniform 0.05 0.08
    let interestRate           = Parameter.randomUniform 0.09 0.15
    let price                  = Parameter.randomUniform 0.9 1.2
    let productionCapacity     = Parameter.randomUniform 2200.0 2600.0
    let requiredInvestment     = Parameter.randomUniform 1800.0 2200.0
    let variableProductionCost = Parameter.randomUniform 0.5 0.7
    { defaultParams with
        AveragePayableDelay    = Parameter.memo averagePayableDelay;
        BillingProcessingTime  = Parameter.memo billingProcessingTime;
        BuildingTime           = Parameter.memo buildingTime;
        FractionalLossRate     = Parameter.memo fractionalLossRate;
        InterestRate           = Parameter.memo interestRate;
        Price                  = Parameter.memo price;
        ProductionCapacity     = Parameter.memo productionCapacity;
        RequiredInvestment     = Parameter.memo requiredInvestment;
        VariableProductionCost = Parameter.memo variableProductionCost }

/// This is the model itself that returns experimental data.
let model (ps: Parameters) : Simulation<ResultSet> = simulation {

    let get (x: Parameter<_>) : Dynamics<_> = Parameter.lift x 

    let taxDepreciationTime    = get ps.TaxDepreciationTime
    let taxRate                = get ps.TaxRate
    let averagePayableDelay    = get ps.AveragePayableDelay
    let billingProcessingTime  = get ps.BillingProcessingTime
    let buildingTime           = get ps.BuildingTime;
    let debtFinancingFraction  = get ps.DebtFinancingFraction
    let debtRetirementTime     = get ps.DebtRetirementTime
    let discountRate           = get ps.DiscountRate
    let fractionalLossRate     = get ps.FractionalLossRate
    let interestRate           = get ps.InterestRate
    let price                  = get ps.Price
    let productionCapacity     = get ps.ProductionCapacity
    let requiredInvestment     = get ps.RequiredInvestment
    let variableProductionCost = get ps.VariableProductionCost

    // the equations below are given in an arbitrary order!

    let rec bookValue = 
        integ (lazy (newInvestment - taxDepreciation)) (num 0.0)
    and taxDepreciation = bookValue / taxDepreciationTime
    and taxableIncome = 
        grossIncome - directCosts - losses
            - interestPayments - taxDepreciation
    and production = availableCapacity
    and availableCapacity = 
        ifThenElse (Dynamics.time .>=. buildingTime) 
            productionCapacity (num 0.0)
    and accountsReceivable =
        integ (lazy (billings - cashReceipts - losses))
            (billings / (num 1.0 / averagePayableDelay 
                + fractionalLossRate))
    and awaitingBilling = 
        integ (lazy (price * production - billings))
            (price * production * billingProcessingTime)
    and billings = awaitingBilling / billingProcessingTime
    and borrowing = newInvestment * debtFinancingFraction
    and cashReceipts = accountsReceivable / averagePayableDelay
    and debt = 
        integ (lazy (borrowing - principalRepayment)) (num 0.0)
    and directCosts = production * variableProductionCost
    and grossIncome = billings
    and interestPayments = debt * interestRate
    and losses = accountsReceivable * fractionalLossRate
    and netCashFlow = 
        cashReceipts + borrowing - newInvestment
            - directCosts - interestPayments
            - principalRepayment - taxes
    and netIncome = taxableIncome - taxes
    and newInvestment = 
        ifThenElse (Dynamics.time .>=. buildingTime)
            (num 0.0) (requiredInvestment / buildingTime)
    and npvCashFlow = 
        npv netCashFlow discountRate (num 0.0) (num 1.0)
    and npvIncome = 
        npv netIncome discountRate (num 0.0) (num 1.0)
    and principalRepayment = debt / debtRetirementTime
    and taxes = taxableIncome * taxRate

    return
        [ResultSource.From ("netIncome", 
            netIncome, "Net income");
         ResultSource.From ("netCashFlow",
            netCashFlow, "Net cash flow");
         ResultSource.From ("npvIncome",
            npvIncome, "NPV income");
         ResultSource.From ("npvCashFlow",
            npvCashFlow, "NPV cash flow")]
                |> ResultSet.create
}
