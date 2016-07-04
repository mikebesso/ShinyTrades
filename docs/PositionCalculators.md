<center><h1>Position Size Calculators</h1></center>


# Introduction

Successful trading plans and strategies include a rules for determining the size of a trade.  
There are three obvious position sizing strategies:

* Constant number of shares/contracts/lots
  It is a stretch to call this a stategy.  Nonetheless, many traders just buy and sell the same number of shares/contracts or lots regardless of the the actual risk or potential reward.
  
* Constant trade total price
  While there is a bit more calculation to this strategy, it still does not take into account the actual risk or potential reward.
  
* Constant risk
  Most successful strategies rely taking the same amount of risk on every trade.  Depending on what is being traded, this can be an interesting calculation.

The position size calculators in this app support the constant risk strategy.


## Definitions

Before we get started, let's define some terminology.  First, for efficiency of explanation, we will assume:

* Shares and lots are synonymous with contracts.  We will use the term "contract" in the discussion below.
* PIPS is synonymous with ticks.  We will use the term "tick" in the the discussion below.

While these assumptions might not be "exactly" true.  These concepts are similar enough that we can avoid repeating the same discussion three times.


<table border = "1">
<th>
  <tr>
    <td>Variable</td>
    <td>Definition</td></th>
  </tr>
</th>
<tbody>
  <tr>
    <td>Account Size</td>
    <td>Amount of money available to trade.</td>
  </tr>
  <tr>
    <td>Max Risk (%)</td>
    <td>The percentage of the account to risk on each trade</td>
  </tr>
  <tr>
    <td>Point</td>
    <td>The integral unit of measurement of value of each contract</td>
  </tr>
  <tr>
    <td>Ticks per Point</td>
    <td>The number of increments between points</td>
  </tr>
  <tr>
    <td>Tick Value</td>
    <td>The dollar value of each tick</td>
  </tr>
</tbody>

</table>


## Calculations

### Max Risk

The maximum risk we are willing to take on this trade based on our trading rules.  
Maximium risk is calculated as follows:

<math display = "block">
  <mi>[Max Risk]</mi>
  <mo>&equals;</mo>
  <mi>[Account Size]</mi>
  <mo>&times;</mi>
  <mi>[Max Risk (%)]</mi>
</math>


### Risk Per Contract

The risk of trading a single contract.

<math display = "block">
  <mi>[Risk Per Contract]</mi>
  <mo>&equals;</mo>
  <mrow>
    <mi>[Pip Value]</mi>
    <mo>&times;</mo>
    <mrow>
      <mfrac numalign = "left">
        <mfenced open = "|" close = "|" separators = "">
          <mi>[Entry]</mi>
          <mo>&minus;</mi>
          <mi>[Stop Loss]</mi>
        </mfenced>
        <mrow>
          <mspace width = "3em" /><mi>[Ticks]</mi><mspace width = "3em" />
        </mrow>
      </mfrac>
    </mrow>
  </mrow>
</math>


### Position Size

Number of contracts to buy or seel based on risk management rules.
<math display = "block">
  <mi>[Position Size]</mi>
  <mo>&equals;</mo>
  <mfrac>
    <mi>[Max Risk]</mi>
    <mi>[Risk Per Contract]</mi>
  </mfrac>
</math>




In the case of Equities, these calculations are simple:

* There are 100 ticks per point
* Each has a value of $0.01

Therefore, each point is $1.00.  Though, we prefer to trade in blocks of 100 shares.

In the case of FOREX, these calculations depend on:

* Lot Size (Standard, Mini, Micro)
* Number of PIPs per Point
* PIP Value (a calculation that depends on the currency pair and lot size)

In the case of Futures, these calculations depend on two concepts that are attributes of what is being traded:

* Number of Ticks per Point
* Tick Value


We use position size calculators to determine the appropriate position size for a trade based on risk management rules.
