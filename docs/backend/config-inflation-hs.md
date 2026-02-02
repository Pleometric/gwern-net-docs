
# Config.Inflation

**Path:** `build/Config/Inflation.hs` | **Language:** Haskell | **Lines:** ~1,100

> Static inflation rate and Bitcoin exchange rate lookup tables

---

## Overview

`Config.Inflation` is a pure data module that provides static lookup tables for historical economic data. It contains no logic—only three exports: a threshold constant, a list of annual USD inflation rates, and a comprehensive table of daily Bitcoin exchange rates.

The module exists to separate raw data from computation. While `Inflation.hs` contains the adjustment algorithms and Pandoc transformations, `Config.Inflation` supplies the underlying rates. This separation allows the data to be updated independently (e.g., when new year-end inflation figures are released) without touching the inflation calculation logic.

The data spans from 1913 to 2023 for US inflation (using CPI through 1958, then PCE thereafter) and from May 2010 (Bitcoin "Pizza Day") through December 2022 for Bitcoin exchange rates. When dates fall outside these ranges, the system carries forward the last available rate.

---

## Public API

### `minPercentage :: Double`

Threshold for triggering inflation adjustment. Set to `1.20` (20% change).

```haskell
minPercentage = 1 + 0.20
```

Amounts that haven't inflated by at least 20% are left unchanged to avoid cluttering the output with trivial adjustments.

**Called by:** `Inflation.dollarAdjuster`

---

### `inflationRatesUSD :: [Double]`

Annual percentage change in USD purchasing power, 1913–2023 (projected forward using `repeat`).

```haskell
inflationRatesUSD :: [Double]
inflationRatesUSD = let
    cpi19131958 = [0.0,1.0,2.0,12.6,18.1,...]  -- CPI: 1913-1958
    pce19592018 = [5.7,2.7,2.1,4.9,4.1,...]    -- PCE: 1959-2018
    pce20192023 = [1.43, 1.09, 4.13, 6.55, 3.77]
  in (cpi19131958 ++ pce19592018 ++ pce20192023) ++ repeat (last pce20192023)
```

**Data format:**
- Index 0 = 1913, Index 1 = 1914, etc.
- Values are percentage changes (e.g., `12.6` means 12.6% inflation that year)
- CPI used for 1913–1958; PCE (Personal Consumption Expenditures) used for 1959+
- List extended infinitely via `repeat` for future years

**Sources:**
- CPI: US Inflation Calculator (1913–1958)
- PCE: Bureau of Economic Analysis (1959–2018)
- Recent years: FRED DPCERG3A086NBEA series (2019–2023)

**Called by:** `Inflation.inflationAdjustUSD`

---

### `bitcoinUSDExchangeRateHistory :: [(String, Double)]`

Daily BTC/USD exchange rates from May 2010 through December 2022.

```haskell
bitcoinUSDExchangeRateHistory :: [(String,Double)]
bitcoinUSDExchangeRateHistory = [
    ("2010-05-20", 0.003)  -- Extrapolated from Pizza Day
  , ("2010-05-21", 0.003)
  ...
  , ("2022-01-01", 47738)
  ]
```

**Data format:**
- List of `(date-string, rate)` tuples
- Date format: `"YYYY-MM-DD"`
- Rate: USD per 1 BTC
- Entries in reverse chronological order for recent data (older data is chronological)

**Coverage:**
- 2010-05-20 to 2010-08-15: Extrapolated from Pizza Day ($30/10,000 BTC = $0.003/BTC)
- 2010-08-16 onwards: Historical prices from btcvol.info
- Data ends at 2022-01-01

**Called by:** `Inflation.bitcoinUSDExchangeRate`

---

### `inflationDollarLinkTestCases :: [((T.Text, T.Text), Inline)]`

Test cases for verifying dollar inflation adjustment. Each entry specifies an input (amount, year) and expected output Pandoc `Inline`.

```haskell
inflationDollarLinkTestCases :: [((T.Text,T.Text), Inline)]
inflationDollarLinkTestCases = [
    (("$0.01","$1913"), Span (...) [...])
  , (("$1","$1913"),    Span (...) [...])
  ...
  ]
```

**Called by:** `Inflation.inflationDollarTestSuite`

---

## Internal Architecture

### Data Layout

The module is structured as three data declarations:

```
┌─────────────────────────────────────────────────────────┐
│  minPercentage (line 8)                                 │
│    Single Double constant: 1.20                         │
├─────────────────────────────────────────────────────────┤
│  inflationRatesUSD (lines 17-24)                        │
│    ~110 CPI/PCE rates + infinite extension              │
│    Format: [rate_1913, rate_1914, ..., rate_2023, ...]  │
├─────────────────────────────────────────────────────────┤
│  inflationDollarLinkTestCases (lines 26-107)            │
│    ~40 test cases for regression testing                │
├─────────────────────────────────────────────────────────┤
│  bitcoinUSDExchangeRateHistory (lines 110-1098)         │
│    ~4,500 daily entries from 2010-2022                  │
│    Format: [(date, usd_per_btc), ...]                   │
└─────────────────────────────────────────────────────────┘
```

### Why Two Index Systems?

USD inflation uses **year-indexed** data (list position = year - 1913) because inflation is measured annually and year granularity is sufficient for dollar adjustments.

Bitcoin uses **date-indexed** data (string keys like `"2017-01-01"`) because BTC's extreme volatility requires daily granularity. A yearly average would be meaningless when the price can vary 10x within a single year.

---

## Key Patterns

### Infinite Extension via `repeat`

```haskell
inflationRatesUSD = (...) ++ repeat (last pce20192023)
```

Rather than failing on future dates, the module projects the last known inflation rate forward indefinitely. This is a pragmatic choice—inflation adjustments for very recent years won't be perfect, but the system won't crash on `[$100]($2025)` inputs.

### Pizza Day Extrapolation

```haskell
-- extrapolate from Pizza Day (22 May 2010), $30/₿10,000
("2010-05-20", 0.003)
```

The famous first commercial Bitcoin transaction (10,000 BTC for two pizzas worth ~$30) establishes the $0.003/BTC baseline. All dates before mid-August 2010 use this extrapolated rate.

### Test Cases as Ground Truth

The `inflationDollarLinkTestCases` list serves dual purposes:
1. Regression testing via `Inflation.inflationDollarTestSuite`
2. Documentation of expected output format (Pandoc `Span` structure with metadata attributes)

---

## Configuration

This module *is* the configuration. To update:

| Data | Update Frequency | Source |
|------|------------------|--------|
| `inflationRatesUSD` | Annually (January) | BEA PCE data, FRED |
| `bitcoinUSDExchangeRateHistory` | Periodically | btcvol.info or similar |
| `minPercentage` | Rarely | Adjust if 20% threshold feels wrong |

When adding new inflation rates, append to `pce20192023` (or rename to `pce2019YYYY`) and the `repeat` will handle the rest.

---

## Integration Points

### Imported By

- **`Inflation.hs`** — The only consumer; uses all three data exports

### Data Flow

```
Config.Inflation                    Inflation.hs
┌─────────────────┐               ┌─────────────────────┐
│ inflationRates  │──────────────▶│ inflationAdjustUSD  │
│ USD             │               │                     │
├─────────────────┤               ├─────────────────────┤
│ bitcoinUSD      │──────────────▶│ bitcoinQuery        │
│ ExchangeRate    │               │ bitcoinUSDExchange  │
│ History         │               │ Rate                │
├─────────────────┤               ├─────────────────────┤
│ minPercentage   │──────────────▶│ dollarAdjuster      │
├─────────────────┤               ├─────────────────────┤
│ inflationDollar │──────────────▶│ inflationDollar     │
│ LinkTestCases   │               │ TestSuite           │
└─────────────────┘               └─────────────────────┘
```

---

## See Also

- [Inflation.hs](/backend/inflation-hs) - Adjustment algorithms that consume this data
- [Typography.hs](/backend/typography-hs) - Related Pandoc AST transformations
- [Config.Typography](/backend/config-typography-hs) - Similar config module pattern for typography
- [Config.Misc](/backend/config-misc-hs) - Related configuration including `currentYear`
- [hakyll.hs](/backend/hakyll-hs) - Build pipeline that uses inflation adjustments
- [GTX.hs](/backend/gtx-hs) - Another data-heavy config module (annotation database)
