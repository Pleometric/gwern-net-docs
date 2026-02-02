
# Inflation.hs

**Path:** `build/Inflation.hs` | **Language:** Haskell | **Lines:** ~237

> Automatic inflation adjustment for historical dollar amounts and Bitcoin exchange rates

---

## Overview

Inflation.hs fights [money illusion](https://en.wikipedia.org/wiki/Money_illusion) by automatically converting historical nominal dollar amounts to present-day real values. A 1950 price displayed without context is nearly meaningless—this module ensures readers see both the original amount and its inflation-adjusted equivalent.

The module handles two currencies: US dollars (using CPI/PCE data from 1913 onward) and Bitcoin (using daily USD exchange rates from 2010 onward). Dollar adjustments use year-level precision while Bitcoin requires full date precision due to its extreme volatility. Both produce rich HTML output with the adjusted amount prominent, original amount in superscript, and year in subscript.

The design philosophy prioritizes reader comprehension over precision—adjustments below 20% are suppressed as not misleading enough to warrant annotation. The module integrates at two levels: as a Pandoc AST transformer for Markdown content, and as a string-munging function for annotation titles.

---

## Public API

### `nominalToRealInflationAdjuster :: Inline -> Inline`

The primary Pandoc transformer. Accepts a `Link` or `Span` inline element containing a dollar/Bitcoin amount and converts it to an inflation-adjusted `Span` with metadata attributes.

**Input formats:**
- Link syntax: `[$50.50]($1970)` or `[₿0.5](₿2017-01-01)`
- Span syntax: `[$50]{inflation=$2000}` (for use inside link anchors)

**Output:** A `Span` with class `inflation-adjusted` containing:
- The adjusted amount as primary text
- Superscript with original amount
- Subscript with original year
- Data attributes for JS/CSS processing

```haskell
-- Example transformation:
-- [$50.50]($1970) →
-- <span class="inflation-adjusted"
--       data-year-original="1970" data-amount-original="50.50"
--       data-year-current="2024" data-amount-current="343.83"
--       title="CPI inflation-adjusted...">
--   $343.83<span class="subsup"><sup>$50.50</sup><sub>1970</sub></span>
-- </span>
```

**Called by:** `hakyll.hs` (page compilation), `LinkMetadata.hs` (annotation processing)
**Calls:** `dollarAdjuster`, `bitcoinAdjuster`

---

### `nominalToRealInflationAdjusterHTML :: String -> String -> String`

String-based inflation adjustment for HTML annotation titles. Takes a date string and HTML content, finds dollar amounts, and wraps them in inflation spans.

```haskell
nominalToRealInflationAdjusterHTML :: String  -- ^ Date (YYYY-MM-DD format)
                                   -> String  -- ^ HTML content with $ amounts
                                   -> String  -- ^ HTML with adjusted amounts
```

**Called by:** `LinkMetadata.hs:439` (title processing)
**Calls:** `nominalToRealInflationAdjuster`, `Utils.toHTML`

**Important:** Must be called *after* `typesetHtmlField`/`titlecase'` as those transforms may break the inline HTML spans.

---

### `inflationDollarTestSuite :: [((Text, Text), Inline, Inline)]`

Returns failing test cases (expected ≠ actual). Used by `Test.hs` for regression testing. Returns empty list when all tests pass.

**Called by:** `Test.hs:24`

---

### `isInflationURL :: Text -> Bool`

Checks if a URL is an inflation pseudo-URL (starts with `$` or `₿`).

**Note:** Defined in `Utils.hs` but re-exported from `Inflation` to avoid circular dependencies.

**Called by:** `Interwiki.hs`, `Query.hs`, `generateLinkBibliography.hs`, `Utils.host`

---

### `isInflationLink :: Inline -> Bool`

Checks if a Pandoc `Inline` is an inflation-annotated link or span.

**Called by:** Various link processing functions

---

## Internal Architecture

### Core Adjustment Flow

```
nominalToRealInflationAdjuster
    ├── dollarAdjuster (for $YYYY URLs)
    │   ├── multiplyByUnits (handles k/m/b/t suffixes)
    │   ├── dollarAdjust
    │   │   └── inflationAdjustUSD (applies cumulative inflation rates)
    │   └── printDouble (formatting)
    │
    └── bitcoinAdjuster (for ₿YYYY-MM-DD URLs)
        ├── bitcoinAdjust
        │   └── bitcoinQuery (daily exchange rate lookup)
        ├── bitcoinUSDExchangeRate (inflation-adjusted rate map)
        └── dollarAdjust (adjusts historical USD to present)
```

### Data Structures

**Inflation rates** (`Config.Inflation.inflationRatesUSD`): List of annual percentage changes indexed by (year - 1913). CPI data for 1913-1958, PCE data from 1959 onward. The last rate is repeated infinitely for future projections.

**Bitcoin rates** (`Config.Inflation.bitcoinUSDExchangeRateHistory`): Association list of `(YYYY-MM-DD, rate)` pairs. Missing dates are linearly interpolated; dates outside range carry forward/backward.

### Output Span Structure

```html
<span class="inflation-adjusted"
      data-year-original="1970"
      data-amount-original="50.50"
      data-year-current="2024"
      data-amount-current="343.83"
      title="CPI inflation-adjusted US dollar: from nominal $50.50 in 1970 → real $343.83 in 2024">
  $343.83
  <span class="subsup">
    <sup>$50.50</sup>
    <sub>1970</sub>
  </span>
</span>
```

---

## Key Patterns

### Minimum Threshold Suppression

Small adjustments (\<20%) are stripped of annotation rather than shown, reducing noise for recent amounts:

```haskell
if (adjustedDollar / oldDollar) < C.minPercentage || oldDollar == 0
   then Span nullAttr text  -- just strip the annotation
   else ...  -- full inflation span
```

This means `[$100]($2023)` in 2024 produces just `$100` with no adjustment markup.

### Unit Multiplier Expansion

The `multiplyByUnits` function expands shorthand notation:

```haskell
-- '$50.5k' → 50500.0
-- '$1.2m'  → 1200000.0
-- '$3.5b'  → 3500000000.0
-- '$1t'    → 1000000000000.0
```

### Cumulative Inflation Calculation

Inflation is computed by multiplying annual rates between the two years:

```haskell
inflationAdjustUSD :: Double -> Int -> Int -> Double
inflationAdjustUSD d yOld yCurrent = d * product rates
  where
    percents = slice (yOld-1913) (yCurrent-1913) C.inflationRatesUSD
    rates = map (\r -> 1 + (r/100)) percents
```

### Bitcoin Double-Adjustment

Bitcoin amounts undergo two adjustments:
1. Convert historical BTC to historical USD at that day's exchange rate
2. Inflation-adjust that USD amount to present day

```haskell
bitcoinAdjust cy oldBitcoinAmount oldDate =
  let oldExchangeRate = bitcoinQuery cy oldDate
  in oldBitcoinAmount * oldExchangeRate  -- then inflationAdjustUSD is applied
```

---

## Configuration

### Config/Inflation.hs (~1,098 lines)

| Export | Type | Description |
|--------|------|-------------|
| `minPercentage` | `Double` | 1.20 — suppress adjustments below 20% |
| `inflationRatesUSD` | `[Double]` | Annual % changes, 1913-present (extended with `repeat`) |
| `bitcoinUSDExchangeRateHistory` | `[(String, Double)]` | Daily rates from 2010-05-20 onward |
| `inflationDollarLinkTestCases` | `[((Text,Text), Inline)]` | Regression test cases |

### Config/Misc.hs

| Export | Type | Description |
|--------|------|-------------|
| `currentYear` | `Int` | Runtime-computed current year (via `unsafePerformIO`) |

### Updating Inflation Data

To update rates:

1. **CPI/PCE rates**: Add new annual values to `pce20192023` (or create a new chunk) in `Config/Inflation.hs`
2. **Bitcoin rates**: Append new daily entries to `bitcoinUSDExchangeRateHistory`
3. **Test cases**: Update expected values in `inflationDollarLinkTestCases` for the new `currentYear`

The last available rate is carried forward indefinitely, so stale data degrades gracefully.

---

## Integration Points

### Pandoc Pipeline Integration

Called in two places during page compilation:

```haskell
-- hakyll.hs:365 — page body processing
walk (map nominalToRealInflationAdjuster) pb

-- LinkMetadata.hs:447 — annotation body processing
walk (linkIcon . linkLive . nominalToRealInflationAdjuster) $
```

### Annotation Title Processing

```haskell
-- LinkMetadata.hs:439
let titleHtml = nominalToRealInflationAdjusterHTML c $ typesetHtmlField $ titlecase' a
```

### URL Filtering

`isInflationURL` is used throughout the codebase to identify and skip inflation pseudo-URLs that shouldn't be treated as real links:

- `Utils.host`: Skip extracting host from inflation URLs
- `Interwiki.hs`: Distinguish from interwiki links
- `Query.hs`, `generateLinkBibliography.hs`: Filter out during link analysis

### CSS Classes

The frontend can style inflation adjustments via:
- `.inflation-adjusted` — the outer container
- `.subsup` — the superscript/subscript wrapper

Data attributes enable JS-based dynamic updates or tooltips.

---

## See Also

- [Config.Inflation](/backend/config-inflation-hs) - Raw inflation data (CPI/PCE rates, Bitcoin history)
- [hakyll.hs](/backend/hakyll-hs) - Main build pipeline that calls inflation transforms
- [Typography.hs](/backend/typography-hs) - Related Pandoc AST transforms (date-range subscripts)
- [LinkMetadata.hs](/backend/link-metadata-hs) - Annotation processing pipeline
- [Interwiki.hs](/backend/interwiki-hs) - Similar pseudo-URL link syntax
- [Config.Misc](/backend/config-misc-hs) - Provides `currentYear` for calculations
