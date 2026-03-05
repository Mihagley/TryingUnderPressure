# Fertility Intentions and Recession Analysis (pairfam Panel)

This repository contains R scripts for preparing panel survey data and estimating weighted regression models on fertility intentions and childbearing outcomes.

## What this script does

The main analysis script (`Regression.R`) takes a prepared panel dataset (`panel_data`) and:

1. Builds an analytical sample of women observed in wave 1 with valid fertility-intention responses.
2. Constructs core covariates (recession exposure, age 35+, lagged partnership, parity, and trying status).
3. Runs parity-stratified weighted OLS models with clustered standard errors.
4. Repeats analysis by baseline trying status (trying vs. not trying in wave 1).
5. Estimates an additional model using a childbearing outcome (`has_child`) instead of trying intentions.

## Problem it solves

The script provides a reproducible way to evaluate how macroeconomic conditions (specifically recession years) relate to fertility-related behavior, while accounting for:

- age-related heterogeneity (`age35`),
- partnership dynamics (`partner_lag`),
- parity differences,
- panel clustering at the respondent level, and
- survey weights.

In short, it standardizes a workflow for comparing fertility intention and childbearing models across meaningful subgroups.

## How it works (high level)

### 1) Data setup

`Regression.R` expects a `data.table` named `panel_data` already loaded in memory, keyed by `id` and `wave`.

### 2) Sample construction

- Selects women (`sex_gen == 2`) with wave-1 fertility intention values in `{1,2}`.
- Creates `trying_w1` based on wave-1 `frt3`.
- Merges baseline trying status back onto all female panel rows.

### 3) Variable engineering

Constructs:

- `recession`: interview year indicator (`2008 <= inty < 2011`),
- `partner`: inferred from partner ID (`pid`),
- `age35`: age threshold indicator,
- `partner_lag`: respondent-level lag of partnership,
- `parity`: binary indicator based on `nkidsbio`,
- `trying`: binary outcome derived from `frt3`.

### 4) Modeling

The `run_parity_models()` function:

- filters to complete cases for required variables,
- splits by parity subgroup,
- fits weighted OLS via `fixest::feols()` with `id`-clustered SEs,
- prints formatted model tables via `modelsummary::msummary()`,
- computes CR2 robust tests via `clubSandwich::coef_test()`,
- additionally estimates respondent fixed-effects variants.

### 5) Extended analyses

- **Analysis A:** runs parity models separately for women trying vs. not trying in wave 1.
- **Analysis B:** changes outcome to `has_child` and re-estimates the baseline-style model.

## Requirements and dependencies

### R version

- Recommended: **R 4.1+** (older versions may work if package versions are compatible).

### R packages

For `Regression.R`:

- `data.table`
- `fixest`
- `modelsummary`
- `clubSandwich`

For `Raw data.R` (data construction):

- `haven`
- `dplyr`
- `tidyr`
- `data.table`

### Input data requirements

`Regression.R` assumes `panel_data` includes at least these fields:

`id`, `wave`, `sex_gen`, `frt3`, `inty`, `age`, `pid`, `nkidsbio`, `cd1weight`, `cd2weight`.

`Raw data.R` expects multiple Stata files (e.g., `anchor1.dta`, ..., `anchor13_cati.dta`) in a configured source directory.

## Installation and run

1. Clone the repository.
2. Open R (or RStudio) in the repository root.
3. Install required packages.
4. Build/load `panel_data`.
5. Run the analysis script.

### Example setup

```r
# Install dependencies
install.packages(c(
  "data.table", "fixest", "modelsummary", "clubSandwich",
  "haven", "dplyr", "tidyr"
))

# Option A: Build data from Stata files
source("Raw data.R")
# Then load/export as needed and ensure an object named panel_data exists

# Option B: If panel_data is already prepared, load it directly
# panel_data <- data.table::fread("path/to/prepared_panel_data.csv")

# Run analysis
source("Regression.R")
```

## Example usage

### Run only regression analysis (with preloaded data)

```r
library(data.table)
panel_data <- fread("AnchorBICLATE_clean.csv")
source("Regression.R")
```

### Expected outputs

The script prints to console:

- model summaries for parity-specific regressions,
- formatted model tables,
- CR2 coefficient tests,
- fixed-effects model summaries,
- childbearing-outcome model summary.

## Key functions and components

### `clean_and_select(df, vars)` in `Raw data.R`

- Keeps only variables present in each source file.
- Coerces columns to vectors to normalize types before row-binding.

### `run_parity_models(dt, label)` in `Regression.R`

Core estimation helper that:

- enforces complete-case modeling data,
- estimates parity-specific weighted models,
- generates publication-style summaries,
- computes alternative CR2 inference,
- estimates respondent fixed-effects variants.

### Script-level analysis blocks in `Regression.R`

- **Analysis A:** subgroup estimation by wave-1 trying behavior.
- **Analysis B:** alternative dependent variable (`has_child`).

## Notes and caveats

- Paths in `Raw data.R` are currently hard-coded for a local Windows environment and should be edited for your system.
- `Regression.R` is not a standalone script unless `panel_data` is created or loaded beforehand.
- Some variable recoding assumptions (e.g., coding of `frt3`) are study-specific and should be validated against the codebook.
