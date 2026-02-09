# packages
library(data.table)
library(fixest)        # fast OLS + clustered SE
library(modelsummary)  # table
library(clubSandwich)  # CR2 if desired

# assume: panel_data with id, wave, sex_gen (1=male,2=female), interview_year, age, parity,
# trying (1 yes, 0 no), partner (1 in partnership, 0 otherwise), weight (pweight)
setDT(panel_data)
setkey(panel_data, id, wave)

# 1) sample: women not trying in wave 1
base_try <- unique(
  panel_data[wave == 1 & sex_gen == 2 & frt3 %in% c(1L, 2L),
             .(id, trying_w1 = as.integer(frt3 == 1L))],
  by = "id"
)

# Work file: females only, merged with baseline; keep those NOT trying at baseline
dt <- panel_data[sex_gen == 2][base_try, on = "id"][trying_w1 == 0L]                 

# 2) construct regressors
dt[, recession := as.integer(inty >= 2008 & inty < 2011)]
dt[, partner := ifelse(is.na(pid),0,1)]
dt[, age35 := as.integer(age >= 35)]
dt[, partner_lag := shift(partner, type = "lag"), by = id]
dt[, parity := fifelse(nkidsbio > 0, 1L, 0L)]
dt[, trying := fifelse(frt3 == 2, 1L, 0L)]

# keep valid rows for modeling
vars_needed <- c("id","wave","parity","trying","recession","age35","partner_lag","cd2weight")
dtm <- dt[ dt[, complete.cases(.SD), .SDcols = vars_needed] ]

# 3) stratify by parity
d0 <- dtm[parity == 0]
d1 <- dt[nkidsbio == 1]

#0 4) weighted OLS with cluster-robust SEs at id
m0 <- feols(trying ~ recession*age35 + partner_lag, data = d0, weights = ~cd2weight, cluster = ~id); summary(m0)
m1 <- feols(trying ~ recession*age35 + partner_lag, data = d1, weights = ~cd2weight, cluster = ~id); summary(m1)

# 5) tidy table
msummary(
  list("Parity 0 (childless)" = m0, "Parity 1" = m1),
  gof_omit = "IC|Log|Adj|Within",
  coef_map = c(
    "(Intercept)"        = "Intercept",
    "age35"              = "Age ≥ 35",
    "recession"          = "Recession (2009–2010)",
    "recession:age35"    = "Age ≥ 35 × Recession",
    "age35:recession"    = "Age ≥ 35 × Recession",  # safe alias
    "partner_lag"        = "Partner (lagged)"
  ),
  stars = TRUE
)

# CR2 example for m0
ct_m0 <- clubSandwich::coef_test(m0, vcov = "CR2", cluster = d0$id)
ct_m1 <- clubSandwich::coef_test(m1, vcov = "CR2", cluster = d1$id)
ct_m0; ct_m1


m0_fe <- feols(
  trying ~ recession*age35 + partner_lag | id,
  data = d0,
  cluster = ~id,
  weights = ~cd2weight
);summary(m0_fe)

m1_fe <- feols(
  trying ~ recession*age35+ partner_lag | id,
  data = d1,
  cluster = ~id,
  weights = ~cd1weight
);summary(m1_fe)
