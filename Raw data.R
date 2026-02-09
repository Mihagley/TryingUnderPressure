#Create raw dataset. 
rm(list=ls())
# Load necessary libraries
library(haven)
library(dplyr)
library(tidyr)
library(data.table)

# Set working directories
source_dir <- "C:/Users/mhagley/Documents/Pairfam_data/Stata"
save_dir <- gsub("\\\\", "/", "C:/Users/mhagley/Documents/Pairfam_data/save")


# List of  variables to retain
original_vars <- c(
  "id",
  "age",
  "wave",
  "inty",
  "intm",
  "doby_gen",
  "dobm_gen",
  "sex_gen",
  "homosex",
  "sex21",
  "sample",
  "frt3",
  "d0",
  "relstat",
  "frt4i1",
  "frt4i2",
  "frt4i3",
  "frt4i4",
  "frt4i5",
  "frt4i6",
  "bula",
  "isced",
  "sex5",
  "frt1",
  "frt2",
  "nkidsp",
  "nkidsbio",
  "sex8",
  "pregnant",
  "cohort",
  "cd1weight",
  "cd2weight",
  "page",
  "pid",
  "ykage",
  "k1doby_gen",
  "k1dobm_gen",
  "k2doby_gen",
  "k2dobm_gen",
  "k1type",
  "k2type",
  "pdoby_gen", 
  "pdobm_gen" ,
  "ehc8k1d",  "ehc8k2d",  "ehc8k3d",  "ehc8k4d",  "ehc8k5d",  "ehc8k6d",  "ehc8k7d",  "ehc8k8d",  "ehc8k9d",  "ehc8k10d", 
  "ehc8k1m",  "ehc8k2m",  "ehc8k3m",  "ehc8k4m",  "ehc8k5m",  "ehc8k6m",  "ehc8k7m",  "ehc8k8m",  "ehc8k9m",  "ehc8k10m", 
  "ehc8k1y",  "ehc8k2y",  "ehc8k3y",  "ehc8k4y",  "ehc8k5y",  "ehc8k6y",  "ehc8k7y",  "ehc8k8y",  "ehc8k9y",  "ehc8k10y", 
  "ehc8k11d", "ehc8k11m", "ehc8k11y", "ehc8k12d", "ehc8k13d", "ehc8k14d", "ehc8k15d", "ehc8k12m", "ehc8k13m", "ehc8k14m", 
  "ehc8k15m", "ehc8k12y", "ehc8k13y", "ehc8k14y", "ehc8k15y","wavedist"
)

clean_and_select <- function(df, vars) {
  common_vars <- intersect(names(df), vars)
  df %>%
    select(all_of(common_vars)) %>%
    mutate(across(everything(), ~ as.vector(.)))
}

# Load and merge all panel files while retaining only the original variables
files <- c("anchor1", "anchor1_DD", "anchor2", "anchor3", "anchor4", "anchor5", 
           "anchor6", "anchor7", "anchor8", "anchor9", "anchor10", "anchor11", 
           "anchor12_capi", "anchor12_cati", "anchor13_capi", "anchor13_cati")

panel_data <- lapply(files, function(file) {
  df <- read_dta(file.path(source_dir, paste0(file, ".dta")))
  clean_and_select(df, original_vars)
}) %>% bind_rows()

# Convert to data.table
setDT(panel_data)

# Sort by id and wave
setorder(panel_data, id, wave)

# Generate intdat
panel_data[, intdat := (inty - 1900) * 12 + intm]

# Generate INTDAT



panel_data[, INTDAT := max(intdat, na.rm = TRUE), by = id]

# Generate DOB
panel_data[, DOB := ((doby_gen - 1900) * 12 + dobm_gen) - 1]

# Save the intermediate data as a data.table binary file
fwrite(panel_data, file.path(save_dir, "AnchorBICLATE_clean.csv"))
