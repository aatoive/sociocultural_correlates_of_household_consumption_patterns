
# title: Robustness analyses for the manuscript:
# "Sociocultural correlates of household consumption patterns: 
# National welfare and cultural values shape household spending.”
# author: Aada Toivettula
# date: 2026-08-27

# Load packages ---------------------------------------------------------------

library(dplyr)
library(tibble)
library(tidyr)
library(writexl)

# Import data -----------------------------------------------------------------

comb_df <- readRDS("comb_df.rds")

# Define robustness-analysis years.

analysis_years <- c(2018, 2019, 2021, 2022)

# Define the display order for welfare and cultural value indicators.

ind_ord <- c("HDI", "LE", "EYS", "MYS", "GNIPC", 
             "IHDI", "GII", "IDVCOLL", "FLXMON")

# Convert p-values to significance stars.

p_stars <- function(p) {
  case_when(p < 0.001 ~ "***",
            p < 0.01  ~ "**",
            p < 0.05  ~ "*",
            TRUE      ~ "")
}

# Create an empty list to store yearly tables.

corr_robust_tbls <- list()

# Run correlation analyses by year --------------------------------------------

for (yr in analysis_years) {
  
  # Create year-specific data set.
  
  comb_yr_df <- comb_df |> filter(year == yr)
  
  # Compute bivariate Pearson correlations between expenditure share and each 
  # welfare or cultural value indicator, separately by consumption category.
  
  corr_yr_rslts <- comb_yr_df |>
    select(-any_of(c("country", "year", "cult_reg"))) |> 
    pivot_longer(cols = all_of(ind_ord),
                 names_to = "ind",
                 values_to = "ind_val") |> 
    group_by(ind, consum_cat) |>
    reframe({
      corr_anal <- cor.test(exp_pct, ind_val, method = "pearson")
      
      tibble(r = unname(corr_anal$estimate),
             p = corr_anal$p.value,
             ci_low = corr_anal$conf.int[1],
             ci_high = corr_anal$conf.int[2])
    }) |>
    ungroup()
  
  # Format correlations, significance stars, and confidence intervals for export.
  
  corr_yr_tbl <- corr_yr_rslts |> 
    mutate(ind = factor(ind, levels = ind_ord),
           stars = p_stars(p),
           cell = paste0(round(r, 2), 
                         stars, 
                         " (", round(ci_low, 2), ", ",
                         round(ci_high, 2), ")")) |>
    select(consum_cat, ind, cell) |>
    pivot_wider(names_from = ind, values_from = cell) |>
    select(consum_cat, all_of(ind_ord))
  
  # Store formatted correlation table.
  
  corr_robust_tbls[[paste0(yr)]] <- corr_yr_tbl
}

# Export correlation tables, with one sheet per year.

write_xlsx(corr_robust_tbls, "corr_robust_tbl.xlsx")

# -----------------------------------------------------------------------------
