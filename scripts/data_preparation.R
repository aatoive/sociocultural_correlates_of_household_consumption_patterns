
# title: Data preparation for the manuscript:
# "Sociocultural correlates of household consumption patterns: 
# National welfare and cultural values shape household spending.”
# author: Aada Toivettula
# date: 2026-08-27

# Load packages ---------------------------------------------------------------

library(dplyr)
library(readxl)
library(tibble)
library(tidyr)
library(writexl)

# Import data -----------------------------------------------------------------

# Household consumption expenditure:
# https://data.un.org/Data.aspx?d=SNA&f=group_code%3A302 

consum_df <- read.csv("consumption_data.txt",
                      sep = ";",
                      header = TRUE,
                      fileEncoding = "UTF-8")

# Human development/welfare indicators:
# https://hdr.undp.org/data-center/documentation-and-downloads

welf_df <- read.csv("welfare_data.csv",
                    sep = ",",
                    header = TRUE,
                    fileEncoding = "latin1")

# Cultural value indicators:
# https://doi.org/10.1016/j.intman.2022.100971 

cult_value_df <- read_excel("cultural_value_data.xlsx")

# Prepare consumption data ----------------------------------------------------

# Keep household consumption rows and remove non-country/footnote rows.

consum_ed_df <- consum_df  |>  
  filter(!grepl("general government|non-profit", 
                Sub.Group, 
                ignore.case = TRUE)) |> 
  mutate(Sub.Group = trimws(Sub.Group)) |> 
  filter(!is.na(Sub.Group) & Sub.Group != "") |> 
  subset(!grepl("[0-9]", Country.or.Area) & Country.or.Area != "footnote_SeqID")

# Keep the most recent (highest) series within each country-year.

# Calculate item shares using:
#   1. Household final consumption expenditure in the domestic market
#   2. Household final consumption expenditure, if the domestic-market total is unavailable

# Helper function for returning the first value if present, otherwise NA.

first_or_na <- function(x) {
  if (length(x) == 0) NA_real_ else first(x)
}

consum_ed_df <- consum_ed_df |>
  group_by(Country.or.Area, Year) |>
  mutate(
    exp_tot_dom = first_or_na(
      Value[Item == "Equals: Household final consumption expenditure in domestic market"]),
    exp_tot_fin = first_or_na(
      Value[Item == "Equals: Household final consumption expenditure"]),
    exp_tot = coalesce(
      exp_tot_dom,
      exp_tot_fin)) |>
  filter(Series == max(Series, na.rm = TRUE)) |>
  mutate(exp_pct = Value / exp_tot) |>
  ungroup()

# Rename the selected columns and remove unnecessary columns
# (metadata and temporary variables).

consum_ed_df <- consum_ed_df |>
  rename(country = Country.or.Area,
         year = Year,
         consum_cat = Item) |>
  select(-any_of(c("SNA93.Table.Code", "Sub.Group", "SNA93.Item.Code", "Series",
                   "Currency", "SNA.System", "Fiscal.Year.Type", "Value",
                   "Value.Footnotes", "exp_tot_dom", "exp_tot_fin", "exp_tot")))

# Remove total, adjustment, and miscellaneous consumption categories.

unnecessary_cats <- c(
  "Miscellaneous goods and services",
  "Equals: Household final consumption expenditure in domestic market",
  "Plus: Direct purchases abroad by residents",
  "Plus:",
  "Less: Direct purchases in domestic market by non-residents",
  "Equals: Household final consumption expenditure"
)

consum_ed_df <- consum_ed_df |>
  filter(!consum_cat %in% unnecessary_cats)

# Count distinct complete-case countries per year.

country_ct_consum_yr <- consum_ed_df |>
  filter(if_all(everything(), ~ !is.na(.))) |>
  summarise(countries = n_distinct(country), .by = year)

# Main analyses use 2020 because it has the largest country coverage in the 2020s.
# Retain 2018, 2019, 2021, and 2022 alongside 2020 for robustness checks.

analysis_years <- c(2018, 2019, 2020, 2021, 2022)

consum_ed_df <- consum_ed_df |>
  filter(year %in% analysis_years)

# Abbreviate consumption category names.

consum_cat_abbr <- c(
  "Food and non-alcoholic beverages" = "FOOD",
  "Alcoholic beverages, tobacco and narcotics" = "ALC",
  "Clothing and footwear" = "CLOTH",
  "Housing, water, electricity, gas and other fuels" = "HOUS",
  "Furnishings, household equipment and routine maintenance of the house" = "FURN",
  "Health" = "HLTH",
  "Transport" = "TRANSP",
  "Communication" = "COMM",
  "Recreation and culture" = "REC",
  "Education" = "EDU",
  "Restaurants and hotels" = "REST"
)

consum_ed_df <- consum_ed_df |>
  mutate(consum_cat = recode(consum_cat, !!!consum_cat_abbr))

# Count distinct countries by consumption category in the 2020 sample.

country_ct_consum_cat_20 <- consum_ed_df |>
  filter(year == 2020) |>
  filter(!is.na(exp_pct)) |>
  summarise(countries = n_distinct(country), .by = consum_cat) |>
  arrange(consum_cat)

# Prepare welfare data --------------------------------------------------------

# Keep the following welfare indicators for the same years 
# as the consumption data (2018–2022):
#    - HDI (Human Development Index) 
#    - LE (life expectancy at birth)
#    - EYS (expected years of schooling)
#    - MYS (mean years of schooling)
#    - GNIPC (Gross National Income per capita)
#    - IHDI (inequality-adjusted Human Development Index)
#    - GII (Gender Inequality Index)

welf_vars <- c("hdi", "le", "eys", "mys", "gnipc", "gii", "ihdi")

# Reshape the data from wide format to long country-year format.

welf_cols <- paste0(rep(welf_vars,
                        each = length(analysis_years)),
                    "_",
                    rep(analysis_years, times = length(welf_vars)))

welf_ed_df <- welf_df |>
  select(any_of(c("country", welf_cols))) |>
  pivot_longer(cols = -country,
               names_to = c(".value", "year"),
               names_pattern = "(.+)_(\\d{4})") |>
  mutate(year = as.integer(year)) |>
  filter(year %in% analysis_years) |>
  rename_with(toupper, -c(country, year))

# Remove aggregate rows from welfare data.

welf_agg_rows <- c("Very high human development",
                   "High human development",
                   "Medium human development",
                   "Low human development",
                   "Arab States",
                   "East Asia and the Pacific",
                   "Europe and Central Asia",
                   "Latin America and the Caribbean",
                   "South Asia",
                   "Sub-Saharan Africa",
                   "World")

welf_ed_df <- welf_ed_df |> filter(!country %in% welf_agg_rows)

# Count distinct countries by welfare indicator in the 2020 sample.

country_ct_welf_ind_20 <- welf_ed_df |>
  filter(year == 2020) |>
  pivot_longer(cols = -c(country, year), names_to = "indicator", values_to = "value") |>
  filter(!is.na(value)) |>
  summarise(countries = n_distinct(country), .by = indicator) |>
  arrange(indicator)

# Prepare cultural value data -------------------------------------------------

# Remove unused columns, rename country column, and drop empty rows.

cult_value_ed_df <- cult_value_df |> 
  select(-any_of("countabbrev")) |> 
  rename(country = VAR00023) |> 
  slice(-(206:252))

# Count distinct countries for both cultural value indicators.

country_ct_cult_ind <- cult_value_ed_df |>
  pivot_longer(cols = -country, names_to = "indicator", values_to = "value") |>
  filter(!is.na(value)) |>
  summarise(countries = n_distinct(country), .by = indicator) |>
  arrange(indicator)

# Combine all three data sets -------------------------------------------------

# Standardize country names.

# Remove text in parentheses and square brackets, trim extra whitespace, and 
# manually harmonize country names that are written differently across data sets.

standardize_countries <- function(x) {
  
  x <- trimws(x)
  
  # Recode names where parentheses contain essential information
  x <- recode(
    x,
    "Korea (Democratic People's Rep. of)" = "North Korea",
    "Korea (Republic of)" = "South Korea",
    "Congo" = "Republic of the Congo",
    "Congo (Democratic Republic of the)" = "Democratic Republic of the Congo",
    .default = x
  )
  
  # Remove non-essential text in parentheses and square brackets
  x <- gsub("\\s*\\([^)]*\\)", "", x)
  x <- gsub("\\s*\\[[^]]*\\]", "", x)
  x <- trimws(x)
  
  # General harmonization
  x <- recode(
    x,
    "HongKong" = "Hong Kong",
    "Hong Kong, China" = "Hong Kong",
    "China, Hong Kong Special Administrative Region" = "Hong Kong",
    "China, Macao Special Administrative Region" = "Macao",
    "Korea" = "South Korea",
    "Republic of Korea" = "South Korea",
    "Lao People's Democratic Republic" = "Laos",
    "Russian Federation" = "Russia",
    "Syrian Arab Republic" = "Syria",
    "Türkiye" = "Turkey",
    "Viet Nam" = "Vietnam",
    "UAE" = "United Arab Emirates",
    "UK" = "United Kingdom",
    "US" = "United States",
    "Burkina" = "Burkina Faso",
    "CapeVerde" = "Cabo Verde",
    "Cayman" = "Cayman Islands",
    "CostaRica" = "Costa Rica",
    "CzechR" = "Czechia",
    "DominicanR" = "Dominican Republic",
    "Ivory" = "Côte d'Ivoire",
    "Luxemburg" = "Luxembourg",
    "NZealand" = "New Zealand",
    "PuertoRico" = "Puerto Rico",
    "SaudiArabia" = "Saudi Arabia",
    "SierraLeon" = "Sierra Leone",
    "Solomon" = "Solomon Islands",
    "SouthAfrica" = "South Africa",
    "SriLanka" = "Sri Lanka",
    "Tajikstan" = "Tajikistan",
    "Timor" = "Timor-Leste",
    "Trinidad" = "Trinidad and Tobago",
    .default = x
  )
  
  x
}

# Apply the same country-name standardization to all data sets before joining.

consum_ed_df <- consum_ed_df %>%
  mutate(country = standardize_countries(country))

welf_ed_df <- welf_ed_df %>%
  mutate(country = standardize_countries(country))

cult_value_ed_df <- cult_value_ed_df %>%
  mutate(country = standardize_countries(country))

# Join welfare and cultural value indicators to the consumption data and 
# remove rows with missing values. 

# Welfare data are joined by country-year.
# Cultural values are joined by country because they do not vary by year.

comb_df <- consum_ed_df |>
  left_join(welf_ed_df, by = c("country", "year")) |> 
  left_join(cult_value_ed_df, by = "country") |> 
  drop_na()

# Keep only country-years that have data for all consumption categories.

cat_list <- comb_df |> distinct(consum_cat) |> pull(consum_cat)

country_years_cc <- comb_df |>
  distinct(country, year, consum_cat) |>
  group_by(country, year) |>
  summarise(cat_count = n(), .groups = "drop") |>
  filter(cat_count == length(cat_list))

comb_df <- comb_df |> semi_join(country_years_cc, by = c("country", "year"))

# Scale all welfare and cultural value indicators and reverse GII 
# (Gender Inequality Index) to align its direction with the other indicators.

comb_df <- comb_df |>
  mutate(across(where(is.numeric) & !any_of(c("year", "exp_pct")),
    ~ as.vector(scale(.x, center = TRUE, scale = TRUE)))) |>
  mutate(GII = -GII)

# Create country tables -------------------------------------------------------

# Count distinct countries by year.

country_ct_yr <- comb_df |> distinct(year, country) |> count(year, name = "n")

# Define year order.

y_ord <- sort(unique(comb_df$year))

# Create country lists with countries aligned across years.

country_df <- comb_df |>
  distinct(year, country) |>
  mutate(included = country) |>
  pivot_wider(id_cols = country,
              names_from = year,
              values_from = included) |>
  arrange(country) |>
  select(all_of(as.character(y_ord)))

# Add country counts to year column names.

names(country_df) <- paste0(
  names(country_df),
  " (n = ",
  country_ct_yr$n[match(names(country_df), country_ct_yr$year)],
  ")"
)

# Export country lists.

write_xlsx(country_df, "country_tbl.xlsx")

# Map countries manually to nine cultural–geographic regions 
# (following Minkov and Kaasa 2022, https://doi.org/10.1016/j.intman.2022.100971) 

cult_regs <- c("Australia" = "Northwestern, and Central Europe",
               "Austria" = "Northwestern, and Central Europe",
               "Azerbaijan" = "South, Central, and West Asia",
               "Belarus" = "Eastern Europe",
               "Belgium" = "Northwestern, and Central Europe",
               "Brazil" = "Latin America",
               "Bulgaria" = "Balkans",
               "Canada" = "Northwestern, and Central Europe",
               "Chile" = "Latin America",
               "Colombia" = "Latin America",
               "Croatia" = "Balkans",
               "Cyprus" = "Balkans",
               "Czechia" = "Northwestern, and Central Europe",
               "Denmark" = "Northwestern, and Central Europe",
               "Estonia" = "Eastern Europe",
               "Finland" = "Northwestern, and Central Europe",
               "France" = "Northwestern, and Central Europe",
               "Germany" = "Northwestern, and Central Europe",
               "Greece" = "Mediterranean Europe",
               "Guatemala" = "Latin America",
               "Hungary" = "Northwestern, and Central Europe",
               "Iceland" = "Northwestern, and Central Europe",
               "India" = "South, Central, and West Asia",
               "Iran" = "South, Central, and West Asia",
               "Iraq" = "Arab States",
               "Ireland" = "Northwestern, and Central Europe",
               "Israel" = "Mediterranean Europe",
               "Italy" = "Mediterranean Europe",
               "Japan" = "East Asia",
               "Kenya" = "Sub-Saharan Africa",
               "Lithuania" = "Eastern Europe",
               "Malaysia" = "South, Central, and West Asia",
               "Mexico" = "Latin America",
               "Mongolia" = "South, Central, and West Asia",
               "Montenegro" = "Balkans",
               "Netherlands" = "Northwestern, and Central Europe",
               "New Zealand" = "Northwestern, and Central Europe",
               "Nicaragua" = "Latin America",
               "Norway" = "Northwestern, and Central Europe",
               "Philippines" = "South, Central, and West Asia",
               "Poland" = "Eastern Europe",
               "Portugal" = "Mediterranean Europe",
               "Romania" = "Balkans",
               "Serbia" = "Balkans",
               "Singapore" = "East Asia",
               "Slovakia" = "Northwestern, and Central Europe",
               "Slovenia" = "Northwestern, and Central Europe",
               "South Africa" = "Sub-Saharan Africa",
               "South Korea" = "East Asia",
               "Spain" = "Mediterranean Europe",
               "Sweden" = "Northwestern, and Central Europe",
               "Switzerland" = "Northwestern, and Central Europe",
               "Thailand" = "South, Central, and West Asia",
               "Turkey" = "Balkans", 
               "United Kingdom" = "Northwestern, and Central Europe",
               "United States" = "Northwestern, and Central Europe")
 
cult_reg_df <- enframe(cult_regs, name = "country", value = "cult_reg")

# Add cultural zones to the combined data set.

comb_df <- comb_df |> left_join(cult_reg_df, by = "country") 

# Create a table of country counts and percentages 
# by cultural–geographic region and year.

country_reg_df <- comb_df |>
  filter(!is.na(cult_reg)) |>
  distinct(year, country, cult_reg) |>
  count(year, cult_reg, name = "n_countries") |>
  group_by(year) |>
  mutate(pct_countries = 100 * n_countries / sum(n_countries)) |>
  ungroup() |>
  mutate(ct_pct = paste0(n_countries, " (", round(pct_countries, 1), "%)")) |>
  select(cult_reg, year, ct_pct) |>
  pivot_wider(names_from = year,
              values_from = ct_pct,
              values_fill = "0 (0.0%)") |>
  arrange(cult_reg)

# Export country counts and percentages.

write_xlsx(country_reg_df, "region_tbl.xlsx")

# Save combined data set ------------------------------------------------------

saveRDS(comb_df, "comb_df.rds")

# -----------------------------------------------------------------------------
