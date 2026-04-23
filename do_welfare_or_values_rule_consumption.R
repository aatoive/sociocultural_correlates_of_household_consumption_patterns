
# title: “Data preparation and correlation analysis for ‘Do welfare or values rule consumption?’”
# author: "Aada Toivettula"
# date: "2026-04-23"

# Load packages -------------------------------------------------------------

library(corrplot)
library(dplyr)
library(ggh4x)
library(ggplot2)
library(gridExtra)
library(readxl)
library(renv)
library(tibble)
library(tidyr)
library(scales)

# Import data ---------------------------------------------------------------

# Download household consumption expenditure data from: 
# https://data.un.org/Data.aspx?d=SNA&f=group_code%3A302 

consumption_data <- read.csv(
  "consumption_data.txt",
  sep =";",
  header = TRUE)

# Download human development (welfare) data from:
# https://hdr.undp.org/data-center/documentation-and-downloads

welfare_data <- read.csv(
  "welfare_data.csv",
  sep = ",",
  header = TRUE)

# Download cultural value data from:
# https://doi.org/10.1016/j.intman.2022.100971 

cultural_value_data <- read_excel("cultural_value_data.xlsx") 

# Prepare consumption data --------------------------------------------------

# Limit to household consumption and clean text fields

# Drop rows mentioning government/non-profit in Sub.Group
# Remove the "[from 1993]" suffix and trim whitespace
# Drop non-country rows and footnote artifacts

consumption_data_edited <- consumption_data %>% 
  filter(!grepl("general government|non-profit", 
                Sub.Group, 
                ignore.case = TRUE)) %>% 
  mutate(Sub.Group = gsub("\\[from 1993\\]", "", Sub.Group), 
         Sub.Group = trimws(Sub.Group)) %>% 
  filter(!is.na(Sub.Group) & Sub.Group != "") %>% 
  subset(!grepl("[0-9]", Country.or.Area) & Country.or.Area != "footnote_SeqID")

# Count distinct countries per year 

countries_per_year <- consumption_data_edited %>%
  summarise(Countries = n_distinct(Country.or.Area, na.rm = TRUE), .by = Year)

# Limit to the year 2020 as it has the largest country coverage in the 2020s

consumption_data_edited <- consumption_data_edited %>%
  filter(Year == 2020)

# Keep only rows with the highest Series value within each country, and add 
# columns for the household final consumption expenditure and item shares

consumption_data_edited <- consumption_data_edited %>%
  group_by(Country.or.Area) %>%
  filter(Series == max(Series, na.rm = TRUE)) %>%
  mutate(
    expenditure_total = first(
      Value[Item == "Equals: Household final consumption expenditure"]),
    expenditure_pct = Value / expenditure_total
  ) %>%
  ungroup()

# Rename the selected columns and remove unnecessary columns
# (metadata and temporary variables)

consumption_data_edited <- consumption_data_edited %>%
  rename(
    country = Country.or.Area,
    consumption_category = Item) %>%
  select(
    -any_of(c(
      "Sub.Group", "Year", "Series", "Currency",
      "SNA.System", "Fiscal.Year.Type", "Value",
      "Value.Footnotes", "expenditure_total"
    ))
  )

# Remove rows where 'consumption_category' is one of the following:
#    - Miscellaneous goods and services
#    - Equals: Household final consumption expenditure in domestic market
#    - Plus: Direct purchases abroad by residents
#    - Less: Direct purchases in domestic market by non-residents
#    - Equals: Household final consumption expenditure

unnecessary_catgories <- c(
  "Miscellaneous goods and services",
  "Equals: Household final consumption expenditure in domestic market",
  "Plus: Direct purchases abroad by residents",
  "Less: Direct purchases in domestic market by non-residents",
  "Equals: Household final consumption expenditure"
)

consumption_data_edited <- consumption_data_edited %>%
  filter(!consumption_category %in% unnecessary_catgories)

# Replace the names of the consumption categories with abbreviations

abbreviations <- c(
  "Food and non-alcoholic beverages" = "FOOD",
  "Alcoholic beverages, tobacco and narcotics" = "ALCOHOL",
  "Clothing and footwear" = "CLOTHING",
  "Housing, water, electricity, gas and other fuels" = "HOUSING",
  "Furnishings, household equipment and routine maintenance of the house" = "FURNISHING",
  "Health" = "HEALTH",
  "Transport" = "TRANSPORT",
  "Communication" = "COMMUNICATION",
  "Recreation and culture" = "RECREATION",
  "Education" = "EDUCATION",
  "Restaurants and hotels" = "RESTAURANTS"
)

consumption_data_edited <- consumption_data_edited %>%
  mutate(consumption_category = recode(consumption_category, !!!abbreviations))

# Prepare welfare data ------------------------------------------------------

# Select the following 2020 columns and drop the "_2020" suffix:
#    - Country
#    - HDI (Human Development Index) 
#    - LE (life expectancy at birth)
#    - EYS (expected years of schooling)
#    - MYS (mean years of schooling)
#    - GNIPC (Gross National Income per capita)
#    - IHDI (inequality-adjusted Human Development Index)
#    - GII (Gender Inequality Index)

welfare_data_edited <- welfare_data %>%
  select(
    any_of(c("country", "hdi_2020", "le_2020", "eys_2020", 
         "mys_2020", "gnipc_2020", "gii_2020", "ihdi_2020"))) %>%
  rename_with(~ sub("_2020$", "", .), ends_with("_2020")) %>% 
  rename_with(toupper, -country)

# Prepare cultural value data -----------------------------------------------

# Remove the country abbreviation column, rename selected columns,
# and drop empty rows 206-252

cultural_value_data_edited <- cultural_value_data %>% 
  select(-any_of("countabbrev")) %>% 
  rename(country = VAR00023) %>% 
  slice(-(206:252))

# Combine all three data sets -----------------------------------------------

# Join welfare and cultural value variables to the consumption data 
# by country and remove rows with missing values

combined_data <- consumption_data_edited %>%
  left_join(welfare_data_edited, by = "country") %>% 
  left_join(cultural_value_data_edited, by = "country") %>% 
  drop_na()

# Keep only countries that have data for all consumption categories

category_list <- combined_data %>%
  distinct(consumption_category) %>%
  pull(consumption_category)

final_countries <- combined_data %>%
  distinct(country, consumption_category) %>%
  group_by(country) %>%
  summarise(category_count = n(), .groups = "drop") %>%
  filter(category_count == length(category_list)) %>%
  pull(country)

combined_data <- combined_data %>%
  filter(country %in% final_countries)

# Scale all welfare and cultural value variables and reverse GII 
# (Gender Inequality Index) to align its direction with the other variables

combined_data <- combined_data %>%
  mutate(across(
    where(is.numeric) & !any_of(c("country", 
                                  "consumption_category", 
                                  "expenditure_pct")),
    ~ as.vector(scale(.x, center = TRUE, scale = TRUE))
  )) %>%
  mutate(GII = -GII)

# Figure 1A -----------------------------------------------------------------

# Build a correlation matrix for welfare and cultural value variables

corr_1A <- combined_data %>%
  group_by(country) %>%
  summarise(
    across(where(is.numeric) & !any_of("expenditure_pct"), ~ first(.x)),
    .groups = "drop"
  ) %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs", method = "pearson")

# Plot and save the correlation matrix

png(file = "figure1A.png", width = 3000, height = 3000, res = 300)
corrplot(
  corr_1A,
  method = "color",
  outline = TRUE,
  addgrid.col = "darkgray",
  order = "hclust",
  addrect = 3,
  rect.col = "black",
  rect.lwd = 4,
  cl.pos = "n",
  tl.pos = "lt",
  tl.cex = 1.5,
  tl.col = "black",
  addCoef.col = "white",
  number.digits = 2,
  number.cex = 1.5,
  number.font = 2,
  col = colorRampPalette(c("#BB5566", "#FFFFFF", "#004488"))(100)
)
dev.off()

# Figure 1B -----------------------------------------------------------------

# Build a correlation matrix for consumption categories

corr_1B <- combined_data %>%
  pivot_wider(
    names_from = consumption_category,
    values_from = expenditure_pct
    ) %>%
  select(
    -any_of(c("country", "HDI", "LE", "EYS", "MYS", 
              "GNIPC", "IHDI", "GII", "IDVCOLL", "FLXMON"))) %>%
  cor(use = "complete.obs", method = "pearson")

# Plot and save the correlation matrix

png(file = "figure1B.png", width = 3000, height = 3000, res = 300)
corrplot(
  corr_1B,
  method = "color",
  outline = TRUE,
  addgrid.col = "darkgray",
  order = "hclust",
  addrect = 3,
  rect.col = "black",
  rect.lwd = 4,
  cl.pos = "r",
  tl.pos = "lt",
  tl.cex = 1.2,
  tl.col = "black",
  cl.cex = 1.5,
  addCoef.col = "black",
  number.digits = 2,
  number.cex = 1.2,
  number.font = 2,
  col = colorRampPalette(c("#BB5566", "#FFFFFF", "#004488"))(100)
)
dev.off()

# Correlation analysis ------------------------------------------------------

# Compute Pearson correlations between each consumption category 
# (expenditure_pct) and each welfare or cultural value variable

corr_results <- combined_data %>%
  select(-any_of("country")) %>%
  pivot_longer(
    cols = all_of(c("HDI", "LE", "EYS", "MYS", "GNIPC", 
                    "IHDI", "GII", "IDVCOLL", "FLXMON")),
    names_to = "indicator",
    values_to = "indicator_value"
  ) %>%
  group_by(indicator, consumption_category) %>%
  reframe({
    correlation_analysis <- cor.test(expenditure_pct, 
                                     indicator_value, 
                                     method = "pearson")
    tibble(
      r = unname(correlation_analysis$estimate),
      p = correlation_analysis$p.value,
      CI_95 = paste0("[", correlation_analysis$conf.int[1],
                     ", ",
                     correlation_analysis$conf.int[2], "]")
    )
  }) %>%
  ungroup()

# Figure 2 ------------------------------------------------------------------

# Map countries to nine geographical regions (following Minkov and Kaasa 2022, 
# https://doi.org/10.1016/j.intman.2022.100971) 

countries_by_region <- c(
  "Australia"   = "Northwestern, and Central Europe",
  "Austria"     = "Northwestern, and Central Europe",
  "Azerbaijan"  = "South, Central, and West Asia",
  "Belarus"     = "Eastern Europe",
  "Belgium"     = "Northwestern, and Central Europe",
  "Brazil"      = "Latin America",
  "Bulgaria"    = "Balkans",
  "Canada"      = "Northwestern, and Central Europe",
  "Colombia"    = "Latin America",
  "Croatia"     = "Balkans",
  "Cyprus"      = "Balkans",
  "Denmark"     = "Northwestern, and Central Europe",
  "Estonia"     = "Eastern Europe",
  "Finland"     = "Northwestern, and Central Europe",
  "France"      = "Northwestern, and Central Europe",
  "Germany"     = "Northwestern, and Central Europe",
  "Greece"      = "Mediterranean Europe",
  "Guatemala"   = "Latin America",
  "Hungary"     = "Northwestern, and Central Europe",
  "Iceland"     = "Northwestern, and Central Europe",
  "India"       = "South, Central, and West Asia",
  "Iraq"        = "Arab States",
  "Ireland"     = "Northwestern, and Central Europe",
  "Israel"      = "South, Central, and West Asia",
  "Italy"       = "Mediterranean Europe",
  "Japan"       = "East Asia",
  "Kenya"       = "Sub-Saharan Africa",
  "Lithuania"   = "Eastern Europe",
  "Mexico"      = "Latin America",
  "Mongolia"    = "East Asia",
  "Montenegro"  = "Balkans",
  "Netherlands" = "Northwestern, and Central Europe",
  "Norway"      = "Northwestern, and Central Europe",
  "Philippines" = "South, Central, and West Asia",
  "Poland"      = "Eastern Europe",
  "Portugal"    = "Mediterranean Europe",
  "Romania"     = "Balkans",
  "Serbia"      = "Balkans",
  "Singapore"   = "East Asia",
  "Slovakia"    = "Northwestern, and Central Europe",
  "Slovenia"    = "Northwestern, and Central Europe",
  "Spain"       = "Mediterranean Europe",
  "Sweden"      = "Northwestern, and Central Europe",
  "Switzerland" = "Northwestern, and Central Europe",
  "Thailand"    = "South, Central, and West Asia",
  "Ukraine"     = "Eastern Europe"
)

region_data <- enframe(countries_by_region, 
                       name = "country", 
                       value = "geographical_region")

combined_data <- combined_data %>%
  left_join(region_data, by = "country") 

# Select only consumption categories with statistically significant results 
# and define the order for geographical regions and consumption categories 
# in the plots

consumption_category_list <- c(
  "FOOD",
  "HOUSING",
  "EDUCATION", 
  "COMMUNICATION", 
  "RECREATION",
  "RESTAURANTS",
  "ALCOHOL"
)

geographical_region_list <- c(
  "Northwestern, and Central Europe",
  "Mediterranean Europe", 
  "Eastern Europe", 
  "Balkans",
  "East Asia", 
  "South, Central, and West Asia",
  "Arab States", 
  "Latin America", 
  "Sub-Saharan Africa"
)

# Select welfare and cultural value variables to plot

plot_variables <- combined_data %>%
  select(any_of(c("HDI", "GII", "IDVCOLL", "FLXMON"))) %>%
  names()

# Prepare plotting data

plot_data <- combined_data %>%
  filter(consumption_category %in% consumption_category_list) %>%
  mutate(geographical_region = factor(geographical_region,
                                      levels = geographical_region_list, 
                                      ordered = TRUE),
         consumption_category = factor(consumption_category, 
                                       levels = consumption_category_list, 
                                       ordered = TRUE))

# Prepare aesthetics (color palette for the geographical regions and a helper 
# to convert p-values to significance stars)

country_colors <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933",
                    "#DDCC77", "#CC6677", "#882255", "#AA4499")

p_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}

# Build one plot per welfare or cultural value variable and combine the plots

plots <- lapply(plot_variables, function(i) {
  corr_label <- corr_results %>%
    filter(indicator == i,
           consumption_category %in% consumption_category_list) %>%
    mutate(consumption_category = factor(consumption_category,
                                         levels = consumption_category_list,
                                         ordered = TRUE),
           stars = p_stars(p),
           label = sprintf("%.2f%s", r, stars),
           x = -Inf,  
           y =  Inf
    )
  
  y_scales <- c(
    consumption_category == "FOOD" ~ scale_y_continuous(
      limits = c(0, 0.5),
      breaks = seq(0, 0.5, length.out = 6),
      labels = label_percent()
    ),
    consumption_category == "HOUSING" ~ scale_y_continuous(
      limits = c(0, 0.4),
      breaks = seq(0, 0.4, length.out = 5),
      labels = label_percent()
    ),
    consumption_category == "EDUCATION" ~ scale_y_continuous(
      limits = c(0, 0.08),
      breaks = seq(0, 0.08, length.out = 5),
      labels = label_percent()
    ),
    consumption_category == "COMMUNICATION" ~ scale_y_continuous(
      limits = c(0, 0.08),
      breaks = seq(0, 0.08, length.out = 5),
      labels = label_percent()
    ),
    consumption_category == "RECREATION" ~ scale_y_continuous(
      limits = c(0, 0.15),
      breaks = seq(0, 0.15, length.out = 4),
      labels = label_percent()
    ),
    consumption_category == "RESTAURANTS" ~ scale_y_continuous(
      limits = c(0, 0.15),
      breaks = seq(0, 0.15, length.out = 4),
      labels = label_percent()
    ),
    consumption_category == "ALCOHOL" ~ scale_y_continuous(
      limits = c(0, 0.1),
      breaks = seq(0, 0.1, length.out = 6),
      labels = label_percent()
    )

  )
  
  ggplot(plot_data, aes(x = .data[[i]], 
                        y = expenditure_pct,
                        color = geographical_region)) +
    geom_point(aes(size = 0.5, stroke = 0)) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.3) +
    geom_text(
      data = corr_label,
      mapping = aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = -0.05, vjust = 1.2,  
      size = 15
    ) +
    scale_color_manual(name = "geographical region", values = country_colors) +
    labs(x = NULL, y = NULL) +
    facet_grid(rows = vars(consumption_category),
               space = "fixed",
               scales = "free",
               drop = TRUE) +
    facetted_pos_scales(y = y_scales) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.line = element_line(colour = "black", linewidth = 0.1),
      axis.text = element_text(size = 15, color = "black"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.1)
    )
})

combined_plot <- do.call(grid.arrange, c(plots, ncol = 4))

# Save Figure 2

ggsave("figure2.png", combined_plot, width = 21, height = 27, dpi = 300)

# ---------------------------------------------------------------------------

