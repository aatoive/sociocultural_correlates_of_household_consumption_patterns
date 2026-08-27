
# title: Main analyses for the manuscript:
# "Sociocultural correlates of household consumption patterns: 
# National welfare and cultural values shape household spending.”
# author: Aada Toivettula
# date: 2026-08-27

# Load packages ---------------------------------------------------------------

library(corrplot)
library(dplyr)
library(ggh4x)
library(ggfortify)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(scales)
library(tibble)
library(tidyr)
library(writexl)

# Import data -----------------------------------------------------------------

comb_df <- readRDS("comb_df.rds")

# Limit data to year 2020 for main analyses.

comb_20_df <- comb_df |> filter(year == 2020)

# Initial correlation and clustering analyses ---------------------------------

set.seed(123)

# Create a country-level correlation matrix 
# for welfare and cultural value indicators.

corr_ind <- comb_20_df |>
  group_by(country) |>
  summarise(across(where(is.numeric) & !any_of(c("exp_pct", "year")), 
                   ~ first(.x)),
            .groups = "drop") |> 
  select(where(is.numeric)) |> 
  cor(use = "complete.obs", method = "pearson")

# Plot and save the indicator correlation matrix.

png(file = "corr_ind.png", width = 3000, height = 3000, res = 300)
corrplot(
  corr_ind,
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

# Create a country-level correlation matrix for consumption categories.

corr_consum <- comb_20_df |>
  select(country, consum_cat, exp_pct) |>
  distinct() |>
  pivot_wider(names_from = consum_cat,
              values_from = exp_pct) |>
  select(-country) |>
  cor(use = "complete.obs", method = "pearson")

# Plot and save the consumption-category correlation matrix.

png(file = "corr_consum.png", width = 3000, height = 3000, res = 300)
corrplot(
  corr_consum,
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

# Correlation analysis --------------------------------------------------------

# Define the display order of welfare and cultural value indicators.

ind_ord <- c("HDI", "LE", "EYS", "MYS", "GNIPC", 
             "IHDI", "GII", "IDVCOLL", "FLXMON")

# Compute bivariate Pearson correlations between expenditure share and each 
# welfare or cultural value indicator, separately by consumption category.

corr_20_rslts <- comb_20_df |>
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

# Convert p-values to significance stars.

p_stars <- function(p) {
  case_when(p < 0.001 ~ "***",
            p < 0.01  ~ "**",
            p < 0.05  ~ "*",
            TRUE      ~ "")
}

# Format correlations, significance stars, and confidence intervals for export.

corr_20_tbl <- corr_20_rslts |> 
  mutate(ind = factor(ind, levels = ind_ord),
         stars = p_stars(p),
         cell = paste0(round(r, 2), 
                       stars, 
                       " (", round(ci_low, 2), ", ",
                       round(ci_high, 2), ")" )) |>
  select(consum_cat, ind, cell) |>
  pivot_wider(names_from = ind, values_from = cell) |>
  select(consum_cat, all_of(ind_ord))

# Export formatted correlation table.

write_xlsx(corr_20_tbl, "corr_20_tbl.xlsx")

# Plot correlation results ----------------------------------------------------

# Select only consumption categories with statistically significant results 
# and define plotting order for cultural–geographic regions and consumption categories.

plot_cats <- c("FOOD", "HOUS", "EDU", "COMM", "REC", "REST", "ALC")

plot_regs <- c("Northwestern, and Central Europe",
               "Mediterranean Europe",
               "Eastern Europe",
               "Balkans",
               "East Asia",
               "South, Central, and West Asia",
               "Arab States",
               "Latin America",
               "Sub-Saharan Africa")

# Select welfare and cultural value indicators to plot.

plot_vars <- comb_20_df |>
  select(any_of(c("HDI", "GII", "IDVCOLL", "FLXMON"))) |>
  names()

# Prepare plotting data.

corr_plot_df <- comb_20_df |>
  filter(consum_cat %in% plot_cats) |>
  mutate(cult_reg = factor(cult_reg, levels = plot_regs, ordered = TRUE),
         consum_cat = factor(consum_cat, levels = plot_cats, ordered = TRUE))

# Define colors for cultural–geographic regions.

country_clrs <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933",
                  "#DDCC77", "#CC6677", "#882255", "#AA4499")

# Create one faceted plot per selected indicator.

corr_plots <- lapply(plot_vars, function(i) {
  corr_lbl <- corr_20_rslts |>
    filter(ind == i, consum_cat %in% plot_cats) |>
    mutate(consum_cat = factor(consum_cat, levels = plot_cats, ordered = TRUE),
           stars = p_stars(p),
           label = sprintf("%.2f%s", r, stars),
           x = -Inf,  
           y =  Inf)
  
  y_scales <- c(
    consum_cat == "FOOD" ~ scale_y_continuous(
      limits = c(0, 0.5),
      breaks = seq(0, 0.5, length.out = 6),
      labels = label_percent()),
    consum_cat == "HOUS" ~ scale_y_continuous(
      limits = c(0, 0.4),
      breaks = seq(0, 0.4, length.out = 5),
      labels = label_percent()),
    consum_cat == "EDU" ~ scale_y_continuous(
      limits = c(0, 0.08),
      breaks = seq(0, 0.08, length.out = 5),
      labels = label_percent()),
    consum_cat == "COMM" ~ scale_y_continuous(
      limits = c(0, 0.08),
      breaks = seq(0, 0.08, length.out = 5),
      labels = label_percent()),
    consum_cat == "REC" ~ scale_y_continuous(
      limits = c(0, 0.15),
      breaks = seq(0, 0.15, length.out = 4),
      labels = label_percent()),
    consum_cat == "REST" ~ scale_y_continuous(
      limits = c(0, 0.15),
      breaks = seq(0, 0.15, length.out = 4),
      labels = label_percent()),
    consum_cat == "ALC" ~ scale_y_continuous(
      limits = c(0, 0.1),
      breaks = seq(0, 0.1, length.out = 6),
      labels = label_percent()))
  
  ggplot(corr_plot_df, aes(x = .data[[i]], y = exp_pct, color = cult_reg)) +
    geom_point(aes(size = 0.5, stroke = 0)) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.3) +
    geom_text(data = corr_lbl,
              mapping = aes(x = x, y = y, label = label),
              inherit.aes = FALSE,
              hjust = -0.05, vjust = 1.2,
              size = 15) +
    scale_color_manual(name = "region", values = country_clrs) +
    labs(x = NULL, y = NULL) +
    facet_grid(rows = vars(consum_cat),
               space = "fixed",
               scales = "free",
               drop = TRUE) +
    facetted_pos_scales(y = y_scales) +
    theme_minimal() +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_blank(),
          axis.line = element_line(colour = "black", linewidth = 0.1),
          axis.text = element_text(size = 15, color = "black"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.1))
})

# Combine the plots.

corr_ind_consum <- do.call(grid.arrange, c(corr_plots, ncol = 4))

# Save combined correlation figure.

ggsave("corr_ind_consum.png", corr_ind_consum, width = 21, height = 27, dpi = 300)

# Principal component analysis ------------------------------------------------

# Create country-level PCA input.

pca_vars <- ind_ord

pca_input <- comb_20_df |>
  group_by(country) |>
  summarise(cult_reg = first(cult_reg),
            across(all_of(pca_vars), ~ first(.x)),
            .groups = "drop") |>
  mutate(cult_reg = factor(cult_reg, levels = plot_regs, ordered = TRUE))

# Run PCA.

pcs <- pca_input |> select(all_of(pca_vars)) |> prcomp(scale. = FALSE)

# Use only the first three PCs, which are retained for interpretation, export,
# and subsequent correlation analyses.

# Flip selected PC axes for interpretability.

flip_pcs <- c("PC1", "PC2", "PC3")

pcs$x[, flip_pcs] <- -pcs$x[, flip_pcs]
pcs$rotation[, flip_pcs] <- -pcs$rotation[, flip_pcs]

# Extract PC loadings.

pc_loads <- as.data.frame(pcs$rotation[, 1:3]) |>
  rownames_to_column("Variable") |>
  mutate(across(starts_with("PC"), ~ sprintf("%.3f", .x)))

# Extract PC scores.

pc_scores <- bind_cols(pca_input |> select(country), 
                       as.data.frame(pcs$x[, 1:3]) |> as_tibble())

# Add PC scores to the 2020 data, replacing existing PC columns if present.

comb_20_df <- comb_20_df |>
  select(-any_of(c("PC1", "PC2", "PC3"))) |>
  left_join(pc_scores, by = "country")

# Compute PCA summary statistics.

pc_eva <- pcs$sdev^2
pc_var <- pc_eva / sum(pc_eva)
pc_var_cum <- cumsum(pc_var)

pc_summary <- tibble(
  Variable = c("Standard deviation", 
               "Eigenvalue", 
               "Variance explained", 
               "Cumulative variance"),
  PC1 = c(sprintf("%.3f", pcs$sdev[1]),
          sprintf("%.3f", pc_eva[1]),
          sprintf("%.2f%%", pc_var[1] * 100),
          sprintf("%.2f%%", pc_var_cum[1] * 100)),
  PC2 = c(sprintf("%.3f", pcs$sdev[2]),
          sprintf("%.3f", pc_eva[2]),
          sprintf("%.2f%%", pc_var[2] * 100),
          sprintf("%.2f%%", pc_var_cum[2] * 100)),
  PC3 = c(sprintf("%.3f", pcs$sdev[3]),
          sprintf("%.3f", pc_eva[3]),
          sprintf("%.2f%%", pc_var[3] * 100),
          sprintf("%.2f%%", pc_var_cum[3] * 100)))

# Combine PC loadings and summary statistics.

pca_tbl <- bind_rows(pc_loads, pc_summary)

# Export PCA table.

write_xlsx(pca_tbl, "pca_tbl.xlsx")

# Create PCA biplots ----------------------------------------------------------

# Select loadings to show.

pca_loads <- c("HDI", "GII", "IDVCOLL", "FLXMON", "MYS", "EYS")

# Prepare loading coordinates. 

biplot_loads <- as.data.frame(pcs$rotation[, 1:3]) |>
  rownames_to_column("Variable") |>
  as_tibble() |>
  filter(Variable %in% pca_loads)

# Create scaling factor for loading arrows.

load_fct <- 0.35

# Prepare loadings for PC1 vs PC2 biplot.

loads_pcs12 <- biplot_loads |>
  transmute(Variable = Variable,
            x = 0,
            y = 0,
            xend = PC1 * load_fct,
            yend = PC2 * load_fct,
            lbl_x = PC1 * load_fct * 1.02,
            lbl_y = PC2 * load_fct * 1.02,
            hjust_lbl = ifelse(PC1 >= 0, 0, 1),
            vjust_lbl = ifelse(PC2 >= 0, 0, 1))

# Prepare loadings for PC1 vs PC3 biplot.

loads_pcs13 <- biplot_loads |>
  transmute(Variable = Variable,
            x = 0,
            y = 0,
            xend = PC1 * load_fct,
            yend = PC3 * load_fct,
            lbl_x = PC1 * load_fct * 1.02,
            lbl_y = PC3 * load_fct * 1.02,
            hjust_lbl = ifelse(PC1 >= 0, 0, 1),
            vjust_lbl = ifelse(PC3 >= 0, 0, 1))

# Create PC1 vs PC2 biplot.

biplot_pcs12 <- autoplot(pcs,
                         data = pca_input,
                         x = 1, y = 2,
                         colour = "cult_reg",
                         frame = FALSE,
                         loadings = FALSE,
                         size = 1.5) +
  geom_segment(data = loads_pcs12,
               aes(x = x, y = y, xend = xend, yend = yend),
               inherit.aes = FALSE,
               color = "black",
               linewidth = 0.3,
               arrow = arrow(length = unit(0.10, "cm"))) +
  geom_label(data = loads_pcs12,
             aes(x = lbl_x,
                 y = lbl_y,
                 label = Variable,
                 hjust = hjust_lbl,
                 vjust = vjust_lbl),
             inherit.aes = FALSE,
             size = 1.5,
             color = "black",
             fill = "white",
             label.padding = unit(0.10, "lines")) +
  scale_color_manual(name = NULL, values = country_clrs, drop = FALSE) +
  theme_minimal(base_size = 8)

# Create PC1 vs PC3 biplot.

biplot_pcs13 <- autoplot(pcs,
                         data = pca_input,
                         x = 1, y = 3,
                         colour = "cult_reg",
                         frame = FALSE,
                         loadings = FALSE,
                         size = 1.5) +
  geom_segment(data = loads_pcs13,
               aes(x = x, y = y, xend = xend, yend = yend),
               inherit.aes = FALSE,
               color = "black",
               linewidth = 0.3,
               arrow = arrow(length = unit(0.10, "cm"))) +
  geom_label(data = loads_pcs13,
             aes(x = lbl_x,
                 y = lbl_y,
                 label = Variable,
                 hjust = hjust_lbl,
                 vjust = vjust_lbl),
             inherit.aes = FALSE,
             size = 1.5,
             color = "black",
             fill = "white",
             label.padding = unit(0.10, "lines")) +
  scale_color_manual(name = NULL, values = country_clrs, drop = FALSE) +
  theme_minimal(base_size = 8)

# Combine the biplots with one shared legend.

pca_biplots <- biplot_pcs12 + biplot_pcs13 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.2, "cm")) &
  guides(color = guide_legend(nrow = 3))

# Save combined figure.

ggsave("pca_biplots.jpg", pca_biplots,
       width = 19, height = 10, dpi = 1000, units = "cm")

# Correlation analysis with PCs -----------------------------------------------

# Compute bivariate Pearson correlations between expenditure share and each 
# retained PC, separately by consumption category.

corr_pc_rslts <- comb_20_df |>
  select(-any_of(c("country", "year", "cult_reg"))) |>
  pivot_longer(cols = all_of(c("PC1", "PC2", "PC3")),
               names_to = "comp",
               values_to = "comp_val") |>
  group_by(comp, consum_cat) |>
  reframe({
    corr_anal <- cor.test(exp_pct, comp_val, method = "pearson")
    
    tibble(r = unname(corr_anal$estimate),
           p = corr_anal$p.value,
           ci_low = corr_anal$conf.int[1],
           ci_high = corr_anal$conf.int[2])
    }) |>
  ungroup()

# Format PC correlations, significance stars, and confidence intervals for export.

corr_pc_tbl <- corr_pc_rslts |>
  mutate(stars = p_stars(p),
         cell = paste0(round(r, 2),
                       stars, 
                       " (", round(ci_low, 2), ", ",
                       round(ci_high, 2), ")" )) |>
  select(consum_cat, comp, cell) |>
  pivot_wider(names_from = comp, values_from = cell) 

# Export PC correlation table.

write_xlsx(corr_pc_tbl, "corr_pc_tbl.xlsx")

# -----------------------------------------------------------------------------