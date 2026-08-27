# Sociocultural correlates of household consumption patterns
Reproducible data preparation and correlation analysis (R) for “Sociocultural correlates of household consumption patterns: National welfare and cultural values shape household spending.”

## Description

This repository contains R scripts for [data preparation](scripts/data_preparation.R), [main analyses](scripts/main_analyses.R), and [robustness analyses](scripts/robustness_analyses.R) for the manuscript "Sociocultural correlates of household consumption patterns: National welfare and cultural values shape household spending." The manuscript investigates the relationship between household consumption patterns and key welfare and cultural value indicators. 

Data sources:
- [Household consumption expenditure data](https://data.un.org/Data.aspx?d=SNA&f=group_code%3A302) (UNSD, 2025)
- [Human development (welfare) data](https://hdr.undp.org/data-center/documentation-and-downloads) (UNDP, 2025)
- [Cultural value data](https://doi.org/10.1016/j.intman.2022.100971) (Minkov & Kaasa, 2022)

Analysis were run with R version 4.5.2. The package versions are recorded into [renv.lock](/renv.lock).

## Outputs

The scripts produce the following figures:
- [Figure 1A](figures/corr_ind.png) (Correlation and clustering analysis between welfare and cultural value indicators)
- [Figure 1B](figures/corr_consum.png) (Correlation and clustering analysis between household consumption categories)
- [Figure 2](figures/pca_biplots.jpg) (Principal component analysis of welfare and cultural value indicators)
- [Figure 3](figures/corr_ind_consum.png) (Results of the correlation analysis of welfare indicators, cultural values, and household consumption categories)

## Lisence

[MIT License](/LICENSE)

## Contact

Corresponding author: Aada Toivettula, Aalto University, aada.toivettula@aalto.fi 
