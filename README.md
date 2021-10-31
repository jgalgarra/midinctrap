# README #

This repository contains the software, data and reproducibility instructions of the paper "Convergence Speed and Growth Patterns: A Dynamical Systems Approach".
Please, clone the repo and follow these steps.

## Data

The input data file contains the following files:

* `GDP2010_WB.csv`: GDP per capita (constant 2010 US$) for each country from 1960 to 2019. This file was downloaded from the World Bank DataBank (https://databank.worldbank.org/) and slightly formated to ease dataset import.

* `GNIS.csv`: GNI per capita, Atlas method (current US$). Downloaded from the same source.

* `countries_msci.csv`: Table with the Country Name, ISO Code, Region and MSCI Category.

* `group_AFRICA/ASIA/EUROPA/LATAM.csv`. Auxiliar tables with groups os MSCI countries for plotting.

## Scripts

You will need R 4.0.1 or later. Execute them following this order:

#### midtrap_GPDs_comparison_plot.R 

Produces the GDP comparison plot `COMPARISON_GDPS`

#### midtrap_phase_plot.R

This is the main script. It computes convergence speed and acceleration and plots TIMELINE, PHASES, RATIOvsSPEED, SPEEDvsACC, SPEEDCOMP and ALL for the magnitudes and selected countries. These plots are stored at the `figs/countries` folder. Computed magnitudes are stored at `output_data/all_speeds_GDP.csv` and `output_data/all_speeds_GNI.csv`.

Configuration:

- `config_data/criteria.txt`: Write `GDP` and/or `GNI` in plain text to select the magnitude to compute and plot. If you need both, write two lines, the order does not matter.

- `config_data/config_plot.csv`: This table contains 7 fields:

   * `CountryCode`: ISO Code of the country you want to study or `MSCI` for all MSCI ACWI countries.
   
   * `SecondCountryCode`: ISO Code of a secound country to plot comparisons or `NONE` if you do not need to compare

   * `LabelsInitialPair`: Label for the first plot of a two countries comparison
   
   * `print_indiv`: if `TRUE` plots are saved to files, else only magnitudes are stored
   
   * `print_tiff`: if `print_indiv` is set to `TRUE` then `png` files are produced by default. To store `tiff` files this second flag must be set to TRUE as well. Be carefule because `tiff` files are huge.
   
   * `initial_year` and `final_year`: limits of the study.

#### midtrap_correlations_plot.R

Produces the convergence speed correlation plots and the Kamada-Kawai `NETWORKSPEED_*` plots. The magnitude(s) to study `GNI` or `GDP` is read from the `config_data/criteria.txt` file.

#### midtrap_all_speeds.R

Produces the `ALL_DISTANCES_` plots, where average GNI ratios vs. average convergence speeds are plot and countries are clustered by region.  The magnitude(s) to study `GNI` or `GDP` is read from the `config_data/criteria.txt` file.

#### midtrap_all_plot_evolution.R

Produces the `AVG_EVOLUTION_` plots, not included in the main paper.

