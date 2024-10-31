
---

# SOFA Report Analysis: Decomposition and Overview of the underlying model assumptions

This repository contains R scripts and data used to generate decomposition figures and the overview of the pathway model assumptions for the 2024 background paper of the SOFA report. The repository is organized into three main folders:

## Folder Structure

1. **Program**: This folder holds the R scripts and associated code. Each sub-folder contains specific scripts used to produce different sets of figures:
   - **"Decomposition_all"**: Contains code to generate decomposition figures for multiple countries, excluding India, displaying the isolated impacts of single scenarios. This includes code for five countries in the following order: Australia, Brazil, Colombia, Ethiopia, and the UK (i.e., Figure 2-7). The final part of the script creates decomposition graphs for all countries, focusing on a single indicator to illustrate the impact of each scenario parameter across the five countries (i.e., Figure 1-8) 
   - **"Scenarios_assumptions_SOFA"**: Includes scripts for generating the overview of the underlying model assumptions, specifically Figure 1-5.
   - **"Decomposition_different_scenarios_order"**: Provides code to perform decomposition analysis in a different way (i.e., compiling and aggregating the scenarios impacts, following two opposite orders, producing Figures A2(a) and A2(b).

2. **Data**: This folder contains all datasets required to run the R scripts.

3. **Output**: Stores all generated figures and results from running the scripts in the "Program" folder.
