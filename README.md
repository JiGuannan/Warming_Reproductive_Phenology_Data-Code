# Warming_Reproductive_Phenology_Data-Code

This repository contains the R code and data used to analyze the effects of climate warming on flowering phenology, fruiting phenology, and reproductive season length (RSL) in woody plants.

The analyses support the study:

**Climate warming advances flowering and fruiting but drives divergent changes in reproductive season length**

---

## Repository structure

- `Code_for_paper.R`  
  Main R script used for data processing, statistical analyses, and figure generation.

- `data_Fig1.xlsx`  
  Data used for analyses and visualization in Figure 1.

- `data_Fig2.xlsx`  
  Data used for analyses and visualization in Figure 2.

- `data_Fig3.xlsx`  
  Data used for analyses and visualization in Figure 3.

- `data_Fig4.xlsx`  
  Data used for analyses and visualization in Figure 4.

---

## Statistical analyses

All data analyses were conducted using **R version 4.3.3**.

- Linear mixed-effects models were fitted using the **`lme4`** package (version 1.1-35).
- The pseudo-R² of linear mixed-effects models was calculated using the **`MuMIn`** package (version 1.47.5).
- The relative contributions of temperature, accumulated forcing, and precipitation to reproductive season length were quantified using the **`partR2`** package (version 0.9.2).

---

## Data availability

- Phenological data are openly available from the  
  **National Earth System Science Data Sharing Infrastructure, National Science & Technology Infrastructure of China**  
  (http://www.geodata.cn)

- Climate data were obtained from the  
  **National Meteorological Information Center of China**  
  (http://data.cma.cn)

---

## Notes

- The xlsx files included in this repository are processed datasets used specifically for the analyses and figures presented in the manuscript.
- Raw data can be accessed from the original data repositories listed above.
