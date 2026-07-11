# SPW Lysimeter Analysis

This repository contains the R scripts and documentation used to analyses the lysimeter data for the publication:

**Impacts of groundwater level management on geochemical processes at an agricultural site with shallow groundwater**

**Authors**

- Jörg Steidl (1) – https://orcid.org/0000-0002-6599-0450
- Ottfried Dietrich (1) – https://orcid.org/0000-0003-4637-9784
- Christoph Merz (1, 2)– https://orcid.org/0000-0001-7434-9828

**Affiliations**

1. Leibniz Centre for Agricultural Landscape Research (ZALF), Eberswalder Straße 84, 15374 Müncheberg, Germany
2. Hydrogeology Group, Institute of Geological Sciences, Freie Universität Berlin, Malteserstr. 74–100, 12249 Berlin, Germany

**Journal:** Environmental Earth Sciences

**Corresponding author**

Jörg Steidl  
Email: jsteidl@zalf.de

---

## Repository Contents

- R scripts for data processing, statistical analyses, and figure generation.
- The lysimeter measurement data are publicly available at:
  https://doi.org/10.4228/zalf-vevz-ys85
- `renv.lock` for restoring the exact R package environment used in the analyses.

## Repository Structure

| Script | Description |
|--------|-------------|
| `Analysis.R` | Starts the required script functions. |
| `Init.R` | Initializes the analysis environment by loading packages, defining global variables, setting directory paths, and establishing data connections.|
| `funcData.R` | Loading, preparing, and visualizing the data. |
| `funcPCA.R` | Functions for analysing the environmental drivers of the principal components. |
| `funcPCADrivers.R` | Functions for identifying and analysing the drivers of the principal components. |
| `funcRForest.R` | Functions for random forest modelling and variable importance analysis. |

## Reproducibility

This project uses the **renv** package to ensure computational reproducibility. All package versions required for the analyses are specified in `renv.lock`.

## Installation

Clone the repository:

```bash
git clone https://github.com/Jost60/SPW-Lysimeter.git
cd SPW-Lysimeter
```

Restore the R environment:

```r
install.packages("renv")   # if not already installed
renv::restore()
```

Dataset:
```
Download the dataset from the data repository https://doi.org/10.4228/zalf-vevz-ys85
Depending on your data and analysis requirements, you may need to adapt the input data or parts of the R code.
If the structure or number of input data differs from the dataset used in this study, 
the corresponding adjustments can be made in the function `funcData::Load_LysimeterData()`.
Please note that Lysimeter III from the dataset was referred to as Lysimeter II in the study. 
Lysimeter II from the dataset was not used in the study.

The local data connections and working directories must be adjusted in the `Init.R` script. 
This can be done using the following environment variables, as intended:
* `DB_USER`
* `DB_LABORATORY_DATA`
* `DB_REDOX`
* `WORKDIRECTORY`
Alternatively, you can add the connections directly to the corresponding variables in the `# Control parameters` section of the `Init.R` script.

```

Afterward, all scripts can be executed using the package versions used for the publication.

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.
