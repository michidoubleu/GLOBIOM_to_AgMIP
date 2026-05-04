# GLOBIOM to AgMIP Post-Processing Routine

This repository hosts the codes to transform GLOBIOM standard output to the AgMIP reporting template.

## User Guide

### 1. Configuring the Routine
Before running the AgMIP reporting pipeline, you can customize the execution by modifying the user settings files located in the `codes/` directory:
- **`user_settings_comprehensive.R`**: For the full pipeline (including Economic, BII, and Mitigation modules).
- **`user_settings_accelerator.R`**: Internal! Only for settings when launching as **ROUTINE** on the **IIASA-Accelerator**.

**Key variables you can set:**
- `GLOBIOM.file`: Path to your input GLOBIOM GDX file (default is in `inputs/`).
- `out_dir`: The directory where the final standardized CSV files will be saved (default is in `output/`).
- `rename.model`: The label for the "Model" column in the output (default: "GLOBIOM").
- `scen.filter`: A vector of scenario names if you want to process only a subset of scenarios (default: NULL = all scenarios in the GDX file).
- `years_keep`: The specific years you want to include in the final report (default: NULL = all years in the GDX file).

*Note: You can also override the input file and output directory via command line arguments when running the scripts.*

### 2. Manual Data Updates
To ensure the reporting is accurate, especially for the modular indicators, several exogenous data files in the `open_input/` directory may require manual updates:
- **Economic Costs**: Update `open_input/LAMASUS_costs_country_*.csv` with the latest regional cost estimates based on LAMASUS costing data.
- **Organic & Hedgerow Targets**: Update `open_input/CAP_SP_organic_plans.csv` and `open_input/CLU_hedgerow_area_*.csv` if scenario assumptions for mitigation technologies change.
- **Emissions & Forest Factors**: Update `open_input/affor_emis_factors.rds` or `open_input/processed_forest_*.rds` when updated afforestation or harvested wood products (HWP) data becomes available (through Forest-GLOBIOM or Forest Navigator runs).

### 3. How to Run
To generate the AgMIP reporting file, execute the main script from the root of the repository using R or the terminal:
```bash
Rscript codes/main_GLOBIOM_to_AgMIP.R [path/to/input.gdx] [path/to/output_dir]
```
If no arguments are provided, the script will use the defaults specified in your user settings file.

## Overview
This routine transforms raw GLOBIOM outputs into standardized CSV files compatible with the AgMIP Accelerator template. The routine is modular, incorporating several sub-modules to calculate specific indicators such as biodiversity (BII), economic costs and revenues, and the effects of mitigation technologies.

## Main Entry Points
The pipeline is typically initialized via the main execution scripts:
- `main_GLOBIOM_to_AgMIP.R`: The comprehensive entry point that executes the full suite of modules (BII, Economics, Mitigation, Afforestation) by calling `prep_GLOBIOM_agmip_accelerator_comprehensive.R`.
- `main_GLOBIOM_to_AgMIP_accelerator.R`: A streamlined entry point launching as **ROUTINE** on the **IIASA-Accelerator**.

User configurations, such as filtering years, scenarios, and setting the root directory, are handled via settings files (e.g., `user_settings_comprehensive.R` and `user_settings_accelerator.R`).

---

## Processing Engine and Sub-Scripts

The core processing is handled by the "prep" scripts, which dynamically read data from GDX files, clean and standardize columns, and merge specialized metrics calculated in the sub-scripts.

### 1. Engine (`prep_GLOBIOM_agmip_accelerator_comprehensive.R`)
This script acts as the backbone of the comprehensive routine. It performs the initial extraction of core agricultural data (production, land use, emissions, nutrition) and sequentially integrates the outputs from the following sub-modules.

### 2. Sub-Modules
* **`integrate_affor_emissions.R`**: Calculates carbon sinks derived from afforestation by tracking the increase in forest area relative to the year 2000 and applying specific conversion factors.
* **`mitigtech_calc.R`**: Calculates the area allocated to different mitigation technologies (e.g., silvopasture, biochar, organic farming, and hedgerows) under various scenarios.
* **`BII_calc.R`**: Calculates the Biodiversity Intactness Index (BII) by mapping GLOBIOM land use allocations (including management intensity and mitigation tech) to PREDICTS land use classes and their associated abundance weightings.
* **`Econ_calc.R`**: Calculates regional economic indicators, including crop revenues, crop costs, feed costs, grassland costs, subsidies, and total agricultural income.

---

## Input Data Inventory

The routine relies on several input datasets. Below is a comprehensive list of these inputs, their function within the routine, and space to annotate their origin.

### Primary GLOBIOM Outputs
**File:** `GLOBIOM.file` (Dynamic argument, e.g., `inputs/acc_pointer.gdx`)
* **Loaded In:** Engine, `BII_calc.R`, `Econ_calc.R`, `mitigtech_calc.R`
* **Purpose:** The primary output database from the GLOBIOM model runs. It provides the core data variables including `OUTPUT_AG` (general agricultural outputs), `OUTPUT` (prices, production, subsidies), `ACR_COMPARE` (cropland management types), and `OUTPUT_AG_REG` (mitigation technologies).
* **Origin:** *[User Input Required: Please specify the generation process or source of this GDX file]*

**File:** `inputs/a6_LAMASUS_baseline.gdx`
* **Loaded In:** `BII_calc.R`
* **Purpose:** Provides the spatial mapping crosswalk (`REGION61_COUNTRY_MAP`) to link the 61 GLOBIOM regions to country-level data.
* **Origin:** *[User Input Required: Please specify the source]*

### Forestry and Emissions Data
**File:** `open_input/processed_forest.rds`
* **Loaded In:** Engine
* **Purpose:** Contains processed baseline data for various forest management categories to be appended to the final outputs.
* **Origin:** *[User Input Required: Please specify the source]*

**File:** `open_input/processed_forest_HWP_emis.rds`
* **Loaded In:** Engine
* **Purpose:** Contains pre-calculated emissions data associated with Harvested Wood Products (HWP).
* **Origin:** *[User Input Required: Please specify the source]*

**File:** `open_input/affor_emis_factors.rds`
* **Loaded In:** `integrate_affor_emissions.R`
* **Purpose:** Provides the conversion factors (`sink.fac`) required to translate new afforested area (in hectares) into carbon sequestration values (MtCO2e).
* **Origin:** *[User Input Required: Please specify the source]*

### Biodiversity Data
**File:** `./open_input/PREDICTS_update_modified.csv`
* **Loaded In:** `BII_calc.R`
* **Purpose:** Contains the PREDICTS database coefficients mapping Land Use Management (LUM) categories to biodiversity abundance indices. Used to compute the area-weighted Biodiversity Intactness Index.
* **Origin:** *[User Input Required: Please specify the source]*

### Economic Data
**File:** `open_input/LAMASUS_costs_country_20260420.csv`
* **Loaded In:** `Econ_calc.R`
* **Purpose:** Provides country-specific exogenous cost estimates for crop production and grassland management. Used in conjunction with GLOBIOM production and area outputs to calculate total sector costs and net incomes.
* **Origin:** *[User Input Required: Please specify the source]*

### Mitigation Scenarios Data
**File:** `./open_input/CAP_SP_organic_plans.csv`
* **Loaded In:** `mitigtech_calc.R`
* **Purpose:** Contains the targeted shares for organic farming expansion under different CAP (Common Agricultural Policy) scenarios.
* **Origin:** *[User Input Required: Please specify the source]*

**File:** `./open_input/CLU_hedgerow_area_FPEA_20260427.csv`
* **Loaded In:** `mitigtech_calc.R`
* **Purpose:** Contains exogenous targets for hedgerow area expansion under various scenarios.
* **Origin:** *[User Input Required: Please specify the source]*
