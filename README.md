# GLOBIOM to AgMIP Post-Processing Routine

This repository transforms a GLOBIOM output GDX into a CSV in the AgMIP reporting format,
including regional economic indicators (value added and subsidies).

It is the generic version of the routine: no project-specific scenario handling and no
additional ex-post integrations (forest, biodiversity, mitigation technologies).

## Quick start

### 1. Requirements
- R (>= 4.1) with `Rscript` available on the command line
- R packages `tidyverse`, `data.table` and `gamstransfer` (all on CRAN). Install them once from
  the repository root with:
  ```bash
  Rscript init.R
  ```

### 2. Put your GLOBIOM output GDX into `inputs/`
Create a folder called `inputs` in the repository root (next to `codes/` and `open_input/`) and
copy your GLOBIOM output GDX into it:

```
GLOBIOM_to_AgMIP/
├── codes/
├── open_input/
├── inputs/
│   └── my_GLOBIOM_run.gdx      <-- your GLOBIOM output
└── outputs/                     <-- created automatically, the CSV is written here
```

The GDX must contain the GLOBIOM reporting symbols `OUTPUT_AG`, `OUTPUT` and `REGION_AG_MAP`
(standard GLOBIOM output / merged scenario GDX).

`inputs/` and `outputs/` are excluded from git (`.gitignore`), so your data is never committed.

### 3. Run it from the command line
Open a terminal **in the repository root** and call the main script with two arguments,
the input GDX and the output CSV:

```bash
Rscript codes/main_GLOBIOM_to_AgMIP.R inputs/my_GLOBIOM_run.gdx outputs/my_GLOBIOM_run_AgMIP.csv
```

On Windows, if `Rscript` is not on your PATH, use the full path, e.g.:

```bat
"C:\Program Files\R\R-4.5.3\bin\Rscript.exe" codes/main_GLOBIOM_to_AgMIP.R inputs/my_GLOBIOM_run.gdx outputs/my_GLOBIOM_run_AgMIP.csv
```

Without arguments the defaults set at the top of `codes/main_GLOBIOM_to_AgMIP.R` are used:

| Setting        | Argument | Default                        |
|----------------|----------|--------------------------------|
| `GLOBIOM.file` | 1st      | `inputs/GLOBIOM_output.gdx`    |
| `output.file`  | 2nd      | `outputs/GLOBIOM_AgMIP.csv`    |

These two are the only user settings. A run of a full merged scenario GDX takes about half a
minute and ends with `Writing <n> scenario(s), <n> rows to <output.file>` and `Done.`

## Output

One CSV with the columns

| Column     | Content                                                                       |
|------------|-------------------------------------------------------------------------------|
| `Model`    | `GLOBIOM`                                                                     |
| `Scenario` | the three scenario dimensions of the GDX joined with `-`, e.g. `SSP2-SCEN-CP` |
| `Region`   | GLOBIOM reporting region (`World` is reported as `WLD`)                       |
| `Item`     | AgMIP item code                                                               |
| `Variable` | AgMIP variable code                                                           |
| `Year`     | 2000, 2010, ..., 2100 (as available in the GDX)                               |
| `Unit`     | AgMIP unit                                                                    |
| `Value`    | value                                                                         |

### Fixed reporting conventions (`codes/prep_GLOBIOM_to_AgMIP.R`)
- Years: 10-year steps from 2000 to 2100.
- Regions `ROW` and `CHE` are not reported.
- Aggregate/helper items (`Meat`, `ALL`, `CER`, `LUC*`, `PEAT`, ...) are not reported.
- Soil organic carbon items (`SOC`, `SOCE`, `SOCC`, `SOCG`, `SOCB`) are not reported as they are
  not yet in the AgMIP template; the code to report them is kept as a comment.
- Fertiliser `FRTN`/`FRTP`: sum of inorganic + organic (`FRTIN`+`FRTON`, `FRTIP`+`FRTOP`) for the
  items where the split exists in the GDX, otherwise the 4-letter total.
- `*_dm` variables are the dry-matter versions; CH4/N2O are converted from Mt to kt.

## Code structure

| File                          | Purpose                                                                       |
|-------------------------------|-------------------------------------------------------------------------------|
| `codes/main_GLOBIOM_to_AgMIP.R` | Entry point: the two user settings, then runs the engine                    |
| `codes/prep_GLOBIOM_to_AgMIP.R` | Engine: reads `OUTPUT_AG`, maps it to the AgMIP format, adds the economic indicators, filters and writes the CSV |
| `codes/region_mapping.R`      | GLOBIOM 61 regions -> reporting regions, from `REGION_AG_MAP` in the GDX       |
| `codes/Econ_calc.R`           | Economic indicators from `OUTPUT` and the exogenous cost data                 |
| `codes/CAP_BUDG_pillar_mapping.md` | Mapping of the CAP payment codes to Pillar 1 / Pillar 2                  |
| `init.R`                      | Installs the required R packages                                              |

## Economic indicators (`codes/Econ_calc.R`)

Reported per reporting region in `bn USD 2005 MER`:

| Variable    | Items         | Content                                                                 |
|-------------|---------------|-------------------------------------------------------------------------|
| `VAAD`      | CRP, LSP, TOT | value added: revenue - costs + subsidies                                |
| `BUDG`      | CRP, LSP, TOT | subsidies (GLOBIOM `BUDG`)                                              |
| `BUDG_1stP` | TOT           | CAP Pillar 1 payments (direct payments)                                 |
| `BUDG_2ndP` | TOT           | CAP Pillar 2 payments (rural development)                               |

- Crops: revenue = price (`XPRP`, USD/t) x production (`Prod`, 1000 t); cost = cost per ha x area.
- Livestock: revenue = price x production of the livestock products; cost = feed (`Feed`, 1000 t,
  valued at crop prices) + grassland area x grassland cost per ha.
- Costs per ha come from `open_input/LAMASUS_costs_country_20260420.csv` (country level, read as
  1000 EUR/ha and treated as USD). As this file covers EU countries only, the economic indicators
  are only reported for regions built from those countries.
- Money values are in million until the final conversion to billion. Note that GLOBIOM prices and
  subsidies are in USD 2000 and the costs in EUR; no deflator or exchange rate is applied, the
  unit label follows the AgMIP template.
