# Global-Soil-Respiration-Analysis

## Overview

This project contains the R code used for data-processing and statistical analysis for the research paper *Temporal changes of global soil respiration since 1987 (Lei et al.,2020)*. All datasets in support of the findings of this paper are also provided.



## List of files in this repository

| File                             | Description                                                  |
| -------------------------------- | ------------------------------------------------------------ |
| reference datasets               | Directory of all input and output datasets and a list of studies containing the original data sources |
| 0_functions_plot.R               | Load R packages and define functions                         |
| 1_data_cleaning.R                | Preprocess srdb data: remove outliers, fill in part of the missing data, and redo classification of ecosystem and measurement method |
| 2_climatic_variable_extraction.R | Extract time-series temperature and precipitation data corresponding to R<sub>S</sub> sites. Retain only R<sub>S</sub> records with available matching climatic data. |
| 3_variables_generation.R         | Match R<sub>S</sub> records with corresponding mean annual temperature (MAT) and precipitation (MAP), calculate T anomaly and P anomaly. Add "altitude" and "SOC stocks" fields, and deal with missing SOC stocks data. Detect and remove outliers by category of biomes. |
| 4_statistical_prep.R             | Apply filtering criteria to obtain final working data. Divide data into multiple time periods and biomes. |
| 5_multivariate_model.R           | Fit multivariate model                                       |
| 6_moving_subset.R                | Moving subset analysis for examining temporal R<sub>S</sub> trends across windows of time period, latitude and SOC stocks. Both simple linear regression and robust regression methods are used to calculate temporal trends. |
| 7_Linear_regressions_Rs.R        | Linear regressions of  R<sub>S</sub> vs. time and R<sub>S</sub> vs. climatic factors. Both simple linear regression and robust regression are used. |
| 8_climate_trends.R               | Examine temporal trends of temperature, precipitation and the anamolies across latitude gradients. |
| 9_RhRa_analysis.R                | All-in-one analysis script for Rh&Ra data, including data screening, multivariate model fitting and linear regressions. |
| 10_montecarlo.R                  | Estimate annual global R<sub>S</sub> with Monte Carlo simulation, including variable extraction, model construction, Monte Carlo simulation, and results summarization. |
| README.md                        | Generates this README                                        |

## List of files under "reference datasets" directory

| File                                            | Description                                                  |
| ----------------------------------------------- | ------------------------------------------------------------ |
| 01_cleaneddata.csv                              | Output file from 1_data_cleaning.R                           |
| 02_sampledataset.csv                            | Output file from 2_climatic_variable_extraction.R            |
| 03_processed_data_complete_final.csv            | Output file from 3_variables_generation.R. **Final output from R<sub>S</sub> data preparation.** |
| 04_processed_data_rhra.csv                      | **Final output from R<sub>h</sub>/R<sub>a</sub> data preparation.** |
| NOLAI-BASE_ANALYSIS_gf_final_1987-2016_1000.csv | Output file from 10_montecarlo.R. Estimated global annual R<sub>S</sub> from 1987-2016. |
| Respiration-study-list.csv                      | A list of studies from which the records were collected      |
| annual.precip.30y.csv                           | Intermediate output file from 3_variables_generation.R       |
| annual.temp.30y.csv                             | Intermediateoutput file from 3_variables_generation.R        |
| delta.precip.30y.csv                            | Intermediateoutput file from 3_variables_generation.R        |
| delta.temp.30y.csv                              | Intermediateoutput file from 3_variables_generation.R        |
| precip_extract_result.csv                       | Output file from 2_climatic_variable_extraction.R            |
| srdb-data.csv                                   | SRDB v20200220a                                              |
| temp_extract_result.csv                         | Output file from 2_climatic_variable_extraction.R            |

## License
This project is covered under the “GNU Affero General Public License” version 3.

## Authors
Jiesi Lei, Xue Guo, Yufei Zeng, Jizhong Zhou, Qun Gao, and Yunfeng Yang.
Genuine thanks go to Dr. Ben Bond-Lamberty for providing access to the SRDB.
