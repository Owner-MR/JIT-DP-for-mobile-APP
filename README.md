# General Introduction

This repository provides the code and data set used in this article - An empirical study on the impact of mislabeled changes by SZZ on the performance and interpretation of just-in-time defect prediction for mobile APP.

### Data Labelling

Our data is labeled using Giovanni's implementaiton of B-SZZ, AG-SZZ, MA-SZZ and RA-SZZ. See https://github.com/grosa1/pyszz

### Environment Preparation

- R 	4.1.2

```
randomForest  4.7

ROSE          0.0

smotefamily   1.3.1

scoring       0.6

SDMTools      1.1
```

### Repository Structure

Part of our code is based on the code provided by Fan et al, See https://github.com/YuanruiZJU/SZZ-TSE

- `./Experimental documentation/code` : Code directory.
  - `code/calculation/main.R`：The script to run all the experimental setups.
  - `code/packages/imbalance.R`: Random undersampling script.
- `./data_results/`: Data set and results
  - `data_results/data_csv_time`: Original Data set labeled B-SZZ, AG-SZZ, MA-SZZ, and RA-SZZ.
  - `data_results/data_csv_time_ln`: Data set after ln (x+1) processing.
  - ``data_results/collinearity.csv`: Features removed by the AutoSpearman algorithm.

### How to run

- Modify three lines in the file `./Experimental documentation/codecalculation/main.R`, the lines are as follows:

  ```R
  # Specify the DIRECTORY path storing the code of this repository
  DIR_PATH = "?"
  # Specifie the number of month period to use
  gap <- 2 or 6
  # Specify whether to use parameter optimization
  optimize_rf = FALSE or TRUE
  ```

- Create the following directories in `data_results/`: `time_order_results/results_imbalance/`, `time_order_results/results_balance/`, 

  and in `time_order_results/results_balance`, create the following directories :`under/`,  `over/`, `rose/`, `smote/`.

- Run  the script `code/calculation/main.R`

### Results

The result of each fold model performance scores (e.g., auc) will be saved to `time_order_results/results_imbalance/`, `time_order_results/results_balance/`.



