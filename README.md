# Stochastic Volatility Workflow
Master's research project: Developing a workflow for fitting classes of Stochastic Volatility models in Stan.

# How to run code

## Environment setup
If using rstudio, open `stochastic_volatility_workflow.Rproj`. Then run `renv::restore()` to create the project libraries locally (the packages used in this project). Note, this may not work for `cmdstanr` since this was not downloaded from CRAN. Instructions to do this can be found in [this vignette](https://mc-stan.org/cmdstanr/articles/cmdstanr.html#saving-fitted-model-objects-1).

## Generate dataset

### Yahoo Dataset
Execute `scripts/create_data.r` to generate dataset. This will create a `data` directory with `raw` (S&P500 data downloaded from Yahoo finance) and `preprocessed` (with log prices and log returns). The `raw` subdirectory contains original and immutable datasets. `preprocessed` contains any additional variables and transformations. We will use dataset from `preprocessed` for modelling S&P500 returns. 

### Kim, Shepherd, Chib simulated dataset
Execute `scripts/create_ksc_data.r`. There is the option to change the default parameters at the top of the script. A csv with `yobs` and `htrue` will be created in the `data/simulated/ksc` directory with the csv name corresponding to the chosen parameter values.

## Fit Stan model
After the data is created, execute `scripts/run_model.r`. This will fit the specified stan model from the `models` directory. Note that you should update the data location parameters to change the dataset the model is fit on. The fitted model objects and artifacts are saved as an `.RDS` file in the `output` directory (which can be reloaded for analysis later without re-fitting the model). Any additional figures, diagnostics and statistics will be saved in the corresponding subdirectory.