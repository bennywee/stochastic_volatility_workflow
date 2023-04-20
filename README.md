# Stochastic Volatility Workflow
Master's research project: Developing a workflow for fitting classes of Stochastic Volatility models in Stan.

```
├── data            # Contains all data files (created after executing data creation scripts)
├── models          # All Stan code lives here, including user defined functions and model executables. 
├── output          # When a model is trained or sampled, all the model artefacts (model objects, samples) and diagnostic plots are here.
├── R               # Location of R functions and modules - these are imported at the top of executable scripts (and could be turned into a R package)
├── renv            # Virtual environment, managed by renv package
├── scripts         # All executable R scripts (creating data, training model)
├── writeups        # Quarto documents. Will present plots and diagnoatics (from output directory) in a html file
├── Makefile        # Contains recipes/commands to create dataset and train model
├── README.md       # Readme 
├── config.r        # Contains all parameters to be set when picking which dataset to create and which model to train
├── renv.lock       # Locked package version (ensure all package dependencies are the same across machines)
└── stochastic_volatility_workflow.Rproj # R project
```


# Getting Started

## Environment setup
If using rstudio, open `stochastic_volatility_workflow.Rproj`. Then run `renv::restore()` to create the project libraries locally (the packages used in this project). Note, since `cmdstanr` was not downloaded from CRAN, you'll need to do this manually. [Instructions to do this can be found here](https://mc-stan.org/cmdstanr/).

## Using make and config files
If you have make installed then we can simply run the make commands to create the dataset and sample the model. First go to `config.r` and update the relevant parameters (current default is to run config script which will produce the KSC simulated data and train the reparameterised stan user guide model with priors from KSC). 

### Create Simulated dataset

```
make ksc_data
```

### Sample Stan model 

```
make sample_model
```

## Generate dataset and training model (manually from executables)
If you are not using `make`, then you can run the executable R scripts manually from the `scripts` directory.

### Kim, Shepherd, Chib simulated dataset
Execute `scripts/create_ksc_data.r`. There is the option to change the default parameters at the top of the script (same parameters are defined in `config.r`). A csv with `yobs` and `htrue` will be created in the `data/simulated/ksc` directory with the csv name corresponding to the chosen parameter values.

### Sample Stan model
After the data is created, execute `scripts/run_model.r`. This will fit the specified stan model from the `models` directory. Note that you should update the data location parameters to change the dataset the model is sampled on. The model objects and artifacts are saved as an `.RDS` file in the `output` directory (which can be reloaded for analysis later without re-fitting the model). 

The script will automatically run the code with and without evaluating the likelihood. This enables to have two sets of estimates to do prior and posterior predictive checking. These are saved to the corresponding model subdirectory. Additionally, the output csvs (containing generated quantities) will also be saved for further evaluation and to ensure all model outputs are contained in the same location.

### Evaluation
Plots for prior and predictive checking are automatically saved after the model is fit. There is also a `scripts/eval.r` which will load any saved model objects and generated quantities for further model evaluation and exploratory analysis (this includes running shinystan).

### (Optional) Yahoo Dataset
Execute `scripts/create_data.r` to generate dataset. This will create a `data` directory with `raw` (S&P500 data downloaded from Yahoo finance) and `preprocessed` (with log prices and log returns). The `raw` subdirectory contains original and immutable datasets. `preprocessed` contains any additional variables and transformations. We will use dataset from `preprocessed` for modelling S&P500 returns. 
