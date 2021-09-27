# repelpredict

An R package to predict from REPEL models and power the REPEL API

## model fitting overview

Example workflows are in `inst/`. For example, `fit_xgboost_nowcast.R` fits the nowcast model using xgboost.

### generating training and validation sets

First, establish a database connection using `repeldata::repel_local_conn()` or ` repeldata::repel_remote_conn()`.

To run the model fitting pipeline, you need a training and validation data set. We have generic functions called `repel_training()` and `repel_validation()`.

These functions rely on internal functions that pull the data from the database, perform data manipulation, and apply certain heuristics prior to augmentation and fitting.

First of these is `repel_init()`. This function reads in the data and applies data manipulation. This stage of data manipulation is specific to model needs and is not something that would be universally applied in wahis transform functions. For example, `repel_init.nowcast_model()`summarizes over serotypes and makes assumptions about disease statuses if unreported. `repel_init.network_model()` determines whether outbreaks are ongoing, contained, or endemic by threading together reports and supplementing with nowcast prediciton data to identify endemic diseases. 

`repel_split()` divides the dataset produced by `repel_init()` into train and validation sets. This uses a lookup table that is generated manually in `inst/` (e.g., `inst/nowcast_generate_data_split_lookup.R`). The lookup table contains potential disease/country combinations into the future to ensure that the test and validation split does not need to be frequently rerun. `repel_split()` also allows the option of standardizing disease names. 

`repel_training()` and `repel_validation()` simply split out the data produced by `repel_split()`.

### data augmentaion
`repel_augment()` takes training or validation data and adds features needed for model fitting. The nowcast augment requires five fields in the input data: `country_iso3c`, `disease`, `taxa`, `report_year`, `report_semester`. The network augment requires three fields in the input data: `country_iso3c`, `disease`, and `month`.

### model fitting
`repel_fit()` runs the model fitting pipeline on augmented data. The nowcast (xgboost) model uses a tidymodel framework. The network (lme) model is not compatible with tidymodels and instead uses a base R/tidyverse framework to prep the data and fit with the `lme4` package. 

Fitted model objects are pushed to AWS S3.

### model predictions
`repel_predict()` takes as input the model object and augmented data and produces model predictions. `repel_forecast()` combines `repel_augment()`and `repel_predict()` on new data. 

### model results and interpretation (API compatable)
Model predictions for all disease + country + month combinations can be accessed with `get_disease_status_predict()`. This function returns a dataframe of disease status (reported/predicted), reported endemic/outbreak status, and predicted imported outbreak probability. 

Network model variable importance can be summarized via `get_network_variable_importance()` and with disaggregated country origins via `get_network_variable_importance_with_origins()`. The function `get_network_origin_contribution_import_risk()` summarizes origin country contributions to import risk over all variables. See `inst/reports/EHA-ASF-outbreak-in-DR-2021.rmd` for an example application of these functions.


