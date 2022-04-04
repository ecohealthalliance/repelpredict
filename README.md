# repelpredict

An R package to predict from REPEL models and power the REPEL API. The NowCast model provides current and next-year estimates of disease status (i.e., presence or absence) and case counts at the country level. The Network (aka TravelCast) model predicts the probability of cross-border disease spread following a new outbreak event and the likely mechanism of spread (e.g., trade).

### model fitting overview

Example workflows are in `inst/`. `fit_xgboost_nowcast.R` is the workflow to fit the nowcast model using xgboost. `fit_lme_network.R` is the workflow to fit the network model using lme4. 

### S3 methods

This package uses R's object-oriented system. This means that functions are generic (e.g., `repel_fit()`) and require a model object to signify which process to follow. For example, `repel_fit()` can be run as `repel_fit.network_model()`  or `repel_fit.nowcast_model()`. Model objects can be separately defined and fed into the function. For example: we can define `model_object <- network_lme_model()` and run `repel_fit(model_object, ...)`.

### generating training and validation sets

First, establish a database connection using `repeldata::repel_local_conn()` or ` repeldata::repel_remote_conn()`.

To run the model fitting pipeline, you need a training and validation data set. We have generic functions called `repel_training()` and `repel_validation()`.

These functions rely on internal functions that pull the data from the database, perform data manipulation, and apply certain heuristics prior to augmentation and fitting.

First of these internal functions is `repel_init()`. This function reads in the data and applies data manipulation. This stage of data manipulation is specific to model needs and is not something that would be universally applied in `wahis` functions. For example, `repel_init.nowcast_model()`summarizes over serotypes and makes assumptions about disease statuses if unreported. `repel_init.network_model()` determines whether outbreaks are ongoing, contained, or endemic by threading together reports and supplementing with nowcast prediciton data to identify endemic diseases. 

The dataset produced by `repel_init()` is then divided into train and validation sets by `repel_split()`. This uses a lookup table that is generated manually in `inst/` (e.g., `inst/nowcast_generate_data_split_lookup.R`). The lookup table contains potential disease/country combinations into the future to ensure that the test and validation split does not need to be frequently rerun.  

`repel_training()` and `repel_validation()` simply split out the data produced by `repel_split()`.

### data augmentaion

`repel_augment()` takes data that has been preprocessed with `repel_init()` and adds features needed for model fitting. By default, `repel_augment()` will augment the full dataset from the database. Users have the option to enter a subset of the data (e.g., only the training dataset).

### model fitting

`repel_fit()` runs the model fitting pipeline on augmented data. The nowcast (xgboost) model uses a tidymodel framework. The network (lme) model is not compatible with tidymodels and instead uses a base R/tidyverse framework to prep the data and fit with the `lme4` package. 

Fitted model objects are pushed to AWS S3.

### model predictions

`repel_predict()` takes as input the model object and augmented data and produces model predictions. `repel_forecast()` combines `repel_augment()`and `repel_predict()` on new data. 

### model results and interpretation (API compatable)

Model predictions for all disease + country + month combinations can be accessed with `get_disease_status_predict()`. This function returns a dataframe of disease status (reported/predicted), reported endemic/outbreak status, and predicted imported outbreak probability. 

Network model variable importance can be summarized via `get_network_variable_importance()` and with disaggregated country origins via `get_network_variable_importance_with_origins()`. The function `get_network_origin_contribution_import_risk()` summarizes origin country contributions to import risk over all variables. See `inst/reports/asf-dominican-republic-2021/EHA-ASF-outbreak-in-DR-2021.rmd` for an example application of these functions.

### model evaluation

Model evaluation reports are in `inst/reports/`


