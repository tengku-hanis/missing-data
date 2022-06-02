# Recipes example for ML
# Tengku Hanis - June 1st, 2022

# Packages
library(tidymodels)


# Data --------------------------------------------------------------------

library(mlbench)
data("PimaIndiansDiabetes2")
dat <- PimaIndiansDiabetes2


# Split data --------------------------------------------------------------

set.seed(123)
ind <- initial_split(dat)
dat_train <- training(ind)
# dat_test <- testing(ind)

set.seed(123)
dat_cv <- vfold_cv(dat_train)


# Recipes -----------------------------------------------------------------

dat_rec <- 
  recipe(diabetes ~., data = dat_train) %>% 
  step_mutate(diabetes = forcats::fct_relevel(diabetes, "pos")) %>% 
  step_impute_bag(insulin, triceps, pressure, mass, glucose, trees = tune())


# Model -------------------------------------------------------------------

rf_spec <- 
  rand_forest() %>% 
  set_mode("classification") %>% 
  set_engine("ranger") #see show_engines("rand_forest")


# Workflow ----------------------------------------------------------------

rf_wf <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(dat_rec)


# Tune --------------------------------------------------------------------

set.seed(123)
dat_tune <- 
  rf_wf %>% 
  tune_grid(
    dat_cv,
    grid = 10,
    metrics = metric_set(roc_auc, pr_auc),
    control = control_grid(verbose = T)
  )

## Tuning result ----
dat_tune %>% 
  autoplot()

dat_tune %>% 
  show_best("roc_auc")

dat_tune %>% 
  show_best("pr_auc")


# Finalise workflow -------------------------------------------------------

best_param <- 
  select_best(dat_tune, "pr_auc")

final_wf <- 
  rf_wf %>% 
  finalize_workflow(best_param)
final_wf


# Last fit ----------------------------------------------------------------

test_fit <- 
  final_wf %>%
  last_fit(ind) 


# Evaluation metrics ------------------------------------------------------

test_fit %>%
  collect_metrics()

test_fit %>% 
  collect_predictions() %>% 
  pr_auc(truth = diabetes, estimate = .pred_pos) 

test_fit %>%
  collect_predictions() %>% 
  pr_curve(truth = diabetes, estimate = .pred_pos) %>% 
  autoplot() +
  labs(title = "Precision-recall curve")
