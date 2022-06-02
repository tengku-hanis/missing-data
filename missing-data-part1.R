# Missing data (Part 1)
# Tengku Hanis - June 1st, 2022

# Packages
library(VIM)
library(mice)
library(tidyverse)
library(naniar)
library(DataExplorer)
library(recipes)

# Data
library(mlbench)
data("PimaIndiansDiabetes2")
dat <- PimaIndiansDiabetes2

# R base functions --------------------------------------------------------

summary(dat)

anyNA(dat)

is.na(dat)
is.na(dat) %>% sum()
is.na(dat) %>% colSums()

dim(dat)
na.omit(dat) %>% dim()

complete.cases(dat) 
dat[complete.cases(dat), ] %>% dim()

mean(dat$insulin)
mean(dat$insulin, na.rm = T)


# Explore - VIM -----------------------------------------------------------

# How many NAs in the data
countNA(dat)

# Pattern of missingness
aggr(dat, prop = F, numbers = T) #not for large data

# Pattern of missingness 
matrixplot(dat) #for large data/lot of variables


# Explore - naniar --------------------------------------------------------

# Percentage of rows/cases contain NAs
pct_miss_case(dat)

# Percentage of NAs in data
pct_miss(dat)

# Percentage of NAs acc to variables
miss_var_summary(dat)


# Explore - other packages ------------------------------------------------

# Plot NAs percentage
plot_missing(dat)

# Profile of NAs
profile_missing(dat)


# Explore - mice ----------------------------------------------------------

# Pattern of NAs
md.pattern(dat, rotate.names = T) #not for large data


# Assessing missing data --------------------------------------------------

# Little's test
mcar_test(dat) #p < 0.05, MAR is indicated

# Assess by plot (numerical var)

# 1) Histogram (need to do for each pair of vars with NAs var is fixed)
## Age by insulin
dat %>% select(age, insulin) %>% histMiss()
niceFunction::histNA_byVar(dat, insulin, age) 

## Age by pressure
dat %>% select(age, pressure) %>% histMiss()
niceFunction::histNA_byVar(dat, pressure, age)

# 2) Scatter plot (need to do for each pair of vars with NAs var is fixed)
## Age vs  insulin
dat %>% select(age, insulin) %>% scattMiss()
dat %>% select(age, insulin) %>% marginplot()

## Mass vs pressure
dat %>% select(mass, pressure) %>% scattMiss() #try side = 2
dat %>% select(mass, pressure) %>% marginplot()

# 3) Show both plots for all variable - slow and impractical for large data
scattmatrixMiss(dat, highlight = "insulin")
marginmatrix(dat)

# Assess by correlation
## Matrix with NAs variable only
x <- as.data.frame(abs(is.na(dat))) %>% 
  select(-c(pregnant, pedigree, age, diabetes)) #pick variable with NAs only
head(x) #data with 1 = NAs, 0 = non-NAs

## 1) Correlation between NAs variable
cor(x) %>% 
  corrplot::corrplot(method = "number")

## 2) Correlation between NAs variable and observed values of other variables
cor(dat %>% mutate_if(is.factor, as.numeric), x, use = "pairwise.complete.obs")
#rows are the observed variables and the columns represent the missingness
# as age increases, triceps values tend to be missing (r = 0.22)


# Deletion ----------------------------------------------------------------

# List-wise deletion
na.omit(dat) %>% dim()


# Simple imputation -------------------------------------------------------

# 1) Mean ----
mean_glucose <- mean(dat$glucose, na.rm = T)

dat_mean <- dat
dat_mean[is.na(dat_mean$glucose), "glucose"] <- mean_glucose

anyNA(dat_mean$glucose)
anyNA(dat$glucose)

## Alternative - use mice
imp_mean <- mice(dat, m = 1, method = "mean", maxit = 1, printFlag = F)
imp_mean

imp_mean$imp$mass
mean(dat$mass, na.rm = T)

## Extract data
dat_mean2 <- complete(imp_mean)

summary(dat)
summary(dat_mean2)

# 2) Median ----
med_glucose <- median(dat$glucose, na.rm = T)

dat_med <- dat
dat_med[is.na(dat_med$glucose), "glucose"] <- med_glucose

anyNA(dat_med$glucose)
anyNA(dat$glucose)

# 3) Mode ----
mode_mass <- DescTools::Mode(dat$mass, na.rm = T)

dat_mode <- dat
dat_mode[is.na(dat_mode$mass), "mass"] <- mode_mass

anyNA(dat_mode$mass)
anyNA(dat$mass)

# 4) Random ----
imp_random <- mice(dat, m = 1, method = "sample", maxit = 1, printFlag = F, seed = 123)
imp_random

imp_random$imp$mass


# Single imputation -------------------------------------------------------

# mice ----
#see details in ?mice

## Random forest ----
imp_rf <- mice(dat, m = 1, method = "rf", maxit = 1, printFlag = F, seed = 123)
imp_rf

imp_rf$imp

dat_rf <- complete(imp_rf)
summary(dat_rf)

## CART ----
imp_cart <- mice(dat, m = 1, method = "cart", maxit = 1, printFlag = F, seed = 123)
imp_cart

imp_cart$imp

dat_cart <- complete(imp_cart)
summary(dat_cart)


## Details on mice ----
### Setting 
init <- mice(dat, maxit = 0, printFlag = F)

### Predictors
predmat <- init$predictorMatrix #also can use quickpred(dat)

#1=predictor, 0=non-predictor
predmat[2, "diabetes"] <- 0 #turn off diabetes for glucose
predmat[3, "triceps"] <- 0 #turn off triceps for pressure
predmat

### Methods
meth <- imp$method
meth[2:4] <- "rf"
meth[5:6] <- "norm.nob" #stochastic linear reg model
#norm.nob add random error to to the predicted value, so to not overfit (in ML term)
#norm.predict fit linear reg model
meth

### Impute
imp <- mice(dat, m = 1, method = meth, predictorMatrix = predmat, maxit = 1, printFlag = F, seed = 123)
imp

### Diagnostic checking
densityplot(imp)
stripplot(imp)

### Extract imputed data
dat_mice <- complete(imp)
summary(dat_mice)


# recipes -----------------------------------------------------------------
dat_rec <- 
  recipe(diabetes ~ ., data = dat) %>% 
  step_impute_bag(glucose, pressure, seed_val = 123) %>% #see ?step_impute_bag
  step_impute_knn(triceps) %>% 
  step_impute_mean(insulin) %>% 
  step_impute_linear(mass)
dat_rec

dat_imputed <- dat_rec %>% prep %>% bake(new_data = NULL)
anyNA(dat_imputed)
