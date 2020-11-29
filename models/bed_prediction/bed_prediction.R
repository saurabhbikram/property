setwd("~/property/analysis")
#load("working.rdata")
library(xgboost)
library(leaflet)
library(tidyverse)
library(stringr)
library(ggplot2)
library(glue)
library(lubridate)
library(furrr)
library(kableExtra)

set.seed(1)

## Get Data --------

#source("make_analysis_data.R") # makes `prices_data_sum`
source("bed_pred_fn.R")

aws.signature::use_credentials(profile = "sb")

raw_data <- aws.s3::s3readRDS("preprocess/ALL.rds","ukpd")

prices_data_sum <- raw_data

lmdf <- make_model_df(prices_data_sum)

## Model Data --------
xgbdf <-  lmdf %>% filter(!is.na(beds)) %>% distinct() #%>% select(beds, area, longitude, latitude, small_address, postcode, energy_use)

# We split train/test/val by postcode.
# We will actually use cross val, so we split the list of unique postcodes
# into 6 groups
set.seed(1)
unique_postcodes <- xgbdf %>% group_by(postcode) %>% count() %>% arrange(desc(n)) %>% sample_frac(size=1)
fold_list <- split(unique_postcodes$postcode, ceiling(seq_along(unique_postcodes$postcode) %% 6))

test_fold <- fold_list[[1]]
train_folds <- fold_list[2:6]

# split the df of features into train and test
traindf <- xgbdf %>% filter(postcode %in% unlist(train_folds))
testdf <- xgbdf %>% filter(!postcode %in% unlist(train_folds))

# for each fold, get index of data from `traindf`
train_fold_ids <- list()
for(i in seq_along(train_folds)) train_fold_ids[[i]] <- which(traindf$postcode %in%  train_folds[[i]])

## Feature engineering ------ 

traindf_ab <- create_feat_df(traindf)

# training matrix
xmat <- traindf_ab %>% select(-postcode, -small_address, -beds) %>% as.matrix
summary(xmat)
## XGBoost training ------

# We use xgboost to train

train_xgb <- function(params) {
  
  set.seed(1)
  res <- xgb.cv(params=params,data=xmat, nrounds=100, label=traindf$beds, 
                folds=train_fold_ids, num_class=5, 
                early_stopping_rounds = 5,
                print_every_n = 10,
                metrics=list("mlogloss"), maximize=FALSE, verbose=FALSE)
  return (res)
  
  
}
random_param <- function(seed) {
  set.seed(seed)
  list(
    objective="multi:softmax",
    eta=sample(c(0.2,0.4,0.5,0.75),1),
    min_child_weight=sample(c(1,2,3,5),1),
    lambda=sample(c(0, 0.1, 0.2, 0.5, 0.75),1),
    alpha=sample(c(0.1,0.2,0.4,0.5,1),1),
    gamma=sample(c(0.05,0.1,0.2,0.5),1),
    max_depth=sample(c(3,5,10,25),1),
    colsample_bytree=sample(c(0.75,0.9,1),1),
    subsample=sample(c(0.9,1),1),
    monotone_constraints="(1,)" # we want area to be increasing in beds
  )
}

## Hyperparam ------

# We sample 200 configs randomly as we want to optimize regularisation etc.

if(length(Sys.glob("bed_prediction_run_hist.rds")) < 1) {
  run_res2 <- map(sample(1:100000,1000), function(x) {
    param <- random_param(x)
    res <- list(params=param)
    try({
      res <- train_xgb(param)
      cat(round(res$evaluation_log[res$best_iteration]$test_mlogloss_mean,3),", ")
    })
    return(res)
  })
  
  saveRDS(run_res2,"bed_prediction_run_hist.rds")
  
} else run_res2 <- readRDS("bed_prediction_run_hist.rds")

# Analysis of hyperparams
res_df2 <- map_df(run_res2, function(res){
  res$evaluation_log[res$best_iteration]
}) %>% mutate(run_id=1:length(test_mlogloss_mean)) %>% arrange(test_mlogloss_mean)

top_25 <- map_df(res_df2$run_id, function(x) {p <- run_res2[[x]]$params; p$eval_metric <- NULL; return(p)})
top_25$test_error <- res_df2$test_mlogloss_mean
top_25 <- select(top_25 %>% head(200), -objective, -num_class, -silent)
p_daig2 <- top_25 %>% gather(var, val, -test_error) %>% 
  mutate(
    val_ord = order(as.numeric(val)),
    val=factor(val, levels=unique( val[val_ord]))
  ) %>% 
  ggplot(aes(val, test_error)) + geom_boxplot(alpha=0.5, fill="sky blue") + facet_wrap("var", scales = "free_x")

(p_daig2 + theme_minimal() + ggtitle("XGBoost hyperparameters vs test error", subtitle = "Lower error is better"))
(lplot <- ggplot(top_25, aes(x=1:nrow(top_25),y=test_error)) + geom_line(colour="black") + theme_minimal() + ggtitle("Model performance") + scale_x_continuous("Model rank (best model = 1)"))


# sample 5 configs from the top 10% of models for ensemble
best_params <- map(1:ceiling(nrow(res_df2)/10), function(x) run_res2[[res_df2$run_id[x]]]) %>% sample(5)

# train ensemble models
ensemble_models <- map(best_params, function(x){
  model <- NULL
  x$params$objective <- "multi:softprob"
  x$params$num_class <- 5
  if(is.null(x$best_iteration)) x$best_iteration <- 100
  try(model <- xgboost(params=x$params,data=xmat, nrounds=x$best_iteration, label=traindf$beds,verbose=F))
  return(model)
})
ensemble_models <- ensemble_models[!map_lgl(ensemble_models,is.null)]

imp_df <- map_df(ensemble_models, function(x) xgb.importance(x$feature_names,x)) %>% group_by(Feature) %>% summarise_all(mean) %>% arrange(desc(Gain))
imp_df

# try regressions as loss function (doesnt work as well as classification)
# ensemble_models_reg <- map(best_params, function(x){
#   model <- NULL
#   x$params$objective <- NULL
#   x$params$num_class <- NULL
#   x$params$eval_metric <- NULL
#   if(is.null(x$best_iteration)) x$best_iteration <- 100
#   try(model <- xgboost(params=x$params,data=xmat, nrounds=x$best_iteration, label=as.numeric(traindf$beds),verbose=F))
#   return(model)
# })
# ensemble_models_reg <- ensemble_models_reg[!map_lgl(ensemble_models_reg,is.null)]
# 
# predict_ensemble_reg <- function(data) map(ensemble_models_reg, function(x) predict(x, data)) %>% do.call(cbind,.) %>% apply(1,mean) %>% floor %>% ifelse(.>4,4,.)
# 
# imp_df_reg <- map_df(ensemble_models_reg, function(x) xgb.importance(x$feature_names,x)) %>% group_by(Feature) %>% summarise_all(mean)
# imp_df_reg

## Prediction ---------

testdff_ab <- create_feat_df(testdf)
label <- testdff_ab$beds
xmat <- testdff_ab[,ensemble_models[[1]]$feature_names] %>% as.matrix

testdff_ab$predicted_beds <- predict_ensemble(xmat, ensemble_models)

caret::confusionMatrix(testdff_ab$predicted_beds %>% factor(levels=0:4),testdff_ab$beds %>% factor(levels=0:4))

#testdff_ab %>% group_by(postcode) %>% summarise(acc = mean(predicted_beds==beds), num_prop=length(beds)) %>% arrange(desc(num_prop))

prod_mat <- create_feat_df(xgbdf)
prod_mat <- prod_mat[,ensemble_models[[1]]$feature_names] %>% as.matrix
prod_label <- xgbdf$beds

table(prod_label)

# train ensemble models
ensemble_models_prod <- map(best_params, function(x){
  model <- NULL
  x$params$objective <- "multi:softprob"
  x$params$num_class <- 5
  if(is.null(x$best_iteration)) x$best_iteration <- 100
  try(model <- xgboost(params=x$params,data=prod_mat, nrounds=x$best_iteration, label=prod_label,verbose=F))
  return(model)
})
ensemble_models_prod <- ensemble_models_prod[!map_lgl(ensemble_models_prod,is.null)]
saveRDS(ensemble_models_prod, "bed_predictor_prod.rds")

