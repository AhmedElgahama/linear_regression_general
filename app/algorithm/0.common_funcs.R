

library(tidyverse)
library(lubridate)
library(data.table)
library(dtplyr)
library(tictoc)
library(glue)
library(pROC)
library(caret)
library(Metrics)
options(dplyr.summarise.inform = FALSE)







trainer_func <- function(train_set, 
                         validation_set, 
                         explanatory_variables) 
{
  
  print(glue('Hyperparameter tuning begins...'))
  
  model <- lm(label ~ ., data = train_set %>% select(explanatory_variables,label))
  
  val_pred <- predict(model, newdata = validation_set %>% select(explanatory_variables))
  rmse <- rmse(validation_set$label, val_pred)
  
  
  stuff <- list()
  stuff$mdl <- model
  stuff$rmse <- rmse

  return(stuff)
}


ohe_encoder <- function(df){
  dummy <- dummyVars(" ~ .", data=df)
  dummy
}




tester_func <- function(mdl, test_set ,feature_names) {
  
  test_features <- test_set %>% select(all_of(feature_names))
  test_predictions <- predict(mdl, test_features %>% as.data.frame())
  
  results <- list()
  results[['test_predictions']] <- 
    tibble(pred = test_predictions#, 
           ) 
  
  results
  
}


## *************************















calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

