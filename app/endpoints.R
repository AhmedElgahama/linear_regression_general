

## ---- Initialising libraries ----
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(httr)))
suppressWarnings(suppressMessages(library(glue)))

## Load model
trained_model  <- read_rds('/opt/ml_vol/model/artifacts/model.rds')
variables_to_encode   <- trained_model$variables_to_encode
id_column             <- trained_model$id_column
exp_vars              <- trained_model$exp_vars
encodings             <- trained_model$encodings
encoding_outputs      <- trained_model$encoding_outputs
variables_numeric     <- trained_model$variables_numeric
scale_func            <- trained_model$scale_func
id_column             <- trained_model$id_column


prediction_scorer <- function(row) {
  ## Function to get data and return probability
  
  ## initialize scores
  score  <- 0
  
  full_data_numeric <- row %>% select(variables_numeric)
  full_data_numeric <- predict(scale_func, as.data.frame(full_data_numeric))
  
  ## Encode categorical features with number of training encoding
  if(length(variables_to_encode) != 0)
  {
    full_data_categorical <-
      row  %>% select(variables_to_encode) 
    
    full_data_categorical <- data.frame(predict(encodings, newdata=full_data_categorical))
    for(i in encoding_outputs) {
      if(i %in% full_data_categorical %>% colnames()){
        invisible()
        
      } else {
        full_data_categorical <- full_data_categorical %>% 
          mutate(missing_column = 0) 
        colnames(full_data_categorical)[colnames(full_data_categorical) == "missing_column"] = i
      }
    }
    
    
    row <-
      cbind( full_data_numeric, full_data_categorical)
    
  } else{
    row <-
      cbind( full_data_numeric)
    
  }
  
  ## Getting probability
  score <-
    predict(trained_model$mdl,
            row %>% select(
              all_of(exp_vars)
            ))
  
  ## return prediction
  score
}




#* @post /infer
#* @serializer json list(auto_unbox=TRUE)
function(req) {
  ## grab the request body 'req' and put it into the variable 'row'
  row <- jsonlite::fromJSON(req$postBody) %>% as_tibble()
  row %>% glimpse()
  
  ids <- row %>% select(id_column)
  ## placeholder for JSON string to be printed at the end
  result <-
    tibble(
      prediction = 0,
      warnings = ''
    )
  
  ## parameters that we need
  necessary_params <- exp_vars
  
  ## if we do NOT have all we need...
  if (!all(necessary_params %in% names(row))) {
    result$prediction <- 0
    result$warnings <- 'Some necessary features are missing'
    
  } else {
    ## keep only the necessary parameters
    row <- row[necessary_params]
    
    ## if any of the necessary parameters are null...
    if (row %>% sapply(is.null) %>% any()) {
      result$prediction <- 0
      result$warnings <-
        paste('The following required parameters were NULL:',
              null_parameters)
      
    } else {
      prediction <- prediction_scorer(row)
    
    
  }
  
    Data <-list(predictions = prediction)
    dataFrame <- as.data.frame(Data)
    dataFrame <- cbind(ids,dataFrame)
    split(dataFrame, 1:nrow(dataFrame)) %>% unname()
 
}

}



#* @get /ping
#* @serializer json list(auto_unbox=TRUE)
endpoint.healthz <- function(req) {
  return("it's working perfectly")
}
