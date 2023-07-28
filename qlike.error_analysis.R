# uses the results of "validation.R"

error_array_analysis <- function(error_array, GM_models_list, h_list){
  # compute of the columns
  error_mean = aaply(error_array, c(1,3), .fun = mean)
  
  row.names(error_mean) <- GM_models_list
  colnames(error_mean) <- h_list
  
  print(error_mean)
  
  # 
  min_indices <- apply(error_mean, 2, which.min)
  
  min_list = apply(error_mean, 2,min)
  error_mean_min = array(NA, dim = dim(error_mean))
  
  row.names(error_mean_min) <- GM_models_list
  colnames(error_mean_min) <- h_list
  
  for(i in seq_along(min_list)){
    error_mean_min[,i] <- error_mean[,i]== min_list[[i]]
  }
  
  print(error_mean_min)
  
  
}


error_array_analysis(error_array, GM_models_list, h_list)
