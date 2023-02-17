permute_column <- function(data_matrix, data, column_index){
  f_perm = sample(data[,column_index], nrow(data_matrix)) 
  data_matrix[,column_index] = f_perm
  data_matrix
}

get_predictions <- function(model, data_matrix){
  pred <- predict(model, data_matrix)$predictions
  apply(pred, 1, function(x){which.max(x)-1})
}

# k for possibility of reducing rewards for longer paths
get_reward <- function(new_predictions, old_predictions, k=1){
  mean(new_predictions != old_predictions)/k
}
