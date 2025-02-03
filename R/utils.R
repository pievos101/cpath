permute_column <- function(data, column_index) {
  data[,column_index] = data[sample(nrow(data), nrow(data)), column_index] 
  data
}

# swap value of sample with nearest neighbor from the other class
permute_column_nearest <- function(data, column_index, pred) {
  vec  = data[,column_index] 
  Dist = as.matrix(dist(vec)) 
  colnames(Dist) = pred
  rownames(Dist) = pred
  NN = pred
  res = apply(Dist, 1, function(x){
      names(x) = NN
      x_sorted = sort(x, decreasing=FALSE)
      id = which(names(x_sorted)[1] != names(x_sorted))[1]
      return(id)   
  })
  data[,column_index] = data[res, column_index]
  data
}

get_predictions <- function(model, data_matrix) {
  pred <- predict(model, data_matrix)$predictions
  if(is.matrix(pred)){
    apply(pred, 1, function(x){which.max(x)-1})
  }else{
    pred
  }
}

# k for possibility of reducing rewards for longer paths
get_reward <- function(new_predictions, old_predictions, k=1){
  mean(new_predictions != old_predictions)/k
}

# reward for drop in performance greater than 1%
# penalize for no drop
get_reward_rl <- function(predictions_new, predictions_old, labels, threshold=1e-2) {
  accuracy_old <- mean(predictions_old == labels)
  accuracy_new <- mean(predictions_new == labels)
  accuracy_difference <- accuracy_old - accuracy_new
  if (accuracy_difference / accuracy_old > threshold) { 
    1
  } else if (accuracy_difference <= 0) { 
    -1
  } else {
    0
  }
}

normalize01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}