permute_column <- function(data, column_index) {
  data[,column_index] = data[sample(nrow(data), nrow(data)), column_index] 
  data
}

get_predictions <- function(model, data_matrix) {
  pred <- predict(model, data_matrix)$predictions
  apply(pred, 1, function(x){which.max(x)-1})
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