choose_state_Q <- function(Q_row_actions, states, epsilon) {
  if (rbinom(1, 1, epsilon)) {
    next_state <- sample(states, 1)
  } else {
    # next_state <- sample(states, 1, prob=normalize01(Q_row_actions))
    next_state <- which.max(Q_row_actions)
  }
  next_state
}

cpath_rl <- function(model, test_set, k, n_iter=1000, 
                     epsilon='decay', alpha='decay', gamma=0.5,
                     verbose=FALSE){
  p <- ncol(test_set)
  states <- 1:p
  labels <- get_predictions(model, test_set)
  
  Q <- matrix(0.01, nrow = p, ncol = p)
  
  if (epsilon == 'decay') {
    epsilon_values <- seq(1, 0.1, length.out=n_iter)
  } else {
    epsilon_values <- rep(epsilon, n_iter)
  }
  
  if (alpha == 'decay') {
    alpha_values <- seq(1, 1/n_iter, length.out=n_iter)
  } else {
    alpha_values <- rep(alpha, n_iter)
  }
  
  for (iter in 1:n_iter) { 
    current_state <- sample(states, 1)
    current_data <- test_set
    old_predictions <- labels
    
    for (step in 1:k) {
      new_data <- permute_column(current_data, current_state)
      new_predictions <- get_predictions(model, new_data)
      reward <- get_reward_rl(new_predictions, old_predictions, labels)
      next_state <- choose_state_Q(Q[current_state,], states, epsilon_values[iter])
      Q[current_state, next_state] <- Q[current_state, next_state] + 
        alpha_values[iter] * (reward + gamma * max(Q[next_state,]) - 
                                Q[current_state, next_state])
      old_predictions <- new_predictions
      current_state <- next_state
    }
  }
  
  V <- t(apply(Q, 2, normalize01))
  return(list(importance=normalize01(colSums(V)), trans_matrix=Q))
}