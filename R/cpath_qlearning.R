# epsilon-greedy action selection from Q-matrix
choose_action_Q <- function(Q_row_state, states, epsilon){
  if (rbinom(1, 1, epsilon)){
    next_state <- sample(states, 1)
  }
  else {
    next_state <- sample(states, 1, prob=Q_row_state) #which.max(Q_row_state)
  }
  next_state
}


# Q-learning
# action <-> next state (variable)
cpaths_qlearning <- function(model, test_set, k, n_iter=1000, 
                             epsilon=0.05, alpha=0.05, gamma=0.2,
                             random_stopping=TRUE){
  p <- ncol(test_set)
  states <- 1:p
  labels <- get_predictions(model, test_set)

  Q <- matrix(0.01, nrow = p, ncol = p)

  for (i in 1:n_iter){
    current_state <- sample(states, 1)
    action <- choose_action_Q(Q[current_state,], states, epsilon)
    dataX <- test_set
  
    for (i in 1:k){
      next_state <- action
      dataX <- permute_column(dataX, test_set, current_state)
      labels_perm <- get_predictions(model, dataX)
      reward <- get_reward(labels_perm, labels)
  
      next_action <- choose_action_Q(Q[next_state,], states, epsilon)
      Q[current_state, action] <- Q[current_state, action] + 
                                  alpha * (reward + gamma * max(Q[next_state,]) - Q[current_state, action])
      
      # if random_stopping == TRUE, end episode with probability of reward value
      if (random_stopping){
        stop <- rbinom(1, 1, reward)
        if (stop) break
      }
      
      current_state <- next_state
      action <- next_action
    }

  }
  return(list(importance=colSums(Q)/sum(Q), trans_matrix=Q))
}

