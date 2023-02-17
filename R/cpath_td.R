# epsilon-greedy action selection from V - state values vector
choose_action_V <- function(V, states, epsilon){
  if (rbinom(1, 1, epsilon)){
    next_state <- sample(states, 1)
  }
  else {
    next_state <- which.max(V)
  }
  next_state
}



# TD(0)-learning
# action <-> next state (variable)
cpaths_tdlearning <- function(model, test_set, k, n_iter=1000, 
                             epsilon=0.05, alpha=0.05, gamma=0.2,
                             random_stopping=TRUE){
  p <- ncol(test_set)
  states <- 1:p
  labels <- get_predictions(model, test_set)
  
  V <- rep(1/p, p)

  for (i in 1:n_iter){
    current_state <- choose_action_V(V, states, epsilon)
    dataX <- test_set
    
    for (i in 1:k){
      dataX <- permute_column(dataX, test_set, current_state)
      labels_perm <- get_predictions(model, dataX)
      reward <- get_reward(labels_perm, labels)
  
      next_state <- choose_action_V(V, states, epsilon)
  
      V[current_state] <- V[current_state] + alpha * (reward + gamma * V[next_state] - V[current_state])
    
      # if random_stopping == TRUE, end episode with probability of reward value
      if (random_stopping){
        stop <- rbinom(1, 1, reward)
        if (stop) break
      }
      
      current_state <- next_state
    }
  
  }
  
  return(list(importance=V/sum(V), V=V))
}



