# first-visit Monte Carlo with possible random stopping
cpaths_monte_carlo <- function(model, test_set, k, n_iter=1000, random_stopping=TRUE){
  p <- ncol(test_set)
  states <- 1:p
  labels <- get_predictions(model, test_set)
  
  TG <- vector("numeric", p) #total gain from rewards per state (variable)
  names(TG) <- 1:p
  
  N <- vector("numeric", p) #total visits per state (variable)
  names(N) <- 1:p
    
  for (tt in 1:n_iter){
      states_episode <- c()
      rewards_episode <- c()
      dataX <- test_set
      for (i in 1:k){
        states_episode[i] <- sample(states, 1)
        dataX <- permute_column(dataX, test_set, states_episode[i])
        labels_perm <- get_predictions(model, dataX)
        rewards_episode[i] <- get_reward(labels_perm, labels)
        
        # if random_stopping == TRUE, end episode with probability of reward value
        if (random_stopping){
          stop <- rbinom(1, 1, rewards_episode[i])
          if (stop) break
        }
      }
      
      # add gains/rewards
      tmp <- c(rewards_episode[1], diff(rewards_episode))
      G <- rev(cumsum(rev(tmp)))
      names(G) <- states_episode
      tmp <- c(TG, G[unique(names(G))])
      TG <- tapply(tmp, names(tmp), sum)
      
      # add visits (first-visit so max one per episode)
      tmp <- c(N, table(unique(states_episode)))
      N <- tapply(tmp, names(tmp), sum)
    }  
  return(list(importance=TG/N, total_rewards=TG, total_visits=N))
}








