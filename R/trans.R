
transition <- function(cpaths){
  paths <- cpaths$paths[cpaths$counterfactuality,]
  k <- ncol(paths)
  trans_matrix <- matrix(0, nrow=cpaths$p, ncol=cpaths$p)
  
  for (i in 1:nrow(paths)){
    path <- paths[i,]
    path <- path[!is.na(path)]
    l <- length(path)
    if (l == 1){
      trans_matrix[path[1], path[1]] <- trans_matrix[path[1], path[1]] + 1 + k - l
    } else {
      for (j in 1:(l-1)){
        trans_matrix[path[j], path[j+1]] <- trans_matrix[path[j], path[j+1]] + 1 + k - l
      }
    }
  }
  trans_matrix
}
