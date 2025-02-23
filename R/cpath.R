#' Counterfactual single path generation
#'
#' 
#' @param model The classifier to be explained 
#' @param test_set The test set for which explanations should be generated.
#' The samples as rows and the features as columns.
#' @param k The maximum length of the perturbation path
#' @param graph A graph structure of the features (igraph object). default=NaN
#' @param nearest Computes minimal perturbation paths (CPATH_min). default=FALSE
#' @return The counterfactual paths and the fraction of swapped classes.
#'
#' @examples
#' NaN
#'
#'@export


cpath <- function(model, test_set, k, graph=NaN, nearest=FALSE){
  labels <- get_predictions(model, test_set)
  
  n <- nrow(test_set)
  p <- ncol(test_set)
  test_setX    <- test_set 
  cf_path      <- rep(NaN, k)
  swapped_fraction <- rep(NaN, k)
  counterfactuality <- FALSE
  swapped_observations <- matrix(nrow=k, ncol=n)

  if(is.na(graph)[1]){
    SAMPLE = sample(1:p, k)
  }else{
    SAMPLE = random_walk(graph, sample(1:p, 1), k)
  }

  for (xx in 1:k){
      
      cf_path[xx] <- SAMPLE[xx] #sample(1:p, 1)
      
      if(nearest){
        test_setX <- permute_column_nearest(test_setX, cf_path[xx], labels)
      }else{
        test_setX <- permute_column(test_setX, cf_path[xx])
      }

      labels_perm <- get_predictions(model, test_setX)
      swapped_observations[xx,] <- (labels!=labels_perm)
      fraction <- mean(swapped_observations[xx,])
      swapped_fraction[xx] <- fraction
      stop <- sample(c(0,1), 1, prob=c(1-fraction,fraction))
      if(stop){
          counterfactuality <- TRUE
          break
      }  
  }
  reswapped_fraction <- rowSums(apply(swapped_observations, 2, diff) == -1) / n
  reswapped_fraction <- c(NaN, reswapped_fraction[1:(k-1)])
  return(list(cf_path=cf_path,
              swapped_fraction=swapped_fraction,
              counterfactuality=counterfactuality,
              reswapped_fraction=reswapped_fraction)
        )
}