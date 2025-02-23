#' Counterfactual multiple path generation
#'
#' 
#' @param model The classifier to be explained 
#' @param data The test set for which explanations should be generated.
#' The samples as rows and the features as columns.
#' @param k The maximum length of the perturbation path
#' @param n_paths Number of samples paths (default=1000)
#' @param graph A graph structure of the features (igraph object). default=NaN
#' @param nearest Computes minimal perturbation paths (CPATH_min). default=FALSE
#' @return The counterfactual paths and the fraction of swapped classes.
#'
#' @examples
#' NaN
#'
#'@export

cpaths <- function(model, data, k=4, n_paths=1000, graph=NaN, nearest=FALSE){

  paths <- matrix(NaN, nrow=n_paths, ncol=k)
  swapped_fractions <- matrix(NaN, nrow=n_paths, ncol=k)
  reswapped_fraction <- matrix(NaN, nrow=n_paths, ncol=k)
  counterfactuality <- rep(FALSE, n_paths)
  lengths <- rep(NaN, n_paths)

  for (xx in 1:n_paths){
    cpath <- cpath(model, data, k=k, graph, nearest)
    paths[xx, ] <- cpath$cf_path[1:k]
    swapped_fractions[xx, ] <- cpath$swapped_fraction
    reswapped_fraction[xx, ] <- cpath$reswapped_fraction
    counterfactuality[xx] <- cpath$counterfactuality
  }
  
  return(list(paths = paths, 
              swapped_fractions = swapped_fractions,
              reswapped_fraction = reswapped_fraction,
              counterfactuality = counterfactuality,
              p = ncol(data)
              ))
}

#' Multiple core Counterfactual path generation
#'
#' 
#' @param model The classifier to be explained 
#' @param data The test set for which explanations should be generated.
#' The samples as rows and the features as columns.
#' @param k The maximum length of the perturbation path
#' @param n_paths Number of samples paths (default=1000)
#' @param graph A graph structure of the features (igraph object). default=NaN
#' @param ncores Number of cores used for computation 
#' @param nearest Computes minimal perturbation paths (CPATH_min). default=FALSE
#' @return The counterfactual paths and the fraction of swapped classes.
#'
#' @examples
#' NaN
#'
#'@export
cpaths_mc <- function(model, data, k=4, n_paths=1000, graph=NaN, 
                                            ncores=NaN, nearest=FALSE){
  
  require(doParallel)
  require(foreach)
  require(parallel)

  if(is.na(ncores)){
    ncores = detectCores() - 2
  }

  cl <- makeCluster(ncores)
  registerDoParallel(cl)

  res <- foreach(i = 1:n_paths, .packages = c("ranger", "igraph", 
        "cpath")) %dopar% {
      #res <- cpaths(model, data, k=k, n_paths=1, graph)
      arun = cpath(model, data, k=k, graph, nearest)
      arun
  }

stopCluster(cl)

ALL = Reduce('rbind', res)
paths = Reduce('rbind', ALL[,1])
swapped_fractions = Reduce('rbind', ALL[,2])
counterfactuality = Reduce('rbind', ALL[,3])
reswapped_fraction = Reduce('rbind', ALL[,4])


return(list(paths = paths, 
              swapped_fractions = swapped_fractions,
              reswapped_fraction = reswapped_fraction,
              counterfactuality = counterfactuality,
              p = ncol(data)
              ))

}