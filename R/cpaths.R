#'@export
cpaths <- function(model, data, k=4, n_paths=1000){

  paths <- matrix(NaN, nrow=n_paths, ncol=k)
  swapped_fractions <- matrix(NaN, nrow=n_paths, ncol=k)
  reswapped_fraction <- matrix(NaN, nrow=n_paths, ncol=k)
  counterfactuality <- rep(FALSE, n_paths)
  lengths <- rep(NaN, n_paths)
  
  for (xx in 1:n_paths){
    cpath <- cpath(model, data, k=k)
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