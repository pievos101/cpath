
cpaths <- function(model, data, target, k=4, n.iter=1000){

# calculate CF paths for each sample
PATHS   = matrix(NaN, n.iter, k+2)
PATHS_l = vector("list", length(target))

for(xx in 1:length(target)){
    PATHS_l[[xx]] = PATHS
}

for (xx in 1:n.iter){

    res = cpath(model, data, k=k)
    ids = which(res$label_switch_all)
  
  for(yy in 1:length(target)){  
    PATHS_l[[yy]][xx,1:length(res$cf_path)] = res$cf_path
    if(is.element(yy, ids)){
        PATHS_l[[yy]][xx,k+1] = TRUE
        PATHS_l[[yy]][xx,k+2] = sum(res$label_switch_all)    
    }else{
        PATHS_l[[yy]][xx,k+1] = FALSE
        PATHS_l[[yy]][xx,k+2] = sum(res$label_switch_all)
    }    
  } 
}

return(PATHS_l)

}