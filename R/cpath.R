# main

cpath <- function(model, test_set, k){
  labels <- get_predictions(model, test_set)
  
  test_setX    <- test_set 
  cf_path      <- rep(NaN, k)
  label_switch <- FALSE
  
  # randomly select a feature
  f_start = sample(1:dim(test_setX)[2], k, replace=TRUE)
  cf_path = f_start # randomly select a feature
      
  label_switch_all = rep(FALSE, length(target))
  
  for (xx in 1:k){
    
      test_setX <- permute_column(test_setX, test_set, f_start[xx])
      labels_perm <- get_predictions(model, test_setX)
      
  
      if(!all(labels_perm==0)){
          # predict and check whether label changes
          p    = mean(labels!=labels_perm)
          stop = sample(c(0,1),1,prob=c(1-p,p))
          if(stop){
              label_switch_all = (labels!=labels_perm)
              label_switch = TRUE
              cf_path = cf_path[1:xx]   
              break
          }  
      }else{cf_path = rep(NaN, k); break}
  }
  
  return(list(orig_path=f_start, cf_path=cf_path, label_switch=label_switch, 
  label_switch_all=label_switch_all))

}