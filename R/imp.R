importance <- function(Tran, agg_type = "matrix"){

  if (agg_type == "matrix"){
    EDGES_all = Tran
    
    #Imp
    IMP_all <- rep(NaN, dim(Tran)[2])
    for (xx in 1:dim(Tran)[2]){
        IMP_all[xx] = sum(EDGES_all[,xx])/sum(EDGES_all)
    }
    
    return(IMP_all)
  } else if (agg_type == "stationary_distribution"){
    T_norm <- Tran/rowSums(Tran)
    p <- diag(nrow(T_norm)) - T_norm
    A <- rbind(t(p),
               rep(1, ncol(T_norm)))
    b <- c(rep(0, nrow(T_norm)),
           1)
    IMP_pi = qr.solve(A, b)
    return(IMP_pi)
  }

}