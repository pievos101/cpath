importance <- function(Tran, agg_type = "matrix"){

  if (agg_type == "matrix"){
    IMP_all = colSums(Tran)/sum(Tran)
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