importance <- function(Tran){

EDGES_all = Tran

#Imp
IMP_all <- rep(NaN, dim(Tran)[2])
for (xx in 1:dim(Tran)[2]){
    IMP_all[xx] = sum(EDGES_all[,xx])/sum(EDGES_all)
}

return(IMP_all)

}