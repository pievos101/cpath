importance <- function(Tran){

EDGES_all = Tran

#All
IMP_all <- rep(NaN, dim(Tran)[2])
for (xx in 1:dim(Tran)[2]){
    IMP_all[xx] = sum(EDGES_all[,xx])/sum(EDGES_all)#sum(EDGES_all[,-xx])
}

return(IMP_all)

}