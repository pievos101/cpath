
transition <- function(PATHS_l, data, target, add1=FALSE){

# Get the length of path
k = dim(PATHS_l[[1]])[2]-2

# INIT #############
EDGES   = matrix(0, dim(data)[2], dim(data)[2])
EDGES_l = vector("list", length(target))

for(xx in 1:length(target)){
    EDGES_l[[xx]] = EDGES
}
####################

for(aa in 1:length(target)){
 
  for(xx in 1:dim(PATHS_l[[aa]])[1]){

    check = TRUE

    for(yy in 1:(k-1)){

     if(check==FALSE){break}# path of length 1 or end of path !!!        

     #for(zz in (yy+1):k){
        zz = yy + 1

        if(is.na(PATHS_l[[aa]][xx,2])){ # path of length 1 

            if(PATHS_l[[aa]][xx,k+1]==1){
                #EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,yy]]  = EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,yy]] + 1
                #EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,yy]]  = EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,yy]] + PATHS_l[[aa]][xx,k+2]
                EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,yy]]  = EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,yy]] + sum(is.na(PATHS_l[[aa]][xx,])) + ifelse(add1, 1, 0) # penalty on path length
                
            }

        check=FALSE
        break # path of length 1
        }

        if(is.na(PATHS_l[[aa]][xx,zz])){ # end of path
        
        #    if(PATHS_l[[aa]][xx,k+1]==1){
        #        EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,yy]]  = EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,yy]] + sum(is.na(PATHS_l[[aa]][xx,])) # penalty on path length         
        #    }

        check=FALSE;
        break # end of path
        }

        if(PATHS_l[[aa]][xx,k+1]==1){
            #EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,zz]]  = EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,zz]] + 1
            #EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,yy+1]] = EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,yy+1]] + PATHS_l[[aa]][xx,k+2]
            EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,zz]]   = EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,zz]] + sum(is.na(PATHS_l[[aa]][xx,])) + ifelse(add1, 1, 0) # penalty on path length
        }
        
        #if(PATHS_l[[aa]][xx,k+1]==0){
        #    EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,zz]]  = EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,zz]] - 1
        #}

     }
    #} 
  }
}

EDGES_all = Reduce("+", EDGES_l)

#pos_ids <- which(target==1)
#neg_ids <- which(target==0)

#EDGES_pos = EDGES_l[pos_ids]
#EDGES_pos = Reduce("+", EDGES_pos)

#EDGES_neg = EDGES_l[neg_ids]
#EDGES_neg = Reduce("+", EDGES_neg)

#EDGES_all=EDGES_all + 1
#EDGES_pos=EDGES_pos + 1
#EDGES_neg=EDGES_neg + 1

return(EDGES_all)

}