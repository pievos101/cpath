# main 
library(ranger)
library(lime)
library(naivebayes)

source("~/GitHub/c-path/CFpath.r")
source("~/GitHub/c-path/sim.r")

n.sim=1
COR_cpath    = rep(NaN, n.sim)
COR_shap     = rep(NaN, n.sim)
COR_between  = rep(NaN, n.sim)
COR_lime     = rep(NaN, n.sim)
COR_cpi      = rep(NaN, n.sim)

# SIM
for(ii in 1:n.sim){

cat(ii, " of ", n.sim, "\n")

res  = sim()
data = res$data
target = res$target

while(TRUE){

    model = gaussian_naive_bayes(x = as.matrix(data),
                     y = as.factor(target))
    pred = predict(model, as.matrix(data), type = "prob")
    pred = apply(pred,1,function(x){which.max(x)-1})

    #print("?")
    if(all(pred==0)|all(pred==1)){
        next
    #}

    #if(ModelMetrics::auc(pred, target)<0.70){
        
    #    print("Accuracy too low!")
    #    rm(.Random.seed, envir=globalenv())
    #    Sys.sleep(2)
    #    next

    }else{break}
}


n.iter= 1000
k = 4 # length of path

# calculate CF paths for each sample
PATHS   = matrix(NaN, n.iter, k+2)
PATHS_l = vector("list", length(target))

for(xx in 1:length(target)){
    PATHS_l[[xx]] = PATHS
}

for (xx in 1:n.iter){

    res = CFpath(model, data, k=k)
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
                EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,yy]]  = EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,yy]] + sum(is.na(PATHS_l[[aa]][xx,])) # penalty on path length
                
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
            EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,zz]]   = EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,zz]] + sum(is.na(PATHS_l[[aa]][xx,])) # penalty on path length
        }
        
        #if(PATHS_l[[aa]][xx,k+1]==0){
        #    EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,zz]]  = EDGES_l[[aa]][PATHS_l[[aa]][xx,yy],PATHS_l[[aa]][xx,zz]] - 1
        #}

     }
    #} 
  }
}

EDGES_all = Reduce("+", EDGES_l)

pos_ids <- which(target==1)
neg_ids <- which(target==0)

EDGES_pos = EDGES_l[pos_ids]
EDGES_pos = Reduce("+", EDGES_pos)

EDGES_neg = EDGES_l[neg_ids]
EDGES_neg = Reduce("+", EDGES_neg)

EDGES_all=EDGES_all + 1
EDGES_pos=EDGES_pos + 1
EDGES_neg=EDGES_neg + 1


print("Model Accuracy")
print(ModelMetrics::auc(pred, target))

print("Positive & Negative Examples")
#w = diag(1,dim(data)[2],dim(data)[2])*EDGES_all +1
#EDGES_all=EDGES_all+w
print(EDGES_all)
print("Positive Examples")
print(EDGES_pos)
print("Negative Examples")
print(EDGES_neg)

#print(treeInfo(model, tree = 1))

# Importances #########################
#######################################

#All
IMP_all <- rep(NaN, dim(data)[2])
for (xx in 1:length(IMP_all)){
    IMP_all[xx] = sum(EDGES_all[,xx])/sum(EDGES_all[,-xx])
}

print("Positive & Negative Examples")
print(IMP_all)

#Pos
IMP_pos <- rep(NaN, dim(data)[2])
for (xx in 1:length(IMP_pos)){
    IMP_pos[xx] = sum(EDGES_pos[,xx])/sum(EDGES_pos[,-xx])
}
print("Positive Examples")
print(IMP_pos)

#Neg
IMP_neg <- rep(NaN, dim(data)[2])
for (xx in 1:length(IMP_neg)){
    IMP_neg[xx] = sum(EDGES_neg[,xx])/sum(EDGES_neg[,-xx])
}
print("Negative Examples")
print(IMP_neg)

} # End of simulation