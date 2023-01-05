# main 
library(ranger)
library(lime)

#data(iris)
#iris2 = iris[1:100,]
#data  = iris2[,1:4]
#target = iris2[1:100,5]

#data(BreastCancer)
#data = BreastCancer[,1:10]
#target = BreastCancer[,11]
#ids = which(apply(data, 1, function(x){any(is.na(x))}))
#data = data[-ids,]
#target = target[-ids]


#data(Glass)
#data = Glass[1:146,1:9]
#target = Glass[1:146,10]

source("~/GitHub/c-path/CFpath.r")
source("~/GitHub/c-path/sim.r")


n.sim=100
COV_cpath    = rep(NaN, n.sim)
COV_shap     = rep(NaN, n.sim)
COV_between  = rep(NaN, n.sim)
COV_lime     = rep(NaN, n.sim)
COV_cpi      = rep(NaN, n.sim)
COV_gini     = rep(NaN, n.sim)

# SIM
for(ii in 1:n.sim){

cat(ii, " of ", n.sim, "\n")

res  = sim()
data = res$data
target = res$target

while(TRUE){

    model = ranger(x=data,y=target, 
                num.trees=100, 
                classification=TRUE, 
                probability=TRUE, 
                #mtry=4, 
                #replace=TRUE,#), 
                importance='impurity')
    pred = predict(model, data)$predictions
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
    IMP_all[xx] = sum(EDGES_all[,xx])/sum(EDGES_all)#sum(EDGES_all[,-xx])
}

print("Positive & Negative Examples")
print(IMP_all)

#Pos
IMP_pos <- rep(NaN, dim(data)[2])
for (xx in 1:length(IMP_pos)){
    IMP_pos[xx] = sum(EDGES_pos[,xx])/sum(EDGES_pos)#sum(EDGES_pos[,-xx])
}
print("Positive Examples")
print(IMP_pos)

#Neg
IMP_neg <- rep(NaN, dim(data)[2])
for (xx in 1:length(IMP_neg)){
    IMP_neg[xx] = sum(EDGES_neg[,xx])/sum(EDGES_neg)#sum(EDGES_neg[,-xx])
}
print("Negative Examples")
print(IMP_neg)

print("")
print("########################################################")
print("########################################################")
print("########################################################")
print("")

## SHAP
library(shapr)
data = as.data.frame(data)
explainer <- shapr(data, model)

explanation <- shapr::explain(
  data,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = mean(target)
)

IMP_shap = apply(abs(explanation$dt),2,sum)[-1]

rm(.Random.seed, envir=globalenv())

#plot(explanation, plot_phi0 = FALSE, index_x_test = c(1, 6))

# LIME (n_labels??)
explainer <- lime(as.data.frame(data), model, bin_continuous = TRUE, quantile_bins = FALSE)
explanation <- lime::explain(data, explainer, n_labels = 2, n_features = dim(data)[2])
feat_weights = explanation$feature_weight
feat_weights = matrix(abs(feat_weights), dim(data)[1], dim(data)[2], byrow=TRUE)
feat_imp = apply(feat_weights,2, sum)
IMP_lime = feat_imp

# CPI #############################
library(cpi)
library(mlr3)
library(mlr3learners)

data2 = cbind(data, target)
data2$target <- as.factor(data2$target)
IN = as_task_classif(target~., data=data2)
#model needs to be trained again
ccc = cpi(task = IN, 
    learner = lrn("classif.ranger", predict_type = "prob", num.trees = 100),
    resampling = rsmp("cv", folds = 5), 
    test_data = data2,
    measure = "classif.logloss", test = "t")

IMP_cpi = ccc$CPI
####################################

# GINI importance
vimp = model$variable.importance

# Normalize -------------- #
#IMP_shap = IMP_shap/max(IMP_shap)
#IMP_lime = IMP_lime/max(IMP_lime)
#IMP_all  = IMP_all/max(IMP_all)
# ------------------------ #

print("GINI importance")
print(vimp)

print("LIME importance")
names(IMP_lime) = colnames(data)
print(IMP_lime) 

print("SHAPE importance")
print(IMP_shap)

print("CPATH importance")
names(IMP_all) = colnames(data)
print(IMP_all)

print("CPI importance")
names(IMP_cpi) = colnames(data)
print(IMP_cpi)

# Coverage
truth = c("V1","V2")
gini = sum(is.element(names(sort(vimp, decreasing=TRUE))[1:2], truth))/length(truth)
cpath = sum(is.element(names(sort(IMP_all, decreasing=TRUE))[1:2], truth))/length(truth)
shap = sum(is.element(names(sort(IMP_shap, decreasing=TRUE))[1:2], truth))/length(truth)
lime = sum(is.element(names(sort(IMP_lime, decreasing=TRUE))[1:2], truth))/length(truth)
cpi = sum(is.element(names(sort(IMP_cpi, decreasing=TRUE))[1:2], truth))/length(truth)


COV_gini[ii]  = gini  
COV_cpath[ii] = cpath
COV_shap[ii]  = shap
COV_lime[ii]  = lime
COV_cpi[ii]   = cpi


RES = cbind(COV_gini, COV_cpath, COV_shap, COV_lime, COV_cpi)
colnames(RES) = c("GINI","CPATH", "SHAP","LIME", "CPI")
print(RES)

} # End of simulation
