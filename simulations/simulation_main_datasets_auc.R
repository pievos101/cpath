# main 
library(ranger)
library(lime)
library(cpath)
library(shapr)
library(ranger)
library(lime)
library(cpath)
library(mlbench)
library(shapr)
library(fastshap)
library(pROC)

n.sim=30
AUC_cpath     = rep(NaN, n.sim)
AUC_cpath_min     = rep(NaN, n.sim)
AUC_shap      = rep(NaN, n.sim)
AUC_between   = rep(NaN, n.sim)
AUC_lime      = rep(NaN, n.sim)
AUC_cpath_Q   = rep(NaN, n.sim)
AUC_cpath_RL  = rep(NaN, n.sim)

### DATASET

# Iris
data(iris)

na.ids = which(apply(iris,1,function(x){any(is.na(x))}))
#iris = iris[-na.ids,]
data   = iris[1:100,1:4]
NN = colnames(data)
data = matrix(as.numeric(unlist(data)), dim(data)[1], dim(data)[2])
#data = apply(data,2,function(x){ (x - mean(x)) / sd(x)})
colnames(data) = NN
data = as.data.frame(data)

target = iris[1:100,5]
target = factor(target, levels=c("setosa", "versicolor"))


CPATH = TRUE
CPATH_min = TRUE
LIME = TRUE
SHAP = FALSE
fastSHAP = TRUE

# SIM
for(ii in 1:n.sim){

cat(ii, " of ", n.sim, "\n")


## 80% of the sample size
smp_size <- floor(0.80 * nrow(data))

train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test  <- data[-train_ind, ]

target_train = target[train_ind]
target_test  = target[-train_ind]


while(TRUE){

    model = ranger(x=train,y=as.factor(target_train), 
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

print("Model Accuracy")
print(ModelMetrics::auc(pred, target))

# Importances #########################
#######################################

# GINI importance (GROUND TRUTH)
vimp = model$variable.importance
ids = vimp!=0


if(CPATH){
## CPATH
# Get the counterfactual paths
P   = cpath::cpaths_mc(model, data, k=4, n_paths= 1000)

# Build transition matrix 
T   = cpath::transition(P)

# Get global feature importances
IMP = cpath::importance(T)
#print("CPATH values")
#print(IMP)
}


if(fastSHAP){
## FAST SHAP
rfo = model
pfun = function(object, newdata){
    pred = predict(object, data=newdata)$predictions
    apply(pred,1,function(x){which.max(x)-1})
}

ex.t1 = fastshap::explain(rfo, X=as.matrix(data), 
        pred_wrapper=pfun, adjust=TRUE, nsim=1000, parallel=FALSE)
#
IMP_shap = colMeans(abs(ex.t1))
}


if(CPATH_min){
## CPATH_min
# Get the counterfactual paths
P   = cpath::cpaths_mc(model, data, k=4, n_paths= 1000, nearest=TRUE)

# Build transition matrix 
T   = cpath::transition(P)

# Get global feature importances
IMP2 = cpath::importance(T)
#print("CPATH values")
#print(IMP)
}

if(SHAP){
## SHAP
library(shapr)
data = as.data.frame(data)
explainer  <- shapr(as.data.frame(data), model, n_combinations = 1000)

explanation <- shapr::explain(
  as.data.frame(data),
  approach = "empirical",
  explainer = explainer,
  prediction_zero = mean(as.numeric(target)-1)
)

IMP_shap = apply(abs(explanation$dt),2,sum)[-1]
#print("SHAP values")
#print(IMP_shap)
}

rm(.Random.seed, envir=globalenv())

if(LIME){
# LIME (n_labels??)
explainer <- lime(as.data.frame(data), model, bin_continuous = TRUE, quantile_bins = FALSE)
explanation <- lime::explain(data, explainer, n_labels = 2, n_features = dim(data)[2])
feat_weights = explanation$feature_weight
feat_weights = matrix(abs(feat_weights), dim(data)[1], dim(data)[2], byrow=TRUE)
feat_imp = apply(feat_weights,2, sum)
IMP_lime = feat_imp
#print("LIME values")
#print(IMP_lime)
}

### NOW TRAIN WITH IMP WEIGHTS

# custom function to implement min max scaling
minMax <- function(x) {
  (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
}

if(LIME){
model = ranger(x=train,y=as.factor(target_train), 
                num.trees=100, 
                classification=TRUE, 
                probability=TRUE, 
                #mtry=4, 
                #replace=TRUE,#), 
                importance='impurity',
                split.select.weights=minMax(IMP_lime))
pred = predict(model, test)$predictions
perf = pROC::auc(target_test, pred[,2])
AUC_lime[ii] = perf
}

if(fastSHAP){
model = ranger(x=train,y=as.factor(target_train), 
                num.trees=100, 
                classification=TRUE, 
                probability=TRUE, 
                #mtry=4, 
                #replace=TRUE,#), 
                importance='impurity',
                split.select.weights=minMax(IMP_shap))
pred = predict(model, test)$predictions
perf = pROC::auc(target_test, pred[,2])
AUC_shap[ii] = perf
}

if(CPATH){
model = ranger(x=train,y=as.factor(target_train), 
                num.trees=100, 
                classification=TRUE, 
                probability=TRUE, 
                #mtry=4, 
                #replace=TRUE,#), 
                importance='impurity',
                split.select.weights=minMax(IMP))
pred = predict(model, test)$predictions
perf = pROC::auc(target_test, pred[,2])
AUC_cpath[ii] = perf
}

if(CPATH_min){
model = ranger(x=train,y=as.factor(target_train), 
                num.trees=100, 
                classification=TRUE, 
                probability=TRUE, 
                #mtry=4, 
                #replace=TRUE,#), 
                importance='impurity',
                split.select.weights=minMax(IMP2))
pred = predict(model, test)$predictions
perf = pROC::auc(target_test, pred[,2])
AUC_cpath_min[ii] = perf
}



RES = cbind(AUC_shap, AUC_lime, AUC_cpath, AUC_cpath_min)
colnames(RES) = c("SHAP_fast", "LIME", "CPATH", "CPATH_min")
print(RES)

} # End of simulation
