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
library(treeshap)
library(fastshap)

n.sim=30
COR_cpath     = rep(NaN, n.sim)
COR_cpath_min     = rep(NaN, n.sim)
COR_shap      = rep(NaN, n.sim)
COR_between   = rep(NaN, n.sim)
COR_lime      = rep(NaN, n.sim)
COR_cpath_Q   = rep(NaN, n.sim)
COR_cpath_RL  = rep(NaN, n.sim)

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



## ----- ##
colnames(data) = paste("V",1:ncol(data),sep="")
NN = colnames(data)

data2 = cbind(data, as.numeric(target))
colnames(data2) = c(colnames(data),"target")
target = as.numeric(target)

CPATH = TRUE
CPATH_min = FALSE
LIME = FALSE
SHAP = FALSE
fastSHAP = FALSE

# SIM
for(ii in 1:n.sim){

cat(ii, " of ", n.sim, "\n")

while(TRUE){

    #model = ranger(x=data,y=as.factor(target), 
    model = ranger(target~., data=as.data.frame(data),
                num.trees=100, 
                classification=TRUE, 
                #probability=TRUE, 
                #mtry=4, 
                #replace=TRUE,#), 
                importance='impurity')
    pred = predict(model, data)$predictions
    #pred = apply(pred,1,function(x){which.max(x)-1})
    #pred = t(pred)
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
print(ModelMetrics::auc(as.factor(pred), as.factor(target)))
#pred = pred
#target = as.factor(target)


# Importances #########################
#######################################


# GET TREESHAP importance as GROUND TRUTH
#print(is(model))
model_unified = unify(model, data)
#print(is(model_unified))
vimp = treeshap(model_unified, data)
#rm(model_unified)
#gc()
#print("done")
vimp = vimp[[1]]
vimp = colMeans(abs(vimp))

ids = vimp!=0


if(CPATH){
## CPATH
# Get the counterfactual paths
P   = cpath::cpaths_mc(model, data, k=ncol(data), n_paths= 10000)

# Build transition matrix 
T   = cpath::transition(P)

# Get global feature importances
IMP = cpath::importance(T)
#print("CPATH values")
#print(IMP)
cor_cpath = cor(vimp[ids], IMP[ids], method="spearman")
COR_cpath[ii] = cor_cpath
}

if(CPATH_min){
## CPATH_min
# Get the counterfactual paths
P   = cpath::cpaths_mc(model, data, k=ncol(data), n_paths= 10000, nearest=TRUE)

# Build transition matrix 
T   = cpath::transition(P)

# Get global feature importances
IMP2 = cpath::importance(T)
#print("CPATH values")
#print(IMP)
cor_cpath_min = cor(vimp[ids], IMP2[ids], method="spearman")
COR_cpath_min[ii] = cor_cpath_min
}

if(fastSHAP){
## FAST SHAP
rfo = model
pfun = function(object, newdata){
    predict(object, data=newdata)$predictions
}
ex.t1 = fastshap::explain(rfo, X=as.matrix(data), 
        pred_wrapper=pfun, adjust=TRUE, nsim=1000)

IMP_shap = colMeans(abs(ex.t1))
cor_shap = cor(vimp[ids], IMP_shap[ids], method="spearman")
COR_shap[ii] = cor_shap
}

## SHAP
#library(shapr)
#data = as.data.frame(data)
#explainer <- shapr(data, model)

#explanation <- shapr::explain(
#  data,
#  approach = "empirical",
#  explainer = explainer,
#  prediction_zero = mean(target)
#)

#IMP_shap = apply(abs(explanation$dt),2,sum)[-1]
#print("SHAP values")
#print(IMP_shap)

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
cor_lime = cor(vimp[ids], IMP_lime[ids], method="spearman")
COR_lime[ii] = cor_lime
}

# CPATH Q ###########################
#cp_q <- cpath_qlearning(model, data, k=4)
#cp_q_imp <- cp_q$importance

# CPATH RL ###########################
#cp_rl = cpath_rl(model, data, k=4)

#cp_rl_imp = cp_rl$importance
#print(cp_q$trans_matrix)
#print("CPATH RL values")
#print(cp_q_imp)



#print("GINI importance")
#print(vimp[ids])

#print("Correlation SHAP")

#print(cor_shap)
#print("Correlation LIME")

#print(cor_lime)
#print("Correlation cpath")

#print(cor_cpath)
#print("Correlation cpath RL")
#cor_cpath_Q = cor(vimp[ids], cp_q_imp[ids], method="spearman")
#print(cor_cpath_RL)
#cor_cpath_RL = cor(vimp[ids], cp_rl_imp[ids], method="spearman")
#print(cor_cpath_RL)


#COR_cpath_Q[ii]  = cor_cpath_Q
#COR_cpath_RL[ii]  = cor_cpath_RL

RES = cbind(COR_shap, COR_lime, COR_cpath, COR_cpath_min)
colnames(RES) = c("SHAP_fast","LIME", "CPATH", "CPATH_min")
print(RES)

} # End of simulation
