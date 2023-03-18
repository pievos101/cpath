# main 
library(ranger)
library(lime)
library(cpath)

n.sim=50
COR_cpath     = rep(NaN, n.sim)
COR_shap      = rep(NaN, n.sim)
COR_between   = rep(NaN, n.sim)
COR_lime      = rep(NaN, n.sim)
COR_cpath_RL  = rep(NaN, n.sim)

# SIM
for(ii in 1:n.sim){

cat(ii, " of ", n.sim, "\n")

res  = sim5()
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

print("Model Accuracy")
print(ModelMetrics::auc(pred, target))

# Importances #########################
#######################################

## CPATH
# Get the counterfactual paths
P   = cpath::cpaths(model, data, k=4, n_paths= 1000)

# Build transition matrix 
T   = cpath::transition(P)

# Get global feature importances
IMP = cpath::importance(T)
#print("CPATH values")
#print(IMP)

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
#print("SHAP values")
#print(IMP_shap)

rm(.Random.seed, envir=globalenv())

# LIME (n_labels??)
explainer <- lime(as.data.frame(data), model, bin_continuous = TRUE, quantile_bins = FALSE)
explanation <- lime::explain(data, explainer, n_labels = 2, n_features = dim(data)[2])
feat_weights = explanation$feature_weight
feat_weights = matrix(abs(feat_weights), dim(data)[1], dim(data)[2], byrow=TRUE)
feat_imp = apply(feat_weights,2, sum)
IMP_lime = feat_imp
#print("LIME values")
#print(IMP_lime)

# CPATH RL ###########################
cp_q = cpaths_qlearning(model, data, k=4)

cp_q_imp = cp_q$importance
print(cp_q$trans_matrix)
print("CPATH RL values")
print(cp_q_imp)

# GINI importance
vimp = model$variable.importance
ids = vimp!=0

print("GINI importance")
print(vimp[ids])

#print("Correlation SHAP")
cor_shap = cor(vimp[ids], IMP_shap[ids], method="spearman")
#print(cor_shap)
#print("Correlation LIME")
cor_lime = cor(vimp[ids], IMP_lime[ids], method="spearman")
#print(cor_lime)
#print("Correlation cpath")
cor_cpath = cor(vimp[ids], IMP[ids], method="spearman")
#print(cor_cpath)
#print("Correlation cpath RL")
cor_cpath_RL = cor(vimp[ids], cp_q_imp[ids], method="spearman")
#print(cor_cpath_RL)


COR_cpath[ii] = cor_cpath
COR_shap[ii] = cor_shap
COR_lime[ii] = cor_lime
COR_cpath_RL[ii]  = cor_cpath_RL


RES = cbind(COR_cpath, COR_shap, COR_lime, COR_cpath_RL)
colnames(RES) = c("CPATH", "SHAP","LIME", "CPATH_RL")
print(RES)

} # End of simulation
