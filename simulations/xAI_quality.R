# quality of explanations
library(ranger)
library(lime)
library(cpath)
library(mlbench)
library(shapr)

source("~/GitHub/cpath/xAImetrics/infidelity.R")
source("~/GitHub/cpath/xAImetrics/xai_sensitivity.R")

#### DATA ######

# Breast Cancer
data(BreastCancer)

na.ids = which(apply(BreastCancer,1,function(x){any(is.na(x))}))
BreastCancer = BreastCancer[-na.ids,]
data   = BreastCancer[,2:10]
NN = colnames(data)
data = matrix(as.numeric(unlist(data)), dim(data)[1], dim(data)[2])
#data = apply(data,2,function(x){ (x - mean(x)) / sd(x)})
colnames(data) = NN
data = as.data.frame(data)

target = BreastCancer[,11]
target = factor(target)#, levels=c("setosa", "versicolor"))

# Ionosphere 
data(Ionosphere)

na.ids = which(apply(Ionosphere,1,function(x){any(is.na(x))}))
#Ionosphere = Ionosphere[-na.ids,]
data   = Ionosphere[,3:10]
NN = colnames(data)
data = matrix(as.numeric(unlist(data)), dim(data)[1], dim(data)[2])
#data = apply(data,2,function(x){ (x - mean(x)) / sd(x)})
colnames(data) = NN
data = as.data.frame(data)

target = Ionosphere[,35]
target = factor(target)#, levels=c("setosa", "versicolor"))


# PimaIndiansDiabetes
data(PimaIndiansDiabetes)

na.ids = which(apply(PimaIndiansDiabetes,1,function(x){any(is.na(x))}))
#Ionosphere = Ionosphere[-na.ids,]
data   = PimaIndiansDiabetes[,1:8]
NN = colnames(data)
data = matrix(as.numeric(unlist(data)), dim(data)[1], dim(data)[2])
#data = apply(data,2,function(x){ (x - mean(x)) / sd(x)})
colnames(data) = NN
data = as.data.frame(data)

target = PimaIndiansDiabetes[,9]
target = factor(target)#, levels=c("setosa", "versicolor"))

# Sonar
data(Sonar)

na.ids = which(apply(Sonar,1,function(x){any(is.na(x))}))
#Ionosphere = Ionosphere[-na.ids,]
data   = Sonar[,1:8]
NN = colnames(data)
data = matrix(as.numeric(unlist(data)), dim(data)[1], dim(data)[2])
#data = apply(data,2,function(x){ (x - mean(x)) / sd(x)})
colnames(data) = NN
data = as.data.frame(data)

target = Sonar[,61]
target = factor(target)#, levels=c("setosa", "versicolor"))

############################################
## CURRENT DATA
############################################

# Sonar
data(Sonar)

na.ids = which(apply(Sonar,1,function(x){any(is.na(x))}))
#Ionosphere = Ionosphere[-na.ids,]
data   = Sonar[,1:8]
NN = colnames(data)
data = matrix(as.numeric(unlist(data)), dim(data)[1], dim(data)[2])
#data = apply(data,2,function(x){ (x - mean(x)) / sd(x)})
colnames(data) = NN
data = as.data.frame(data)

target = Sonar[,61]
target = factor(target)#, levels=c("setosa", "versicolor"))
#####
######################################################

# train-test split 

n_iter=20

INFIDELITY_cpath = rep(NaN, n_iter)
INFIDELITY_shap  = rep(NaN, n_iter)
INFIDELITY_lime  = rep(NaN, n_iter)

SENSITIVITY_cpath = rep(NaN, n_iter)
SENSITIVITY_shap  = rep(NaN, n_iter)
SENSITIVITY_lime  = rep(NaN, n_iter)

for(xx in 1:n_iter){

    cat(xx, " of ", n_iter, "\n")

    ## 80% of the sample size
    smp_size <- floor(0.80 * nrow(data))

    train_ind <- sample(seq_len(nrow(data)), size = smp_size)

    train <- data[train_ind, ]
    test  <- data[-train_ind, ]

    target_train = target[train_ind]
    target_test  = target[-train_ind]

    # Train a random forest classifier
    model = ranger(x=train,y=target_train, 
                num.trees=100, 
                classification=TRUE, 
                probability=TRUE, 
                importance='impurity')
    pred = predict(model, test)$predictions
    pred = apply(pred,1,function(x){which.max(x)-1})


    ## CPATH ##############################
    print("CPATH")
    # Get the counterfactual paths
    P   = cpath::cpaths(model, test, k=4, n_paths= 1000)

    # Build transition matrix 
    tran_matrix   = cpath::transition(P)

    # Get global feature importances
    IMP = cpath::importance(tran_matrix)
    IMP = IMP/sum(IMP)


    ## SHAP ###############################
    print("SHAP")
    explainer  <- shapr(as.data.frame(train), model, n_combinations = 1000)

    explanation <- shapr::explain(
    as.data.frame(test),
    approach = "empirical",
    explainer = explainer,
    prediction_zero = mean(as.numeric(target_train)-1)
    )

    IMP_shap = apply(abs(explanation$dt), 2, mean)[-1]
    rm(.Random.seed, envir=globalenv())

    ### LIME #############################
    print("LIME")
    explainer <- lime(as.data.frame(train), model, bin_continuous = TRUE, quantile_bins = FALSE)
    explanation <- lime::explain(test, explainer, n_labels = 2, n_features = dim(test)[2])
    feat_weights = explanation$feature_weight
    feat_weights = matrix(abs(feat_weights), dim(test)[1], dim(test)[2], byrow=TRUE)
    feat_imp = apply(feat_weights,2,mean)
    IMP_lime = feat_imp

    # INFIDELITY
    sigma = 1#
    INFIDELITY_shap[xx] = infidelity_computation(model, test, IMP_shap, sigma)
    INFIDELITY_lime[xx] = infidelity_computation(model, test, IMP_lime, sigma)
    INFIDELITY_cpath[xx] = infidelity_computation(model, test, IMP, sigma)

    # SENSITIVITY   
    SENSITIVITY_shap[xx] = sensitivity_n_computation(model, test, as.numeric(target_test), IMP_shap, dim(test)[2])
    SENSITIVITY_lime[xx] = sensitivity_n_computation(model, test, as.numeric(target_test), IMP_lime, dim(test)[2])
    SENSITIVITY_cpath[xx] = sensitivity_n_computation(model, test, as.numeric(target_test), IMP, dim(test)[2])


RES_inf = cbind(INFIDELITY_shap, INFIDELITY_lime, INFIDELITY_cpath)
colnames(RES_inf) = c("SHAP","LIME","CPATH")

print("INFIDELITY")
print(RES_inf)
write.table(RES_inf, file=paste("INF_",as.character(sigma),sep=""))

RES_sens = cbind(SENSITIVITY_shap, SENSITIVITY_lime, SENSITIVITY_cpath)
colnames(RES_sens) = c("SHAP","LIME","CPATH")
print("SENSITIVITY")
print(RES_sens)
write.table(RES_sens, file="SENS")

}
# source("~/GitHub/cpath/simulations/xAI_quality.R")