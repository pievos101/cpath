# main 
library(ranger)
library(lime)
library(cpath)
library(shapr)

n.sim=30
RES = matrix(NaN, n.sim, 3)

colnames(RES) = c("spearman","pearson","kendall")

# SIM
for(ii in 1:n.sim){

cat(ii, " of ", n.sim, "\n")

#res  = sim5()# conditional dependency (2)
#res  = sim()# conditional dependency (1)
#res  = sim3()# correlation
res  = sim4()# conditional in-dependency



data = res$data
target = res$target

while(TRUE){

    model = ranger(x=data,y=as.factor(target), 
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
P   = cpath::cpaths_mc(model, data, k=4, n_paths= 1000)

# Build transition matrix 
T   = cpath::transition(P)

# Get global feature importances
IMP = cpath::importance(T)

## CPATH_min 
# Get the counterfactual paths
P   = cpath::cpaths_mc(model, data, k=4, n_paths= 1000, nearest=TRUE)

# Build transition matrix 
T   = cpath::transition(P)

# Get global feature importances
IMP2 = cpath::importance(T)

#print("CPATH values")
#print(IMP)

cor1_cpath = cor(IMP, IMP2, method="spearman")
cor2_cpath = cor(IMP, IMP2, method="pearson")
cor3_cpath = cor(IMP, IMP2, method="kendall")


#print("Correlation cpath RL")
#cor_cpath_Q = cor(vimp[ids], cp_q_imp[ids], method="spearman")
#print(cor_cpath_RL)


RES[ii,1] = cor1_cpath
RES[ii,2] = cor2_cpath
RES[ii,3] = cor3_cpath

print(RES)

} # End of simulation
