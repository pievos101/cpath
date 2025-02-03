# main 
library(ranger)
library(lime)
library(cpath)
library(shapr)

n.sim=50
feats = c(4, 6, 8, 10, 12)
RES = matrix(NaN, n.sim, length(feats))
colnames(RES) = feats



for(xx in 1:length(feats)){
for(ii in 1:n.sim){

cat(ii, " of ", n.sim, "\n")

#res  = sim5(feats[xx])# conditional dependency (2)
#res  = sim(feats[xx])# conditional dependency (1)
#res  = sim3(feats[xx])# correlation
res  = sim4(feats[xx])# conditional in-dependency



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

## CPATH_min 
# Get the counterfactual paths
P   = cpath::cpaths_mc(model, data, k=4, n_paths= 1000, nearest=TRUE)

# Build transition matrix 
T   = cpath::transition(P)

# Get global feature importances
IMP2 = cpath::importance(T)

# Get coverage
names(IMP2) = 1:length(IMP2)
IMP2_sorted = sort(IMP2, decreasing=TRUE)

cov_cpath_min = sum(is.element(names(IMP2_sorted)[1:2],1:2))/2


RES[ii,xx] = cov_cpath_min

print(RES)

} # End of simulation
}