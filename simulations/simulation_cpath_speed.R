# main 
library(ranger)
library(lime)
library(cpath)
library(shapr)

n.sim=50
feats = c(10, 50, 100, 1000)
RES = matrix(NaN, n.sim, length(feats))
colnames(RES) = feats
RES_speed = RES


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

speed = system.time({
## CPATH_min 
# Get the counterfactual paths
P   = cpath::cpaths_mc(model, data, k=ncol(data), n_paths= 10000, nearest=FALSE)
if(sum(P$counterfactuality)<=2){
    RES_speed[ii,xx] = speed[3]
    RES[ii,xx] = 0
    print(RES)
    print(RES_speed)
    next
}

# Build transition matrix 
T   = cpath::transition(P)

# Get global feature importances
IMP2 = cpath::importance(T)
})

#print(speed)
RES_speed[ii,xx] = speed[3]

# Get coverage
names(IMP2) = 1:length(IMP2)
IMP2_sorted = sort(IMP2, decreasing=TRUE)

cov_cpath_min = sum(is.element(names(IMP2_sorted)[1:2],1:2))/2


RES[ii,xx] = cov_cpath_min

print(RES)
print(RES_speed)

} # End of simulation
}