# main
CFpath <- function(model, test_set, k){

# Get the labels 
pred   = predict(model, test_set)
labels = pred$predictions
#labels = pred
labels = apply(labels,1,function(x){which.max(x)-1})

test_setX    <- test_set 
cf_path      <- rep(NaN, k)
label_switch <- FALSE

# randomly select a feature
f_start = sample(1:dim(test_setX)[2], k, replace=TRUE)
cf_path = f_start # randomly select a feature
    
label_switch_all = rep(FALSE, length(target))

for (xx in 1:k){

    f_perm  = sample(test_set[,f_start[xx]], dim(test_set)[1]) 
    test_setX[,f_start[xx]] = f_perm
    pred = predict(model, test_setX)
    #labels_perm = pred
    labels_perm  = pred$predictions
    labels_perm  = apply(labels_perm,1,function(x){which.max(x)-1})

    if(!all(labels_perm==0)){
        # predict and check whether label changes
        p    = sum(labels!=labels_perm)/length(labels)
        stop = sample(c(0,1),1,prob=c(1-p,p))
        if(stop){
            label_switch_all = (labels!=labels_perm)
            label_switch = TRUE
            cf_path = cf_path[1:xx]   
            break
        }  
    }else{cf_path = rep(NaN, k); break}
}

return(list(orig_path=f_start, cf_path=cf_path, label_switch=label_switch, 
label_switch_all=label_switch_all))

}