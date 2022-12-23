# main
CFpath <- function(model, test_set, k){

# Get the labels 
pred   = predict(model, test_set)
labels = pred$predictions
labels = apply(labels,1,function(x){which.max(x)-1})

test_setX    <- test_set 
cf_path      <- rep(NaN, k)
label_switch <- FALSE

# randomly select a feature
f_start = sample(1:dim(test_setX)[2], k, replace=FALSE)
cf_path = f_start # randomly select a feature
    
label_switch_all = rep(FALSE, length(target))
#IGNORE =  rep(FALSE, k) 

for (xx in 1:k){

    ##########################################
    # perm
    # check also the next - if better JUMP!
    # test current
    #test_setX1 = test_setX
    #f_perm  = sample(test_setX1[,f_start[xx]], dim(test_setX1)[1]) 
    #test_setX1[,f_start[xx]] = f_perm
    #pred = predict(model, test_setX1)
    #labels_perm  = pred$predictions
    #labels_perm1  = apply(labels_perm,1,function(x){which.max(x)-1})
    #frac1 = sum(labels!=labels_perm1)/length(labels)
    # test next
    #test_setX2 = test_setX
    #f_perm  = sample(test_setX2[,f_start[xx+1]], dim(test_setX2)[1]) 
    #test_setX2[,f_start[xx+1]] = f_perm
    #pred = predict(model, test_setX2)
    #labels_perm  = pred$predictions
    #labels_perm2  = apply(labels_perm,1,function(x){which.max(x)-1})
    #frac2 = sum(labels!=labels_perm2)/length(labels)
    
    #if(frac2>frac1){
    #    labels_perm = labels_perm2
    #    test_setX = test_setX2
    #    IGNORE[xx] = TRUE
    #    xx = xx + 1 # JUMP
    #}else{
    #    labels_perm = labels_perm1
    #    test_setX = test_setX1
    #}
    # test_setX1 = test_setX
    ###################################################

    f_perm  = sample(test_set[,f_start[xx]], dim(test_set)[1]) 
    test_setX[,f_start[xx]] = f_perm
    pred = predict(model, test_setX)
    labels_perm  = pred$predictions
    labels_perm  = apply(labels_perm,1,function(x){which.max(x)-1})

    if(!all(labels_perm==0)){
        # predict and check whether label changes
        p    = sum(labels!=labels_perm)/length(labels)
        stop = sample(c(0,1),1,prob=c(1-p,p))
        if(stop){
            label_switch_all = (labels!=labels_perm)
            label_switch = TRUE
            #if(any(IGNORE)){
             #cf_path[IGNORE] = NA
             #cf_path = cf_path[1:xx]
             #cf_path = cf_path[!is.na(cf_path)]
            #}else{
             cf_path = cf_path[1:xx]   
            #}
            break
        }  
    }else{cf_path = rep(NaN, k); break}
}

return(list(orig_path=f_start, cf_path=cf_path, label_switch=label_switch, 
label_switch_all=label_switch_all))

}