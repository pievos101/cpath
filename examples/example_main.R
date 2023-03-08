# main 
library(ranger)
library(cpath)

res  = sim5()
data = res$data
target = res$target

# train-test split 

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
pred = predict(model, data)$predictions
pred = apply(pred,1,function(x){which.max(x)-1})

# Get the counterfactual paths
P   = cpath::cpaths(model, test, target_test, k=4, n.iter= 1000)

# Build transition matrix 
T   = cpath::transition(P, test, target_test)
T2  = cpath::transition(P, test, target_test, add1=TRUE)

# Get global feature importances
IMP = cpath::importance(T)
print(IMP)

IMP_st = cpath::importance(T, agg_type="stationary_distribution")
print(IMP_st)


IMP2 = cpath::importance(T2)
print(IMP2)

IMP2_st = cpath::importance(T2, agg_type="stationary_distribution")
print(IMP2_st)

