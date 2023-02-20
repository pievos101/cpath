# main 
library(ranger)
library(cpath)

res  = sim()
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

# Get global feature importances
IMP = cpath::importance(T)

print(IMP)

cp_mc <- cpaths_monte_carlo(model, test, k=4)
print(cp_mc$importance/sum(cp_mc$importance))

cp_q <- cpaths_qlearning(model, test, k=4)
print(cp_q$importance)

cp_td <- cpaths_tdlearning(model, test, k=4)
print(cp_td$importance)

print(model$variable.importance/sum(model$variable.importance)) 



