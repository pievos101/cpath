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
P   = cpath::cpaths(model, test, k=4, n_paths= 1000)

cpath_summary <- cpath::get_cpath_summary(P)
cpath::plot_paths(cpath_summary)


# Build transition matrix 
tran_matrix   = cpath::transition(P)

# Get global feature importances
IMP = cpath::importance(tran_matrix)
print(IMP)

IMP_st = cpath::importance(tran_matrix, agg_type="stationary_distribution")
print(IMP_st)

