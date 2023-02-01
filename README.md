# cpath

<p align="center">
<img src="https://github.com/pievos101/cpath/blob/main/logo.png" width="400">
</p>


## Usage

```{r}
library(ranger)
library(cpath)

res  = sim()
data = res$data
target = res$target

model = ranger(x=data,y=target, 
            num.trees=100, 
            classification=TRUE, 
            probability=TRUE, 
            importance='impurity')
pred = predict(model, data)$predictions
pred = apply(pred,1,function(x){which.max(x)-1})

# Get the counterfactual paths
P   = cpath::cpaths(model, data, target, k=4, n.iter= 1000)

# Build transition matrix 
T   = cpath::transition(P, data, target, k=4)

# Get global feature importances
IMP = cpath::importance(T)

print(IMP)
```