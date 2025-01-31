# cpath

## Explaining black-box models through counterfactual paths and conditional permutations

<p align="center">
<img src="https://github.com/pievos101/cpath/blob/main/logo.png" width="400">
</p>


## Installation
The cpath R-package can be installed using devtools.

```{r}
install.packages("devtools")
library(devtools)

devtools::install_github("pievos101/cpath")
library(cpath)

# ranger package is required 
install.packages("ranger")
library(ranger)
```

## Usage

```{r}
library(ranger)
library(cpath)

# Generate simulated data
res  = sim()
data = res$data
target = as.factor(res$target)

# Train-test split 

## 80% of the sample size
smp_size <- floor(0.80 * nrow(data))

train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test  <- data[-train_ind, ]

target_train = target[train_ind]
target_test  = target[-train_ind]

# Train a random forest classifier
model = ranger(x=train,y=as.factor(target_train), 
            num.trees=100, 
            classification=TRUE, 
            probability=TRUE, 
            importance='impurity')

# Predictions on test data
pred = predict(model, test)$predictions
pred = apply(pred,1,function(x){which.max(x)-1})

# Get the counterfactual paths
P   = cpath::cpaths(model, test, k=4, n_paths = 1000)
# P   = cpath::cpaths_mc(model, test, k=4, n_paths = 1000) #multi-core

# Build transition matrix 
T   = cpath::transition(P)

# Get global feature importances
IMP = cpath::importance(T)
print(IMP)

# Get summary of the counterfactual paths
cpath_summary = get_cpath_summary(P)

# Plot the paths
plot_paths(cpath_summary)

####################################
# The RL Q-Learning solution
####################################
cp_q <- cpaths_rl(model, test, k=4)
print(cp_q$importance)

```

## CITATION
If you find cpath useful please cite 

```
@misc{pfeifer2023explainable,
      title={Explainable AI with counterfactual paths}, 
      author={Bastian Pfeifer and Mateusz Krzyzinski and Hubert Baniecki and Anna Saranti and Andreas Holzinger and Przemyslaw Biecek},
      year={2023},
      eprint={2307.07764},
      archivePrefix={arXiv},
      primaryClass={cs.AI}
}
```
