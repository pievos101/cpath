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
target = res$target

# Train-test split 

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

# Predictions on test data
pred = predict(model, test)$predictions
pred = apply(pred,1,function(x){which.max(x)-1})

# Get the counterfactual paths
P   = cpath::cpaths(model, test, k=4, n_paths = 1000)

# Build transition matrix 
T   = cpath::transition(P)

# Get global feature importances
IMP = cpath::importance(T)
print(IMP)

# The RL Q-Learning alternative
cp_q <- cpaths_rl(model, test, k=4)
print(cp_q$importance)
```

## CITATION
If you find cpath useful please cite 

```
@software{cpath2023,
  author = {Bastian, Pfeifer and Mateusz, Krzyzinski and Hubert, Baniecki and Anna, Saranti and Andreas, Holzinger and Przemyslaw, Biecek},
  doi = {},
  month = {},
  title = {{Explaining black-box models through counterfactual paths and conditional permutations}},
  url = {https://github.com/pievos101/cpath},
  version = {1.2},
  year = {2023}
}
```

A scientific paper is in progress and will be available soon.