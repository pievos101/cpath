library(cpath)
library(ranger)
library(ggplot2)
library(dplyr)
library(DALEX)
library(GGally)
#library(MASS)

ITER <- 1000
STEPS <- 3

noise <- 0.01
n <- 1000
p <- 6
mu <- rep(0, p)
Sigma <- matrix(0, p, p)
diag(Sigma) <- 1
Sigma[1, 2] <- Sigma[2, 1] <- 3/4
Sigma[1, 3] <- Sigma[3, 1] <- 1/2
Sigma[2, 3] <- Sigma[3, 2] <- 1/2
set.seed(0)
df <- as.data.frame(MASS::mvrnorm(n, mu, Sigma))
df[,4] <- runif(n, -3*noise, 3*noise)
df$target <- as.numeric(
  df[,1] * (df[,6] > 0) + 
      df[,4] + rnorm(n, 0, noise) > 0
)

ggpairs(df, columns = 1:6, aes(color = as.factor(target), alpha = 0.5)) 

set.seed(0)
id <- sample(nrow(df), round(nrow(df)*2/3))
df_train <- df[id,]
df_test <- df[-id,]
X_test <- df_test %>% select(-target)
y_test <- df_test$target
df_train$target <- as.factor(df_train$target)

set.seed(0)
model <- ranger(
  target~., 
  data=df_train, 
  importance='impurity',
  num.trees=100,
  max.depth=3,
  probability=TRUE
) 
model

mvi <- model$variable.importance
df_vi <- data.frame(
  variable=factor(names(mvi), levels=names(mvi)[order(mvi)]),
  importance=unname(mvi),
  measure='impurity (baseline)'
)
ggplot(df_vi) + geom_col(aes(importance, variable))  

df_pred <- predict(model, X_test)$predictions
print(mean(abs(y_test - df_pred[,2]) < 0.5))

exp <- DALEX::explain(model, X_test, y_test, verbose = FALSE)
mp <- model_parts(exp, type="ratio", N=NULL, 
                  loss_funciton=DALEX::loss_one_minus_accuracy)
mp_vi <- as.data.frame(mp[mp$permutation==0,])[c(-1, -1-ncol(df_test)),]
df_mp <- data.frame(
  variable=factor(mp_vi$variable,
                  levels=mp_vi$variable[order(mp_vi$dropout_loss)]),
  importance=mp_vi$dropout_loss/sum(mp_vi$dropout_loss),
  measure='permutational (marginal)'
)

df2 <- rbind(df_vi, df_mp)
ggplot(df2) + geom_col(aes(importance, variable)) +
  facet_grid(~measure, scales = "free_x")


set.seed(0)
cp <- cpaths(model, X_test, y_test, k=STEPS, n.iter=ITER)
imp <- cpath::importance(transition(cp, X_test, y_test))
df3 <- rbind(df2, data.frame(
  variable=factor(colnames(X_test),
                  levels=colnames(X_test)[order(imp)]),
  importance=imp,
  measure="cpath"
))
ggplot(df3) + geom_col(aes(importance, variable)) +
  facet_grid(~measure, scales = "free_x")


set.seed(0)
cp_rl <- cpath_rl(model, X_test, k=STEPS, n_iter=ITER, 
               epsilon='decay', alpha='decay', gamma=0.5,
               verbose=FALSE) 
df4 <- rbind(df3, data.frame(
  variable=factor(colnames(X_test),
                  levels=colnames(X_test)[order(cp_rl$importance)]),
  importance=cp_rl$importance,
  measure="cpath_rl"
)) 

p <- ggplot(df4) + geom_col(aes(importance, variable)) +
  facet_grid(~measure, scales = "free_x")
p
ggsave("simulations/output/rl_synthetic.png", p, width=7, height=4)