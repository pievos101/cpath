library(cpath)
library(ranger)
# remotes::install_github("https://github.com/StatsGary/MLDataR")
library(MLDataR)
library(ggplot2)
library(dplyr)
library(DALEX)

df <- as.data.frame(MLDataR::diabetes_data)
df <- df[complete.cases(df),]
dim(df)
# colnames(df)[1] <- "target"
colnames(df)[ncol(df)] <- "target"
table(df$target)
df$target <- as.factor(df$target)
set.seed(0)
id <- sample(nrow(df), round(nrow(df)*2/3))
df_train <- df[id, ]
df_test <- df[-id, ]
X_test <- df_test %>% select(-target)
y_test <- as.numeric(df_test$target=="Positive")

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
# ggplot(df_vi) + geom_col(aes(importance, variable))  

df_pred <- predict(model, X_test)$predictions
mean(abs(as.numeric(df_test$target=="Positive") - df_pred[,"Positive"]) < 0.5)

exp <- DALEX::explain(model, X_test, y_test, verbose = FALSE)
mp <- model_parts(exp, type="ratio", N=NULL, 
                  loss_funciton=DALEX::loss_one_minus_accuracy)
# plot(mp) + ylab("1-ACC")
mp_vi <- as.data.frame(mp[mp$permutation==0,])[c(-1, -1-ncol(df_test)),]
df_mp <- data.frame(
  variable=factor(mp_vi$variable,
                  levels=mp_vi$variable[order(mp_vi$dropout_loss)]),
  importance=mp_vi$dropout_loss/sum(mp_vi$dropout_loss),
  measure='permutational (marginal)'
)

df2 <- rbind(df_vi, df_mp)

ITER <- 3000
STEPS <- 3

set.seed(0)
cp <- cpaths(model, X_test, y_test, k=STEPS, n.iter=ITER)
imp <- cpath::importance(transition(cp, X_test, y_test))
df3 <- rbind(df2, data.frame(
  variable=factor(colnames(X_test),
                  levels=colnames(X_test)[order(imp)]),
  importance=imp,
  measure="cpath"
))
# df3 <- df2

set.seed(0)
cp <- cpath_rl(model, X_test, k=STEPS, n_iter=ITER, 
               epsilon='decay', alpha='decay', gamma=0.5,
               verbose=FALSE) 

df4 <- rbind(df3, data.frame(
  variable=factor(colnames(X_test),
                  levels=colnames(X_test)[order(cp$importance)]),
  importance=cp$importance,
  measure="cpath_rl"
)) 

p <- ggplot(df4) +
  geom_col(aes(importance, variable)) +
  facet_grid(~measure, scales = "free_x")
p
ggsave("simulations/output/rl_diabetesle.png", p, width=8, height=5)
