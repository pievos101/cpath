

#### DATASETS AUC ####
setwd("~/GitHub/cpath/SIM_DATASETS_auc/")
library(reshape)
library(ggplot2)

L = list()
NN = c("SHAP","LIME","CPATH","CPATH_min")

L[[1]] = read.table("BREAST_auc.txt")
L[[2]] = read.table("DIABETES_auc.txt")
L[[3]] = read.table("IONOSPHERE_auc.txt")

colnames(L[[1]]) = NN
colnames(L[[2]]) = NN
colnames(L[[3]]) = NN

names(L) = c("Breast","Diabetes","Ionosphere")

L_melt = melt(L)

p = ggplot(L_melt, aes(x=variable, y=value)) + 
  geom_boxplot(notch=TRUE) +
  #facet_wrap(. ~ variable, scales="free")
  #facet_grid(cols = vars(L1), scales = "free_y")
  ylab("AUC")+
  xlab("Methods") +
  theme(text = element_text(size=15)) +
  facet_grid(L1~., scales = "free") 
##########################################

## CPATH correlation with CPATH_min
setwd("~/GitHub/cpath/SIM_CPATH_corr_CPATH_min/")
library(reshape)
library(ggplot2)

L = list()
L[[1]] = read.table("COR_sim.txt")
L[[2]] = read.table("COR_sim3.txt")
L[[3]] = read.table("COR_sim4.txt")
L[[4]] = read.table("COR_sim5.txt")

NN = c("Conditional dependency (1)",
      "Correlation","Conditional in-dependency",
      "Conditional dependency (2)")

names(L) = NN

L_melt = melt(L)

p = ggplot(L_melt, aes(x=variable, y=value)) + 
  geom_boxplot(notch=FALSE) +
  #facet_wrap(. ~ variable, scales="free")
  #facet_grid(cols = vars(L1), scales = "free_y")
  ylab("CPATH Correlation with CPATH_min")+
  xlab("") +
  theme(text = element_text(size=15)) +
  facet_wrap(~L1,ncol=2) 
##########################################


setwd("~/GitHub/cpath/SIM_DATASETS_corr_with_gini/")
library(ggplot2)
library(reshape)

NN = c("SHAP","LIME","CPATH","CPATH_min")
NN2 = c("Breast","Diabetes","Ionosphere","Iris")


L = list()
L[[1]] = read.table("BREAST_corr_with_gini.txt")
L[[2]] = read.table("DIABETES_corr_with_gini.txt")
L[[3]] = read.table("IONOSPHERE_corr_with_gini.txt")
L[[4]] = read.table("IRIS_corr_with_gini.txt")



L[[1]] = cbind(read.table("SHAP_BREAST_corr_with_gini.txt")[,1],L[[1]])
L[[2]] = cbind(read.table("SHAP_DIABETES_corr_with_gini.txt")[,1],L[[2]])
L[[3]] = cbind(read.table("SHAP_IONOSPHERE_corr_with_gini.txt")[,1],L[[3]])
L[[4]] = cbind(read.table("SHAP_IRIS_corr_with_gini.txt")[,1],L[[4]])

# CPATH with k=ncol(data)
L[[1]][,3] = read.table("BREAST_corr_with_gini_CPATH_full.txt")[,3]
L[[2]][,3] = read.table("DIABETES_corr_with_gini_CPATH_full.txt")[,3]
L[[3]][,3] = read.table("IONOSPHERE_corr_with_gini_CPATH_full.txt")[,3]
L[[4]][,3] = read.table("IRIS_corr_with_gini_CPATH_full.txt")[,3]


colnames(L[[1]]) = NN
colnames(L[[2]]) = NN
colnames(L[[3]]) = NN
colnames(L[[4]]) = NN

names(L) = NN2

L_melt = melt(L)

p = ggplot(L_melt, aes(x=variable, y=value)) + 
  geom_boxplot(notch=FALSE) +
  #facet_wrap(. ~ variable, scales="free")
  #facet_grid(cols = vars(L1), scales = "free_y")
  ylab("Spearman Correlation wih Gini")+
  xlab("") +
  theme(text = element_text(size=15)) +
  facet_wrap(~L1,ncol=2) 

##############################################################


setwd("~/GitHub/cpath/SIM_DATASETS_corr_with_treeSHAP/")
library(ggplot2)
library(reshape)

NN = c("SHAP","LIME","CPATH","CPATH_min")
NN2 = c("Breast","Diabetes","Ionosphere","Iris")


L = list()
L[[1]] = read.table("SIM_BREAST_corr_with_treeSHAP.txt")
L[[2]] = read.table("SIM_DIABETES_corr_with_treeSHAP.txt")
L[[3]] = read.table("SIM_IONOSPHERE_corr_with_treeSHAP.txt")
L[[4]] = read.table("SIM_IRIS_corr_with_treeSHAP.txt")


# CPATH with k=ncol(data)
L[[1]][,3] = read.table("BREAST_corr_with_treeSHAP_CPATH_full.txt")[,3]
L[[2]][,3] = read.table("DIABETES_corr_with_treeSHAP_CPATH_full.txt")[,3]
L[[3]][,3] = read.table("IONOSPHERE_corr_with_treeSHAP_CPATH_full.txt")[,3]
L[[4]][,3] = read.table("IRIS_corr_with_treeSHAP_CPATH_full.txt")[,3]


colnames(L[[1]]) = NN
colnames(L[[2]]) = NN
colnames(L[[3]]) = NN
colnames(L[[4]]) = NN

names(L) = NN2

L_melt = melt(L)

p = ggplot(L_melt, aes(x=variable, y=value)) + 
  geom_boxplot(notch=FALSE) +
  #facet_wrap(. ~ variable, scales="free")
  #facet_grid(cols = vars(L1), scales = "free_y")
  ylab("Spearman Correlation wih TreeShap")+
  xlab("") +
  theme(text = element_text(size=15)) +
  facet_wrap(~L1,ncol=2) 