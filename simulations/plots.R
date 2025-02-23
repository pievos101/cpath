## PLOT 
library(ggplot2)
library(reshape)


signal = as.factor(c(2/2, 2/4, 2/6, 2/8, 2/10))

#conditional dependency (1)
gini  = c(0.990, 0.940, 0.875, 0.860, 0.815)
cpath = c(0.960, 0.910, 0.830, 0.850, 0.715)
shap  = c(0.855, 0.700, 0.635, 0.640, 0.615)
lime  = c(0.720, 0.655, 0.595, 0.600, 0.600)
cpi   = c(1,     0.955, 0.860, 0.505, 0.520)
cpath_min = c(0.960, 0.850, 0.820, 0.900, 0.790)

a = cbind(cpath, cpath_min)
colnames(a) = c("CPATH","CPATH_min")
rownames(a) = signal

#conditional dependency (2)
#
gini  = c(0.990, 0.955, 0.820, 0.865, 0.795)
cpath = c(0.955, 0.920, 0.830, 0.840, 0.795)
shap  = c(0.855, 0.730, 0.625, 0.630, 0.625)
lime  = c(0.725, 0.605, 0.500, 0.575, 0.585)
cpi   = c(0.985, 0.980, 0.870, 0.380, 0.350)
cpath_min = c(0.960, 0.890, 0.820, 0.720, 0.750)
#

b = cbind(cpath, cpath_min)
colnames(b) = c("CPATH","CPATH_min")
rownames(b) = signal


# correlation
gini = c(0.90,0.74,0.73,0.78,0.64)
cpath = c(0.80,0.72,0.71,0.70,0.63)
shap = c(0.86,0.71,0.75,0.72,0.57)
lime = c(0.88,0.74,0.76,0.78,0.67)
cpi = c(0.93,0.80,0.78,0.47,0.32)
cpath_min = c(0.770, 0.690, 0.600, 0.650, 0.620)
#


c = cbind(cpath, cpath_min)
colnames(c) = c("CPATH","CPATH_min")
rownames(c) = signal

# independency 
gini = c(1,1,1,1,1)
cpath = c(1,1,1,1,1)
shap = c(1,1,1,1,1)
lime = c(1,1,1,1,1)
cpi = c(1,1,1,0.5,0.49)
cpath_min = c(1,1, 0.97,0.98,0.99)


d = cbind(cpath, cpath_min)
colnames(d) = c("CPATH","CPATH_min")
rownames(d) = signal


RES = list(a,b,c,d)
names(RES) = c("Conditional dependency (1)",
"Conditional dependency (2)","Correlation","Conditional in-dependency")

df_melt = melt(RES)


colnames(df_melt) = c("signal","Method","value","type")
df_melt$value  <- as.numeric(df_melt$value)
df_melt$signal <- as.numeric(df_melt$signal)

p <- ggplot(df_melt, aes(x=signal, y=value, group=Method)) +
  coord_cartesian(ylim = c(0, 1)) +
  geom_line(aes(color=Method), size=1)+
  geom_point(aes(color=Method), size=2)+
  #scale_x_reverse()+
  ylab("Coverage") +
  xlab("Signal to noise ratio")+
  #scale_x_continuous(name="signal to noise ratio", 
  #labels=c("2/2", "2/4", "2/6", "2/8", "2/10")) +
  scale_x_reverse(labels=rev(c("2/2", "2/4", "2/6", "2/8", "2/10")))+
  theme_bw() + 
  theme(text=element_text(size=15)) +
  facet_wrap(~type, nrow = 2)
p


#### DATASETS AUC ####
setwd("~/GitHub/cpath/SIM_DATASETS_auc/")
library(reshape)
library(ggplot2)

L = list()
NN = c("SHAP","LIME","CPATH","CPATH_min")

L[[1]] = read.table("BREAST_auc.txt")
L[[2]] = read.table("DIABETES_auc.txt")
L[[3]] = read.table("IONOSPHERE_auc.txt")

L[[2]][,3] = read.table("DIABETES_CPATH_full_auc.txt")[,3]
L[[3]][,3] = read.table("IONOSPHERE_CPATH_full_auc.txt")[,3]


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


##### Computational Speed and Coverage 
#COV
setwd("~/GitHub/cpath/SIM_SPEED/")

COV = list()
cov_10 = colMeans(read.table("COV_10paths.txt"))
cov_100 = colMeans(read.table("COV_100paths.txt"))
cov_1000 = colMeans(read.table("COV_1000paths.txt"))
cov_10000 = colMeans(read.table("COV_10000paths.txt"))

COV[[1]] = cov_10
COV[[2]] = cov_100
COV[[3]] = cov_1000
COV[[4]] = cov_10000

names(COV) = c("10","100","1000","10000")

COV_full = list()
cov_10_full = colMeans(read.table("COV_10paths_full.txt"))
cov_100_full = colMeans(read.table("COV_100paths_full.txt"))
cov_1000_full = colMeans(read.table("COV_1000paths_full.txt"))
cov_10000_full = colMeans(read.table("COV_10000paths_full.txt"))

COV_full[[1]] = cov_10_full
COV_full[[2]] = cov_100_full
COV_full[[3]] = cov_1000_full
COV_full[[4]] = cov_10000_full

names(COV_full) = c("10","100","1000","10000")

library(ggplot2)
library(reshape)
COV_melt = melt(COV)
COV_full_melt = melt(COV_full)

COV_melt = cbind("cpath (k=4)",COV_melt)
colnames(COV_melt) = c("Strategy","Coverage","nPaths")
COV_full_melt = cbind("cpath (k=nfeat)",COV_full_melt)
colnames(COV_full_melt) = c("Strategy","Coverage","nPaths")

COV_all = rbind(COV_melt, COV_full_melt)
nfeatures = rep(c(10,50,100,1000),4*2)
COV_all = cbind(COV_all, nfeatures) 
COV_all$Coverage = as.numeric(COV_all$Coverage)
COV_all$nfeatures = as.numeric(COV_all$nfeatures)

# Plot
p1 = ggplot(data=COV_all, aes(x=nfeatures, y=Coverage, group=nPaths)) +
  geom_line(aes(color=nPaths), linewidth=1)+
  geom_point(aes(color=nPaths), size=2)+
  scale_x_continuous(breaks = c(10,50,100,1000))+
  theme(text = element_text(size=12)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~Strategy, ncol=2) 



##### Computational Speed and Coverage 
#TIME
setwd("~/GitHub/cpath/SIM_SPEED/")

COV = list()
cov_10 = colMeans(read.table("TIME_10paths.txt"))
cov_100 = colMeans(read.table("TIME_100paths.txt"))
cov_1000 = colMeans(read.table("TIME_1000paths.txt"))
cov_10000 = colMeans(read.table("TIME_10000paths.txt"))

COV[[1]] = cov_10
COV[[2]] = cov_100
COV[[3]] = cov_1000
COV[[4]] = cov_10000

names(COV) = c("10","100","1000","10000")

COV_full = list()
cov_10_full = colMeans(read.table("TIME_10paths_full.txt"))
cov_100_full = colMeans(read.table("TIME_100paths_full.txt"))
cov_1000_full = colMeans(read.table("TIME_1000paths_full.txt"))
cov_10000_full = colMeans(read.table("TIME_10000paths_full.txt"))

COV_full[[1]] = cov_10_full
COV_full[[2]] = cov_100_full
COV_full[[3]] = cov_1000_full
COV_full[[4]] = cov_10000_full

names(COV_full) = c("10","100","1000","10000")

library(ggplot2)
library(reshape)
COV_melt = melt(COV)
COV_full_melt = melt(COV_full)

COV_melt = cbind("cpath (k=4)",COV_melt)
colnames(COV_melt) = c("Strategy","Time","nPaths")
COV_full_melt = cbind("cpath (k=nfeat)",COV_full_melt)
colnames(COV_full_melt) = c("Strategy","Time","nPaths")

COV_all = rbind(COV_melt, COV_full_melt)
nfeatures = rep(c(10,50,100,1000),4*2)
COV_all = cbind(COV_all, nfeatures) 
COV_all$Coverage = as.numeric(COV_all$Time)
COV_all$nfeatures = as.numeric(COV_all$nfeatures)

# Plot
p1 = ggplot(data=COV_all, aes(x=nfeatures, y=Time, group=nPaths)) +
  geom_line(aes(color=nPaths), linewidth=1)+
  geom_point(aes(color=nPaths), size=2)+
  scale_x_continuous(breaks = c(10,50,100,1000))+
  theme(text = element_text(size=12)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~Strategy, ncol=2) 

