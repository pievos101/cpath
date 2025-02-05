
library(reshape)
library(ggplot2)

L = list()

L[[1]] = read.table("BREAST_auc.txt")
L[[2]] = read.table("DIABETES_auc.txt")
L[[3]] = read.table("IONOSPHERE_auc.txt")

names(L) = c("Breast","Diabetes","Ionosphere")

L_melt = melt(L)

p = ggplot(L_melt, aes(x=variable, y=value)) + 
  geom_boxplot(notch=TRUE) +
  #facet_wrap(. ~ variable, scales="free")
  #facet_grid(cols = vars(L1), scales = "free_y")
  theme(text = element_text(size=20))+
  facet_grid(L1~., scales = "free")
  