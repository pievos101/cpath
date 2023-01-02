#plots

## PLOT 
library(ggplot2)
library(reshape)

RES_melt = melt(RES[,1:3])
colnames(RES_melt) = c("id", "method","value")
#df_melt$var <- factor(df_melt$var, levels=length(IMP_all):1)

p1 <- ggplot(RES_melt, aes(x=method, y=value, fill=method)) +
geom_violin()+
geom_boxplot()+
geom_jitter(position=position_jitter(0.2))+
theme_bw() +
ylab("Spearman corellation") +
xlab("Method")

