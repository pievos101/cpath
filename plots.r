#plots

## PLOT 
library(ggplot2)
library(reshape)

RES_melt = melt(RES[,1:3])
#colnames(df_melt) = c("var", "type","value")
#df_melt$var <- factor(df_melt$var, levels=length(IMP_all):1)

p1 <- ggplot(RES_melt, aes(x=variable, y=value, fill=variable)) +
geom_violin()+
geom_boxplot()+
geom_jitter(position=position_jitter(0.2))+
theme_bw() +
ylab("Spearman corellation") +
xlab("Method")

