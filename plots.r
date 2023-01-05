#plots

## PLOT 
library(ggplot2)
library(reshape)


## BOXPLOT
###########################
RES_melt = melt(RES[,1:3])
#colnames(RES_melt) = c("Method","value")
colnames(RES_melt) = c("id", "Method","value")
#df_melt$var <- factor(df_melt$var, levels=length(IMP_all):1)
RES_melt = RES_melt[,2:3]
RES_melt$value = as.numeric(RES_melt$value)
p1 <- ggplot(RES_melt, aes(x=Method, y=value, fill=Method)) +
coord_cartesian(ylim = c(0, 1)) +
#scale_y_continuous(limits=c(-1,1))+
geom_violin()+
#geom_boxplot()+
#geom_jitter()+
theme_bw() +
ylab("Spearman corellation") +
xlab("Method")
p1


## BARPLOT
