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

## LINEPLOT
library(ggplot2)
library(reshape)
gini  = c(0.990, 0.940, 0.875, 0.860, 0.815)
cpath = c(0.960, 0.910, 0.830, 0.850, 0.715)
shap  = c(0.855, 0.700, 0.635, 0.640, 0.615)
lime  = c(0.720, 0.655, 0.595, 0.600, 0.600)
cpi   = c(1,     0.955, 0.860, 0.505, 0.520)
signal = rep(c(2/2, 2/4, 2/6, 2/8, 2/10),5)

df = cbind(gini, cpath, shap, lime, cpi)
df_melt = melt(df)
df_melt[,1] = signal

colnames(df_melt) = c("signal","method","value")
df_melt$value  <- as.numeric(df_melt$value)
df_melt$signal <- as.numeric(df_melt$signal)

p <- ggplot(df_melt, aes(x=signal, y=value, group=method)) +
  geom_line(aes(color=method), size=1)+
  geom_point(aes(color=method), size=2)+
  scale_x_reverse()+
  ylab("coverage") +
  xlab("signal to noise ratio")
p
