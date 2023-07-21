# counterfactual graph stuff
library(igraph)
library(bnlearn)


PATHS = P_loc$paths

DATA = matrix(NaN, dim(PATHS)[1], 5)

for (xx in 1:nrow(PATHS)){

    l = PATHS[xx,]
    DATA[xx,] = "no"
    ids = l[!is.na(l)]
    DATA[xx, ids] ="yes"
}

RES = cbind(DATA)#, P$counterfactuality)
RES = RES[P_loc$counterfactuality,]
#colnames(RES) = c(colnames(data))#,"swap")
colnames(RES) = names(IMP_loc)
bn_df = as.data.frame(RES)
for (xx in 1:ncol(bn_df)){
    bn_df[,xx] = as.factor(bn_df[,xx])
}

wl = rbind(c("pregnant","swap"),
            c("glucose","swap"),
            c("pressure","swap"),
            c("triceps","swap"),
            c("insulin","swap"),
            c("mass","swap"),
            c("pedigree","swap"),
            c("age","swap"))

bl = cbind(wl[,2],wl[,1])

res <- hc(bn_df) #, whitelist=wl, blacklist=bl)

fittedbn <- bn.fit(res, data = bn_df)

library(Rgraphviz)
hlight = list(nodes= nodes(res), arcs = arcs(res), col="blue", 
    textCol="black")
pp <- graphviz.plot(res, highlight=hlight) #layout = "dot", highlight=hlight)
#nodeRenderInfo(pp) <- list(col="black", fill=c("swap","grey"))
renderGraph(pp)



