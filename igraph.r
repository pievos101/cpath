#igraph
library(igraph)
library(reshape)

# define the markovchain object
statesNames <- c("v1", "v2", "v3","v4")

TRAN = EDGES_all
#diag(TRAN) = 0
colnames(TRAN) = statesNames 
rownames(TRAN) = statesNames
TRAN = TRAN/rowSums(TRAN)


TRAN_melted = melt(TRAN)
EDGES = TRAN_melted[,1:2]
WEIGHTS = round(TRAN_melted[,3], digits=2)

g = graph_from_data_frame(EDGES, directed = TRUE, vertices = NULL)
#plot(g, edge.label = WEIGHTS)

curve.reciprocal.edges <- function(g, curve=.3){
    # Return a graph where the edge-attribute $curved is reset to highlight reciprocal edges
    el <- t(apply(get.edgelist(g),1,sort))
    E(g)$curved <- 0
    E(g)[duplicated(el) | duplicated(el,fromLast =TRUE)]$curved <- curve
    (g)
}

#plot(g, layout=layout.circle, edge.curved=.2)
plot(curve.reciprocal.edges(g), #layout=layout.circle, 
    vertex.label.cex = 1.5,
    edge.label = WEIGHTS,
    #edge.arrow.size=1,#+WEIGHTS*8,                           # Arrow size, defaults to 1
    #edge.arrow.width=1+WEIGHTS, 
    edge.width=1+WEIGHTS*9)