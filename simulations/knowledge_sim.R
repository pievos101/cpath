# knowledge graph
library(igraph)
library(cpath)
library(ranger)

p = 30 # number of nodes
n_iter = 50
npaths = 100 
k_length = 20

COVERAGE = matrix(NaN, n_iter, 2)
colnames(COVERAGE) =c("noG","G")


for (xx in 1:n_iter){

    # Create the knowledge graph
    g = sample_pa(p, directed=FALSE, power=0.5)

    edge_list = as_edgelist(g, names = TRUE)

    CON = FALSE
    NCON = FALSE
    cn1 = 0
    cn2 = 0
    ncn1 = 0
    ncn2 = 0
    
    while(TRUE){
        #print("Get the selected nodes")
        # randomly sample two nodes
        node1 = sample(1:p, 1) 
        node2 = sample(1:p, 1)
        if(node1==node2){next}

        # check connectivitiy
        if(!CON){
        if(are.connected(g, node1, node2)){
            cn1 = node1
            cn2 = node2
            CON=TRUE
            break
        }}

        if(!NCON){
        if(!are.connected(g, node1, node2)){
            ncn1 = node1
            ncn2 = node2
            NCON=TRUE
        }}

    diff = sum(is.element(c(cn1, cn2),c(ncn1, ncn2)))
    if (CON & NCON & (diff==0)){break}else{next}
    }

    # Create data 
    X = replicate(p, rnorm(100))
    #X = replicate(p, rbinom(100, 1, 0.5))
    X = matrix(X, nrow(X), ncol(X)) #"X" can also be a matrix
    #z = with(X, 5*X1 + 3*X2 + 2*X3 + 1*X4 -
    #5*X5 - 9*X6 - 2*X7 + 1*X8 )
    z = 5*X[,cn1] + 3*X[,cn2]
    #y = as.factor(xor(X[,cn1],X[,cn2]))
    pr = 1/(1+exp(-z)) # pass through an inv-logit function
    target = as.factor(rbinom(100,1,pr))
    #X[,ncn1] = X[,cn1]
    #X[,ncn2] = X[,cn2]
    X = as.data.frame(X)

    # Train test split 
    #id <- sample(nrow(df), round(nrow(df)*2/3))
    #X_train <- X[id,]
    #X_test <- X[-id,]
    #y_train <- as.factor(target[id])
    #y_test <- as.factor(target[-id])

    # Train the model
    
    # Train a random forest classifier
    model = ranger(x=X,y=target, 
            num.trees=100, 
            classification=TRUE, 
            probability=TRUE, 
            importance='impurity')


    pred = predict(model, X)$predictions
    pred = apply(pred,1,function(x){which.max(x)-1})

    library(ModelMetrics)
    print("Model Accuracy")
    print(ModelMetrics::auc(pred, target))

    ### Without graph ####
    ######################

    # Get the counterfactual paths
    P   = cpath::cpaths(model, X, k=k_length, n_paths = npaths)
    if(sum(P$counterfactuality==TRUE)<=1){
        print("No counterfactuals")
        next
    }

    # Build transition matrix 
    T   = cpath::transition(P)

    # Get global feature importances
    IMP = cpath::importance(T)
    #print(IMP)

    # Calculate Coverage
    names(IMP) = 1:p
    IMP_sorted = sort(IMP, decreasing=TRUE)

    m = match(c(cn1,cn2),names(IMP_sorted[1:2]))
    COV = sum(!is.na(m))/2


    ### With graph ####
    ######################

    # Get the counterfactual paths
    P   = cpath::cpaths(model, X, k=k_length, n_paths = npaths, g)

    if(sum(P$counterfactuality==TRUE)<=1){
        print("No counterfactuals")
        next
    }
    # Build transition matrix 
    T   = cpath::transition(P)

    # Get global feature importances
    IMP = cpath::importance(T)
    #print(IMP)

    # Calculate Coverage
    names(IMP) = 1:p
    IMP_sorted = sort(IMP, decreasing=TRUE)

    m = match(c(cn1,cn2),names(IMP_sorted[1:2]))
    COV_graph = sum(!is.na(m))/2

COVERAGE[xx,] = c(COV, COV_graph)
print(COVERAGE)

}


stop()

library(ggplot2)
library(reshape)

##############################
# plot varying 
##############################
k_length = c(3, 4, 7, 10, 20)
cpath_cov = c(0.59, 0.60, 0.70, 0.82, 0.77)
cpath_graph_cov = c(0.67, 0.74, 0.74, 0.77, 0.57)

df = cbind(cpath_cov, cpath_graph_cov)
colnames(df) = c("CPATH","CPATH_know")
df_melt = melt(df)
df_melt[,1] = k_length

colnames(df_melt) = c("signal","method","value")
df_melt$value  <- as.numeric(df_melt$value)
df_melt$signal <- as.numeric(df_melt$signal)

p <- ggplot(df_melt, aes(x=signal, y=value, group=method)) +
  coord_cartesian(ylim = c(0, 1)) +
  geom_line(aes(color=method), size=1)+
  geom_point(aes(color=method), size=2)+
  #scale_x_reverse()+
  ylab("Coverage") +
  xlab("Length of paths (k)")+
  scale_x_continuous(name="Length of paths (k)", breaks=k_length,
  labels=as.character(k_length), guide = guide_axis(angle = 90)) +
  #scale_x_reverse(labels=rev(c("2/2", "2/4", "2/6", "2/8", "2/10")))+
  theme_minimal() 
p


############################################
# plot varying npaths
############################################

npaths = c(20, 50, 100, 500, 1000)
cpath_cov = c(0.455, 0.564, 0.60, 0.78, 0.87 )
cpath_graph_cov = c(0.591, 0.679, 0.74, 0.82, 0.90 ) 

df = cbind(cpath_cov, cpath_graph_cov)
colnames(df) = c("CPATH","CPATH_know")
df_melt = melt(df)
df_melt[,1] = npaths

colnames(df_melt) = c("signal","method","value")
df_melt$value  <- as.numeric(df_melt$value)
df_melt$signal <- as.numeric(df_melt$signal)

p <- ggplot(df_melt, aes(x=signal, y=value, group=method)) +
  coord_cartesian(ylim = c(0, 1)) +
  geom_line(aes(color=method), size=1)+
  geom_point(aes(color=method), size=2)+
  #scale_x_reverse()+
  ylab("Coverage") +
  xlab("Number of paths")+
  scale_x_continuous(name="Number of paths", breaks=npaths,
  labels=as.character(npaths), guide = guide_axis(angle = 90)) +
  #scale_x_reverse(labels=rev(c("2/2", "2/4", "2/6", "2/8", "2/10")))+
  theme_minimal() 
p