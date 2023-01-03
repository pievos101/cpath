library(markovchain)

# define the markovchain object
statesNames <- c("v1", "v2", "v3","v4")

TRAN = EDGES_all
colnames(TRAN) = statesNames 
rownames(TRAN) = statesNames
TRAN = TRAN/rowSums(TRAN)

mcB <- new("markovchain", states = statesNames, 
                transitionMatrix=TRAN)

# show the sequence
outs <- markovchainSequence(n = 4, markovchain = mcB,
t0 = sample(mcB@states, 1, prob=diag(TRAN)), include.t0 = TRUE)