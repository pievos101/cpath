# sim 

sim <- function(){

data = matrix(rnorm(300, 0, 2), 100, 3)
colnames(data) <- paste("V", 1:dim(data)[2], sep="")
target = numeric(dim(data)[1]) #sample(c(0,1),dim(data)[1], replace=TRUE)
#data[,3] <- 0

for(xx in 1:dim(data)[1]){

    if(data[xx,1]>2){

        if(data[xx,2]>0){
            target[xx] = 1
        }

    }else{

        if(data[xx,2]>2){
            target[xx] = 1
        }
    }
}   

return(list(data=data, target=target))

}

# not that informative
sim2 <- function(){

data = matrix(rnorm(300, 0, 2), 100, 3)
colnames(data) <- paste("V", 1:dim(data)[2], sep="")
target = numeric(dim(data)[1]) #sample(c(0,1),dim(data)[1], replace=TRUE)
#data[,3] <- 0

for(xx in 1:dim(data)[1]){

    if(data[xx,1]>2){

            target[xx] = 1
    }
}   

return(list(data=data, target=target))

}

sim3 <- function(){

data = matrix(rnorm(300, 0, 2), 100, 3)
colnames(data) <- paste("V", 1:dim(data)[2], sep="")
target = numeric(dim(data)[1]) #sample(c(0,1),dim(data)[1], replace=TRUE)
#data[,3] <- 0

for(xx in 1:dim(data)[1]){

    if((data[xx,1]>0) & (data[xx,2]>0)){

            target[xx] = 1
    }
}   

return(list(data=data, target=target))

}

sim4 <- function(){

data = matrix(rnorm(300, 0, 2), 100, 3)
colnames(data) <- paste("V", 1:dim(data)[2], sep="")
target = sample(c(0,1),dim(data)[1], replace=TRUE)

for(xx in 1:dim(data)[1]){

    if((data[xx,1]>0)){

            target[xx] = 1
    }
    if((data[xx,2]>0)){

            target[xx] = 0
    }
    
}   

return(list(data=data, target=target))

}

sim5 <- function(){

data = matrix(rnorm(300, 0, 2), 100, 3)
colnames(data) <- paste("V", 1:dim(data)[2], sep="")
target = numeric(dim(data)[1]) #sample(c(0,1),dim(data)[1], replace=TRUE)

for(xx in 1:dim(data)[1]){

    if((data[xx,1]>0)){
        
        if(data[xx,2]<0){

            target[xx] = 1

        }
    }
    
}   

return(list(data=data, target=target))

}
