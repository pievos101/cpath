# sim 

# [conditional dependent] - cPATH wins
#'@export
sim <- function(){

data = matrix(rnorm(400, 0, 2), 100, 4)
colnames(data) <- paste("V", 1:dim(data)[2], sep="")
target = sample(c(0,1),dim(data)[1], replace=TRUE)

count = 1 
a = 0
b = 0 
for(xx in 1:dim(data)[1]){

    if(data[xx,1]>=a){

        if(data[xx,2]>=b){
            target[xx] = 1
            count = count + 1 
        }

    }else{

        if(data[xx,2]>=b){
            target[xx] = 0
            count = count + 1
        }
    }
}   

return(list(data=data, target=target, noise=1-(count/length(target))))

}

# not that informative
#'@export
sim2 <- function(){

data = matrix(rnorm(400, 0, 2), 100, 4)
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

# [correllated] - 
#'@export
sim3 <- function(){

data = matrix(rnorm(1200, 0, 2), 100, 12)
colnames(data) <- paste("V", 1:dim(data)[2], sep="")
#target = numeric(dim(data)[1]) #sample(c(0,1),dim(data)[1], replace=TRUE)
target = sample(c(0,1),dim(data)[1], replace=TRUE)
#data[,3] <- 0

count = 1 
a     = 0

for(xx in 1:dim(data)[1]){

    if((data[xx,1]>=a) & (data[xx,2]>=a)){

            target[xx] = 1
            count = count + 1 
    }

    #if((data[xx,1]<a) & (data[xx,2]<a)){

    #        target[xx] = 0
    #        count = count + 1 
    #}   
}

return(list(data=data, target=target, noise=1-(count/length(target))))

}

# [Features are independent] - LIME wins - cpath looses
#'@export
sim4 <- function(){

data = matrix(rnorm(1200, 0, 2), 100, 12)
colnames(data) <- paste("V", 1:dim(data)[2], sep="")
target = sample(c(0,1),dim(data)[1], replace=TRUE)

count = 1 
a = 0
for(xx in 1:dim(data)[1]){

    if((data[xx,1]>=a) & (data[xx,2]>=a)){
        next
    }

    if((data[xx,1]>=a)){

            target[xx] = 1
            count = count + 1
    }

    if((data[xx,2]>=a)){

            target[xx] = 0
            count = count + 1
    }
    
}   

return(list(data=data, target=target, 1-(count/length(target))))

}

#[conditional dependent] - CPATH wins
#'@export
sim5 <- function(){

data = matrix(rnorm(400, 0, 2), 100, 4)
colnames(data) <- paste("V", 1:dim(data)[2], sep="")
target = sample(c(0,1),dim(data)[1], replace=TRUE)

for(xx in 1:dim(data)[1]){

    if((data[xx,1]>0)){
        
        if(data[xx,2]<0){

            target[xx] = 1

        }else{

            target[xx] = 0

        }
    }
    
}   

return(list(data=data, target=target))

}

# [conditional dependent] - cPATH wins
#'@export
sim7 <- function(){

data = matrix(rnorm(400, 0, 2), 100, 4)
colnames(data) <- paste("V", 1:dim(data)[2], sep="")
target = sample(c(0,1),dim(data)[1], replace=TRUE)

count = 1 
a = 0
b = 0 
for(xx in 1:dim(data)[1]){

    if(data[xx,1]>=a){

        if(data[xx,2]>=b){
            target[xx] = 1
            count = count + 1 
        }

    }else{

        if(data[xx,3]>=b){
            target[xx] = 0
            count = count + 1
        }
    }
}   

return(list(data=data, target=target, noise=1-(count/length(target))))

}