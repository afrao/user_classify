####libraries
library(rjson)
library(e1071)
library(plyr)
library(LiblineaR)
library(ROCR)

source("../tools/tools.R")
source("../tools/silly_tools.R")



train.file.name <- "./data/clean/sampled_data3"
train.content.file <- "./data/clean/sampled3.csv"
test.file.name <- "./data/clean/sampled_data4"
test.content.file <- "./data/clean/sampled4.csv"



### data reading utility
read.in.data <- function(file.name,num.lines=0,trim.func,labels = TRUE) {
    if(num.lines == 0){
        dat <- readLines(file.name)
    } else {
        dat <- readLines(file.name,n = num.lines)
    }
    dat.l <- apply(as.matrix(dat),1,fromJSON)
    bigOne <- ldply(dat.l,trim.func)
    return(bigOne)
}


### the trimmer for this kind of data
trimmer <- function(x) {
    spams <- x$spam_so_far
    hams <- x$ham_so_far
    number.posts <- spams+hams
    spam.past.ten <- x$spam_past_10
    ## is_bulk <- x$is_bulk
    ## bulk.past.ten <- x$bulk_past_10
    
    ## if(number.posts > 0){
    ##     percent.spam <- spams/number.posts
    ## } else {
    ##     percent.spam = 0
    ## }
    
    if(number.posts > 0){
        if(number.posts >= 10){
            percent.spam <- spam.past.ten/10
        } else {
            percent.spam <- spams/number.posts
        }
    } else {
        percent.spam = 0
    }
    return(c(x$is_spam ,number.posts,percent.spam #,is_bulk,bulk.past.ten
             ))
}


#### OUTPUT
result.printer <- function(classifier,true.vals,predicted){
    print("-------------------------------------------------------------------------------------------------")
    print(classifier)
    print(paste("the confusion matrix for",classifier))
    print(Make.groups.table(true.vals,predicted))
    print(paste("the f.score for",classifier))
    print(get.f.score(true.vals,predicted))
    return(get.f.score(true.vals,predicted))
}

### then creating the training and testing sets
## number of lines to read from file if 0 then reads entire file
num.lines <- 200000
#num.lines <- 0

## if want to compensate for weighted set weighted to true otherwise set to false
weighted = TRUE
#weighted = FALSE


## if other models (note that weighted must be on for these)
other.models = FALSE
#other.models = TRUE

## if write to file
get.names = TRUE
output = TRUE

get.names = FALSE
output = FALSE

## if make the hist of scores
plot.scores = TRUE
plot.scores = FALSE

##





train.scores <- read.csv(train.content.file,header=FALSE,colClasses=c("numeric"))
test.scores <- read.csv(test.content.file,header=FALSE,colClasses=c("numeric"))

blah <- function(x){if(x > 0){
    return(1)} else {
        return(0)
    }
                }
choices <- c(laply(train.scores[,1],blah),laply(test.scores[,1],blah))

train.dat <- cbind(read.in.data(train.file.name,num.lines,trimmer),train.scores)
test.dat <- cbind(read.in.data(test.file.name,num.lines,trimmer),test.scores)

## train.dat <- read.in.data(train.file.name,num.lines,trimmer)
## test.dat <- read.in.data(test.file.name,num.lines,trimmer)

## n1 <- nrow(train.dat)
## n2 <- nrow(test.dat) 
## r1 <- rnorm(n1,mean = 0,sd=100)
## r2 <- rnorm(n2,mean = 0,sd =100)
## train.dat <- cbind(train.dat,r1)
## test.dat <- cbind(test.dat,r2)
## colnames(train.dat)[4] <- "V4"
## colnames(test.dat)[4] <- "V4"

dat <- rbind(train.dat,test.dat)
#dat <- cbind(dat,choices)

print("---------------------------------------------------------------------------")
print("JUST FROM THE CONTENT MODEL")
print(get.f.score(dat[,1],choices))



rows <- nrow(dat)            
dat <- dat[sample.int(rows),]
fold.size <- floor(rows/10)
fold <- rep(1,times = fold.size + (rows %% 10))
for(i in 2:10){
    fold <- c(fold,rep(i,times = fold.size))
}
dat <- cbind(fold,dat)



results <- vector(length = 10)                  
for(i in 1:10){
    indexs <- which(dat[,1] == i)
    num <- 1
    
    train.dat <- dat[-indexs,-1]
    train.dat <- train.dat[which(train.dat[,2] >= num),]
    test.dat <- dat[indexs,-1]
    test.dat <- test.dat[which(test.dat[,2] >= num),]
    C <- 1

    labels <- train.dat[,1]
    if(weighted){
        wts <- length(labels)/table(labels)
        logistic3.model <- LiblineaR(data = train.dat[,-1],labels = train.dat[,1],type = 3, cost=C, wi = wts)
    } else {
        logistic3.model <- LiblineaR(data = train.dat[,-1],labels = train.dat[,1],type = 3, cost=C)
    }
    
    logistic3.predicted <- predict(logistic3.model,test.dat[,-1],decisionValues=TRUE)
    
    logistic3.decisions <- logistic3.predicted$decisionValues
    logistic3.predicted <- logistic3.predicted$predictions
    
    result.printer("#3 L2 regularized L1 loss svc dual",test.dat[,1],logistic3.predicted)
    results[i] <- get.f.score(test.dat[,1],logistic3.predicted)

    scores <- logistic3.decisions[,1]
    predicted <- logistic3.predicted
    values <- test.dat[,1]
    n <- length(values)

    score <- sort(scores,index.return=TRUE)
    values <- values[score$ix]

    states <- c()
    count <- 0
    for(i in 1:n){
        if(predicted[i] == 1){
            count <- count + 1
            if(values[i] ==1){
                states[count] <- 1
            }
            if(values[i] == 0){
                states[count] <- -1
            }
        }
    }
    
    size <- length(states)
    rights <- 0
    wrongs <- 0
    for(i in 1:size){
        

    }

    

}









if(get.names && output){
    write.table(cbind(test.post.names,logistic3.decisions[,1],test.dat[,1]),
                "user_scores.csv",row.names=FALSE,col.names=FALSE,
                sep=",")
}

