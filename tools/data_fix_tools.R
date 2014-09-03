### Alexander Rao
## loading in libaries
library(plyr)



##################################### functions to fix for catagorical variables

############# For Nominal catagorical random variables

## create the named list that will get passed to cata.var
create_catagories <- function(vec.of.names){
    n <- length(vec.of.names)
    result <- list()

    for(i in 1:n){
        name <- vec.of.names[i]
        added.vec <- vector("numeric",length = n)
        added.vec[i] <- 1
        result[[name]] <- added.vec
    }
    return(result)
}


### give correct vector for x from catagories
cata.var <- function(x,catagories){
    ## make catagories be a named dataframe where the name is the catagory and the value is correct vector swap
    return(catagories[[x]])
}

## test case
## joke <- create_catagories(c("hello","world","foo","baz"))
## laugh <- cata.var("hello",joke)

the_fixer <- function(col){
    catagories <- create_catagories(unique(col))
    flipper <-
    result <- ldply(col, function(x){return(cata.var(x,catagories))})
    return(result)
}

## test case
## the_fixer(c("a","b","b","c"))

