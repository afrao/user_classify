#### Alexander Rao

###libraries
library(rjson)
library(plyr)

## my tools
source("tools.R")


## picking the size of the data set
size.of.set <- 1000000
percentage.spam <- .05

## getting the file names
all.files <- system("ls data/",intern=TRUE)
files <- all.files[grep(".out",all.files)]
num.files <- length(files)

## can make it more complicated later for now just sample the same number from each day

num.from.each <- size.of.set/num.files


new.dataset <- c()


for(i in 1:num.files){
    print("*")
    file <- files[i]
    lines <- readLines(file)
    labels <- laply(lines,get.spam)
    n <- length(lines)
    ham <- which(!labels)
    spam <- which(labels)
    lines <- c(lines[sample(ham,size=(num.from.each*(1-percentage.spam)))],
               lines[sample(spam,size=(num.from.each*percentage.spam))])
    new.dataset <- c(new.dataset,lines)
}




new.dataset <- new.dataset[sample.int(length(new.dataset))]
write(new.dataset,"sampled_data5")


