library(plyr)

read.in.data <- function(file.name,trim.fun,num.lines){
    if(num.lines == 0){
        lines <- readLines(file.name)
    } else {
        lines <- readLines(file.name,n=num.lines)
    }
    
    result <- llply(lines,trim.fun)
    return(result)
}


