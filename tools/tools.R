#### To find the True positives, false positives, true negatives, false negatives

## note to self
###########################
    #Precision and recall
    # precision is the fraction of retrieved instances that are relevant
    # recall (sensitivity) is the fraction of relevant instances that are retrieved.
###########################

###### Input
# true.data -is a "binary" vector of the classes for the true data set
# model.data -is a "binary" vector of the classes for the model put onto the data set
# val - is the value of the class selected by positve
# positive - tells you if the val is the positive or the negative value

###### Input
# returns the length of the group that your asking for
Finder.of.groups <- function(true.data,model.data,val=0,positive=TRUE,safe.mode = FALSE) {
    if(positive){
        return(length(which(model.data[which(true.data == val)] == val)))
    } else {
        return(length(which(model.data[which(true.data == val)] != val)))
    }
}

###### Output
# returns the 4 components of the confusion matrix in a data.frame
Make.groups.table <- function(true.data,model.data,val.pos=1,val.neg=0,positive=TRUE,safe.mode = FALSE) {

    TP <- Finder.of.groups(true.data,model.data,val.pos)
    FP <- Finder.of.groups(true.data,model.data,val.pos,positive=FALSE)
    TN <- Finder.of.groups(true.data,model.data,val.neg)
    FN <- Finder.of.groups(true.data,model.data,val.neg,positive=FALSE)

    return(data.frame(TP,FP,TN,FN))

}


###### Output
# returns a message but it will hopefully create a file with the file name
# with a pretty table in it.
Make.sweet.table <- function(df,file.name) {
    stargazer(df, summary=FALSE, out=file.name,out.header=TRUE)
    return(paste("made it in",file.name,sep=" "))
}




##### GETS THE F SCORE IMPUT THE TRUE VALUES THEN THE PREDICTED VALUES

get.f.score <- function(true.vals,pred.vals){
    result <- Make.groups.table(true.vals,pred.vals)
    ## if.all.is.ham <- table(labels)/length(labels)
    TP <- result$TP
    FP <- result$FP
    TN <- result$TN
    FN <- result$FN
    f.score <- 2*TP/(2*TP+FP+FN)
    return(f.score)
}


########## GLMNET WORKAROUND

ResortDtm <- function(working.dtm) {
      # sorts a sparse matrix in triplet format (i,j,v) first by i, then by j.
      # Args:
      #   working.dtm: a sparse matrix in i,j,v format using $i $j and $v respectively. Any other variables that may exist in the sparse matrix are not operated on, and will be returned as-is.
      # Returns:
      #   A sparse matrix sorted by i, then by j.
      working.df <- data.frame(i = working.dtm$i, j = working.dtm$j, v = working.dtm$v)  # create a data frame comprised of i,j,v values from the sparse matrix passed in.
        working.df <- working.df[order(working.df$i, working.df$j), ] # sort the data frame first by i, then by j.
        working.dtm$i <- working.df$i  # reassign the sparse matrix' i values with the i values from the sorted data frame.
        working.dtm$j <- working.df$j  # ditto for j values.
        working.dtm$v <- working.df$v  # ditto for v values.
        return(working.dtm) # pass back the (now sorted) data frame.
  }  # end function



#############################  Generating the cross validation folds







    
