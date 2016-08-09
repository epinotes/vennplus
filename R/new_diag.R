function(data, expr,colvec,ignore.case = T){
  # regexp=regular expressions for the data
  # a unique vector of the columns of interest (columns with the diagnsotics
  c1 <- length(colvec)
  mat1 <- unlist(data[,colvec])
  # rebuilding the original matrix with cell values of T or F
  # adding "0" transform the logical into 1 for TRUE and 0 for FALSE
  mat2 <- matrix((grepl(expr,mat1, ignore.case = ignore.case)+0),byrow=F,ncol=c1)
  # create a new vector (the new.diag) 
  tab.sum <- apply(mat2,MARGIN=1,FUN=sum)
  # the new vector with 1 for T and 0 for F
  tab.sum[tab.sum!=0] <- 1
  # the new vector is ready to be added to the original data
  as.factor(tab.sum)
}
