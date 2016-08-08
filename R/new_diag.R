function(data, expr,colvec,ignore.case = T){
  #regexp=regular expressions for the data
  #a vector of the columns of interest (columns with the diagnsotics
  c1<-length(colvec)
  mat1<-unlist(data[,colvec])
  mat2<-matrix((grepl(expr,mat1, ignore.case = ignore.case)+0),byrow=F,ncol=c1)
  #addiding "0" transform the logical into 1 for TRUE and 0 for FALSE
  tab.sum<-apply(mat2,MARGIN=1,FUN=sum)
  tab.sum[tab.sum!=0]<-1
  as.factor(tab.sum)
}
