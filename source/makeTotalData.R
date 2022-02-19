matchWithSampleID <- function(meta,otu,taxa){
  check <- length(which(colnames(otu)=="Run")) + length(which(colnames(meta)=="Run"))
  if(check!=2){
    print("Must include Run column!!!")
  }
  otuInd <- which(otu$Run %in% intersect(otu$Run,meta$Run))
  otu <- otu[otuInd,]
  metaInd <- which(meta$Run %in% intersect(otu$Run,meta$Run))
  meta <- meta[metaInd,]
  
  otu <- otu %>% arrange(Run) 
  meta <- meta %>% arrange(Run) 
  
  otu <- otu[,-"Run"]
  meta <- meta[,-"Run"]
  
  # Remove columns which have sum as zero
  otu <- as.data.frame(otu)
  index <- as.vector(which(sapply(otu,sum)==0))
  otu <- otu[,-index]
  
  write.csv(otu,file=paste0(path,"\\totalData\\","totalOTU",taxa,".csv"),row.names=FALSE)
  write.csv(meta,file=paste0(path,"\\totalData\\","totalMetaData.csv"),row.names=FALSE)
  
  return(list(totalOTU=otu,totalMetaData=meta))
}



