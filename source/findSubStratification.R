drNclusterARI <- function(data,meta,taxa,method,rep,visual=FALSE){
  data <- as.data.frame(data)
  meta <- as.data.frame(meta)
  ARItable <- matrix(0,nrow = rep,ncol = ncol(meta))
  for(i in seq_len(ncol(meta))){
    # Dimensional Reduction for visualize
    if(visual==TRUE){
      if(method==1){
        result <- Rtsne(X=data,dims=2,check_duplicates=FALSE) # Barnes-Hut-SNE: parameter theta=0.0 is original t-SNE
        dr.data <- result$Y
      }else if(method==2){
        result <- umap(data,n_components=2) # UMAP
        dr.data <- result$layout 
      }
      df <- data.frame(x = dr.data[,1],y = dr.data[,2],label = meta[,i])
      ggplot(df, aes(x, y, colour = label)) + geom_point() + theme_classic() + labs(colour=colnames(meta)[i])
      ggsave(paste0(getwd(),"/plot/",taxa,"_",colnames(meta)[i],".png"),width=16,height=7,type="cairo")
    }
    # Make ARI table with dr + clustering + ARI 
    colVec <- foreach(j=seq_len(rep),.packages = c("umap","Rtsne","fossil","ggplot2"),.combine = "c") %dopar%{
      result <- umap(data,n_components=2) 
      dr.data <- result$layout 
      km <- kmeans(dr.data,length(levels(meta[,i])))
      ari <- round(adj.rand.index(as.integer(meta[,i]),km$cluster),3)
      return(ari)
    }
    ARItable[,i] <- colVec
    #print(ARItable)
  }
  ARItable <- as.data.frame(ARItable)
  colnames(ARItable) <- colnames(meta)
  return(ARItable)
}


hyposAssumption <- function(data,control){
  substratf <- c()
  data <- melt(data)
  bfpvalue<-bf.test(value~variable,data=data)
  if(bfpvalue$p.value>0.05){
    # ANOVA
    anov <- aov(value~variable,data = data)
    anov.pvalue <- unlist(summary(anov))[["Pr(>F)1"]]
  }else{
    # Welch ANOVA
    anov <- oneway.test(value~variable,data=data)
    anov.pvalue <- anov$p.value
  }
  if(anov.pvalue<0.05){
    # post-hoc test
    dntResult <- DunnettTest(x=data$value,g=data$variable,control = control)
    dntResult <- as.data.frame(dntResult[[control]])
    substratf <- c(control,gsub(paste0("-",control),"",rownames(dntResult %>% arrange(pval) %>% filter(pval<0.05))))
    # visualize
    print(ggplot(data = dntResult,aes(x=diff,y=rownames(dntResult))) + geom_point()+geom_errorbar(aes(xmin=lwr.ci,xmax=upr.ci,width=0.5))+
      labs(x="difference",y="Others - Control")+ggtitle("Post-hoc test with 95% confidence level")+theme_light())
  }else{
    # No need for post-hoc test
    substratf <- levels(data$variable)
  }
  return(substratf)
}


