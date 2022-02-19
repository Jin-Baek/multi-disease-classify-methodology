regressWithCovaAdjmt <- function(data,meta,covariates){
  meta <- as.data.frame(meta)
  newData <- meta[,covariates]
  baseDesign <- model.matrix(~0+.,data=newData)
  index <- c()
  if(!is.fullrank(baseDesign)){
    collinearity <- nonEstimable(baseDesign)
    for(i in 1:length(collinearity)){
      index <- append(index,which(str_detect(collinearity[i],covariates)))
    }
    # Remove column which make not full rank error
    covariates <- covariates[-unique(index)]
  }
  # Update covariates
  newData <- meta[,covariates] 
  targetDesign <- model.matrix(~0+.,data=newData)
  print("Design complete!!!")
  
  nDisease <- length(levels(meta[,1]))
  nOthers <- ncol(targetDesign) - nDisease
  
  # Make user define contrast 
  mycontrasts <- list()
  for(i in 1:nDisease){
    myCont <- c(rep(-1/(nDisease-1),nDisease-1),rep(0,nOthers))
    myCont <- append(myCont,1,i-1)
    mycontrasts[[colnames(targetDesign)[i]]] <- myCont
  }
  print("User define contrasts complete!!!")
  modelResult <- list()
  # Use edgeR package
  count.matrix<-t(as.matrix(data))  
  Y <- DGEList(counts = count.matrix) 
  Y <- calcNormFactors(Y,method = "TMM") 
  Y <- estimateDisp(Y,targetDesign,robust=TRUE) 
  logCPM <- cpm(Y,log=TRUE,prior.count = 1) 
  logCPM <- t(logCPM)
  rownames(logCPM) <- seq_len(nrow(logCPM))
  modelResult[["LogCPM"]] <- logCPM
  fit <- glmQLFit(Y,targetDesign,robust=TRUE)  # Perform quasi likelihood negbin glm : Some microbes are not perfectly follow negbin
  modelResult[["glmResult"]] <- fit
  print("glmFit complete!!!")
  
  # Total FTest based on Disease for feature selection
  testForTF <- glmQLFTest(fit,coef = c(1:nDisease))
  modelResult[["TotalFTest"]] <- topTags(testForTF,n=ncol(data))$table %>% arrange(desc(F))
  print("Testing for Total F statistic complete!!!")
  
  # Contrast test 
  for(i in 1:length(mycontrasts)){
    testForContrast <- glmQLFTest(fit,contrast = mycontrasts[[i]])
    modelResult[[paste0(colnames(targetDesign)[i],"VSothers")]] <- topTags(testForContrast,n=ncol(data),sort.by = "PValue")$table
  }
  print("Contrast test for each disease complete!!!")
  return(modelResult)
}




