clftWithONOF <- function(data,target,featureImpt,method){
  # Order data based on feature importance
  data <- data[,rownames(featureImpt)]
  numofFeature <- seq(20,ncol(data),round((ncol(data)-20)/15))
  # Classification for finding optimal number of feature
  trainControl <- trainControl(method = "cv",number = 10)
  vec <- foreach(i=numofFeature,.packages = c("caret"),.combine = "c") %dopar% {
    model <- train(x = data[,seq_len(i)],y=target,method = method,trControl = trainControl)
    accuracy <- model$results[as.numeric(rownames(model$bestTune)),][,"Accuracy"]
    return(accuracy)
  }
  # Visualize the result: Number of feature vs Accuracy
  onofDF <- data.frame(num=numofFeature,accuracy=round(vec,4))
  print(ggplot(data=onofDF,aes(x=num,y=accuracy)) + geom_line(color="cornflowerblue",size=1.5) + geom_point(color="darkslateblue",size=1.5)+
    ggtitle(paste0(method," : Accuracy based on number of significant feature")) + labs(x="Number of feature",y="Accuracy"))
  
  # Get optimal number of feature 
  tmp <- onofDF %>% mutate(diff=abs(accuracy-max(accuracy))) %>% filter(diff<0.05) %>% head(1)
  optimalNum <- tmp[,'num']
  
  # Best classification modeling
  MySummary  <- function(data, lev = NULL, model = NULL){
    cfm <- confusionMatrix(data$pred,data$obs)
    overall <- as.data.frame(cfm$overall)
    byClass <- as.data.frame(cfm$byClass)
    out <- c(overall['Accuracy',1],mean(byClass[,'Sensitivity']),byClass[,'Sensitivity'],mean(byClass[,'Sensitivity']),byClass[,'Specificity']
             ,mean(byClass[,'Balanced Accuracy']),byClass[,"Balanced Accuracy"])
    names(out) <- c("Accuracy","Mean_Sensitivity",paste0("Sensitivity_",lev),"Mean_Specificity",paste0("Specificity_",lev),"Mean_Balanced_Accuracy"
                    ,paste0("Balanced_Accuracy_",lev))
    return(out)}
  
  trctl <- trainControl(method = 'cv',number = 3,classProbs=TRUE,summaryFunction=MySummary)
  model <- train(x = data[,seq_len(optimalNum)],y=target,method = method,trControl = trctl)
  performance <- model$results[as.numeric(rownames(model$bestTune)),]
  # Visualize the result: Sensitivity by disease
  ## pre-process for visual data frame
  byDisease <- performance %>% dplyr::select(starts_with("Sensitivity_")) 
  byDisease <- byDisease[,seq_len(length(levels(target)))]
  byDisease <- melt(byDisease)  
  byDisease <- byDisease %>% mutate(variable=as.factor(gsub("Sensitivity_","",variable)),value=round(value,4))
  colnames(byDisease) <- c("disease","sensitivity")
  
  print(ggplot(data=byDisease,aes(x=disease,y=sensitivity,fill=disease)) + geom_bar(stat="identity") +
          ggtitle(paste0(method," : Mean sensitivity at each Disease")) + 
          geom_text(aes(label=sensitivity),vjust=-0.2) + theme_bw() + 
          theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.8 )))
  
  return(performance)
}
