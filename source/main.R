## Set repositories 
setRepositories(ind = 1:8)

## Set directory
path <- "/disk3/bijb/microbiome/source"
setwd(path)
getwd()

## Load library
library(lubripack)
lubripack('phyloseq','data.table','dplyr','tidyr','rgl','factoextra','clValid','clusterSim','fossil','onewaytests','car','report',
          'nnet','cluster','devtools','umap','Rtsne','gridExtra','grid','multcomp','DescTools','stringr','caret','limma','edgeR',
          'parallel','doParallel','iterators','janitor')

# Set parallel
detectCores()
ci <- makeCluster(16)
registerDoParallel(ci)

# makeMergedOTUdata.R
dataPath <- paste0(path,"/samples")
source('makeMergedOTUdata.R')
OTU.table(dataPath,taxa="Genus")
OTU.table(dataPath,taxa="Family")
OTU.table(dataPath,taxa="Order")
OTU.table(dataPath,taxa="Class")
OTU.table(dataPath,taxa = "Phylum")

# makeMergedMetadata.R
source('makeMergedMetadata.R',encoding = "utf-8")

# makeTotalData.R
meta <- fread(paste0(path,"/mergedData/mergedMetaData.csv"),header=T)
otu <- fread(paste0(path,"/mergedData/mergedOTUGenus.csv"),header=T)
source('makeTotalData.R')
matchWithSampleID(meta,otu,"Genus")
# Performing all at once by taxa level.
totalMetaData <- fread(paste0(path,"/totalData/","totalMetaData.csv"),header=T,stringsAsFactors = TRUE)
totalOTUGenus <- fread(paste0(path,"/totalData/","totalOTUGenus.csv"),header=T)

# Find substratification
source('findSubStratification.R')
Genus_ARItable <- drNclusterARI(totalOTUGenus,totalMetaData,taxa="Genus",method=2,rep=30,visual=FALSE)
save(Genus_ARItable,file=paste0(getwd(),"/object/Genus_ARItable.RData"))
load(paste0(path,"/object/Genus_ARItable.RData"))

Genus_covariate <- hyposAssumption(Genus_ARItable,"disease")
save(Genus_covariate,file=paste0(getwd(),"/object/Genus_covariate.RData"))
load(paste0(path,"/object/Genus_covariate.RData"))

# Generalized linear modeling
source('glmModeling.R')
Genus_modelresult <- regressWithCovaAdjmt(totalOTUGenus,totalMetaData,Genus_covariate)
save(Genus_modelresult,file=paste0(getwd(),"/object/Genus_modelresult.RData"))
load(paste0(getwd(),"/object/Genus_modelresult.RData"))

# Classification 
source('classification.R') 
Genus_clftKNN <- clftWithONOF(data=Genus_modelresult$LogCPM,target=totalMetaData$disease,featureImpt=Genus_modelresult$TotalFTest[,'F',drop=FALSE],method="kknn")
save(Genus_clftKNN,file=paste0(getwd(),"/object/Genus_clftKNN.RData"))
load(paste0(getwd(),"/object/Genus_clftKNN.RData"))

stopCluster(ci)