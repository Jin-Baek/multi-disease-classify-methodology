## Set working directory
path <- "G:\\내 드라이브\\2021-sv\\Microbiome\\source"
metaPath <- paste0(path,"\\metadata")

## Load data 
metadata.list <- list.files(path = metaPath)

## [1] Pre-processing each metadata

# %%%%% ERP005534 %%%%%
ERP005534.metadata <- read.table(paste0(path,"\\metadata\\",metadata.list[1]),sep=",",header=T,fill=T)   

ERP005534.metadata<-ERP005534.metadata%>% 
  dplyr::select(Run,AGE,sex,diagnosis,body_product,BioProject)

ERP005534.metadata <- ERP005534.metadata %>%
  mutate(diagnosis=case_when(
    diagnosis == "Cancer" ~ "Colorectal cancer",
    diagnosis == "Normal" ~ "Healthy",
    diagnosis == "Large adenoma" ~ "Colorectal adenoma",
    diagnosis == "Small adenoma" ~ "Colorectal adenoma"
  )
)

ERP005534.metadata <- ERP005534.metadata %>%
  mutate(AGE=case_when(
    90 <= AGE & AGE < 100 ~ "90",
    80 <= AGE & AGE < 90 ~ "80",
    70 <= AGE & AGE < 80 ~ "70",
    60 <= AGE & AGE < 70 ~ "60",
    50 <= AGE & AGE < 60 ~ "50",
    40 <= AGE & AGE < 50 ~ "40",
    30 <= AGE & AGE < 40 ~ "30",
    20 <= AGE & AGE < 30 ~ "20",
    10 <= AGE & AGE < 20 ~ "10",
    1 <= AGE & AGE < 10 ~ "0"
  )
  )

ERP005534.metadata <- ERP005534.metadata %>% rename(disease=diagnosis,age=AGE)

# %%%%% ERP022287 %%%%%
ERP022287.metadata <- read.table(paste0(path,"\\metadata\\",metadata.list[2]),sep=",",header=T,fill=T)   

ERP022287.metadata<-ERP022287.metadata%>% 
  dplyr::select(Run,host_sex,host_disease_status,environment_.material.,BioProject)

ERP022287.metadata <- ERP022287.metadata %>%
  mutate(host_disease_status=case_when(
    host_disease_status == "healthy control group" ~ "Healthy",
    host_disease_status == "BV patients study group" ~ "Bacterial vaginosis"
  )
  )

ERP022287.metadata <- ERP022287.metadata %>% rename(disease=host_disease_status,sex=host_sex,body_product=environment_.material.)

# %%%%% ERP109659 %%%%%
ERP109659.metadata <- read.table(paste0(path,"\\metadata\\",metadata.list[3]),sep=",",header=T,fill=T)   

ERP109659.metadata<-ERP109659.metadata%>% 
  dplyr::select(Run,parkinson,environment_.material.,BioProject)

ERP109659.metadata <- ERP109659.metadata %>%
  mutate(parkinson=case_when(
    parkinson == "yes" ~ "Parkinson",
    parkinson == "no" ~ "Healthy"
  )
  )

ERP109659.metadata <- ERP109659.metadata %>% rename(disease=parkinson,body_product=environment_.material.)


# %%%%% ERP113090 %%%%%
ERP113090.metadata <- read.table(paste0(path,"\\metadata\\",metadata.list[4]),sep=",",header=T,fill=T)   

ERP113090.metadata<-ERP113090.metadata%>% 
  dplyr::select(Run,host_sex,Host_Age,host_disease_status,environment_.material.,BioProject)

ERP113090.metadata <- ERP113090.metadata %>%
  mutate(host_disease_status=case_when(
    startsWith(as.character(host_disease_status),"Parkinsons") ~ "Parkinson",
    host_disease_status == "healthy control" ~ "Healthy"
  )
  )

ERP113090.metadata <- ERP113090.metadata %>%
  mutate(Host_Age=case_when(
    90 <= Host_Age & Host_Age < 100 ~ "90",
    80 <= Host_Age & Host_Age < 90 ~ "80",
    70 <= Host_Age & Host_Age < 80 ~ "70",
    60 <= Host_Age & Host_Age < 70 ~ "60",
    50 <= Host_Age & Host_Age < 60 ~ "50",
    40 <= Host_Age & Host_Age < 50 ~ "40",
    30 <=Host_Age & Host_Age < 40 ~ "30",
    20 <= Host_Age & Host_Age < 30 ~ "20",
    10 <= Host_Age & Host_Age < 20 ~ "10",
    1 <= Host_Age & Host_Age < 10 ~ "0"
  )
  )

ERP113090.metadata <- ERP113090.metadata %>% rename(disease=host_disease_status,age=Host_Age,sex=host_sex,body_product=environment_.material.)

# %%%%% SRP026107 %%%%%
SRP026107.metadata <- read.table(paste0(path,"\\metadata\\",metadata.list[5]),sep=",",header=T,fill=T)   

SRP026107.metadata<-SRP026107.metadata%>% 
  dplyr::select(Run,host_sex,AGE,nugent_score,environment_material,BioProject)

SRP026107.metadata <- SRP026107.metadata %>%
  mutate(nugent_score=case_when(
    0 <= nugent_score & 3>= nugent_score ~ "Healthy",
    7 <= nugent_score & 10>= nugent_score ~ "Bacterial vaginosis"
  )
  )

SRP026107.metadata <- SRP026107.metadata %>%
  mutate(AGE=case_when(
    90 <= AGE & AGE < 100 ~ "90",
    80 <= AGE & AGE < 90 ~ "80",
    70 <= AGE & AGE < 80 ~ "70",
    60 <= AGE & AGE < 70 ~ "60",
    50 <= AGE & AGE < 60 ~ "50",
    40 <= AGE & AGE < 50 ~ "40",
    30 <= AGE & AGE < 40 ~ "30",
    20 <= AGE & AGE < 30 ~ "20",
    10 <= AGE & AGE < 20 ~ "10",
    1 <= AGE & AGE < 10 ~ "0"
  )
  )

SRP026107.metadata <- SRP026107.metadata %>% rename(disease=nugent_score,age=AGE,sex=host_sex,body_product=environment_material)


# %%%%% SRP042023 %%%%%
SRP042023.metadata <- read.table(paste0(path,"\\metadata\\",metadata.list[6]),sep=",",header=T,fill=T)   

SRP042023.metadata<-SRP042023.metadata%>% 
  dplyr::select(Run,Host_Age,Dysentery,material,BioProject)

SRP042023.metadata <- SRP042023.metadata %>%
  mutate(Dysentery=case_when(
    Dysentery == "yes" ~ "Dysentery",
    Dysentery == "no" ~ "Healthy"
  )
)

SRP042023.metadata <- SRP042023.metadata %>%
  mutate(Host_Age=case_when(
    90 <= Host_Age & Host_Age < 100 ~ "90",
    80 <= Host_Age & Host_Age < 90 ~ "80",
    70 <= Host_Age & Host_Age < 80 ~ "70",
    60 <= Host_Age & Host_Age < 70 ~ "60",
    50 <= Host_Age & Host_Age < 60 ~ "50",
    40 <= Host_Age & Host_Age < 50 ~ "40",
    30 <=Host_Age & Host_Age < 40 ~ "30",
    20 <= Host_Age & Host_Age < 30 ~ "20",
    10 <= Host_Age & Host_Age < 20 ~ "10",
    1 <= Host_Age & Host_Age < 10 ~ "0"
  )
)

SRP042023.metadata <- SRP042023.metadata %>% rename(disease=Dysentery,age=Host_Age,body_product=material)

# %%%%% SRP056779 %%%%%
SRP056779.metadata <- read.table(paste0(path,"\\metadata\\",metadata.list[7]),sep=",",header=T,fill=T)   

SRP056779.metadata<-SRP056779.metadata%>% 
  dplyr::select(Run,Host_Age,Host_disease,env_material,BioProject)

SRP056779.metadata <- SRP056779.metadata %>%
  mutate(Host_disease=case_when(
    Host_disease == "Acute Respiratory Infection" ~ "Nasopharyngeal",
    Host_disease == "Healthy" ~ "Healthy"
  )
)

SRP056779.metadata <- SRP056779.metadata %>% rename(disease=Host_disease,age=Host_Age,body_product=env_material)

## [2] Merge all metadata and pre-processing merged metadata

metaData.list <- list(ERP005534=ERP005534.metadata,ERP022287=ERP022287.metadata,ERP109659=ERP109659.metadata,
                      ERP113090=ERP113090.metadata,SRP026107=SRP026107.metadata,SRP042023=SRP042023.metadata,
                      SRP056779=SRP056779.metadata)

allMetaData <- Reduce(function(x,y) merge(x,y,all=TRUE),metaData.list)

# Remove rows which Disease value is NA 
na.row<-which(is.na(allMetaData$disease))
allMetaData <- allMetaData[-na.row,]

# Handling NA or empty 
allMetaData<- allMetaData %>% 
  mutate_if(is.character,~na_if(.,"")) %>% # Change empty data into NA 
  mutate_if(is.character,~replace(.,is.na(.),"NA")) %>% # Change AN into string NA
  mutate_if(is.character,~gsub(" ","",.)) # Remove space in meta column

allMetaData <- allMetaData %>% mutate_if(is.character,as.factor) # Change character into factor type

write.csv(allMetaData,file=paste0(path,"/mergedData/","mergedMetaData.csv"),row.names=FALSE)

