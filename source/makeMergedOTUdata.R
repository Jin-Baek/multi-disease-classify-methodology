OTU.table <- function(dataPath,taxa){
  data.list <- list.files(path = dataPath)
  result <- list()
  study.name <- c()
  
  for(i in 1:length(data.list)){
    ## Load dataset
    data <- fread(paste0(dataPath,"/",data.list[i]),header=T)
    
    ## Formatting origin data set to apply phyloseq package
    origin <- data
    origin<-origin%>%
      mutate(otu=paste0("OTU",1:length(rownames(origin))))
    
    origin <- origin[,-1]
    
    origin <- origin%>%
      tibble::column_to_rownames("otu")
    
    origin <- as.matrix(origin)
    origin <- otu_table(origin,taxa_are_rows = TRUE)
    
    ## Formatting taxonomy table data set
    tax.table <- data %>%
      separate(col ="#SampleID",sep=";",into=c("Domain","Kingdom","Phylum","Class","Order","Family","Genus","Species"))
    
    tax.table <- tax.table%>%
      separate(col=Domain,sep = "__",into = c(NA,"Domain"))%>%
      separate(col=Kingdom,sep = "__",into = c(NA,"Kingdom"))%>%
      separate(col=Phylum,sep = "__",into = c(NA,"Phylum"))%>%
      separate(col=Class,sep = "__",into = c(NA,"Class"))%>%
      separate(col=Order,sep = "__",into = c(NA,"Order"))%>%
      separate(col=Family,sep = "__",into = c(NA,"Family"))%>%
      separate(col=Genus,sep = "__",into = c(NA,"Genus"))%>%
      separate(col=Species,sep = "__",into = c(NA,"Species"))
    
    tax.table <- tax.table%>%
      mutate(otu=paste0("OTU",1:length(rownames(tax.table))))
    
    tax.table <- tax.table%>%
      tibble::column_to_rownames("otu")
    
    tax.table <- as.matrix(tax.table)
    tax.table <- tax_table(tax.table)
    tax.table <- tax.table[,1:8]
    tax.table <- gsub(pattern = "[]]|[[]",replacement = "",x=tax.table)
    tax.table <- tax.table %>%
      na_if("")
    
    ## Apply phyloseq 
    carbom <- phyloseq(origin,tax.table)
    taxglom = tax_glom(carbom,taxa)
    
    Genus_id <- c(taxglom@tax_table[,taxa])
    Genus_taxo <- as.data.frame(taxglom@otu_table@.Data)
    merge_Genus <- cbind(Genus_id,Genus_taxo)
  
    ## Merge all the duplicate bacteria
    merge_Genus<-merge_Genus%>%
      group_by(Genus_id)%>%
      dplyr::summarise(dplyr::across(everything(),sum))
    merge_Genus <- as.data.frame(merge_Genus)
    
    ## Make data format into Sample X Gene data.frame
    rownames(merge_Genus) <- merge_Genus$Genus_id
    merge_Genus <- as.data.frame(t(merge_Genus[,-1]))
    
    Run <- rownames(merge_Genus)
    merge_Genus <- merge_Genus %>% mutate(Run=Run)
    rownames(merge_Genus) <- 1:nrow(merge_Genus)
    
    study.name <- append(study.name,unlist(strsplit(data.list[i],split="_"))[1])
    result[[i]] <- merge_Genus
  }
  #result <- setNames(result,study.name)
  result <- Reduce(function(x,y) merge(x,y,all=TRUE),result)
  
  ## Replace NA into 0
  result <- result %>%
    mutate_if(is.numeric,~replace(.,is.na(.),0))
  
  write.csv(result,file=paste0(path,"/mergedData/","mergedOTU",taxa,".csv"),row.names=FALSE)
  return(result)
}
