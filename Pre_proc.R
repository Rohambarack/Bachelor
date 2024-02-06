#### Preprocess Data

##libraries
library(tidyverse)

# When loading transcripts, they often present ABAAAB structures. We need to
# sure that all successive utterances are by different speakers
# so we merge successive utterances by the same speaker
# N.B. make sure it doesn't cross session/task boundaries

merge_adjacent <- function(DfMerge){
  
  
  
  
  keep = 1
  
  
  
  
  while (keep == 1) {
    
    DataT1 = DfMerge
    
    DfMerge = NULL
    
    k = 1
    
    l1 = length(DataT1$Speaker)
    
    
    
    if (l1 > 1) {
      
      while (k < length(DataT1$Speaker)) {
        
        if (DataT1$Speaker[k] != DataT1$Speaker[k + 1]) {
          
          DfMerge = rbind(DfMerge, DataT1[k, ])
          
          k = k + 1
          
        } else if (DataT1$Speaker[k] == DataT1$Speaker[k + 1]) {
          
          x = DataT1[k, ]
          
          x$Transcript = paste(DataT1$Transcript[k],DataT1$Transcript[k + 1], sep = " ")
          
          x$EndTime = DataT1$EndTime[k + 1]
          
          DfMerge = rbind(DfMerge, x)
          
          k = k + 2
          
        }
        
      }
      
      if (DataT1$Speaker[l1 - 1] != DataT1$Speaker[l1]) {
        
        DfMerge = rbind(DfMerge, DataT1[l1, ])
        
      }
      
    }
    
    
    
    if (length(DfMerge$Speaker) == length(DataT1$Speaker)) {
      
      keep = 0
      
    }
    
  }
  
  
  
  
  for (rep in seq(10)) {
    
    DfMerge$Transcript <- gsub(" "," ",DfMerge$Transcript)
    
  }
  
  
  
  
  DfMerge$Turn <- seq(nrow(DfMerge))
  
  return(DfMerge)
  
}

####### another function for the whole Dataset

merge_adjacent_fullDF <- function(DF){
  for  (i in unique(DF$Pair)){
    for (j in unique(DF$Session)){
      
      #change names to fit o.g. function
      DF_filter <- DF %>% 
        select(!Image) %>% 
        mutate( Speaker = Interlocutor , # if reused change according to column name 
                EndTime = endtime, # if reused change according to column name 
                Transcript = Transcription) %>%  # if reused change according to column name
        filter( Pair == i, # 1 session of 1 pair at a time
                Session == j)
      
      #run function, 1 at a time
      if (i == 1 && j == 1 ){
        
        DF_fin <- merge_adjacent(DF_filter)
        
      } else {
        
        DF_temp <- merge_adjacent(DF_filter)
        
        #merge
        DF_fin <- rbind(DF_fin,DF_temp)
      }
    }
  }
  return(DF_fin)
}

######## Import data
setwd(".")
DF1_RAW <- read_tsv("data.tsv")

#remove negative duration and surrounding turns

Df_negless <- DF1_RAW %>% 
  mutate(across(latency, ~ifelse(lag(Duration) < 0 |
                                   Duration < 0 |
                                   lead(Duration) < 0, NA, latency)))
Df_negless <- Df_negless %>% 
  filter(is.na(latency)==F)

length(DF1_RAW$latency)-length(Df_negless$latency)
#421 were removed

#concatinate
Df_negless_con <- merge_adjacent_fullDF(Df_negless)

#cut to length
Df_cut <- Df_negless_con %>%
  filter( latency > -2) %>% 
  filter(latency < 8)

#inspect
Df_cut %>% 
  ggplot(aes(x=latency))+
  geom_density()

#save
write.csv(Df_cut,"data_p.csv")
