## WRANGLING SCRIPT - KEIRIN
## -------------------------
## Filter dataframes from K
cond <- lapply(str_detect(analysis, "Keirin"), "[", 1)
data_k <- analysis %>%
  keep(., unlist(cond))




## Begin dealing with dataframes

for (x in 1:length(data_k)) {
  
  df1 <- data_k[[x]]
  

  for (i in 1:(length(df1) - 2)) {
    
    df <- df1[[i + 2]]
    
    # Check if the dataframe is right size for results
    if (ncol(df) > 5) {
    
      # Finals or heats
      if (TRUE %in% str_detect(df[,1], "Final")) {
        
        df <- df %>% mutate(newcol = NA) %>%
          select(X, newcol, everything())
        
        for (j in 1:length(df$X)){
          df$newcol[j] <- ifelse(nchar(df$X[j]) < 3, 
                                 df$X[j],
                                 "")
          df$X[j] <- ifelse(nchar(df$X[j]) < 3, 
                            "",
                            df$X[j])
        }
        
        
      
      # Rename columns
      names(df) <- c("race",
                     "rank",
                     "bib",
                     "name",
                     "X.3",
                     "X.4",
                     "nation",
                     "timediff")
      
      # First assign heat # to each observation
      df$race[df$race == ""] <- NA
      df <- df %>% fill(race, .direction = "down")
      df$race[df$race == "Final 1-6"] <- 1
      df$race[df$race == "Final 7-12"] <- 2
      #df$race[df$race == "Final 13-18"] <- 3
      df$race <- as.numeric(df$race)
      
      } else {
        
        # Rename columns
        names(df) <- c("race",
                       "rank",
                       "bib",
                       "name",
                       "X.3",
                       "X.4",
                       "nation",
                       "timediff")
      
        # First assign heat # to each observation
        df$race[df$race == ""] <- NA
        df <- df %>% fill(race, .direction = "down")
        df$race <- as.numeric(str_replace(string = df$race, pattern = "Heat ", replacement = ""))
      
      }
      
      
      # Pull out stats from last lap
      stats <- df$X.4[df$X.4 != ""]
      speed <- stats %>% keep(., str_detect(., "km/h") == TRUE)
      lastlaptime <- stats[stats %not_in% speed]
      speed <- speed %>% str_replace_all("km/h", "") %>%
        str_replace_all(",", ".")
      
      # Now remove blank lines and columns
      df <- df[-1,]; df <- df[-1,]
      df <- df[df$rank != "",]
      df <- df %>% select(-X.3, -X.4)
      
      # Compute actual last lap time
      df$laptime <- as.numeric(sub('+', "", df$timediff))
      for (k in 1:length(df$laptime)) {
        df$laptime[k] <- ifelse(df$timediff[k] == "",
                                as.numeric(lastlaptime[df$race[k]]),
                                df$laptime[k] + as.numeric(lastlaptime[df$race[k]]))
      }

      # Finished dataframes
      df_new <- df %>%
        mutate(event = as.character(data_k[[x]][[1]]),
               stage = as.character(data_k[[x]][[2]]))
      
      data_k[[x]] <- df_new
      
    }
  }
  
  
  # Write dataframes to xlsx
  library(xlsx)
  # write.xlsx(data_k[[1]], file="data/data_k.xlsx", sheetName="sheet1", row.names=FALSE)
  # for (y in 2:length(data_k)) {
  #   write.xlsx(data_k[[y]], file="data/data_k.xlsx", sheetName=paste0("sheet", y), append=TRUE, row.names=FALSE)
  # }
  
  all <- do.call(rbind, data_k) %>%
    select(event, stage, everything()) %>%
    mutate(rank = as.numeric(rank))
  
  write.xlsx(all, file="data/data_k.xlsx", sheetName="data", row.names=FALSE)

}



