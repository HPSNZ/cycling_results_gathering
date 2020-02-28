## WRANGLING SCRIPT - TEAM PURSUIT
## -------------------------
## Filter dataframes from TP
cond <- lapply(str_detect(analysis, "Team Pursuit"), "[", 1)
data_tp <- analysis %>%
   keep(., unlist(cond))


## Begin dealing with dataframes

for (x in 1:length(data_tp)) {

  df <- data_tp[[x]]
  
  ## Extract team names from tables
  teams = vector(mode="character", length = 0)
  for (i in 1:(length(df) - 2)) {
    
    
    if (TRUE %in% str_detect(df[[i+2]][1,], " - ")) {
      if (TRUE %in% str_detect(df[[i+2]][3,], "Distance")) {
        df[[i+2]] <- df[[i+2]][-2,]
      }
      names(df[[i+2]]) <- df[[i+2]][2,]
      team_names <- unique(df[[i+2]][1,])
      team_names <- team_names[team_names != ""]
      team_names <- sapply(str_split(team_names, " - "), "[", 1)
      teams <- cbind(teams, team_names)
      df[[i+2]] <- df[[i+2]][-1,]
      df[[i+2]] <- df[[i+2]][-1,]
    }
    
    else {
      if (TRUE %in% str_detect(df[[i+2]][2,], "Distance")) {
        df[[i+2]] <- df[[i+2]][-1,]
      }
      teams <- cbind(teams, gsub("\\...*", "", names(df[[i+2]])))
      names(df[[i+2]]) <- df[[i+2]][1,]
      df[[i+2]] <- df[[i+2]][-1,]
    }
    
    teams <- unique(teams[teams != "Lap" & teams != "X"])
    
  }
  
  # ## Extract team names from tables
  # teams = vector(mode="character", length = 0)
  # for (i in 1:(length(df) - 1)) {
  #   
  #   if (i+2 == length(df)) {
  #     
  #     # If first row of dataframe contains distance then
  #     if (str_detect(df[[i+2]][1,], "Distance") == TRUE) {
  #       teams <- cbind(teams, gsub("\\...*", "", names(df[[i+2]])))
  #       names(df[[i + 1]]) <- as.character(df[[i+2]][1,])
  #       df[[i + 1]] <- df[[i+2]][-1,]
  #       
  #       #if (unique(lengths(df[[i+2]])) > unique(lengths(df[[i]]))) {
  #       #  df[[i + 1]] <- df[[i+2]][-1,]
  #       #}
  #     }
  #     
  #     else {
  #       
  #       if (str_detect(df[[i+2]][2,], "Distance") == TRUE) {
  #         team_names <- unique(df[[i+2]][1,])
  #         team_names <- team_names[team_names != ""]
  #         team_names <- sapply(str_split(string = team_names, pattern = " - "), "[", 1)
  #         teams <- cbind(teams, team_names)
  #         names(df[[i + 1]]) <- as.character(df[[i+2]][1,])
  #         df[[i + 1]] <- df[[i+2]][-1,]
  #       }
  #       
  #       else {
  #         teams <- cbind(teams, gsub("\\...*", "", names(df[[i+2]])))
  #       #df[[i + 1]] <- df[[i+2]][-1,]
  #       names(df[[i + 1]]) <- as.character(df[[i+2]][1,])
  #       #df[[i + 1]] <- df[[i+2]][-1,]
  #       
  #       }
  #     
  #       if (unique(lengths(df[[i+2]])) > unique(lengths(df[[i]]))) {
  #         df[[i + 1]] <- df[[i+2]][-1,]
  #       }
  #     }
  #   }
  #   
  #   else {
  #     team_names <- unique(df[[i+2]][1,])
  #     team_names <- team_names[team_names != ""]
  #     team_names <- sapply(str_split(string = team_names, pattern = " - "), "[", 1)
  #     teams <- cbind(teams, team_names)
  #     
  #     df[[i + 1]] <- df[[i+2]][-1,]
  #     names(df[[i + 1]]) <- as.character(df[[i+2]][1,])
  #     df[[i + 1]] <- df[[i+2]][-1,]
  #   }
  # }
  # 
  # teams <- unique(teams[teams != "Lap" & teams != "X"])
  
  
  for (i in 1:(length(df) - 2)) {
    
    if("Distance Time Rank" %in% names(df[[i+2]])) {
      
      df[[i+2]][,"Distance Time Rank"] <- sapply(df[[i+2]][,"Distance Time Rank"], function(x) ifelse(str_count(x, " ") > 2,
                                                                                                      remove_first_element(x), x))
      
      # For each column of the data frame, run this function
      # Splits columns by spaces
      for (k in 1:ncol(df[[i+2]])) {
        df[[i+2]][,k] <- str_split_fixed(df[[i+2]][,k], " ",
                                         1 + max(str_count(df[[i+2]][,k], " ")))
      }
      
      # Bind rows together into a big data frame
      df[[i+2]] <- as.data.frame(cbind(df[[i+2]][,1], 
                                       df[[i+2]][,2][,1], 
                                       df[[i+2]][,2][,2],
                                       df[[i+2]][,3],
                                       df[[i+2]][,4][,1], 
                                       df[[i+2]][,4][,2],
                                       df[[i+2]][,4][,3],
                                       df[[i+2]][,5]))
      
      # Rename columns
      names(df[[i+2]]) <- c("Distance 1",
                            "Time 1",
                            "Rank 1",
                            "Lap Time 1",
                            "Distance 2",
                            "Time 2",
                            "Rank 2",
                            "Lap Time 2")
      
    }
    
    else {
      
      # For each column of the data frame, run this function
      # Splits columns by spaces
      for (k in 1:ncol(df[[i+2]])) {
        df[[i+2]][,k] <- str_split_fixed(df[[i+2]][,k], " ",
                                         1 + max(str_count(df[[i+2]][,k], " ")))
      }
      
      # Bind rows together into a big data frame
      df[[i+2]] <- as.data.frame(cbind(df[[i+2]][,1], 
                                       df[[i+2]][,2][,1], 
                                       df[[i+2]][,2][,2],
                                       df[[i+2]][,3]))
      
      # Rename columns
      names(df[[i+2]]) <- c("Distance 1",
                            "Time 1",
                            "Rank 1",
                            "Lap Time 1")
      
    }
  }
  
  
  ## Append team names to dataframe
  df <- bind_cols(df[3:length(df)])
  
  df_new <- data.frame(matrix("", ncol = 5))
  
  colnames(df_new) <- c("distance",
                        "time",
                        "rank",
                        "lap time",
                        "nation")
  
  for (l in 1:length(teams)) {
    temp <- df[((l*4)-3):(l*4)] %>%
      mutate(nation = teams[l])
    
    df_new <- force_bind(df_new, temp)
  }
  
  df_new <- df_new[-1,] %>%
    mutate(event = as.character(data_tp[[x]][[1]]),
           stage = as.character(data_tp[[x]][[2]]))
  
  data_tp[[x]] <- df_new

}


## Write dataframes to xlsx
library(xlsx)
write.xlsx(data_tp[[1]], file="data_tp.xlsx", sheetName="sheet1", row.names=FALSE)
for (y in 2:length(data_tp)) {
  write.xlsx(data_tp[[y]], file="data_tp.xlsx", sheetName=paste0("sheet", y), append=TRUE, row.names=FALSE)
}



