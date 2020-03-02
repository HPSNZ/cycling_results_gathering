## WRANGLING SCRIPT - TEAM SPRINT
## -------------------------
## Filter dataframes from TS
cond <- lapply(str_detect(analysis, "Team Sprint"), "[", 1)
data_ts <- analysis %>%
   keep(., unlist(cond))


## Begin dealing with dataframes

for (x in 1:length(data_ts)) {

  df <- data_ts[[x]]
  
  # Dirty data hack
  #if (x == 4) {df[[3]][2,4] <- "125m"}
  
  ## Extract team names from tables
  teams = vector(mode="character", length = 0)
  for (i in 1:(length(df) - 2)) {
    
    
    if (TRUE %in% str_detect(df[[i+2]][1,], " - ")) {
      
      if (TRUE %in% str_detect(df[[i+2]][3,], "Distance")) {
        df[[i+2]] <- df[[i+2]][-2,]
      }
      
      if (TRUE %in% str_detect(df[[i+2]][2,], "Distance")) {
        names(df[[i+2]]) <- df[[i+2]][2,]
        team_names <- unique(df[[i+2]][1,])
        team_names <- team_names[team_names != ""]
        team_names <- sapply(str_split(team_names, " - "), "[", 1)
        teams <- cbind(teams, team_names)
        df[[i+2]] <- df[[i+2]][-1,]
        df[[i+2]] <- df[[i+2]][-1,]
      }
      
      else {
        # Get team names from df[1,1]
        team_names <- df[[i+2]][1,1] %>%
          sub("Lap", "", .) %>% 
          sub("Distance", "", .) %>% 
          sub("Time", "", .) %>%
          str_split(" ") %>%
          unlist()
        team_names <- team_names[team_names != "-" & team_names!= ""] %>%
          keep(str_length(.) == 3)
        teams <- cbind(teams, team_names)
      }
      
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
  

  
  
  for (i in 1:(length(df) - 2)) {
    
    if("Distance Time Rank" %in% names(df[[i+2]])) {
      
      if(min(str_count(df[[i+2]][,"Distance Time Rank"], " ") < 2)) {
        
        df[[i+2]][,"Distance Time Rank"] <- sapply(df[[i+2]][,"Distance Time Rank"], 
                                                   function(x) ifelse(str_count(x, " ") > 0, remove_first_element(x), x))
        df[[i+2]][,"Distance Time Rank"] <- paste(df[[i+2]][,"Distance Time Rank"], "-", "-", sep = " ")
        
      }
      else {
        df[[i+2]][,"Distance Time Rank"] <- sapply(df[[i+2]][,"Distance Time Rank"], function(x) ifelse(str_count(x, " ") > 2,
                                                                                                     remove_first_element(x), x))
      }
      
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
      
      # Then fix dataframe
      df[[i+2]][1,1] <- "Distance"
      df[[i+2]][,5] <- sapply(df[[i+2]][,5], function(x) ifelse(str_count(x, " ") > 0, remove_first_element(x), x))
      df[[i+2]] <- df[[i+2]] %>% mutate_all(as.character)
      names(df[[i+2]]) <- df[[i+2]][1,]
      names(df[[i+2]])[is.na(names(df[[i+2]]))] <- "na"
      df[[i+2]] <- df[[i+2]][-1,]
      
      for (k in 1:ncol(df[[i+2]])) {
        df[[i+2]][,k] <- str_split_fixed(df[[i+2]][,k], " ", 1 + max(str_count(df[[i+2]][,k], " "), na.rm = TRUE))
      }
      df[[i+2]] <- df[[i+2]][,-2]; df[[i+2]] <- df[[i+2]][,-6]
      names(df[[i+2]])[names(df[[i+2]]) == "na" | names(df[[i+2]]) == "na.1"] <- "Rank"
      
      # Bind rows together into a big data frame
      df[[i+2]] <- as.data.frame(cbind(df[[i+2]][,1][,1], 
                                       df[[i+2]][,1][,2],
                                       df[[i+2]][,2],
                                       df[[i+2]][,3],
                                       df[[i+2]][,4], 
                                       df[[i+2]][,5],
                                       df[[i+2]][,6],
                                       df[[i+2]][,7]))
      # Rename columns
      names(df[[i+2]]) <- c("Distance 1",
                            "Time 1",
                            "Rank 1",
                            "Lap Time 1",
                            "Distance 2",
                            "Time 2",
                            "Rank 2",
                            "Lap Time 2")
      
      
      
      # For each column of the data frame, run this function
      # Splits columns by spaces
      # for (k in 1:ncol(df[[i+2]])) {
      #   df[[i+2]][,k] <- str_split_fixed(df[[i+2]][,k], " ",
      #                                    1 + max(str_count(df[[i+2]][,k], " ")))
      # }
      # 
      # # Bind rows together into a big data frame
      # df[[i+2]] <- as.data.frame(cbind(df[[i+2]][,1], 
      #                                  df[[i+2]][,2][,1], 
      #                                  df[[i+2]][,2][,2],
      #                                  df[[i+2]][,3]))
      # 
      # # Rename columns
      # names(df[[i+2]]) <- c("Distance 1",
      #                       "Time 1",
      #                       "Rank 1",
      #                       "Lap Time 1")
      
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
    mutate(event = as.character(data_ts[[x]][[1]]),
           stage = as.character(data_ts[[x]][[2]]))
  
  data_ts[[x]] <- df_new

}


all <- do.call(rbind, data_ts) %>%
  select(event, stage, everything()) %>%
  mutate(time = as.numeric(time),
         rank = as.numeric(rank),
         distance = as.numeric(sub("m", "", distance)))
  


## Write dataframes to xlsx
library(xlsx)

write.xlsx(all, file = "data/data_ts.xlsx", sheetName = "data", row.names=FALSE)



