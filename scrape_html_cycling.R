### TISSOT TIMING -- SCRAPE DATA
## 2020/02/13
# Ben Day

library(tidyverse)
library(rvest)
library(tabulizer)

setwd("C:/Users/bend/OneDrive - SportNZGroup/Documents/INTELLIGENCE/DATA PROJECTS/20200128 Cycling")

start.time <- Sys.time()

## COMPETITION DISCIPLINES
# ---------------------------
# Event url
event_url <- "https://www.tissottiming.com/2020/ctrwcVI"
tissot <- "https://www.tissottiming.com"

# Get discipline pages
competition_home <- read_html(event_url)
links <- competition_home %>%
  html_nodes(".Results") %>%
  html_attr("href") %>%
  map(~ paste0(tissot, .))


## ITERATIVELY EXTRACT TABLES
# ---------------------------
## Define list of results to put results in
results_list <- vector("list", length(links))
analysis <- vector("list", length(links))
html_elements_list <- vector("list", length(links))
html_elements_list <- lapply(links, read_html)

# Iterate through pages and extract table elements, if error give NULL
for (k in 1:length(results_list)){
  
  # Extract the table from kth page
  results_list[[k]] <- tryCatch(html_table(html_elements_list[[k]]), error = function(e) NULL)
  
  # Parse country names
  c <- html_elements_list[[k]] %>%
    html_nodes("h6 img") %>%
    html_attr("alt") %>%
    str_replace_all(., "[\r\n\t]" , "") %>%
    str_replace_all(., " ", "")
    
  # Append to list of tables for kth page
    results_list[[k]] <- results_list[[k]] %>%
      append(., c)
    
  # Parse event name
  d <- html_elements_list[[k]] %>%
    html_nodes("h2 a small") %>%
    html_text()
    
  # Prepend to list the name of the discipline
    results_list[[k]] <- results_list[[k]] %>%
      prepend(., d)
    
  # Let's try to get the pdf tables as well  
    pdfs <- html_elements_list[[k]] %>% 
      html_nodes("select option")  %>%
      html_attr("value") %>%
      keep(., negate(is.na)) %>%
      str_c(., ".pdf")
      
    analysis[[k]] <- extract_tables(pdfs[[length(pdfs)]], output = "data.frame", header = TRUE) %>%
      prepend(., d)
}



## EXTRACT PAGE, DISCIPLINE NAMES FOR WRANGLING
# ----------------------------------------------
# Page names stored in first element of the list
page_names <- unlist(lapply(results_list, `[[`, 1), use.names = FALSE)

# Strip M or W from page name to get discipline
disciplines <- page_names %>%
  str_replace_all(., "Men's ", "") %>%
  str_replace_all(., "Women's ", "")


## Now run wrangling code based on the disciplin (e.g. 8 distinct disciplines, 8 different routines to deal with code)


#source(file = "event_code/tissot_keirin.R")
#source("tissot_tp.R")















## Compute time taken to scrape the regatta
end.time <- Sys.time()
time.taken <- signif(end.time - start.time, 2)
time.taken
