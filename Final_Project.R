


library(igraph)
library(tidyverse)

nodes <- read_csv("SpotifyNetworkNodes.csv")

nodes$genres <- gsub("\\[", "", nodes$genres)
nodes$genres <- gsub("\\]", "", nodes$genres)

nodes$chart_hits <- gsub("\\]", "", nodes$chart_hits)
nodes$chart_hits <- gsub("\\]", "", nodes$chart_hits)



genres = nodes %>% 
  separate_rows(genres, sep = ",") %>%
  count(genres)

chart_hits = nodes %>% 
  separate_rows(chart_hits, sep = ", ") %>% 
  count(chart_hits)



  
#Next Steps

#Combine nodes and edges into one dataset
#clean chart hits
#filter to top 600-800
#filter to pop and rap 

#centrality tests
#structural similarity
#bridges #edge betweeness 




