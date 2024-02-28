library(igraph)
library(tidyverse)
library(disparityfilter)

nodes <- read_csv("SpotifyNetworkNodes.csv")
edges <- read_csv("SpotifyNetworkEdges.csv")


#remove duplicates
nodes <- nodes %>% distinct(spotify_id, .keep_all = TRUE)

nodes$genres <- gsub("\\[", "", nodes$genres)
nodes$genres <- gsub("\\]", "", nodes$genres)

nodes$chart_hits <- gsub("\\]", "", nodes$chart_hits)
nodes$chart_hits <- gsub("\\]", "", nodes$chart_hits)

genres=nodes %>% 
  separate_rows(genres, sep = "'") %>% 
  count(genres)

# filter the genres -------------------------------------------------------
nodes_filtered <- nodes %>% 
  filter(popularity>70) %>% 
  filter(!str_detect(genres, "classic") & !str_detect(genres, "jazz"))

# Reorganizing the Dataset an Alternate Way -------------------------------

edges_with_nodes_0 <- merge(cleaned_edges, nodes_filtered, by.x = "id_0", by.y ="spotify_id", all.x = TRUE)

names(edges_with_nodes_0)[names(edges_with_nodes_0) == "name"] <- "name_0"
names(edges_with_nodes_0)[names(edges_with_nodes_0) == "followers"] <- "followers_0"
names(edges_with_nodes_0)[names(edges_with_nodes_0) == "popularity"] <- "popularity_0"
names(edges_with_nodes_0)[names(edges_with_nodes_0) == "genres"] <- "genres_0"

final_edges_with_nodes <- merge(edges_with_nodes_0, nodes, by.x = "id_1", by.y = "spotify_id", all.x = TRUE)

names(final_edges_with_nodes)[names(final_edges_with_nodes) == "name"] <- "name_1"
names(final_edges_with_nodes)[names(final_edges_with_nodes) == "followers"] <- "followers_1"
names(final_edges_with_nodes)[names(final_edges_with_nodes) == "popularity"] <- "popularity_1"
names(final_edges_with_nodes)[names(final_edges_with_nodes) == "genres"] <- "genres_1"


final_df <- final_edges_with_nodes %>%
  select(-id_0, -id_1) %>% # Remove ID columns
  select(name_0, name_1, followers_0, followers_1, popularity_0, popularity_1, genres_0, genres_1) # Reorder columns


# Create the Graph --------------------------------------------------------

# Get the unique Spotify IDs from nodes
node_ids <- unique(nodes_filtered$spotify_id)

# Identify edges to remove (any edge where either id_0 or id_1 is not in node_ids)
edges_to_remove <- !(edges$id_0 %in% node_ids) | !(edges$id_1 %in% node_ids)
cleaned_edges <- edges[!edges_to_remove, ]
network_graph <- graph_from_data_frame(d = cleaned_edges, vertices = nodes_filtered, directed = FALSE)
network_graph_simplified <- simplify(network_graph)

# getting largest component -----------------------------------------------
decomp = components(network_graph_simplified)
largest_comp = which.max(decomp$csize)
large_ids = V(network_simplified)[decomp$membership == largest_comp]
giant_comp = induced_subgraph(network_graph_simplified, large_ids)



# Pruning   ----------------------------------------------------------------

#Weighting the edges for unweighted graphs

network_simplified.adj = get.adjacency(network_graph_simplified, sparse = F)
network_simplified.2 = network_simplified.adj %*% network_simplified.adj
diag(network_simplified.2) = 0
network_simplified.2[network_simplified.adj ==0]=0

#Convert it to an edgelist
network_simplfied.g2 = graph.adjacency(network_simplified.2, weighted = TRUE)
network_simplified.weight = as.data.frame(cbind(get.edgelist(network_simplfied.g2), E(network_simplfied.g2)$weight))

names(network_simplified.weight) = c("artist1", "artist2", "weight")


# K-Core Composition ------------------------------------------------------
network.core <- coreness(network_graph_simplified,  mode = "all")
network_core_df <- data.frame(node = names(network.core), coreness = network.core)

#rename

#Serrano Method Pruning --------------------------------------------------

#Serrano method
network_weights.g = graph.edgelist(as.matrix(network_simplified.weight[,1:2]))
E(network_weights.g)$weight <- as.numeric(network_simplified.weight[,3])

E(network_weights.g)$weight

network_weights.b = backbone(network_weights.g, weights = E(network_weights.g)$weight, alpha= .5)

str(network_weights.b)



# Flexible Backbone Pruning -----------------------------------------------

mean.collabs = aggregate(network_simplified.weight$weight, list(network_simplified.weight$artist1), FUN = mean)



# Structural Similarity ---------------------------------------------------

fg <- cluster_fast_greedy(giant_comp)
plot(fg, giant_comp)

layout <- layout_with_eigen(giant_comp)


