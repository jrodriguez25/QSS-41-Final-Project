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

# FILTER OUT GENRES -------------------------------------------------------
nodes_filtered <- nodes %>% 
  filter(popularity>70) %>% 
  filter(!str_detect(genres, "classic") & !str_detect(genres, "jazz"))

# CREATE GRAPH --------------------------------------------------------

# Get the unique Spotify IDs from nodes
node_ids <- unique(nodes_filtered$spotify_id)

# Identify edges to remove (any edge where either id_0 or id_1 is not in node_ids)
edges_to_remove <- !(edges$id_0 %in% node_ids) | !(edges$id_1 %in% node_ids)
cleaned_edges <- edges[!edges_to_remove, ]
network_graph <- graph_from_data_frame(d = cleaned_edges, vertices = nodes_filtered, directed = FALSE)


#SIMPLIFIED GRAPH
network_graph_simplified <- simplify(network_graph)


# LARGEST COMPONENT -----------------------------------------------
decomp = components(network_graph_simplified)
largest_comp = which.max(decomp$csize)
large_ids = V(network_graph_simplified)[decomp$membership == largest_comp]
giant_comp = induced_subgraph(network_graph_simplified, large_ids)

# PRUNING   ----------------------------------------------------------------


# K-Core Composition ------------------------------------------------------
network.core <- coreness(network_graph_simplified,  mode = "all")
network_core_df <- data.frame(node = names(network.core), coreness = network.core)


#Weighting the edges for unweighted graphs
network_simplified.adj = get.adjacency(network_graph_simplified, sparse = F)
network_simplified.2 = network_simplified.adj %*% network_simplified.adj
diag(network_simplified.2) = 0
network_simplified.2[network_simplified.adj ==0]=0

#Convert it to an edgelist
network_simplfied.g2 = graph.adjacency(network_simplified.2, weighted = TRUE)
network_simplified.weight = as.data.frame(cbind(get.edgelist(network_simplfied.g2), E(network_simplfied.g2)$weight))
#rename
names(network_simplified.weight) = c("artist1", "artist2", "weight")



#Serrano Method Pruning NOT COMPLETE--------------------------------------------------

#Serrano method
network_weights.g = graph.edgelist(as.matrix(network_simplified.weight[,1:2]))
E(network_weights.g)$weight <- as.numeric(network_simplified.weight[,3])

E(network_weights.g)$weight

network_weights.b = backbone(network_weights.g, weights = E(network_weights.g)$weight, alpha= .5)

str(network_weights.b)



# Flexible Backbone Pruning NOT COMPLETE -----------------------------------------------




# STRUCTURAL SIMILAIRITY--NOT COMPLETE ---------------------------------------------------


#want to set up similar distance matrix as slides
euclidean_matrix <- as.matrix(dist(network_simplified.adj, method = "euclidean"))
euclidean_matrix <- cbind()


#looking at communities
fg <- cluster_fast_greedy(giant_comp)
plot(fg, giant_comp)

layout <- layout_with_eigen(giant_comp)


