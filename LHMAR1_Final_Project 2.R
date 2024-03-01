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
<<<<<<< HEAD:Final_Project.R
  filter(popularity>70) %>%  # i think we need to remove this populatiry filter
=======
  # filter(popularity>70) %>%  # i think we need to remove this populatiry filter
>>>>>>> ad3c3a0296ad75ef38ef1f97e86eab04f927d93e:LHMAR1_Final_Project 2.R
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
V(network_graph_simplified) # 151678 vertices

# LARGEST COMPONENT -----------------------------------------------
decomp = components(network_graph_simplified)
largest_comp = which.max(decomp$csize)
large_ids = V(network_graph_simplified)[decomp$membership == largest_comp]
giant_comp = induced_subgraph(network_graph_simplified, large_ids)
V(giant_comp) # this is only 136287 vertices
# PRUNING   ----------------------------------------------------------------

# K-Core Composition ------------------------------------------------------
network.core <- coreness(network_graph_simplified,  mode = "all")
network_core_df <- data.frame(node = names(network.core), coreness = network.core)

<<<<<<< HEAD:Final_Project.R

=======
#Serrano Method Pruning NOT COMPLETE--------------------------------------------------

#Serrano method
network_weights.g = graph.edgelist(as.matrix(network_simplified.weight[,1:2]))
E(network_weights.g)$weight <- as.numeric(network_simplified.weight[,3])

E(network_weights.g)$weight

network_weights.b = backbone(network_weights.g, weights = E(network_weights.g)$weight, alpha= .5)

str(network_weights.b)
>>>>>>> ad3c3a0296ad75ef38ef1f97e86eab04f927d93e:LHMAR1_Final_Project 2.R



# Flexible Backbone Pruning COMPLETE -----------------------------------------------
#paths of distance 2
collabs.adj = get.adjacency(network_graph_simplified, sparse = F)
collabs.2 = collabs.adj %*% collabs.adj
diag(collabs.2) = 0
collabs.2[collabs.adj == 0] = 0

#Convert it to an edgelist
collabs.g2 = graph.adjacency(collabs.2, weighted = TRUE)
collabs.weight = as.data.frame(cbind(get.edgelist(collabs.g2), E(collabs.g2)$weight))

#rename
names(collabs.weight) = c("artist1", "artist2", "weight")
collabs.weight$weight <- as.numeric(collabs.weight$weight) # make numeric

#Merge the mean and sd of collaborations for each artist1 to the edgelist
mean.collabs = aggregate(collabs.weight$weight, list(collabs.weight$artist1), FUN = mean)
collabs.weight = merge(collabs.weight, mean.collabs, by.x = "artist1", by.y = "Group.1")
names(collabs.weight)[4] = "mean.collabs1"
sd.collabs = aggregate(collabs.weight$weight, list(collabs.weight$artist1), FUN = sd)
collabs.weight = merge(collabs.weight, sd.collabs, by.x = "artist1", by.y = "Group.1")
names(collabs.weight)[5] = "sd.collabs1"

#Generate z1 – the normalized weight of each edge from the perspective of artist1
collabs.weight$z1 = (collabs.weight$weight-collabs.weight$mean.collabs1)/collabs.weight$sd.collabs1

#Same as for artist1, but for artist2
mean.collabs = aggregate(collabs.weight$weight, list(collabs.weight$artist2), FUN = mean)
collabs.weight = merge(collabs.weight, mean.collabs, by.x = "artist2", by.y = "Group.1")
names(collabs.weight)[7] = "mean.collabs2"
sd.collabs = aggregate(collabs.weight$weight, list(collabs.weight$artist2), FUN = sd)
collabs.weight = merge(collabs.weight, sd.collabs, by.x = "artist2", by.y = "Group.1")
names(collabs.weight)[8] = "sd.collabs2"

#Generate z1 – the normalized weight of each edge from the perspective of artist1
collabs.weight$z2 = (collabs.weight$weight-collabs.weight$mean.collabs2)/collabs.weight$sd.collabs2

#We can now write our own rules in terms of z-scores. -> we need to mess around with these weight!!
collabs.prune = collabs.weight[collabs.weight$z1 > 1 | collabs.weight$z2 > 1,]
collabs.prune = collabs.prune[!is.na(collabs.prune$z1), 1:3]

collabsgraph.prune <- graph.data.frame(collabs.prune)
plot(collabsgraph.prune)
V(collabsgraph.prune) # 457 vertices w/ 2, 1.3, 183 vertices w/ 2, 2. 401 vertices with 1.5, 1.5. 1 and 1 give 522 vert
<<<<<<< HEAD:Final_Project.R

# STRUCTURAL SIMILAIRITY--NOT COMPLETE ---------------------------------------------------

#adjacency matrix
=======

# STRUCTURAL SIMILAIRITY--NOT COMPLETE ---------------------------------------------------
>>>>>>> ad3c3a0296ad75ef38ef1f97e86eab04f927d93e:LHMAR1_Final_Project 2.R

collabs.prune_adjacency <- as_adjacency_matrix(collabsgraph.prune, sparse = FALSE)

# #adding followers and popularity to adjacency matrix
# colnames(collabs.prune_adjacency)[1] <- "Artist"
# collabs.prune_adjacency$followers <- nodes_filtered$followers
# collabs.prune_adjacency$popularity <- nodes_filtered$popularity

<<<<<<< HEAD:Final_Project.R

# #change all numbers to be between 0 and 1
# collabs.prune_adjacency$followers <- (collabs.prune_adjacency$followers - min(collabs.prune_adjacency$followers)) / (max(collabs.prune_adjacency$followers) - min(collabs.prune_adjacency$followers))
# collabs.prune_adjacency$popularity <- (collabs.prune_adjacency$popularity - min(collabs.prune_adjacency$popularity)) / (max(collabs.prune_adjacency$popularity) - min(collabs.prune_adjacency$popularity))
=======
#looking at communities
fg <- cluster_fast_greedy(giant_comp)
plot(fg, giant_comp)
>>>>>>> ad3c3a0296ad75ef38ef1f97e86eab04f927d93e:LHMAR1_Final_Project 2.R

collabs.dist <- dist(collabs.prune_adjacency, method = "euclidean", diag = TRUE, upper = TRUE)
distance_matrix_df <- as.data.frame(as.matrix(collabs.dist))

<<<<<<< HEAD:Final_Project.R
#Hierarchical Clustering from class

collabs.dend = hclust(collabs.dist, method = "single")
plot(collabs.dend)





=======
>>>>>>> ad3c3a0296ad75ef38ef1f97e86eab04f927d93e:LHMAR1_Final_Project 2.R
# BRIDGES AND WEAK TIES -----------------------------------------------------------

decomp = components(collabsgraph.prune)
largest_comp = which.max(decomp$csize)
members = V(collabsgraph.prune)[decomp$membership == largest_comp]
giant_comp = induced_subgraph(collabsgraph.prune, members)
bridges(giant_comp) # no strict bridges
print(V(giant_comp)) # gives 456 vertices


#looking at local bridges
prunedcollabs.adj = as_adj(collabsgraph.prune, sparse = F)
prunedcollabs.adj.2 = prunedcollabs.adj %*% prunedcollabs.adj
diag(prunedcollabs.adj.2) = 0
prunedcollabs.adj.2[prunedcollabs.adj.2 == 0 & prunedcollabs.adj == 1] = -1
prunedcollabs.adj.2[prunedcollabs.adj.2 != -1] = 0
prunedcollabs.adj.2 = prunedcollabs.adj.2 * -1
bridgegraph = graph.adjacency(prunedcollabs.adj.2)
bridgegraph.edge = get.data.frame(bridgegraph)

#count appearances in either to or from column
counts <- table(c(bridgegraph.edge$from, bridgegraph.edge$to))
print(counts)/2

#test: getting rid of NAs: dom't need rn storage spaceeeeeee
collabs.weight$weight[!is.na(collabs.weight$weight)]
collabs.weight$weight <- as.numeric(collabs.weight$weight)

collabs.weight$artist1[!is.na(collabs.weight$artist1)]
collabs.weight$artist2[!is.na(collabs.weight$artist2)]
