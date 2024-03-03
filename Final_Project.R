library(igraph)   
library(tidyverse)
library(sandwich)
library(disparityfilter)
library(lmtest)

nodes <- read_csv("SpotifyNetworkNodes.csv")
edges <- read_csv("SpotifyNetworkEdges.csv")


#remove duplicates
nodes <- nodes %>% distinct(spotify_id, .keep_all = TRUE)

nodes$genres <- gsub("\\[", "", nodes$genres)
nodes$genres <- gsub("\\]", "", nodes$genres)

nodes$chart_hits <- gsub("\\]", "", nodes$chart_hits)
nodes$chart_hits <- gsub("\\]", "", nodes$chart_hits)




# FILTER OUT GENRES -------------------------------------------------------


nodes_filtered <- nodes %>% 
  filter(popularity>50) %>%  # i think we need to remove this populatiry filter
  filter(!str_detect(genres, "classical") & !str_detect(genres, "jazz"))



genres=nodes_filtered%>% 
  separate_rows(genres, sep = ",") %>% 
  count(genres)
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


# Add collaborations ------------------------------------------------------

vertex_names <- V(network_graph_simplified)$name
vertex_degrees <- degree(network_graph_simplified)
degree_df <- data.frame(vertex= vertex_names, degree = vertex_degrees)

degree_df <-degree_df %>% 
  select(vertex, degree) 

nodes_filtered <- merge(nodes_filtered, degree_df, by.x="name", by.y = "vertex", all.x= T)

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
collabs.prune = collabs.weight[collabs.weight$z1 > 2.5 | collabs.weight$z2 > 2.5,]
collabs.prune = collabs.prune[!is.na(collabs.prune$z1), 1:3]

collabsgraph.prune <- graph.data.frame(collabs.prune)

plot(collabsgraph.prune)

V(collabsgraph.prune) # 457 vertices w/ 2, 1.3, 183 vertices w/ 2, 2. 401 vertices with 1.5, 1.5. 1 and 1 give 522 vert



# STRUCTURAL SIMILAIRITY--NOT COMPLETE ---------------------------------------------------

#adjacency matrix to dataframe
collabs.prune_adjacency <- as_adjacency_matrix(collabsgraph.prune, sparse = FALSE)
write.csv(collabs.prune_adjacency, "Collabs.Prune_Adjacency")

adjacency_df <-read_csv("Collabs.Prune_Adjacency")
colnames(adjacency_df)[1] <- "name"

#Matrix of just each artists properties
adjacency_df_refined <- merge(adjacency_df, nodes_filtered, by.x= "name", by.y = "name", all.x = TRUE) %>% 
  select(name,followers,popularity, genres, chart_hits, degree)

#change row names to artists
rownames(adjacency_df_refined) <- adjacency_df_refined$name
#now remove the "name" row


adjacency_df_final <- adjacency_df_refined %>% 
  select(followers, popularity, genres, degree)

adjacency_df_final <- adjacency_df_final %>% 
  mutate(genres = str_replace_all(genres, "'", "")) %>% 
  mutate(main_genre = map_chr(str_split(genres, pattern = ","), ~.x[1])) 



# Trying to map -----------------------------------------------------------
genre_mapping <- list(
  "Hip Hop/Rap" = c("atl hip hop", "cali rap", "detroit trap", "east coast hip hop", "emo rap", "melodic rap", "gangster rap",
                    "desi hip hop", "canadian hip hop", "hip pop", "uk hip hop", "detroit hip hop", "chicago rap", "north carolina hip hop",
                    "dmv rap", "kentucky hip hop", "conscious hip hop", "deep underground hip hop", "boston hip hop", "trap latino", "hip hop",
                    "drill brasileiro", "chicago drill", "brooklyn drill", "drill francais", "memphis hip hop", "drill chileno", "melodic drill", "minnesota hip hop",
                    "trap chileno", "crunk", "german drill", "dfw rap", "rap canario", "trap argentino", "dirty south rap", "miami hip hop", "grime", "london rap", "italian hip hop", "french hip hop",
                    "rap rock", "baton rouge rap", "florida rap", "christlicher rap", "francoton", "ohio hip hop", "trap carioca", "nigerian pop",
                    "hawaiian hip hop", "trap colombiano", "australian hip hop", "brazilian hip hop", "puerto rican pop", "uk hip hop",
                    "drill espanol", "german hip hop", "chicago bop","dark trap", "lgbtq+ hip hop", "argentine hip hop", "manchester hip hop"),
  "Pop" = c("desi pop", "scandipop","boy band", "deep talent show", "canadian latin", "latin pop", "dance pop", "canadian pop", "k-pop", "pop rap", "pop argentino", "colombian pop",
            "australian pop", "pop venezolano", "indie pop rap", "pop rap brasileiro", "uk contemporary r&b", "spanish pop",
            "barbadian pop", "panamanian pop", "filmi", "modern bollywood", "cancion melodica", "alt z", "pop"),
  "Electronic/Dance" = c("big room", "deep groove house", "electro house", "dutch trance", "edm", "belgian edm", "deep house", "electropop", "dembow",
                         "trap boricua", "glam rock", "emo", "brostep", "complextro", "australian dance", "brazilian edm", "electro latino"),
  "Latin Music" = c("latin arena pop","cantautor", "mexican hip hop", "banda", "reggaeton", "latin hip hop", "bachata", "trap brasileiro", "funk carioca", "reggaeton flow",
                    "urbano espanol", "musica mexicana", "champeta", "flamenco urbano", "corrido", "salsa", "sertanejo", "corridos tumbados",
                    "cumbia 420", "mariachi", "norteno", "reggaeton", "trap latino"),
  "R&B/Soul" = c("canadian contemporary r&b", "neo mellow", "r&b en espanol", "afro r&b", "alternative r&b", "uk contemporary r&b"),
  "Country/Folk" = c("contemporary country", "folk-pop", "arrocha", "pagode"),
  "Rock/Alternative" = c("modern rock","metropopolis", "piano rock", "alternative rock", "british indie rock", "indie poptimism", "alternative metal", "art pop",
                         "permanent wave", "modern rock", "irish rock", "alternative metal"),
  "World/Regional Music" = c("g funk", "bhangra", "desi", "afro dancehall", "musica mexicana", "champeta", "k-pop", "pagode",
                             "samba", "fado", "flamenco", "regional mexican", "brazilian edm", "afrofuturism", "bollywood","filmi", "j-pop", "c-pop", "kwaito", "afrobeats", "highlife", "soca", "calypso", "reggae", "dancehall", "brazilian hip hop", "funk carioca", "samba", "forro", "axe", "brega", "sertanejo", "pagode", "arab pop", "turkish pop", "greek pop", "balkan trap", "russian hip hop", "uk drill", "grime", "afroswing"),
  "Experimental/Other" = c("escape room", "speedrun", "black americana", "christlicher rap", "lgbtq+ hip hop", "hollywood", "alternative r&b", "neo mellow", "indie poptimism", "art pop", "chillwave", "hyperpop", "vaporwave", "future bass", "pc music", "witch house"),
  "Funk/Soul/Disco" = c("funk", "funk carioca", "soul", "disco", "motown", "northern soul", "southern soul", "funk rock", "psychedelic soul"),
  "Jazz/Blues" = c("jazz", "smooth jazz", "jazz fusion", "blues", "rhythm and blues", "soul blues", "jazz blues", "swing", "bebop", "vocal jazz"),
  "Classical/Instrumental" = c("classical", "instrumental", "orchestra", "symphony", "chamber music", "solo instrumental", "piano", "violin", "cello", "guitar", "classical crossover", "opera"),
  "Metal" = c("metal", "heavy metal", "death metal", "black metal", "thrash metal", "metalcore", "power metal", "symphonic metal", "gothic metal", "folk metal", "viking metal"),
  "Punk/Hardcore" = c("punk", "hardcore punk", "pop punk", "punk rock", "skate punk", "hardcore", "post-hardcore", "emo", "screamo"),
  "Reggae/Dancehall" = c("reggae", "dancehall", "reggaeton", "dub", "roots reggae", "reggae fusion", "ska", "rocksteady"),
  "Electronic" = c("electronic", "house", "techno", "trance", "dubstep", "drum and bass", "electro", "edm", "ambient", "downtempo", "electronic rock", "industrial", "synth-pop", "eurodance"),
  "World/Folk" = c("world", "folk", "ethnic", "traditional", "worldbeat", "folklore", "celtic", "native american", "african", "latin", "asian", "middle eastern", "balkan", "scandinavian", "andean", "indian classical", "arab classical"),
  "Blues/Rock" = c("blues", "rock", "classic rock", "hard rock", "blues rock", "southern rock", "folk rock", "psychedelic rock", "glam rock", "garage rock", "punk blues", "rockabilly", "surf rock"),
  "Soundtracks/Themes" = c("soundtrack", "film score", "video game music", "tv music", "musical", "disney", "broadway", "hollywood", "theme", "score", "cinematic"),
  "Miscellaneous" = c("children's music", "comedy", "spoken word", "educational", "novelty", "holiday", "religious", "gospel", "spiritual", "healing", "meditation", "new age", "environmental", "nature", "sound effects")
)


# Mapping -----------------------------------------------------------------


map_genre_to_category <- function(genre) {
  for (category in names(genre_mapping)) {
    if (genre %in% unlist(genre_mapping[category])) {
      return(category)
    }
  }
  return("Other") # Default category if no match is found
}




adjacency_df_final$genre_category <- sapply(adjacency_df_final$main_genre, map_genre_to_category)

adjacency_df_final <- adjacency_df_final %>% 
  mutate(Hip_hop_Rap = ifelse(genre_category == "Hip Hop/Rap", 1, 0),
         Pop = ifelse(genre_category == "Pop", 1, 0),
         Electronic_Dance= ifelse(genre_category == "Electronic/Dance", 1, 0),
         Latin_Music = ifelse(genre_category == "Latin Music", 1, 0),
         RnB_Soul = ifelse(genre_category == "R&B/Soul", 1, 0),
         Country= ifelse(genre_category == "Country/Folk", 1, 0),
         Rock= ifelse(genre_category == "Rock/Alternative", 1, 0),
         World_Regional = ifelse(genre_category == "World/Regional Music", 1, 0),
         Experimental = ifelse(genre_category == "Experimental/Other", 1, 0),
         Funk_soul_disco = ifelse(genre_category == "Funk/Soul/Disco", 1, 0),
         Other=ifelse(genre_category == "Other", 1, 0)
  ) %>% 
  mutate(followers = followers/100000000,
         popularity = popularity/100, 
         collaborations = degree/100)

adjacency_df_final <- adjacency_df_final %>% 
  select(-genres, -main_genre, -degree, -genre_category)






#run euclidean distance
collabs.dist <- dist(adjacency_df_final, method = "euclidean")

#clusters
hc <- hclust(collabs.dist, method = "single")
c <- cutree(hc, k =11)
table(c) #seems k level of 11 gives good clusters

#add clusters to data and analyze
clusters_data <-adjacency_df_final %>% 
  mutate(name = adjacency_df_refined$name,
         cluster = c,
         genre_category = adjacency_df_final$genre_category) %>% 
  select(name, cluster, popularity, followers)


cluster_averages <- aggregate(cbind(popularity, followers) ~ cluster, data = clusters_data, mean)


#next step look at whether the same process but only with popularity and followers

adj_df_pop_foll <- adjacency_df_final %>% 
  select(followers, popularity, collaborations) 

collabs.dist_2 <- dist(adj_df_pop_foll, method = "euclidean")

#clusters
hc_2 <- hclust(collabs.dist_2, method = "single")
c_2 <- cutree(hc_2, h =5)
table(c_2) 

#RESULT: Hierarchical clustering is not a good measure to use

# Some Hypothesis Tests ---------------------------------------------------
adj_df_with_names <- rownames_to_column(adjacency_df_final, var = "name")

collaborations_detailed <- collabs.prune %>% 
  select(-weight, artist1, artist2) %>% 
  left_join(adj_df_with_names, by = c("artist1" = "name")) %>%
  rename(
    followers_1 = followers, 
    popularity_1 = popularity, 
    HipHop_Rap_1 = Hip_hop_Rap, 
    Pop_1 = Pop, 
    Electronic_Dance_1 = Electronic_Dance, 
    Latin_Music_1 = Latin_Music, 
    RnB_Soul_1 = RnB_Soul, 
    Country_1 = Country, 
    Rock_1 = Rock, 
    World_Regional_1 = World_Regional, 
    Experimental_1 = Experimental, 
    Funk_Soul_Disco_1 = Funk_soul_disco, 
    Other_1 = Other, 
    Collaborations_1 = collaborations
  )  %>% 
  # Join for artist_2
  left_join(adj_df_with_names, by = c("artist2" = "name")) %>%
  rename(
    followers_2 = followers, 
    popularity_2 = popularity, 
    HipHop_Rap_2 = Hip_hop_Rap, 
    Pop_2 = Pop, 
    Electronic_Dance_2 = Electronic_Dance, 
    Latin_Music_2 = Latin_Music, 
    RnB_Soul_2 = RnB_Soul, 
    Country_2 = Country, 
    Rock_2 = Rock, 
    World_Regional_2 = World_Regional, 
    Experimental_2 = Experimental, 
    Funk_Soul_Disco_2 = Funk_soul_disco, 
    Other_2 = Other, 
    Collaborations_2 = collaborations
  )

collaborations_detailed <- collaborations_detailed %>% 
  select(artist1, artist2, Collaborations_1, Collaborations_2, followers_1, followers_2, popularity_1, popularity_2, HipHop_Rap_1, HipHop_Rap_2, Pop_1, Pop_2, 
         Electronic_Dance_1, Electronic_Dance_2, Latin_Music_1, Latin_Music_2, RnB_Soul_1, RnB_Soul_2, Country_1, Country_2, 
         Rock_1, Rock_2, World_Regional_1, World_Regional_2, Experimental_1, Experimental_2, Funk_Soul_Disco_1, Funk_Soul_Disco_2, Other_1, 
         Other_2) %>% 
  mutate(Collaborations_1 = Collaborations_1*100,
         Collaborations_2 = Collaborations_2*100)



# Linear and Logistic Regressions -----------------------------------------

#linear regression checking if artists that frequenty collaboratate are also popular
collabs_correl = lm(Collaborations_1 ~popularity_1, collaborations_detailed)
summary(collabs_correl)


#logistic regression checking if artist who frequently collaborate tend to be in hip hop or pop
nodes.attr = unique(cbind(collaborations_detailed$artist1,collaborations_detailed$Collaborations_1, collaborations_detailed$followers_1, collaborations_detailed$popularity_1,
                          collaborations_detailed$HipHop_Rap_1, collaborations_detailed$Pop_1))
nodes.attr <- as.data.frame(nodes.attr)
names(nodes.attr) <- c("Artist1", "Collaborations1", "Followers1", "Popularity1", "HipHop_Rap1", "Pop1")

nodes.attr$Collaborations1 <- as.numeric(nodes.attr$Collaborations1)
nodes.attr$HipHop_Rap1 <- as.numeric(nodes.attr$HipHop_Rap1)
nodes.attr$Pop1 <- as.numeric(nodes.attr$Pop1)
nodes.attr$Popularity1 <- as.numeric(nodes.attr$Popularity1)
nodes.attr$Followers1 <- as.numeric(nodes.attr$Followers1)

#Collaborate 
collabs_logit <- glm(HipHop_Rap1~Pop1, data = nodes.attr, family = binomial)

coeftest(collabs_logit, vcov = vcovHAC)


#logitisic regression checking if artist who frequenctly collaborate 
collabs_logit_popularity <- glm(Pop)



#add clusteres to the data set
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
