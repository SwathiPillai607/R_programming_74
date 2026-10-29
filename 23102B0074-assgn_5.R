library(igraph)

simple_graph <- graph(edges = c(1, 2, 2, 3, 3, 4, 4, 1), directed = FALSE, n = 7)

people_graph <- graph(
  edges = c("Amy", "Ram", "Ram", "Li", "Li", "Amy", "Amy", "Li", "Kate", "Li"),
  directed = TRUE
)

degree(people_graph, mode = "all")
degree(people_graph, mode = "in")
degree(people_graph, mode = "out")

diameter(people_graph, directed = FALSE, weights = NA)

edge_density(people_graph, loops = FALSE)
ecount(people_graph) / (vcount(people_graph) * (vcount(people_graph) - 1))

reciprocity(people_graph)

closeness(people_graph, mode = "all", weights = NA)
betweenness(people_graph, directed = TRUE, weights = NA)
edge_betweenness(people_graph, directed = TRUE, weights = NA)

network_data <- read.csv(
  "https://raw.githubusercontent.com/bkrai/R-files-from-YouTube/main/networkdata.csv",
  header = TRUE
)

edge_list <- data.frame(network_data$first, network_data$second)

social_net <- graph.data.frame(edge_list, directed = TRUE)
V(social_net)$label <- V(social_net)$name
V(social_net)$degree <- degree(social_net)

hist(V(social_net)$degree)

plot(social_net)

plot(
  social_net,
  vertex.color = rainbow(52),
  vertex.size = V(social_net)$degree * 0.4,
  edge.arrow.size = 0.1,
  layout = layout.fruchterman.reingold
)

hub_scores <- hub_score(social_net)$vector
authority_scores <- authority.score(social_net)$vector

par(mfrow = c(1, 2))
set.seed(123)

plot(
  social_net,
  vertex.size = hub_scores * 30,
  main = "Hubs",
  vertex.color = rainbow(52),
  edge.arrow.size = 0.1,
  layout = layout.kamada.kawai
)

plot(
  social_net,
  vertex.size = authority_scores * 30,
  main = "Authorities",
  vertex.color = rainbow(52),
  edge.arrow.size = 0.1,
  layout = layout.kamada.kawai
)

par(mfrow = c(1, 1))

undirected_net <- graph.data.frame(edge_list, directed = FALSE)
communities <- cluster_edge_betweenness(undirected_net)
plot(communities, undirected_net)
