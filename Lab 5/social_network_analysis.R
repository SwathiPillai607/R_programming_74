# Load package
library(igraph)

# 1. Basic network creation
g <- make_graph(
  c(1, 2, 2, 3, 3, 4, 4, 1),
  directed = FALSE,
  n = 7
)

g

# 2. Directed network
g1 <- graph(
  c(
    "Amy", "Ram",
    "Ram", "Li",
    "Li", "Amy",
    "Amy", "Li",
    "Kate", "Li"
  ),
  directed = TRUE
)

g1

# 3. Network measures
# Degree of each node
degree(g1, mode = "all")

# In-degree
degree(g1, mode = "in")

# Out-degree
degree(g1, mode = "out")

# Diameter
diameter(g1, directed = FALSE, weights = NA)

# Edge density
edge_density(g1, loops = FALSE)

# Alternative density calculation
ecount(g1) / (vcount(g1) * (vcount(g1) - 1))

# Reciprocity
reciprocity(g1)

# Closeness centrality
closeness(g1, mode = "all", weights = NA)

# Betweenness centrality
betweenness(g1, directed = TRUE, weights = NA)

# Edge betweenness
edge_betweenness(g1, directed = TRUE, weights = NA)

# 4. Import network dataset
data <- read.csv(
  "https://raw.githubusercontent.com/bkrai/R-files-from-YouTube/main/networkdata.csv",
  header = TRUE
)

# View first few records
head(data)

# Convert data into edge list
y <- data.frame(data$first, data$second)

# Display dimensions
dim(data)

# 5. Create social network
net <- graph_from_data_frame(
  y,
  directed = TRUE
)

# Assign node labels
V(net)$label <- V(net)$name

# Calculate degree
V(net)$degree <- degree(net)

# Display network information
net

# Number of nodes
vcount(net)

# Number of edges
ecount(net)

# 6. Histogram of node degree
hist(
  V(net)$degree,
  main = "Histogram of Node Degree",
  xlab = "Degree",
  ylab = "Frequency"
)

# 7. Basic network visualization
plot(
  net,
  main = "Social Network"
)

# 8. Network with degree-based node size
plot(
  net,
  vertex.size = V(net)$degree * 0.4,
  edge.arrow.size = 0.1,
  layout = layout_with_fr(net),
  main = "Social Network - Degree Based"
)

# 9. Hub and authority scores
hs <- hub_score(net)$vector

as <- authority_score(net)$vector

# Display hub scores
hs

# Display authority scores
as

# 10. Hub visualization
par(mfrow = c(1, 2))

set.seed(123)

plot(
  net,
  vertex.size = hs * 30,
  main = "Hubs",
  edge.arrow.size = 0.1,
  layout = layout_with_kk(net)
)

# 11. Authority visualization
plot(
  net,
  vertex.size = as * 30,
  main = "Authorities",
  edge.arrow.size = 0.1,
  layout = layout_with_kk(net)
)

par(mfrow = c(1, 1))

# 12. Community detection
# Convert network to undirected
net_undirected <- graph_from_data_frame(
  y,
  directed = FALSE
)

# Detect communities using edge betweenness
cnet <- cluster_edge_betweenness(net_undirected)

# Display communities
cnet

# Plot communities
plot(
  cnet,
  net_undirected,
  main = "Community Detection"
)

# Final results
cat("Number of Nodes:", vcount(net), "\n")
cat("Number of Edges:", ecount(net), "\n")
cat("Network Diameter:", diameter(net, directed = TRUE, weights = NA), "\n")
cat("Edge Density:", edge_density(net), "\n")
cat("Reciprocity:", reciprocity(net), "\n")

cat("\nDegree of Nodes:\n")
print(degree(net))

cat("\nTop 10 Nodes by Degree:\n")
print(sort(degree(net), decreasing = TRUE)[1:10])

cat("\nTop 10 Nodes by Betweenness:\n")
print(sort(betweenness(net), decreasing = TRUE)[1:10])

cat("\nTop 10 Hub Scores:\n")
print(sort(hub_score(net)$vector, decreasing = TRUE)[1:10])

cat("\nTop 10 Authority Scores:\n")
print(sort(authority_score(net)$vector, decreasing = TRUE)[1:10])

cat("\nNumber of Communities:\n")
print(length(cnet))