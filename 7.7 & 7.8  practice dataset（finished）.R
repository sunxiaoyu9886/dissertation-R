library(igraph)

# 2.3  specific graph and graph models
eg <- make_empty_graph(40)
plot(eg, vertex.size=10, vertex.label=NA)

fg <- make_full_graph(40)
plot(fg, vertex.size=10, vertex.label=NA)

st <- make_star(40)
plot(st, vertex.size=10, vertex.label=NA) 

tr <- make_tree(40, children = 3, mode = "undirected")
plot(tr, vertex.size=10, vertex.label=NA) 

rn <- make_ring(40)
plot(rn, vertex.size=10, vertex.label=NA)

er <- sample_gnm(n=100, m=40) 
plot(er, vertex.size=6, vertex.label=NA) 

## Watts-Strogatz small-world model
# Creates a lattice (with dim dimensions and size nodes across dimension) and rewires edges randomly with probability p. The neighborhood in which edges are connected is nei. You can allow loops and multiple edges.
sw <- sample_smallworld(dim=2, size=10, nei=1, p=0.2)
plot(sw, vertex.size=6, vertex.label=NA, layout=layout_in_circle)

## Barabasi-Albert preferential attachment model for scale-free graphs
#(n is number of nodes, power is the power of attachment (1 is linear); m is the number of edges added on each time step)
ba <-  sample_pa(n=100, power=1, m=1,  directed=F)
plot(ba, vertex.size=6, vertex.label=NA)

zach <- graph("Zachary") # the Zachary carate club
plot(zach, vertex.size=10, vertex.label=NA)

rn.rewired <- rewire(rn, each_edge(prob=0.1))
plot(rn.rewired, vertex.size=10, vertex.label=NA)

rn.neigh = connect.neighborhood(rn, 5)
plot(rn.neigh, vertex.size=8, vertex.label=NA)

plot(rn, vertex.size=10, vertex.label=NA) 
plot(tr, vertex.size=10, vertex.label=NA) 
plot(rn %du% tr, vertex.size=10, vertex.label=NA) 


# 3 read network data from files
## 3.1 datasets--edgelist
setwd("C:\\Users\\sunxi\\Desktop\\dissatation\\igraph\\7.7 practice dataset")
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T) 
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
?aggregate()
colnames(links)[4] <- "weight"
rownames(links) <- NULL

## 3.2 datasets--matrix
nodes2 <- read.csv("Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)
head(nodes2)
head(links2)
links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)

# 4.turn networks into igraph objects
## 4.1 dataset 1
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
class(net)

E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$type  # Edge attribute "type"
V(net)$media # Vertex attribute "media"

plot(net, edge.arrow.size=.4,vertex.label=NA)
net <- simplify(net, remove.multiple = F, remove.loops = T) 
#  simplify(net, edge.attr.comb=list(weight="sum","ignore"))
#  E(net)$type  #combine multiple edge type 
as_edgelist(net, names=T)
as_adjacency_matrix(net, attr="weight")

as_data_frame(net, what="edges") 
as_data_frame(net, what="vertices")

# 4.2 dataset 2
head(nodes2)
head(links2)
net2 <- graph_from_incidence_matrix(links2) 
table(V(net2)$type)
net2[]
# bipartite projections for two mode network
net2.bp <- bipartite.projection(net2)
as_incidence_matrix(net2) %*% t(as_incidence_matrix(net2)) 
t(as_incidence_matrix(net2)) %*% as_incidence_matrix(net2)

plot(net2.bp$proj1, vertex.label.color="black", vertex.label.dist=1, vertex.size=7, vertex.label=nodes2$media[!is.na(nodes2$media.type)])
plot(net2.bp$proj2, vertex.label.color="black", vertex.label.dist=1, vertex.size=7, vertex.label=nodes2$media[!is.na(nodes2$media.type)])
# ´í plot(net2.bp, vertex.label.color="black", vertex.label.dist=1, vertex.size=7, vertex.label=nodes2$media[!is.na(nodes2$media.type)])

# 5.plot network
##  color our network nodes based on type of media, and size them based on audience size (larger audience -> larger node). We will also change the width of the edges based on their weight.

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Set node size based on audience size:
V(net)$size <- V(net)$audience.size*0.7

# The labels are currently node IDs.

# Setting them to NA will render no labels:
V(net)$label.color <- "black"
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"

E(net)$width <- 1+E(net)$weight/12


plot(net, edge.color="orange", vertex.color="gray50") 

plot(net)
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# 6. network and node descriptives
#6.1 density
edge_density(net, loops=F)
ecount(net)/(vcount(net)*(vcount(net)-1)) #for a directed network
?ecount()
?vcount()
#6.2 reciprocity
reciprocity(net) 
dyad_census(net) # Mutual, asymmetric, and nyll node pairs 
2*dyad_census(net)$mut/ecount(net) # Calculating reciprocity

#6.3 transitivity
  plot(net)
transitivity(net, type="global")  # net is treated as an undirected network
transitivity(as.undirected(net, mode="collapse")) # same as above
transitivity(net, type="local")
triad_census(net) # for directed networks 
?triad_census
 
# 6.4diameter
diameter(net, directed=F, weights=NA)
diameter(net, directed=F)
diam <- get_diameter(net, directed=T)
diam
net

class(diam)
as.vector(diam)

vcol <- rep("gray40", vcount(net))
vcol[diam] <- "gold"
ecol <- rep("gray80", ecount(net))
ecol[E(net, path=diam)] <- "orange" 

# E(net, path=diam) finds edges along a path, here 'diam'
plot(net, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)

#6.5  node degree
deg <- degree(net, mode="all")
plot(net, vertex.size=deg*3)
V(net)[3]
hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")
deg1 <- degree(net, mode="in")
deg2 <- degree(net, mode="out")

# 6.6 degree distribution
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

#6.7 centrality & centralization
# degree 
degree(net, mode="in")
centr_degree(net, mode="in", normalized=T)
#closeness
closeness(net, mode="all", weights=NA) 
centr_clo(net, mode="all", normalized=T) 
#eigenvector
eigen_centrality(net, directed=T, weights=NA)
centr_eigen(net, directed=T, normalized=T) 
?eigen_centrality()
# betweenness
betweenness(net, directed=T, weights=NA)
edge_betweenness(net, directed=T, weights=NA)
centr_betw(net, directed=T, normalized=T)


# 7. distance and paths
##7.1 average path length
mean_distance(net, directed=F)
mean_distance(net, directed=T)

distances(net) # with edge weights
distances(net, weights=NA) # ignore weights

## 7.2 extract the distance to a node or set of nodes
dist.from.NYT <- distances(net, v=V(net)[media=="NY Times"], to=V(net), weights=NA)

# Set colors to plot the distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.NYT)+1)
col <- col[dist.from.NYT+1]

plot(net, vertex.color=col, vertex.label=dist.from.NYT, edge.arrow.size=.6, 
     vertex.label.color="white")

## 7.3 find the shortest path between specfic nodes
news.path <- shortest_paths(net, 
                            
                            from = V(net)[media=="MSNBC"], 
                            
                            to  = V(net)[media=="New York Post"],
                            
                            output = "both") # both path nodes and edges



# Generate edge color variable to plot the path:
ecol <- rep("gray80", ecount(net))
ecol[unlist(news.path$epath)] <- "orange"

# Generate edge width variable to plot the path:
ew <- rep(2, ecount(net))
ew[unlist(news.path$epath)] <- 4

# Generate node color variable to plot the path:
vcol <- rep("gray40", vcount(net))
vcol[unlist(news.path$vpath)] <- "gold"

plot(net, vertex.color=vcol, edge.color=ecol, 
     edge.width=ew, edge.arrow.mode=0)

## 7.4 identify the edges going into or out of a vertex. 
## e.g. wall stra=eet journal
inc.edges <- incident(net,  V(net)[media=="Wall Street Journal"], mode="all")

# Set colors to plot the selected edges.
ecol <- rep("gray80", ecount(net))
ecol[inc.edges] <- "orange"
vcol <- rep("grey40", vcount(net))
vcol[V(net)$media=="Wall Street Journal"] <- "gold"

plot(net, vertex.color=vcol, edge.color=ecol)

## 7.5 identify the immediate neighbors of a vertex
neigh.nodes <- neighbors(net, V(net)[media=="Wall Street Journal"], mode="out")

# Set colors to plot the neighbors:
vcol[neigh.nodes] <- "#ff9d00"

plot(net, vertex.color=vcol)

## 7.6 index of edge sequence
# e.g. select edges from newspapers to online sources:
E(net)[ V(net)[type.label=="Newspaper"] %->% V(net)[type.label=="Online"] ]
# how many shared nominations they have
cocitation(net)

# 8.subgroups and community
net.sym <- as.undirected(net, mode= "collapse",
                         edge.attr.comb=list(weight="sum", "ignore"))

## 8.1 cliques
cliques(net.sym) # list of cliques       
sapply(cliques(net.sym), length) # clique sizes
largest_cliques(net.sym) # cliques with max number of nodes

vcol <- rep("grey80", vcount(net.sym))
vcol[unlist(largest_cliques(net.sym))] <- "gold"

plot(as.undirected(net.sym), vertex.label=V(net.sym)$name, vertex.color=vcol)

## 8.2 community detection
# Community detection based on edge betweenness (Newman-Girvan)
ceb <- cluster_edge_betweenness(net) 

dendPlot(ceb, mode="hclust")

plot(ceb, net) 

# examine the community detection
class(ceb)
length(ceb)
membership(ceb)
modularity(ceb)
crossing(ceb,net)

# Community detection based on based on propagating labels
clp <- cluster_label_prop(net)

plot(clp, net)


# Community detection based on greedy optimization of modularity
cfg <- cluster_fast_greedy(as.undirected(net))

plot(cfg, as.undirected(net))

# without based on built-in plot
V(net)$community <- cfg$membership 
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6) 
plot(net, vertex.color=colrs[V(net)$community])
?adjustcolor()


# 8.3 K-core decomposition
kc <- coreness(net, mode="all") 
plot(net, vertex.size=kc*6, vertex.label=kc, vertex.color=colrs[kc])


# 9 assortativity and hemophily
#for categorical variable
assortativity_nominal(net, V(net)$media.type, directed=F)

# for ordinal and above variables
assortativity(net, V(net)$audience.size, directed=F)

#  checks assortativity in node degrees
assortativity_degree(net, directed=F)

