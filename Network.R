advice_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-Advice.txt')
friendship_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-Friendship.txt')
reports_to_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-ReportsTo.txt')
colnames(advice_data_frame) <- c('ego', 'alter', 'advice_tie')
head(advice_data_frame)

colnames(friendship_data_frame) <- c('ego', 'alter', 'friendship_tie')
head(friendship_data_frame)

colnames(reports_to_data_frame) <- c('ego', 'alter', 'reports_to_tie')
head(reports_to_data_frame)
fix(advice_data_frame)
fix(friendship_data_frame)
fix(reports_to_data_frame)


krack_full_data_frame <- cbind(advice_data_frame, friendship_data_frame$friendship_tie, reports_to_data_frame$reports_to_tie)
head(krack_full_data_frame)

names(krack_full_data_frame)[4:5] <- c("friendship_tie", 
                                       "reports_to_tie")  
head(krack_full_data_frame)
krack_full_nonzero_edges <- subset(krack_full_data_frame, (advice_tie > 0 | friendship_tie > 0 | reports_to_tie > 0))
head(krack_full_nonzero_edges)
library(igraph)
krack_full <- graph.data.frame(krack_full_nonzero_edges) 
summary(krack_full)
get.edge.attribute(krack_full, 'advice_tie')
get.edge.attribute(krack_full, 'friendship_tie')
get.edge.attribute(krack_full, 'reports_to_tie')
krack_full_symmetrized <- as.undirected(krack_full, mode='collapse')
summary(krack_full_symmetrized)

#############################################################################################
require(igraph)
x <- read.table("http://www.ats.ucla.edu/stat/data/mat25.txt", header = FALSE)
head(x)
network <- as.matrix(x)
g1 <- graph.adjacency(network, weighted = "3")
b1 <- betweenness(g1, directed = FALSE)
c1 <- closeness(g1, mode = "out")
d1 <- degree(g1, mode = "out")
xlist <- read.graph("http://www.ats.ucla.edu/stat/data/elist1.txt", format = "edgelist")
str(xlist)
xlist.8un <- read.graph("http://www.ats.ucla.edu/stat/data/elist1.txt", format = "edgelist",
                        n = 8, directed = FALSE)
plot.igraph(xlist)
g2 <- graph(c(1, 2, 2, 3, 2, 4, 2, 5, 4, 6, 5, 7, 7, 5))
str(g2)
plot.igraph(g1)

DMC_net <- read.csv("~/Medicare Referral/Data/2014_9_18_Development/network.csv", header = TRUE)
network <- as.matrix(DMC_net[1:20,2:21])
g_dmc<- graph.adjacency(network, mode = "undirected",weight = "3")
V(g_dmc)$color <- c(rep("yellow", 10),rep("green", 10))
plot.igraph(g_dmc, layout=layout.fruchterman.reingold,
                    vertex.size=c(a,rep(5,10)),         # sets size of the vertex, default is 15
                    vertex.label.cex=0.7,    # size of the vertex label
                    edge.arrow.size=.5,
                    mark.shape = c(1,2))
legend("topright", c("Employed Physicians", "Unemployed Physicians Contributors"), pch = c(19,19) , cex = c(0.7, 0.7),col = c("green", "yellow"))

#############################################################################################
require(igraph)
Specialty_Net <- read.csv("~/Medicare Referral/Data/2014_9_18_Development/2014_10_8)Specialty_NetworkDMC.csv", header = TRUE)
network <- as.matrix(Specialty_Net[1:15,2:16])
network2 <- as.matrix(Specialty_Net[16:31,17:32])
colSums(network)
g_specialty <- graph.adjacency(network2, mode = "undirected", weight = "1")
plot.igraph(g_specialty, layout=layout.circle,
            vertex.label.cex=0.7,    # size of the vertex label
            edge.arrow.size=.1,
            mark.shape = c(1,2))

################################## James Wudel 2015/4/28 ###################################
library(igraph)
James_Wudel <- read.csv("~/Medicare Referral/Data/Physician_Compare_Databases/2015_4_24_NE_enhancement/James_Wudel.csv", header = TRUE)
James_Wudel_2 <- read.csv("~/Medicare Referral/Data/Physician_Compare_Databases/2015_4_24_NE_enhancement/James_Wudel.csv", header = TRUE)
Legend <- as.matrix(read.csv("~/Medicare Referral/Data/Physician_Compare_Databases/2015_4_24_NE_enhancement/INPUT.csv", header = TRUE))
JW <- as.matrix(James_Wudel_2)
JW_network <- graph.adjacency(JW, mode = "undirected", weighted = TRUE) 
V(JW_network)$shape <- c("circle", tolower(Legend[,2]))
V(JW_network)$color <- c("Yellow", tolower(Legend[,3]))
E(JW_network)$weight <- as.numeric(Legend[,1])/50

plot(JW_network, mark.shape = c(1,2),vertex.label.cex=0.9, layout= layout.auto, edge.width = E(JW_network)$weight,edge.color = "black")
legend("topright")

rglplot(a)
layout.auto(graph, dim=2, ...)
layout.random(graph, params, dim=2)
layout.circle(graph, params)
layout.sphere(graph, params)
layout.fruchterman.reingold(graph, ..., dim=2, params)
layout.kamada.kawai(graph, ..., dim=2, params)
layout.spring(graph, ..., params)
layout.reingold.tilford(graph, ..., params)
layout.fruchterman.reingold.grid(graph, ..., params)
layout.lgl(graph, ..., params)
layout.graphopt(graph, ..., params=list())
layout.svd(graph, d=shortest.paths(graph), ...)
layout.norm(layout, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL,
            zmin = NULL, zmax = NULL)
