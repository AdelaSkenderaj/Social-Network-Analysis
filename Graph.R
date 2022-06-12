library(igraph)
#Loads nodes and edges from csv files
nodes <- read.csv("Students.csv", header=TRUE, stringsAsFactor=FALSE, strip.white = TRUE)
head(nodes)
links <- read.csv("Links.csv",header=TRUE, stringsAsFactor=FALSE, strip.white = TRUE)
head(links)

set.seed(2)
#Create a data frame and display the information
net <- graph.data.frame(links,directed=F, nodes)
net
E(net)
V(net)
V(net)$Gender
V(net)$Grade
V(net)$Nationality

deg <- degree(net, mode = "all")
sort(deg, decreasing = TRUE)

bet <- betweenness(net)
sort(bet, decreasing = TRUE)

eigenvector <- evcent(net)$vector %>% round(2) %>% print()
sort(eigenvector, decreasing = TRUE)

transitivity <- net %>% transitivity(type = "local") %>% print()

net <- simplify(net)
layout1 <-layout.fruchterman.reingold(net)
plot(net, edge.arrow.size=.2, edge.color = "black",vertex.size = 4 ,
     vertex.label = NA, vertex.label.color = "dark red", vertex.color = "dark red", 
     layout = layout1, vertex.frame.color = "black", edge.width = 2, main = "Friendship Network")


colorsCase <- ifelse(V(net)$Gender=="M", "skyblue", "pink")
plot(net, edge.arrow.size=.2, edge.color="purple",
     vertex.color=colorsCase, vertex.frame.color="#ffffff",
     vertex.label=V(net)$Name, vertex.label.color="black", vertex.size = 5, layout = layout1, edge.width = 2)

plot(net, edge.arrow.size=.2, edge.color="black",
     vertex.color=colorsCase, vertex.frame.color="#ffffff",
     vertex.label.color="black", vertex.size = 5, layout = layout1, vertex.label = NA, edge.width = 2)
legend("bottomleft", c("Boys","Girls"), pch=21,
       col="#777777", pt.bg= c("skyblue", "pink"), pt.cex=2, cex=.8, bty="n", ncol=1)

plot(net, edge.arrow.size=.2, edge.color="purple",
     vertex.color=colorsCase, vertex.frame.color="#ffffff",vertex.label = NA,
     vertex.label.color="black", vertex.size = deg+5, layout = layout1, vertex.label = NA, edge.width = 2)

colorNat <- ifelse(V(net)$Nationality == "American", "yellow", ifelse(V(net)$Nationality =="Mexican", "red", ifelse(V(net)$Nationality =="Italian", "blue", "green" )))
plot(net, edge.arrow.size=.2, edge.color="black",
     vertex.color=colorNat, vertex.frame.color="#ffffff",
     vertex.label.color="black", vertex.size = 5, layout = layout1, vertex.label =NA, edge.width = 2)
legend("bottomleft", c("American","Mexican", "Italian", "French"), pch=21,
       col="#777777", pt.bg= c("yellow", "red", "blue", "green"), pt.cex=2, cex=.8, bty="n", ncol=1, title = "Nationality")

grade <- ifelse(V(net)$Grade == "7", "yellow", ifelse(V(net)$Grade =="8", "red", ifelse(V(net)$Grade =="9", "orange", ifelse(V(net)$Grade =="10", "purple", ifelse(V(net)$Grade =="11", "skyblue", "blue" )))))
plot(net, edge.arrow.size=.2, edge.color="black",
     vertex.color=grade, vertex.frame.color="#ffffff",
     vertex.label.color="black", vertex.size = 5, layout = layout1, vertex.label =NA, edge.width = 2)
legend("bottomleft", c("7","8", "9", "10", "11", "12"), pch=21,
       col="#777777", pt.bg= c("yellow", "red", "orange", "purple", "skyblue", "blue"), pt.cex=2, cex=.8, bty="n", ncol=1, title = "Grade")

cliques(net) #list of cliques
sapply(cliques(net),length) #clique sizes
max_cliques(net, min = NULL, max = NULL, subset = NULL, file = NULL) #finds all maximal cliques
# find all the largest cliques
a <- largest.cliques(net)
a

#Distinguish largest cliques in the graph
vcol <- rep("grey80", vcount(net))
vcol[unlist(largest_cliques(net))] <- "gold"
plot(as.undirected(net), vertex.label=NA, vertex.color=vcol, vertex.size = 4)

#Finding clusters
ceb <- cluster_edge_betweenness(net) 
ceb
plot(ceb, net, vertex.label = NA, vertex.size = 4)
length(ceb)
membership(ceb)
modularity(ceb)


