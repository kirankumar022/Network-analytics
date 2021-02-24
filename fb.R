library("igraph")
library(readr)
fb=read.csv("E:/Assignments/Assignments week 7/Assignments/facebook.csv")
head(fb)
fb=data.frame(fb)
star=graph.adjacency(as.matrix(fb),mode="undirected",weighted = TRUE)
plot(star)
vcount(star)
edges(star)


?degree
#indegree
indegree <- degree(star, mode = "in")
max(indegree)
indegree
#outegree
outdegree=degree(star,mode = "out")
max(outdegree)
#closeness
close=closeness(star,normalized = TRUE,mode="in")
max(close)
index <- which(close == max(close))
close[index]
# betweenness
bet=betweenness(star,normalized = TRUE)
(bet)
#centrality
cen=cbind(indegree,outdegree,close,bet)
colnames(cen) <- c("inDegree","outDegree","closenessIn","betweenness")
cen=data.frame(cen)
plot(cen$closenessIn,cen$betweenness)
#eigen vectors
eigenv <- eigen_centrality(star,directed = TRUE, scale = FALSE, weights = NULL)
eigenv$vector
max(eigenv$vector)
#page rank
pg_rank <- page_rank(star, damping = 0.999) # do not put damping=1; the solution not necessarily converges; put a value close to 1.
pg_rank$vector
max(pg_rank$vector)
?as
