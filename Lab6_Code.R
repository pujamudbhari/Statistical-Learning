library(ISLR)
nci.data = NCI60$data
nci.labs = NCI60$labs

pr.out = prcomp(nci.data, scale=TRUE)

pr.data =pr.out$x[, 1:5]
pr.data

km.out= kmeans(pr.data, centers=4, nstart=20)
km.clusters = km.out$cluster

km.clusters
