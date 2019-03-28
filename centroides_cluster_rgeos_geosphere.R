################################################################################
#                    Testes com Clusters, Centroides e Medoides                #
#                              Fagner Sutel de Moura                           #
#                                   28/03/2019                                 #
################################################################################

pol <- rbind(c(-180,-20), c(-60,-20), c(-60,-10), c(-180,-35), c(-100,-35), c(-160,25), c(-100,9), c(-60, 0), c(-160,-60), c(-180,-20))
pol = as.data.frame(pol)
names(pol) = c("X", "Y")
pol
library(geosphere)
centroid(pol) 
plot(pol, pch = 7, col = "black", xlab = 'Coordenada X', ylab = 'Coordenada Y', 
     main = 'Centróides por biblioteca')
points(centroid(pol), pch = 17, col = "red")

library(cluster)
points(pam(pol, 1)$medoids, pch = 13, col = "green")
library(rgeos)
coordinates(pol) = ~X+Y
#gCentroid(spdf[spdf$Name == 12,], byid=FALSE)
gCentroid(pol, byid=FALSE)
points(gCentroid(pol, byid=FALSE), pch = 19, col = "blue")

legend("bottomright", 
       legend = c("Geosphere", "Cluster", "rgeos"), 
       col = c("red","green","Blue"), 
       pch = c(17,13,19), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))



pol <- rbind(c(-180,-20), c(-60,-20), c(-110,-20), c(-60,-10), c(-180,-35), c(-100,-40), c(-160,25), c(-120,9), c(-60, 0), c(-160,-60), c(-180,-20))
pol = as.data.frame(pol)
names(pol) = c("X", "Y")
pol
library(geosphere)
centroid(pol) 
plot(pol, pch = 7, col = "black", xlab = 'Coordenada X', ylab = 'Coordenada Y', 
     main = 'Centróides por biblioteca')
points(centroid(pol), pch = 17, col = "red")

library(cluster)
points(pam(pol, 1)$medoids, pch = 13, col = "green")
library(rgeos)
coordinates(pol) = ~X+Y
#gCentroid(spdf[spdf$Name == 12,], byid=FALSE)
gCentroid(pol, byid=FALSE)
points(gCentroid(pol, byid=FALSE), pch = 19, col = "blue")

legend("bottomright", 
       legend = c("Geosphere", "Cluster", "rgeos"), 
       col = c("red","green","Blue"), 
       pch = c(17,13,19), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))





x <- rbind(matrix(rnorm(100, mean = 0.5, sd = 4.5), ncol = 2),
           matrix(rnorm(100, mean = 0.5, sd = 0.1), ncol = 2))
dim(x)
colnames(x) <- c("x", "y")

# using 2 clusters because we know the data comes from two groups
cl <- kmeans(x, 3) 
kclus <- pam(x,3)

par(mfrow=c(1,2))
plot(x, col = kclus$clustering, main="Kmedoids Cluster")
points(kclus$medoids, col = 1:3, pch = 10, cex = 4)
plot(x, col = cl$cluster, main="Kmeans Cluster")
points(cl$centers, col = 1:3, pch = 10, cex = 4)


pol <- x
pol = as.data.frame(pol)
names(pol) = c("X", "Y")
pol
library(geosphere)
centroid(pol) 
par(mfrow=c(1,1))
plot(pol, pch = 7, col = "black", xlab = 'Coordenada X', ylab = 'Coordenada Y', 
     main = 'Centróides por biblioteca')
points(centroid(pol), pch = 17, col = "red")

library(cluster)
points(pam(pol, 1)$medoids, pch = 13, col = "green")
library(rgeos)
coordinates(pol) = ~X+Y
#gCentroid(spdf[spdf$Name == 12,], byid=FALSE)
gCentroid(pol, byid=FALSE)
points(gCentroid(pol, byid=FALSE), pch = 19, col = "blue")

legend("bottomright", 
       legend = c("Geosphere", "Cluster", "rgeos"), 
       col = c("red","green","Blue"), 
       pch = c(17,13,19), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


################################################################################
#                    Utilizando dados do dataset de alvarás                    #
#                              Fagner Sutel de Moura                           #
#                                   28/03/2019                                 #
################################################################################



setwd("/Users/fsmoura/Documents/R-files/TCC/")
rm(list = ls(all.names = TRUE))
filenamess <- list.files(path = "/Users/fsmoura/Documents/R-files/TCC/geo/s") 
filenamess

filenamesc <- list.files(path = "/Users/fsmoura/Documents/R-files/TCC/geo/c") 
filenamesc

data1 <- do.call("rbind", lapply(paste("geo/s/", filenamess, sep = ""), read.csv, header = TRUE, sep = ";")) 
data2 <- do.call("rbind", lapply(paste("geo/c/", filenamesc, sep = ""), read.csv, header = TRUE, sep = ";")) 
data = rbind(data1, data2)
head(data1)
dim(data)

names(data)


dados <- cbind(data$long, data$lat, as.character(data$default), as.character(data$tipo))
dados <- as.data.frame(dados)
dados$V1 <- as.numeric(as.character(dados$V1))
dados$V2 <- as.numeric(as.character(dados$V2))
dados <- dados[dados$V2 < 0, ]
dados <- subset(dados, !is.na(V1))
dim(dados)


rm(data)
rm(filenamess)
rm(filenamesc)


pol <- cbind(dados$V1, dados$V2)
pol <- pol[complete.cases(pol), ]
dim(pol)


cl <- kmeans(pol, 133) 
kclus <- pam(pol[1:50000,],133)
par(mfrow=c(1,1))
plot(pol, col = kclus$clustering, main="Kmedoids Cluster")
points(kclus$medoids, col = 1:3, pch = 10, cex = 1)
plot(pol, col = cl$cluster, main="Kmeans Cluster")
points(cl$centers, col = 1:3, pch = 10, cex = 1)

cl
cl$iter



################################################################################
#                         Utilizando dados randomizados                        #
#                              Fagner Sutel de Moura                           #
#                                   28/03/2019                                 #
################################################################################



clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}

# create a simple data set with two clusters

xy <- rbind(matrix(rnorm(100, mean = 0.5, sd = 4.5), ncol = 2),
           matrix(rnorm(100, mean = 0.5, sd = 0.1), ncol = 2))
dim(xy)
colnames(xy) <- c("x", "y")

x = xy[sample(nrow(xy), 70), ]
dim(x)
x

x_new <- rbind(xy, x)
x_new = x_new[! duplicated(x_new, fromLast=TRUE) & seq(nrow(x_new)) <= nrow(xy), ]
dim(x_new)

cl <- kmeans(x, centers=10)

all.equal(cl[["cluster"]], clusters(x, cl[["centers"]]))
# [1] TRUE
clusters(x_new, cl[["centers"]])
# [1] 2 2 2 2 2 1 1 1 1 1

plot(x)
points(x_new, pch = 13, col = "green")


