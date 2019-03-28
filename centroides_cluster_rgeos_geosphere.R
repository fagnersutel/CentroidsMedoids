pol <- rbind(c(-180,-20), c(-60,-20), c(-180,-35), c(-100,-40), c(-160,25), c(-120,9), c(-60, 0), c(-160,-60), c(-180,-20))
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
