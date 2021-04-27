require(MASS)
analyse_clust <- function(x, y, class) {
  k <- length(unique(class))
  clust <- kmeans(cbind(x, y), k)
  print(clust$totss)
  dev.new()
  plot(x, y, col=as.factor(class))
  dev.new()
  plot(x, y, col=as.factor(clust$cluster))
  points(clust$centers, col=1:length(clust$centers),
         pch=4, cex=2)
}
dat <- read.table('C:\\Users\\stud\\Desktop\\lab\\02-birth.txt', skip = 18, header = T)
analyse_clust(dat$Head, dat$Chest,
              as.factor(dat$Sex))
n1 <- 1000
a1 <- c(-1, -1)
r1 <- cbind(c(1, 0.9), c(0.9, 2))
n2 <- 2000
a2 <- c(2, 3)
r2 <- cbind(c(2, 0.9), c(0.9, 1))
dat <- rbind(mvrnorm(n1, a1, r1), mvrnorm(n2, a2, r2))
analyse_clust(dat[,1], dat[,2], c(rep(1, n1), rep(2,
                                                  n2)))

