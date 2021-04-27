require(class)
require(MASS)
require(rgl)


plot_points <- function(train, test, clazz.train,
                        clazz.test) {
  rgl.open()
  plot3d(train[, 1], train[, 2], train[, 3],
         col=clazz.train, type='p', size=5, add=FALSE)
  plot3d(test[, 1], test[, 2], test[, 3],
         col=clazz.test, type='s', size=1, add=TRUE)
}

analyse_knn <- function(dat, clazz) {
  n <- nrow(dat)
  rnd.num <- sample(1 : n)
  train.num <- rnd.num[1 : (n %/% 2)]
  test.num <- rnd.num[(n %/% 2 + 1) : n]
  train <- dat[train.num,]
  test <- dat[test.num,]
  clazz.train <- clazz[train.num]
  clazz.test <- clazz[test.num]
  clazz.knn <- knn(train, test, clazz.train)
  print(sum(clazz.test != clazz.knn) /
          length(clazz.test))
  plot_points(train, test, clazz.train, clazz.test)
  plot_points(train, test, clazz.train, clazz.knn)
}



dat <- read.table('C:\\Users\\stud\\Desktop\\lab\\02-birth.txt', skip = 18, header = T)
analyse_knn(cbind(dat$Head, dat$Chest,
                  dat$Length), unclass(dat$Sex))


df  <- cbind(dat$Head, dat$Chest, dat$Length)
n <- nrow(df)
rnd.num <- sample(1 : n)
train.num <- rnd.num[1 : (n %/% 2)]
test.num <- rnd.num[(n %/% 2 + 1) : n]
train <- df[train.num,]
test <- df[test.num,]
clazz.train <- dat$Sex[train.num]
clazz.test <- dat$Sex[test.num]
clazz.knn <- knn(train, test, clazz.train)
print(sum(clazz.test != clazz.knn) /
        length(clazz.test))



n1 <- 1000
a1 <- c(-2, 0, 2)
r1 <- cbind(c(2, 1, 1), c(1, 4, 1.4), c(1, 1.4, 2))
n2 <- 2000
a2 <- c(4, 6, 8)
r2 <- cbind(c(2, 1, 1), c(1, 2, 1.4), c(1, 1.4, 4))
dat <- rbind(mvrnorm(n1, a1, r1), mvrnorm(n2, a2, r2))
analyse_knn(dat, c(rep(1, n1), rep(2, n2)))
