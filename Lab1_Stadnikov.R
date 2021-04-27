# Готовый датасет

df <- read.table('C:\\Users\\stud\\Desktop\\lab\\02-birth.txt', skip = 18, header = T)
plot (df)
0.summary(df)
cor.test(df[,5],df[,6])
plot(df$Head, df$Chest)



# Сгенерированный датасет

n <- 1000
a <- c(-1,0)
r <- cbind(c(1,0.9),c(0.9,2))
require(MASS)
dat <- mvrnorm(n, a, r)
head(dat)
plot(dat)
summary(dat)
cor.test(dat[,1], dat[,2])

