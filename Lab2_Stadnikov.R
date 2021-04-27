analyse_regression <- function(x, y) {
  model <- lm(y ~ x)
  print(summary(model))
  dev.new()
  plot(x, y)
  abline(model)
}
dat <- read.table('C:\\Users\\stud\\Desktop\\lab\\02-birth.txt', skip = 18, header = T)
result1  <- analyse_regression(dat$Head, dat$Chest)
n <- 1000
a <- -2
b <- 0.5
s2 <- 0.1
x <- seq(0.0, 1.0, length=n)
y <- a * x + b + rnorm(n, 0, sqrt(s2))
result2  <-  analyse_regression(x, y)

