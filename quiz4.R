mean.and.sd = function(x) { return(c(mean=mean(x), sd=sd(x))) } 
mean.and.sd(c(1, 2, 3, 4, 5))
lapply(X=mtcars, FUN=sd)
sapply(X=mtcars, FUN=mean)

scoreCar = function(mpg, hp) { return(20*mpg + hp) }
m = mtcars
#score = scoreCar(m$mpg,m$hp)
#m$score = score
#m
#w <- which(m$score == 743)
#w
score = mapply(scoreCar,m$mpg,m$hp)
m$score = score
which.max(m$score)

m = matrix(data=1:6, nrow=2, ncol=3)
m
rowSums(m)

rowStandardDeviations = function(x) { standard_deviation = apply(X=x, MARGIN=1, FUN=sd); return(standard_deviation) } 
rowStandardDeviations(m)

head(iris)
?iris
tapply(X=iris$Petal.Length, INDEX=iris$Species, FUN=mean)

h = function(x, main="", xlab="", ylab="", ...) {
  hist(x=x, main=main, xlab=xlab, ylab=ylab)
}
hist(mtcars$mpg);
h(mtcars$mpg);
h(mtcars$mpg, main="mileages", breaks=seq(from=0, to=40, by=5))

