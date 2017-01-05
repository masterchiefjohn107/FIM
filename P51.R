pdf(file="C:\\Users\\think\\Desktop\\FixedIncomeModelling.git\\trunk\\P51.pdf")
x <- c()
y <- c()
x[1] <- 100
y[1] <- 100
mu <- 0.1/200
sigma1 <- 0.2/sqrt(200)
sigma2 <- 0.5/sqrt(200)
epsilon <- rnorm(200)
for (i in 1:200) {
  x[i+1] <- x[i]+mu * x[i] + sigma1 * x[i] * epsilon[i]
  y[i+1] <- y[i]+mu * y[i] + sigma2 * y[i] * epsilon[i]
}
plot(c(0,200),c(100,110),type="l",ylim = c(min(y),max(y)))
lines(0:200,x,type="l",col="red")
lines(0:200,y,type="l",col="blue")
grid()
legend("topleft",legend=expression(paste(sigma,"=0",sep=""),paste(sigma,"=0.2",sep=""),paste(sigma,"=0.5",sep="")),lty=1,col=c("black","red","blue"))
dev.off()