require(ggplot2)
require(reshape2)
n <- 200 #将两天200等分
u_kappa=log(2)
u_beta=0.03
u_theta=0.08
x0=0.08
u_epsilon <- rnorm(n,mean=0,sd=1)
gx <- function(f_x0=x0,f_kappa=u_kappa,f_theta=u_theta,f_beta=u_beta,f_epsilon=u_epsilon,f_n=n){
  tmp <- c()
  tmp[1]=f_x0
  for (i in 1:f_n) {
    tmp[i+1] <- tmp[i]+f_kappa*(2/n)*(f_theta-tmp[i])+f_beta*f_epsilon[i]*sqrt(2/n)
  }
  tmp
}
ax1 <- gx(f_x0=0.06)
ax2 <- gx(f_x0=0.08)
ax3 <- gx(f_x0=0.12)
bk1 <- gx(f_kappa=0.17)
bk2 <- gx(f_kappa = 0.69)
bk3 <- gx(f_kappa=2.27)
ct1 <- gx(f_theta = 0.04)
ct2 <- gx(f_theta=0.08)
ct3 <- gx(f_theta=0.12)
db1 <- gx(f_beta = 0.01)
db2 <- gx(f_beta=0.03)
db3 <- gx(f_beta=0.05)
re <- data.frame(ax1,ax2,ax3,bk1,bk2,bk3,ct1,ct2,ct3,db1,db2,db3)
names(re) <- c("x0=0.06","x0=0.08","x0=0.12",
               "ka=0.17","ka=0.69","ka=2.77",
               "th=0.04","th=0.08","th=0.12",
               "be=0.01","be=0.03","be=0.05")
rem <- melt(data=re)
rem$c1 <- c(rep("x0",3*n+3),rep("ka",3*n+3),rep("th",3*n+3),rep("be",3*n+3))
rem$c1 <- factor(rem$c1,levels=c("x0","ka","th","be"))
rem$x <- rep(1:201,12)
p <- ggplot(rem,aes(x=x,y=value))
qp <- p+geom_line(aes(color=variable))+facet_wrap(~c1)
ggsave("C:\\Users\\think\\Desktop\\FixedIncomeModelling.git\\trunk\\P54.PDF",qp)
