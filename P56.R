pdf("C:\\Users\\think\\Desktop\\FixedIncomeModelling.git\\trunk\\P56.pdf")
n <- 200;l <- 2
u_x0 <- 0.08
u_kappa <- log(2)
u_OU_beta <- 0.03
u_SQ_beta <- 0.03/sqrt(0.08)
u_theta <- 0.08
set.seed(209)
u_epsilon1 <- rnorm(n)
u_epsilon2 <- rnorm(n)
f_OU <- function(f_x0=u_x0,f_kappa=u_kappa,f_theta=u_theta,f_beta=u_OU_beta,f_epsilon=u_epsilon1,f_n=n,f_l=l){
  tmp <- c()
  tmp[1]=f_x0
  for (i in 1:f_n) {
    tmp[i+1] <- tmp[i]+f_kappa*(f_l/n)*(f_theta-tmp[i])+f_beta*f_epsilon[i]*sqrt(f_l/n)
  }
  tmp
}
f_SQ <- function(f_x0=u_x0,f_kappa=u_kappa,f_theta=u_theta,f_beta=u_SQ_beta,f_epsilon=u_epsilon1,f_n=n,f_l=l){
  tmp <- c()
  tmp[1] <- f_x0
  for (i in 1:f_n){
    tmp[i+1] <- tmp[i]+f_kappa*(f_l/n)*(f_theta-tmp[i])+f_beta*f_epsilon[i]*sqrt(f_l/n)*sqrt(tmp[i])
  }
  tmp
}
plot(f_OU(),type="l",col="blue",ylim=c(min(f_OU(),f_SQ()),max(f_OU(),f_SQ())))
lines(f_SQ(),type="l",col="red")
legend("topleft",legend=c("OU","SQRT"),col=c("blue","red"),lty=1)
title("OU vs SQRT");mtext(expression(paste("all parameters are same except beta, OU_beta=0.03,SQRT_beta=0.03/",sqrt(0.08),sep="")),3)
dev.off()

pdf("C:\\Users\\think\\Desktop\\FixedIncomeModelling.git\\trunk\\P56_2.pdf")
df1 <- f_OU(f_x0=0.06,f_epsilon = u_epsilon1)
df2 <- f_SQ(f_x0=0.06,f_epsilon = u_epsilon2)
plot(df1,type="l",col="blue",ylim=c(min(df1,df2),max(df1,df2)))
lines(df2,type="l",col="red")
legend("topleft",legend=c("OU","SQRT"),col=c("blue","red"),lty=1)
title("OU vs SQRT");mtext("all parameters are same except epsilon",3)
dev.off()