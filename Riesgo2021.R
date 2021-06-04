###Riesgo2021

###Distribuciones

library(ggfortify)
library(ggplot2)
library(gridExtra)
library(actuar)
library(fitdistrplus)
library(psych)
library(ADGofTest)
library(BB)
library(MASS)
library(evir)
library(evd)
library(ismev)
library(vcd)
library(ChainLadder)
library(lattice)
library(mixtools)
library(mclust)
library(mixdist)
library(eva)
library(MASS)
library(VaRES)
library(boot)

#####################################################################
####### Gráficas distribuciones de riesgo #########################

x=seq(0,1,.01)
q=dbeta(x,2.2,1.7)
z=dbeta(x,3.3,1.2)
df=data.frame(x,q,z)
t=ggplot(df, aes(x)) +                       # basic graphical object
       geom_line(aes(y=q), colour="red") +   # first layer
       geom_line(aes(y=z), colour="green")+  # second layer
       xlab("X value")+ylab("PDF")+
       ggtitle("Beta Probability Distribution  Functions")



x_lower <- 0
x_upper <- 6

max_height2 <- max(dexp(x_lower:x_upper, rate = 1, log = FALSE), 
                   dexp(x_lower:x_upper, rate = 2, log = FALSE),
                   dexp(x_lower:x_upper, rate = 0.5, log = FALSE),
                   dexp(x_lower:x_upper, rate = 3, log = FALSE))

max_height2

ggplot(data.frame(x = c(x_lower, x_upper)), aes(x = x)) + xlim(x_lower, x_upper) + 
  ylim(0, max_height2) +
  stat_function(fun = dexp, args = list(rate = 0.5), aes(colour = "0.5")) + 
  stat_function(fun = dexp, args = list(rate = 1), aes(colour = "1")) + 
  stat_function(fun = dexp, args = list(rate = 2), aes(colour = "2")) +
  stat_function(fun = dexp, args = list(rate = 3), aes(colour = "3")) + 
  scale_color_manual("Rate", values = c("#CC0033", "#FF00CC","#FF0000","#9933FF")) +
  labs(x = "\n x", y = "f(x) \n", title = "Distribución Exponencial \n") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 8),
        axis.title.y = element_text(face="bold", colour="blue", size = 8),
        legend.title = element_text(face="bold", size = 8),
        legend.position = "top")


#"#FF0000", "#0000FF", "#00FF00"

###Exponencial

p10<-ggplot(data.frame(x = c(0,6)), aes(x)) + 
  mapply(function(rate, col) {
    stat_function(fun = dexp, args = list(rate = rate), col = col,lwd=1)
  }, 
  rate = c(0.5,1,2,3), 
  col = c("#CC0033", "#FF00CC", "#FF0000","#9933FF")
)

p10+ 
  scale_color_manual("Rate", values = c("#CC0033", "#FF00CC","#FF0000","#9933FF")) +
  labs(x = "\n x", y = "f(x) \n", title = "Distribución Exponencial \n") 



p <- ggdistribution(dexp, seq(0, 6, 0.1), rate = 0.5, colour = "#CC0033")
p <- ggdistribution(dexp, seq(0, 6, 0.1), rate = 1, colour = "#FF00CC", p = p)
p <- ggdistribution(dexp, seq(0, 6, 0.1), rate = 2, colour = "#FF0000", p = p)
p<-ggdistribution(dexp, seq(0, 6, 0.1), rate = 3, colour = "#9933FF", p = p)
p

###Gamma

p0<-ggplot(data.frame(x = c(0,35)), aes(x)) + 
  mapply(function(shape, scale, col) {
    stat_function(fun = dgamma, args = list(shape = shape, scale = scale), col = col,lwd=1)
  }, 
  shape = c(1.3, 1.8, 3, 7), 
  scale = c(2, 2, 2, 2), 
  col = c("#CC0033", "#FF00CC", "#FF0000","#9933FF")
)

p0<-p0+labs(x = "\n x", y = "f(x) \n", title = "Distribución gamma: Cambios en el parámetro de forma")

p0


p00<-ggplot(data.frame(x = c(0,40)), aes(x)) + 
  mapply(function(shape, scale, col) {
    stat_function(fun = dgamma, args = list(shape = shape, scale = scale), col = col,lwd=1)
  }, 
  shape = c(3, 3, 3, 3), 
  scale = c(1.2, 2, 3.2, 5.2), 
  col = c("#CC0033", "#FF00CC", "#FF0000","#9933FF")
)

p00<-p00+labs(x = "\n x", y = "f(x) \n", title = "Distribución gamma: Cambios en el parámetro de escala")


grid.arrange(p0, p00,ncol=2)

###Weibull

p1<-ggplot(data.frame(x = c(0,40)), aes(x)) + 
  mapply(function(shape, scale, col) {
    stat_function(fun = dweibull, args = list(shape = shape, scale = scale), col = col,lwd=1)
  }, 
  shape = c(0.3, 0.8, 3, 4), 
  scale = c(10, 10, 10, 10), 
  col = c("#CC0033", "#FF00CC", "#FF0000","#9933FF")
)

p1<-p1+labs(x = "\n x", y = "f(x) \n", title = "Distribución Weibull: Cambios en el parámetro de forma")



p2<-ggplot(data.frame(x = c(0,40)), aes(x)) + 
  mapply(function(shape, scale, col) {
    stat_function(fun = dweibull, args = list(shape = shape, scale = scale), col = col,lwd=1)
  }, 
  shape = c(3, 3, 3, 3), 
  scale = c(5, 10, 15, 20), 
  col = c("#CC0033", "#FF00CC", "#FF0000","#9933FF")
)

p2<-p2+labs(x = "\n x", y = "f(x) \n", title = "Distribución Weibull: Cambios en el parámetro de escala")


grid.arrange(p1, p2,ncol=2)

###Log-normal


p3<-ggplot(data.frame(x = c(0,300)), aes(x)) + 
  mapply(function(meanlog, sdlog, col) {
    stat_function(fun = dlnorm, args = list(meanlog = meanlog, sdlog = sdlog), col = col,lwd=1)
  }, 
  meanlog = c(3, 4, 5, 6), 
  sdlog = c(1, 1, 1, 1), 
  col = c("#CC0033", "#FF00CC", "#FF0000","#9933FF")
)

p3<-p3+labs(x = "\n x", y = "f(x) \n", title = "Distribución Log-ormal: Cambios en meanlog")


p4<-ggplot(data.frame(x = c(0,200)), aes(x)) + 
  mapply(function(meanlog, sdlog, col) {
    stat_function(fun = dlnorm, args = list(meanlog = meanlog, sdlog = sdlog), col = col,lwd=1)
  }, 
  meanlog = c(4, 4, 4, 4), 
  sdlog = c(1.5, 2.5, 3, 3.5), 
  col = c("#CC0033", "#FF00CC", "#FF0000","#9933FF")
)

p4<-p4+labs(x = "\n x", y = "f(x) \n", title = "Distribución Log-normal: Cambios en sdlog")

grid.arrange(p3, p4,ncol=2)

###Pareto


p5<-ggplot(data.frame(x = c(0,100)), aes(x)) + 
  mapply(function(shape, scale, col) {
    stat_function(fun = dpareto, args = list(shape = shape, scale = scale), col = col,lwd=1)
  }, 
  shape = c(0.3, 0.8, 3, 4), 
  scale = c(10, 10, 10, 10), 
  col = c("#CC0033", "#FF00CC", "#FF0000","#9933FF")
)

p5<-p5+labs(x = "\n x", y = "f(x) \n", title = "Distribución pareto: Cambios en el parámetro de forma")


p6<-ggplot(data.frame(x = c(0,100)), aes(x)) + 
  mapply(function(shape, scale, col) {
    stat_function(fun = dpareto, args = list(shape = shape, scale = scale), col = col,lwd=1)
  }, 
  shape = c(3, 3, 3, 3), 
  scale = c(5, 10, 15, 20), 
  col = c("#CC0033", "#FF00CC", "#FF0000","#9933FF")
)

p6<-p6+labs(x = "\n x", y = "f(x) \n", title = "Distribución pareto: Cambios en el parámetro de escala")


grid.arrange(p5, p6,ncol=2)

###Burr

p7<-ggplot(data.frame(x = c(0,100)), aes(x)) + 
  mapply(function(shape1, shape2, scale, col) {
    stat_function(fun = dburr, args = list(shape1 = shape1, shape2 = shape2, scale = scale), col = col,lwd=1)
  }, 
  shape1 = c(0.3, 0.8, 3, 4), 
  shape2 =c(5,5,5,5),
  scale = c(10, 10, 10, 10), 
  col = c("#CC0033", "#FF00CC", "#FF0000","#9933FF")
)

p7<-p7+labs(x = "\n x", y = "f(x) \n", title = "Distribución burr: Cambios en el primer parámetro de forma")



p8<-ggplot(data.frame(x = c(0,100)), aes(x)) + 
  mapply(function(shape1, shape2, scale, col) {
    stat_function(fun = dburr, args = list(shape1 = shape1, shape2 = shape2, scale = scale), col = col,lwd=1)
  }, 
  shape1 =c(1.5,1.5,1.5,1.5),
  shape2 = c(1.3, 1.8, 3, 4), 
  scale = c(10, 10, 10, 10), 
  col = c("#CC0033", "#FF00CC", "#FF0000","#9933FF")
)

p8<-p8+labs(x = "\n x", y = "f(x) \n", title = "Distribución burr: Cambios en el segundo parámetro de forma")



p9<-ggplot(data.frame(x = c(0,100)), aes(x)) + 
  mapply(function(shape1, shape2, scale, col) {
    stat_function(fun = dburr, args = list(shape1 = shape1, shape2 = shape2, scale = scale), col = col,lwd=1)
  }, 
  shape1 =c(1.5,1.5,1.5,1.5),
  shape2 =c(5,5,5,5),
  scale = c(10, 20, 30, 40), 
  col = c("#CC0033", "#FF00CC", "#FF0000","#9933FF")
)

p9<-p9+labs(x = "\n x", y = "f(x) \n", title = "Distribución burr: Cambios en el primer parámetro de escala")


grid.arrange(p7, p8,p9,ncol=3)

###Otros

g1=ggdistribution(dnorm, seq(-3.5, 3.5, 0.1), mean = 0, sd = 1)+
     ggtitle("Standard Normal Density")
g2=ggdistribution(dexp, seq(-.5, 4, 0.1), rate =1)+
     ggtitle("Exponential Density\nwith Rate 1")
g3=ggdistribution(dunif, seq(-3.5, 3.5, 0.1), min = -.5, max = 1)+
     ggtitle("Uniform Density on [-.5,1]")
g4=ggdistribution(dchisq, seq(-1, 12, 0.1), df=4)+
     ggtitle("Chi-Squared Density\nwith 4 Degrees of Freedom")

grid.arrange(g1, g2, g3, g4, nrow=2)


ggdistribution(dnorm, seq(-3.5, 3.5, 0.1), mean = 0, sd = 1)+
     ggtitle("Standard Normal Density")


### Datos para ajustar alguna distribución paramética

monto<-read.csv("C:/Users/Salvador/Desktop/SolvenciaII-2017/Montos.csv")

length(monto)

attach(monto)

summary(Monto)

max(Monto)-min(Monto)

sd(Monto)

hist(Monto,col="darkorange",border="white",main="Histograma: Monto",col.main="red",col.lab="blue",breaks=50)

plot(density(Monto),col="darkred",main="Densidad suavizada: Monto",xlab="Monto",ylab="",col.main="darkblue",col.lab="darkblue",lwd=2)

hist(Monto,col="darkorange",border="white",main="Histograma: Monto",col.main="red",col.lab="blue",breaks=50,prob=T)
lines(density(Monto),col="darkblue",lwd=2)

boxplot(Monto,col="darkviolet",main="Boxplot: Monto",col.main="darkgreen",lwd=2)

Forma<- function(x) {
    n <- length(x)
    mean<-sum(x)/n
    M <-rep(0,4)
    for(i in 1:4){
    M[i] <- sum((x-mean)^i)/n
    }
    skew<-M[3]/(M[2])^(3/2)
    kurt<-(M[4]/(M[2]^2))-3
    return(list(Momentos=M,sesgo=skew,curtosis=kurt))
}

Forma(Monto)

Monto.fd<-ecdf(Monto)

plot(Monto.fd,col="magenta",main="Función de distribución empírica: Monto",xlab="x",ylab=expression(hat(F)[n](x)),col.main="blue",col.lab="darkgreen")

par(mfrow=c(2,2))

hist(Monto,col="darkorange",border="white",main="Histograma: Monto",col.main="red",col.lab="blue",breaks=50)
legend("topright", legend=c(paste("Sesgo=",round(Forma(Monto)$sesgo,4)),paste("Curtosis=",round(Forma(Monto)$curtosis,4))))


plot(density(Monto),col="darkred",main="Densidad suavizada: Monto",xlab="Monto",ylab="",col.main="darkblue",col.lab="darkblue",lwd=2)

boxplot(Monto,col="darkviolet",main="Boxplot: Monto",col.main="darkgreen",lwd=2)

plot(Monto.fd,col="magenta",main="Función de distribución empírica: Monto",xlab="x",ylab=expression(hat(F)[n](x)),col.main="blue",col.lab="darkgreen")


### Tiene cola derecha larga y pesada (Ajustamos una exponencial, que tiene la cola más ligera)

plot(qexp(ppoints(Monto),rate=1/mean(Monto)),sort(Monto),col="red",main="QQplot exponencial",xlab="",ylab="",col.main="darkblue")
abline(c(0,1),col="blue")

### Posibles densidades asociadas (no todas)

descdist(Monto,discrete=FALSE,,boot=5000,obs.pch = 19, boot.col="darkviolet")


### Weibull, Lognormal, Gamma...tal vez...Pareto, Burr

source("C:/Users/Salvador/Desktop/SolvenciaII-2017/autodistfit.R")

fitData(Monto,c("lognormal","weibull","pareto","burr"),sample=1)

### La mejor...BURR.

fit.burr<-fitdist(Monto, "burr",method="mle",start=list(shape1=1,shape2=1,scale=100))

ks.test(Monto, "pburr",shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]])
ad.test(Monto,pburr,shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]])

### Porqué no funcionó la gamma? mme

fitdist(Monto,"gamma",method="mle")
fitdist(Monto,"gamma",method="mme")
fitdist(Monto,"gamma",start=c(shape=0.5312323499,rate= 0.0001617227),method="mle")


plot(ecdf(Monto),verticals=TRUE,do.points=FALSE,col.hor="red", col.vert="bisque",main="Comparación entre las funciones de distribución: Monto",ylab="",col.main="darkorange",lwd=2)
curve(pgamma(x,shape=0.5312323499,rate= 0.0001617227),from=0,to=max(Monto),add=TRUE,col="blue",lwd=2)
k<-c ("Distribución empírica","Distribución teórica estimada (gamma)") 
legend (30000,0.6,paste(k),lty=1,col=c("red","blue"))

plot(qgamma(ppoints(Monto),shape=0.5312323499,rate= 0.0001617227),pch=19,col="darkgreen",sort(Monto),main="QQplot: Monto",xlab="",ylab="",col.main="darkblue")
abline(c(0,1),col="magenta")


### Ajuste gráfico "a mano" de la burr

### QQplot

plot(qburr(ppoints(Monto),shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]]),pch=19,col="darkgreen",sort(Monto),main="QQplot: Monto",xlab="",ylab="",col.main="darkblue")
abline(c(0,1),col="magenta")

### Density

plot(density(Monto),type="l",col="red",main="Comparación de densidades: Monto",xlab="",ylab="",ylim=c(0,0.0003),lwd=2,col.main="magenta")
curve(dburr(x,shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]]),from=0,to=max(Monto),add=TRUE,col="blue",lwd=2)
k<-c ("Densidad empírica","Densidad teórica estimada") 
legend (30000,0.0001,paste(k),lty=1,col=c("red","blue"))

### FDE

plot(ecdf(Monto),verticals=TRUE,do.points=FALSE,col.hor="red", col.vert="bisque",main="Comparación entre las funciones de distribución: Monto",ylab="",col.main="darkorange",lwd=2)
curve(pburr(x,shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]]),from=0,to=max(Monto),add=TRUE,col="blue",lwd=2)
k<-c ("Distribución empírica","Distribución teórica estimada") 
legend (30000,0.6,paste(k),lty=1,col=c("red","blue"))

### PPplot

plot(ppoints(length(Monto)),sort(pburr(Monto,shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]])),pch=19,col="darkgreen",main="PPplot: Monto",
     xlab="Percentiles teóricos",ylab="Percentiles muestrales",col.main="darkblue")
abline(c(0,1),col="magenta")


### Automático

plot(fit.burr,col=rainbow(4))

### Momentos de esta burr

mburr(1,shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]])
mean(Monto)


mburr(2,shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]])-mburr(1,shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]])^2
var(Monto)

### como lo usamos???

burr.fde<-function(x){pburr(x,shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]])}

1-burr.fde(10000)

###Cómo calculamos una prima???


### Comparando los mejores ajustes: lognormal y burr

fit.lognormal<-fitdist(Monto,"lnorm")

cdfcomp(list(fit.burr, fit.lognormal), legendtext=c("burr","lognormal"))
denscomp(list(fit.burr, fit.lognormal), legendtext=c("burr","lognormal"))
qqcomp(list(fit.burr, fit.lognormal), legendtext=c("burr","lognormal"))
ppcomp(list(fit.burr, fit.lognormal), legendtext=c("burr","lognormal"))
gofstat(list(fit.burr, fit.lognormal))

### Definiendo la verosimilitud y maximizando
### The Burr distribution with parameters shape1 = a, shape2 = b and scale = s has density:
### f(x) = (a b (x/s)^b)/(x [1 + (x/s)^b]^(a + 1))
### for x > 0, a > 0, b > 0 and s > 0.
### The Burr distribution has the following special cases: 
### A Loglogistic distribution when shape1 == 1; 
### A Paralogistic distribution when shape2 == shape1; 
### A Pareto distribution when shape2 == 1. 

mlestR<-function(x,theta){-sum(dburr(x,shape1=theta[1],shape2=theta[2],scale=theta[3],log=T))}
optim(theta<-c(1,1,1),mlestR,x=Monto,method="L-BFGS-B",lower=0.001,upper=Inf)$par

mlest<- function(x,theta) {-sum(log(theta[1])+log(theta[2])+theta[2]*log(x)-theta[2]*log(theta[3])-log(x)-(theta[1]+1)*log(1+(x/theta[3])^(theta[2])))}
optim(theta<-c(1,1,1),mlest,x=Monto,method="L-BFGS-B",lower=0.001,upper=Inf)$par

### Se pueden plantear las ecuaciones de verosimilitud y resolverlas numericamente (dlogL/dtheta=0)
### En este caso, son "medio feas".

#############################################################################################################################################################
##################################################################################################################################################################
### Algunas cantidades de interés, su simulación y bootstrap

mean.mod<-mburr(1,shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]])
mean.mod

median.mod<-qburr(p=0.5, shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]],lower.tail = TRUE, log.p = FALSE)
median.mod

var.mod<-mburr(2,shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]])-mburr(1,shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]])^2
var.mod

q.mod<-qburr(p=c(0.9,0.95,0.99), shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]],lower.tail = TRUE, log.p = FALSE)
q.mod

q.dat<-quantile(Monto, prob=c(0.9,0.95,0.99))
q.dat

mean.dat<-mean(Monto)
mean.dat

median.dat<-median(Monto)

var.dat<-var(Monto)
var.dat

mod<-c(mean.mod,median.mod,var.mod,q.mod)
dat<-c(mean.dat,median.dat,var.dat,q.dat)
cbind(mod,dat)

### Variabilidad de estas cantidades

n<-5000
m<-5000
y<-matrix(0,n,6)
x<-matrix(0,n,m)

for(i in 1:n){
x[i,]<-rburr(m,shape1=fit.burr$estimate[[1]], shape2=fit.burr$estimate[[2]],scale=fit.burr$estimate[[3]])
y[i,]<-c(mean(x[i,]),median(x[i,]),var(x[i,]),quantile(x[i,],probs=c(0.9,0.95,0.99)))
}

sim1<-apply(y,2,mean)

q<-function(x){quantile(x,probs=c(0.025,0.975))}

sim2<-apply(y,2,q)

###bootstrap


summary.fun<-function(datos,i){
c(mean(datos[i]),median(datos[i]),var(datos[i]),quantile(datos[i],probs=0.9),quantile(datos[i],probs=0.95),quantile(datos[i],probs=0.99))

}

sum.boot<-boot(Monto,summary.fun,R=5000)

mean.boot<-apply(sum.boot$t,2,mean)
mean.boot

I.mean<-boot.ci(sum.boot,index=1)$perc[4:5]
I.median<-boot.ci(sum.boot,index=2)$perc[4:5]
I.var<-boot.ci(sum.boot,index=3)$perc[4:5]
I.q90<-boot.ci(sum.boot,index=4)$perc[4:5]
I.q95<-boot.ci(sum.boot,index=5)$perc[4:5]
I.q99<-boot.ci(sum.boot,index=6)$perc[4:5]

I.boot<-matrix(c(I.mean,I.median,I.var,I.q90,I.q95,I.q99),2,6,byrow=FALSE)
I.boot

### Tabla de comparación de resultados e intervalos de confianza

options(scipen=999)
Resultados<-matrix(c(mean.dat,median.dat,var.dat,q.dat,mean.mod,median.mod,var.mod,q.mod,sim1,mean.boot,sim2[1,],sim2[2,],I.boot[1,],I.boot[2,]),6,8)
colnames(Resultados)<-c("Datos","Modelo","Simulación","Bootstrap","simLi","simLs","bootLi","bootLs")
rownames(Resultados)<-c("Media","Mediana","Varianza","q90","q95","q99")
Resultados

#####VALORES EXTREMOS
###########################################################################################################################################################
################################################### VALORES EXTREMOS ##############################################################################
### VALORES EXTREMOS
### Ajuste del modelo GPD a los datos de la SOA

montos<-read.table("C:/Users/Salvador/Desktop/Cursos2021-I/Riesgo2021/SOA91.TXT",sep=",")

summary(montos[,4])

par(mfrow=c(1,2))

plot(density(montos[,4]),col="darkblue",xlab="",ylab="Densidad suavizada",col.lab="darkred",main="Densidad tipo kernel",col.main="darkgreen")

plot(density(log(montos[,4])),col="darkblue",xlab="",ylab="Densidad suavizada",col.lab="darkred",main="Densidad tipo kernel(log)",col.main="darkgreen")


par(mfrow=c(1,2))

hist(montos[,4],col="darkblue",border="white",main="Histograma montos de reclamación")

truehist(log(montos[,4]),col="darkgreen",border="white",main="Histograma montos de log(reclamación)")


par(mfrow=c(1,2))

boxplot(montos[,4],col="red",main="Montos")

boxplot(log(montos[,4]),col="blue",main="Log(montos)")


skew(montos[,4]);kurtosi(montos[,4])

skew(log(montos[,4]));kurtosi(log(montos[,4]))



### Grafica de la funcion media de excesos

mrlplot(montos[,4],col="darkgreen",conf=FALSE)

abline(v=230000,lty=2,col="blue")

abline(v=215000,lty=3,col="red")

abline(v=200000,lty=4,col="green")


mrlplot(montos[,4],lty=rep(1,3),col=c("darkblue","darkgreen","darkblue"))
abline(v=230000,lty=2,col="blue")

abline(v=215000,lty=3,col="red")

abline(v=200000,lty=4,col="green")


### Será más clara con el logaritmo?

lmontos<-log(montos[,4])

mrlplot(lmontos,lty=rep(1,3),col=c("darkblue","darkgreen","darkblue"))

### Confirmando con otra gráfica

mrl.plot(montos[,4], umin = min(montos[,4]), umax = max(montos[,4]) - 0.1,conf = 0.95, nint = 100)
abline(v=200000,lty=3,col="red")


### Gráfica de estabilidad de los parámeros

source("C:/Users/Salvador/Desktop/Cursos2021-I/Riesgo2021/parStabilityPlot.R")
source("C:/Users/Salvador/Desktop/Cursos2021-I/Riesgo2021/pareto2.goftest.R")

parStabilityPlot(montos[,4], c(1000,500000))

### Ajuste de un modelo GPD con un umbral de 210000

### Libreria evir

fit<-GPD.fit<-gpd(montos[,4],210000)

plot.gpd(GPD.fit)

ks.test(montos[,4],"pgpd",shape=fit$par.ests[[1]],scale=fit$par.ests[[2]])

ks.test(montos[,4][montos[,4]>210000],"pgpd",shape=fit$par.ests[[1]],scale=fit$par.ests[[2]])

pareto2.goftest(montos[,4], fit$par.ests[[2]],fit$par.ests[[1]], method="anderson-darling")

pareto2.goftest(montos[,4], fit$par.ests[[2]],fit$par.ests[[1]], method="kolmogorov")

pareto2.goftest(montos[,4][montos[,4]>210000], fit$par.ests[[2]],fit$par.ests[[1]], method="anderson-darling")

pareto2.goftest(montos[,4][montos[,4]>210000], fit$par.ests[[2]],fit$par.ests[[1]], method="kolmogorov")

gpdAd(montos[,4])
gpdCvm(montos[,4])

gpdAd(montos[,4][montos[,4]>210000])

gpdCvm(montos[,4][montos[,4]>210000])

####################VaR y TVaR

shape=fit$par.ests[[1]],scale=fit$par.ests[[2]]

###VaR: vargenpareto(p, k=1, c=1, log.p=FALSE, lower.tail=TRUE)
###Expected Shortfall (TVaR): esgenpareto(p, k=1, c=1)
###k: Escala; c: Forma

vargenpareto(c(0.9,0.95,0.99), k=fit$par.ests[[2]], c=fit$par.ests[[1]], log.p=FALSE, lower.tail=TRUE)

esgenpareto(c(0.9,0.95,0.99), k=fit$par.ests[[2]], c=fit$par.ests[[1]])

###Por fórmula

s<-fit$par.ests[[2]];xi<-fit$par.ests[[1]]

TVaRgpd<-function(x){s*( (1-x)^(-xi)/(1-xi) + ((1-x)^(-xi)-1)/xi)}

TVaRgpd(c(0.9,0.95,0.99))

VaRgpd<-function(x){s*(((1-x)^{-xi}-1)/xi)}

VaRgpd(c(0.9,0.95,0.99))

###Por simulación
###Simularemos utilizando las expresiones de las fórmulas
###Nuestro método de simulación será el de INVERSIÓN DE LA FUNCIÓN DE DISTRIBUCIÓN

s<-fit$par.ests[[2]];xi<-fit$par.ests[[1]]

u<-runif(1000000)
u<-matrix(data=u,nrow=1000)
sim.gpd<-VaRgpd(u)
qua<-function(x){quantile(x,prob=c(0.9,0.95,0.99))}
GPD.VaR<-apply(sim.gpd,2,qua)
dim(GPD.VaR)
apply(GPD.VaR,1,mean)

sim.gpd1<-TVaRgpd(u)
GPD.TVaR<-apply(sim.gpd1,2,qua)
dim(GPD.TVaR)
apply(GPD.TVaR,1,mean)

###Intervalos de confianza

###VaR90
quantile(GPD.VaR[1,],prob=c(0.025,0.975))

###VaR95
quantile(GPD.VaR[2,],prob=c(0.025,0.975))

###VaR99
quantile(GPD.VaR[3,],prob=c(0.025,0.975))

###TVaR90
quantile(GPD.TVaR[1,],prob=c(0.025,0.975))

###TVaR95
quantile(GPD.TVaR[2,],prob=c(0.025,0.975))

###TVaR99
quantile(GPD.TVaR[3,],prob=c(0.025,0.975))

###Bootstrap

datos<-montos[,4]

summary.fun<-function(datos,i){
c(mean(datos[i]),quantile(datos[i],probs=0.9),quantile(datos[i],probs=0.95),quantile(datos[i],probs=0.99))
}

set.seed(321123)

GPD.boot<-boot(datos,summary.fun,R=5000)

apply(GPD.boot$t,2,mean)

q2<-function(x){quantile(x,prob=c(0.025,0.975))}

apply(GPD.boot$t,2,q2)


#####
#########################################################################################################################################################
############################################################# VaR y TVaR ################################################################################

### Comparación del VaR entre una distribución con colas ligeras y una con colas pesadas

curve(dgamma(x,shape=5,scale=1/3),from=0,to=10,col="darkred",xlab="x",ylab="",
main="Comparación colas distribuciones Gamma y Pareto",col.main="darkgreen",col.lab="darkorange")

curve(dpareto(x,shape=6/5,scale=1/3),from=0,to=10,col="darkblue",add=T)

k<-c ("Gamma (5,1/3)","Pareto (6/5,1/3)") 
legend (5,0.5,paste(k),lty=1, col=c("darkred","darkblue"))


qgamma(c(0.95,0.99,0.995),shape=5,scale=1/3)

qpareto(c(0.95,0.99,0.995),shape=6/5,scale=1/3)

###Comparacion de funciones de supervivencia

curve(1-pgamma(x,shape=5,scale=1/3),from=0,to=10,col="red",main="Comparación de supervivencias",col.main="darkviolet",ylab="")

curve(1-ppareto(x,shape=6/5,scale=1/3),from=0,to=10,col="blue",add=T)

k<-c ("Sup Gamma (5,1/3)","Sup Pareto (6/5,1/3)") 
legend (5,0.5,paste (k),lty=1, col=c("red","blue"))


#### Comparacion de funciones de riesgo

curve(dgamma(x,shape=5,scale=1/3)/(1-pgamma(x,shape=5,scale=1/3)),from=0,to=10,col="red",ylab="",
      main="Comparación de funciones de riesgo",col.main="darkorange")

curve(dpareto(x,shape=6/5,scale=1/3)/(1-ppareto(x,shape=6/5,scale=1/3)),from=0,to=10,col="blue",ylab="",add=T)

k<-c ("Riesgo Gamma (5,1/3)","Riesgo Pareto (6/5,1/3)") 
legend (5,0.8,paste (k),lty=1, col=c("red","blue"))

### Simulación para dar una medida de variabilidad al VaR de estas distribuciones
### Gamma
n<-1000
m<-1000
y<-rep(0,n)
x<-matrix(0,n,m)

for(i in 1:n){
x[i,]<-rgamma(m,shape=5,scale=1/3)
y[i]<-quantile(x[i,],probs=0.95)

}

c(mean(y),quantile(y,probs=c(0.025,0.975)))

### Pareto

y1<-rep(0,n)
x1<-matrix(0,n,m)

for(i in 1:n){
x1[i,]<-rpareto(m,shape=6/5,scale=1/3)
y1[i]<-quantile(x1[i,],probs=0.95)

}

c(mean(y1),quantile(y1,probs=c(0.025,0.975)))


### Simulación para dar una medida de variabilidad al TVaR de estas distribuciones

#### TVaR(0.95) Gamma

n<-1000
m<-1000
k<-rep(0,n)
y2<-rep(0,n)
x2<-matrix(0,n,m)

for(i in 1:n){

x2[i,]<-sort(rgamma(m,shape=5,scale=1/3))
k<-floor(0.95*m)+1
y2[i]<-sum(x2[i,][k:m])/(m-k+1)

}

c(mean(y2),quantile(y2,probs=c(0.025,0.975)))

### TVaR(0.95) Pareto

y3<-rep(0,n)
x3<-matrix(0,n,m)

for(i in 1:n){

x3[i,]<-sort(rpareto(m,shape=6/5,scale=1/3))
k<-floor(0.95*m)+1
y3[i]<-sum(x3[i,][k:m])/(m-k+1)

}

c(mean(y3),quantile(y3,probs=c(0.025,0.975)))

##############################################################################################################################################################
##############################################################################################################################################################
############################################ El ejemplo de distribuciones clases (a,b,0) y (a,b,1) ###########################################################

#### Proceso recurrente sin modificaciones: BN(beta=0.5,r=2.5)

beta<-0.5; r<-2.5

a<-beta/(1+beta);b<-(r-1)*beta/(1+beta)

P<-numeric()

P[1]<-(1+beta)^(-r)

for(k in 1:4){P[k+1]<-P[k]*(a+b*(1/k))}

P

### Distribucion truncada en cero. (PT0=0)

PT<-numeric()

PT[1]<-P[2]/(1-P[1])

for(k in 2:3){PT[k]<-(a+b/k)*PT[k-1]}

PT<-c(0,PT)

PT


#### Distribucion modificada en cero (PM0=0.6)

PM<-numeric()

PM[1]<-(1-0.6)*(0.302406)/(1-0.362887)

for(k in 2:3){PM[k]<-(a+b/k)*PM[k-1]}

PM<-c(0.6,PM)

PM


###############################################################################################################################################################
###############################################################################################################################################################
### Graficas con modificaciones de covertura

### Pago del pago: Y^P

f<-coverage(dpareto,ppareto,deductible=500)
curve(f(x,shape=3,scale=2000),xlab="x",ylab="f(x)",xlim=c(0.01,5000),main="Pareto(3,2000)",col="red",col.main="darkred")
curve(dpareto(x, shape=3, scale=2000), col="blue", add=TRUE)
t<- c("X",expression(Y^P))
legend(3000, 0.0008, paste(t), lty=1, col=c("blue", "red"))

### Pago de la pérdida: Y^L

g<-coverage(dpareto,ppareto,deductible=500,per.loss=TRUE)
curve(g(x, shape=3, scale=2000), xlab="x", ylab="f(x)",xlim=c(0.0001,5000),main="Pareto(3,2000)",col="red",col.main="darkred")
curve(dpareto(x, shape=3, scale=2000), col="blue", add=TRUE)
points(0, g(0,3,2000), pch=19, col="red")
t<- c("X", expression(Y^L))
legend(3000, 0.0004, paste(t), lty=1, col=c("blue", "red"))


### Severidad

### Pareto 3, 2000, d=500

### Vimos que la nueva variable es una Pareto(3,2500)

curve(dpareto(x,shape=3,scale=2000),from=0,to=5000,col="darkred",xlab="x",ylab="",
      main="Comparación de las distribuciones Pareto sin y con deducible",col.main="darkgreen",col.lab="darkorange")

curve(dpareto(x,shape=3,scale=2500),from=0,to=5000,col="darkblue",add=T)

k<-c ("Sin deducible: Pareto(3,2000)","Con deducible: Pareto(3,2500)") 
legend (2800,0.0008,paste(k),lty=1, col=c("darkred","darkblue"),cex=1.3)



### Funciones de los modelos modificados por la presencia de un deducible

### Variable de pago

FYP<-coverage(cdf=ppareto,deductible=500,limit=Inf)

FYP(500,3,2000)

curve(FYP(x,3,2000),0,500)

### Sin utilizar las funciones de actuar. Esta funcion corresponde a

Fyp1<-function(y){1-(2500/(2500+y))^(3)}

Fyp1(500)

curve(Fyp1(x),0,500)


### variable de pérdida

FYL<-coverage(cdf=ppareto,deductible=500,limit=Inf,per.loss=TRUE)

FYL(0,shape=3,scale=2000)


curve(FYP(x,shape=3,scale=2000),from=0,to=4000,col="darkred")
curve(FYL(x,shape=3,scale=2000),from=0,to=4000,col="darkblue", add=T)
k<-c ("FYP","FYL") 
legend (2500,0.4,paste (k),lty=1, col=c("darkred","darkblue"))

### Densidades

fYP<-coverage(dpareto,cdf=ppareto,deductible=500, limit=Inf)
fYL<-coverage(dpareto,cdf=ppareto,deductible=500,limit=Inf,per.loss=TRUE)

curve(fYP(x,shape=3,scale=2000),from=0,to=4000,col="darkgreen",ylab="")
curve(fYL(x,shape=3,scale=2000),from=0,to=4000,col="darkviolet", add=T,ylab="")
k<-c ("fYP","fYL") 
legend (2000,0.0006,paste (k),lty=1, col=c("darkgreen","darkviolet"))


### Momentos restringidos y momentos sin restriccion 

mpareto(shape=3,scale=2000,order=1)

levpareto(500,shape=3,scale=2000,order=1) 

### costo esperado de perdida

mpareto(shape=3,scale=2000,order=1)-levpareto(500,shape=3,scale=2000,order=1)

### costo esperado por el pago

(mpareto(shape=3,scale=2000,order=1)-levpareto(500,shape=3,scale=2000,order=1))/(1-ppareto(500,shape=3,scale=2000))

### Tasa de eliminacion de perdida

levpareto(500,shape=3,scale=2000,order=1)/mpareto(shape=3,scale=2000,order=1)

### Pago esperado de reclamo considerando el efecto de inflacion

levpareto(500/1.1,shape=3,scale=2000,order=1)

### Costo esperado por perdida despues del efecto de inflacion

1.1*(mpareto(shape=3,scale=2000,order=1)-levpareto(500/1.1,shape=3,scale=2000,order=1))

### costo del pago

fyLI<-coverage(dpareto, ppareto,deductible=500,limit = Inf,inflation = 0.1,per.loss = TRUE)

(1.1*(mpareto(shape=3,scale=2000,order=1)-levpareto(500/1.1,shape=3,scale=2000,order=1)))/(1-fyLI(0,shape=3,scale=2000))

### Limite de poliza con inflacion: costo esperado. u=3000

levpareto(3000,shape=3,scale=2000,order=1)  

### Tasa eliminacion de perdida

(mpareto(shape=3,scale=2000,order=1)-levpareto(3000,shape=3,scale=2000,order=1))/mpareto(shape=3,scale=2000,order=1)

### Aplicando (que es gerundio) la tasa de inflacion

1.1*levpareto(3000/1.1,shape=3,scale =2000, order = 1)


### Funcion con todas las modificaciones consideradas.

fyLI<-coverage(dpareto,ppareto,limit=3000,inflation=0.1,deductible=500,coinsurance=0.8,per.loss = TRUE)

curve(fyLI(x,3,2000),from=500,to=1500)

fyLI(500,3,2000)

fyLI(500,3,2000);fyLI(3000,3,2000)

###

levpareto(3000,shape=3,scale=2000,order=1)


################################################ EJEMPLO ###################################

### Ejemplo uso de sala de emergencias.
### Exp(1000) d=200, u=5000, alfa=0.8

###Tasa de eliminación de pérdida

levexp(200,1/1000,order=1)/1000

levexp(200,1/1000,order=1)/mexp(1/1000,order=1)

### Calcule la cantidad de reclamación esperada por el evento de pérdida y la cantidad esperada por
### pago, que debe realizar la empresa

0.8*(levexp(5000,1/1000,order=1)-levexp(200,1/1000,order=1))

0.8*(levexp(5000,1/1000,order=1)-levexp(200,1/1000,order=1))/(1-pexp(200,1/1000))

### Exp(1000) d=200, u=5000, alfa=0.8 r=0.08

###Ejercicio lector

0.8*(1+0.08)*(levexp(5000/1.08,1/1000,order=1)-levexp(200/1.08,1/1000,order=1))


####El impacto del deducible en la frecuencia de los reclamos

nu<-1-ppareto(250,3,1000)
nu

r<-2;b<-3;p<-1/(1+b)

b.star<-nu*b
r.star<-r

x<-seq(0:15)

BN<-dnbinom(x,size=r,prob=p)
BN.star<-dnbinom(x,size=r.star,prob=1/(1+b.star))

library(dplyr)
library(ggplot2)

data.frame(x = 0:15, prob = dnbinom(x = 0:15, size = 2, prob = p)) %>%
  mutate(Failures = ifelse(x == n, n, "other")) %>%
ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,2), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 5.5
  )

library(ggplot2)
library(reshape2)
df1 <- data.frame(BN,BN.star,x)
df2 <- melt(df1, id.vars="x")
df2
ggplot(df2, aes(x=x, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')

ggplot(data = df2, aes(x = x, y = value, fill = variable)) + geom_bar(stat = "identity")+ facet_wrap(~ variable) + scale_x_continuous(breaks=seq(1,15,2))

### Ejemplo: Convolución de poisson compuesta

f1<-c(0.25,0.75,0,0)
f2<-c(0.10,0.40,0.40,0.10)
lambda1=3 ; lambda2=2

lambda<-lambda1+lambda2

fS<-(lambda1/lambda)*f1+(lambda2/lambda)*f2

ES<-sum(seq(1:4)*fS)
VS<-sum((seq(1:4)-ES)^2*fS)

########################################################################################################################
### Ejemplo: Modificaciones en los modelos agregados

### Características

### Frecuencia~BinNeg(beta=1.5,r=12); Pérdidas~Pareto(3,150); r=0.03, d=40, u=250; alpha=0.85

### Objetivo: Determinar la esperanza y la varianza de las pérdidas agregadas

r<-0.03
d=40
u=250
alpha=0.85

########################### Variable de pérdida Y^{L} ###########################################

### La función modificada de pérdida de una pareto (3,150) 

fyL<-coverage(dpareto,ppareto,limit=u,inflation=r,deductible=d,coinsurance=alpha,per.loss=TRUE)

fyL(0,3,150)

### Esperanza de fyL

alpha*(1+r)*(levpareto(u/(1+r),shape=3,scale=150,order=1)-levpareto(d/(1+r),shape=3,scale=150,order=1))


### Segundo momento de fyL

alpha*(1+r)*(levpareto(u/(1+r),shape=3,scale=150,order=2)-levpareto(d/(1+r),shape=3,scale=150,order=2)
             -2*(d/(1+r))*levpareto(u/(1+r),shape=3,scale=150,order=1)+2*(d/(1+r))*levpareto(d/(1+r),shape=3,scale=150,order=1))

###ES=E(Y^{L})*E(N^{L})
ES<-31.85255*12*1.5
ES

### Var S

12*1.5*(4217.442+1.5*31.85244^2)

##############################Variable de pago Y^{p} ############################################

### Variable de pago del pago

### Fracción a la que realmente se paga

nu<-1-ppareto(d/(1+r),shape=3,scale=150)
nu

### 

EyP<-alpha*(1+r)*(levpareto(u/(1+r),shape=3,scale=150,order=1)-levpareto(d/(1+r),shape=3,scale=150,order=1))/nu
EyP

beta1<-1.5*nu

ENP<-12*beta1

ES<-EyP*ENP
ES

###Para la varianza de yP

###Segundo momento

EyP2<-4217.442/nu
EyP2

VS<-12*beta1*(EyP2+beta1*EyP^2)
VS




























