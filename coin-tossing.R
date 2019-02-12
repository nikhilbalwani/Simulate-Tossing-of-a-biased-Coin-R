x1 <- seq(0,10,by=1)
y10_0.5 = dbinom(x1,10,0.5)
y10_0.3 = dbinom(x1,10,0.3)
y10_0.16 = dbinom(x1,10,0.16)

plot(x1,y10_0.5,'l', ylim=c(0,0.4))
par(new = TRUE)
plot(x1,y10_0.3,'l', ylim=c(0,0.4))
par(new = TRUE)
plot(x1,y10_0.16,'l', ylim=c(0,0.4))

x2 <- seq(0,100,by=1)
y100_0.5 = dbinom(x2,100,0.5)
y100_0.3 = dbinom(x2,100,0.3)
y100_0.16 = dbinom(x2,100,0.16)

plot(x2,y100_0.5,'l',ylim=c(0,0.15))
par(new = TRUE)
plot(x2,y100_0.3,'l',ylim=c(0,0.15))
par(new = TRUE)
plot(x2,y100_0.16,'l',ylim=c(0,0.15))

x3 <- seq(0,1000,by=1)
y1000_0.5 = dbinom(x3,1000,0.5)
y1000_0.3 = dbinom(x3,1000,0.3)
y1000_0.16 = dbinom(x3,1000,0.16)

plot(x3,y1000_0.5,'l',ylim=c(0,0.04))
par(new = TRUE)
plot(x3,y1000_0.3,'l',ylim=c(0,0.04))
par(new = TRUE)
plot(x3,y1000_0.16,'l',ylim=c(0,0.04))

mul1 = x1*y10_0.5
mean1 = sum(mul1)
ex21 = sum(mul1*x1)
sd1 = (ex21 - (mean1^2))^0.5
sum(y10_0.5*(abs(x1 - mean1) < 2*mul1))

mul2 = x2*y100_0.5
mean2 = sum(mul2)
ex22 = sum(mul2*x2)
sd2 = (ex22 - (mean2^2))^0.5
sum(y100_0.5*(abs(x2 - mean2) < 2*mul2))

mul3 = x3*y1000_0.5
mean3 = sum(mul3)
ex23 = sum(mul3*x3)
sd3 = (ex23 - (mean3^2))^0.5
sum(y1000_0.5*(abs(x3 - mean3) < 2*mul3))

mul1 = x1*y10_0.3
mean1 = sum(mul1)
ex21 = sum(mul1*x1)
sd1 = (ex21 - (mean1^2))^0.5
sum(y10_0.3*(abs(x1 - mean1) < 2*mul1))

mul2 = x2*y100_0.3
mean2 = sum(mul2)
ex22 = sum(mul2*x2)
sd2 = (ex22 - (mean2^2))^0.5
sum(y100_0.3*(abs(x2 - mean2) < 2*mul2))


mul3 = x3*y1000_0.3
mean3 = sum(mul3)
ex23 = sum(mul3*x3)
sd3 = (ex23 - (mean3^2))^0.5
sum(y1000_0.3*(abs(x3 - mean3) < 2*mul3))


mul1 = x1*y10_0.16
mean1 = sum(mul1)
ex21 = sum(mul1*x1)
sd1 = (ex21 - (mean1^2))^0.5
sum(y10_0.16*(abs(x1 - mean1) < 2*mul1))

mul2 = x2*y100_0.16
mean2 = sum(mul2)
ex22 = sum(mul2*x2)
sd2 = (ex22 - (mean2^2))^0.5
sum(y100_0.16*(abs(x2 - mean2) < 2*mul2))

mul3 = x3*y1000_0.16
mean3 = sum(mul3)
ex23 = sum(mul3*x3)
sd3 = (ex23 - (mean3^2))^0.5
sum(y1000_0.16*(abs(x3 - mean3) < 2*mul3))



