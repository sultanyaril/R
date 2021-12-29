# ������ ��������� ��������� ������

# ������������� � ����������: ?RNGkind

# ����������������� ��������� �����

# �������: 3 ������ �������� � �� 1000 ��������� ��������� �������������� �������� ��� ���
example = data.frame(
  Factor = rep(c("A", "B", "C"), each = 1000),
  Variable = c(rnorm(1000, 5, 2),
               rnorm(1000, 4, 3),
               rnorm(1000, 2, 1)))

tapply(example$Variable, example$Factor, summary)

example1 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 1000),
  Variable = c(rnorm(1000, 5, 2),
               rnorm(1000, 4, 3),
               rnorm(1000, 2, 1)))

example2 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 1000),
  Variable = c(rnorm(1000, 5, 2),
               rnorm(1000, 4, 3),
               rnorm(1000, 2, 1)))

example3 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 1000),
  Variable = c(rnorm(1000, 5, 2),
               rnorm(1000, 4, 3),
               rnorm(1000, 2, 1)))

example4 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 1000),
  Variable = c(rnorm(1000, 5, 2),
               rnorm(1000, 4, 3),
               rnorm(1000, 2, 1)))

# ���������� ggplot2
library(ggplot2)

# aes: Construct aesthetic mappings
p1 = ggplot(example1, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p2 = ggplot(example2, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p3 = ggplot(example3, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p4 = ggplot(example4, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)

library(gridExtra) # ��� ������� grid.arrange()
grid.arrange(p1, p2, p3, p4, ncol = 2)

# ��������� ��������� ����� ��� ���������� ��������������� �����
set.seed(1020)

example1 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 1000),
  Variable = c(rnorm(1000, 5, 2),
               rnorm(1000, 4, 3),
               rnorm(1000, 2, 1)))

# ����� ��������� ����� ������ �������������� 
set.seed(1020)
example2 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 1000),
  Variable = c(rnorm(1000, 5, 2),
               rnorm(1000, 4, 3),
               rnorm(1000, 2, 1)))

set.seed(1020)
example3 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 1000),
  Variable = c(rnorm(1000, 5, 2),
               rnorm(1000, 4, 3),
               rnorm(1000, 2, 1)))

set.seed(1020)
example4 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 1000),
  Variable = c(rnorm(1000, 5, 2),
               rnorm(1000, 4, 3),
               rnorm(1000, 2, 1)))

p1 = ggplot(example1, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p2 = ggplot(example2, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p3 = ggplot(example3, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)
p4 = ggplot(example4, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)

# ������ ����������������� �����������
grid.arrange(p1, p2, p3, p4, ncol = 2)

# ������ ������������� ������������, ������������� � R

# �������� ��� ������� ��� ������ � ���������������
# d (�density� � ���������): ������� ��������� �����������
# p (�probability� � �����������): ������� ������������� ������������
# q (�quantile� � ��������): ������� ��� ���������� ���������
# r (�random� � ���������): ������� ��� ��������� ��������� �����

# ��. ������� dbeta, dbinom, dcauchy, dchisq, dexp, df, dgamma, dgeom,
# dhyper, dlnorm, dmultinom, dnbinom, dnorm, dpois, dt, dunif, dweibull





dnorm(-1)
pnorm(-1)
qnorm(c(0.25, 0.75))
rnorm(10, mean=5, sd=2)

# ��. ����� ����������  VGAM, actuar, gamlss � ActuDistns

# ������ ������ � ���������� �������������
library(MASS)

# ������������� ������� �� ����� ������������� � ���������� ����������
set.seed(0)
x.gam <- rgamma(200, rate = 0.5, shape = 3.5) 

# ����� �������� 
med.gam <- mean(x.gam)                  # ���������� ������� 
var.gam<-var(x.gam)                     # ���������� ��������� 
(l.est <- med.gam/var.gam)              # ������ ��������� �������� rate
(g.est <- ((med.gam)^2)/var.gam)        # ������ ��������� ����� shape

# ����� ������� ������� ���������
library(rootSolve)
f1 <- function(x){c(F1 = x[1]/x[2] - med.gam, F2 = x[1]/x[2]^2 - var.gam)}
multiroot(f1, c(3, 0.6))

# ����������, ��� �������� ���
ml <- function(params, y.in, x.in) {
  beta <- params[1]
  s2 <- params[2]
  res <- -0.5*s2*length(y.in)-0.5/exp(s2)*sum((y.in-beta*x.in)^2)
  # R ����� ������ �������������� �������, ������� ������� ������� � �������
  return(-res)
}

# ������� ������
n = 1000
x <- rnorm(n,mean=5,sd=3)
hist(norm, breaks=20)

f <- function(X){c(F1 = (sum(x)-n*X[1])/X[2], 
                   F2=-n/(2*X[2]) + sum((x-X[1])^2)/(2*(X[2]^2)))}
multiroot(f, c(1, 3))

# ������������� �������� �� ��������� ���������� ��������
set.seed(1946)
x = sort(rweibull(100, 2, (1 + 1.21*rbinom(100, 1, 0.05)) ))
x  # ��������������� �������� 
summary(x)  # ���������� ��������������

hist(x, freq = FALSE, breaks = 15, col = "grey88", 
     main="����������� � ������� ���������")
lines(density(x), lwd = 2, col = "blue")

# ������� ��� ������ �������� ������������� � ������������ ������������ ������� �������������

graph_distr <- function(x, pc, pd, main_name = "")
{ 
  op <- par(mfrow = c(1, 1), pty = "s")
  par(mfrow = c(1, 2))
  mn <- paste(c("������������ �� � ", main_name))
  plot(x,pc, type = "l", col = "red", lwd = 2, main = mn) 
  plot(ecdf(x), add = TRUE) 
  mn <- paste(c("������������ ��������� � ", main_name))
  plot(density(x), lwd = 2, col = "blue", main = mn) 
  lines(x, pd, col = "red", lwd = 2)
  par(op)
}

# �������� ��� ������

# ������ ���������� ����������� �������������
(dof <- fitdistr(x,"normal"))
ep1 <- dof$estimate[1]; ep2 <- dof$estimate[2]
ks.test(x,pnorm, mean = ep1, sd = ep2)
graph_distr(x, pnorm(x, mean = ep1, sd = ep2),
            dnorm(x, mean = ep1, sd = ep2),
            "���������� �������������")

# ������ ���������� ���-����������� �������������
(dof <- fitdistr(x,"log-normal")) 
ep1 <- dof$estimate[1]; ep2 <- dof$estimate[2]
ks.test(x, plnorm, meanlog = ep1, sdlog = ep2)
graph_distr(x, plnorm(x, meanlog = ep1, sdlog = ep2),
            dlnorm(x, meanlog = ep1, sdlog = ep2),
            "������������� �������������")

# ������ ���������� ������������ ��������
library(fitdistrplus)
# ���������� ���������� ������� ������������� ����� �������������� � ������ ����������
(dof <- fitdist(x, "weibull"))
ep1 <- dof$estimate[1]; ep2 <- dof$estimate[2]
ks.test(x, pweibull, shape = ep1, scale = ep2)
graph_distr(x, pweibull(x, scale = ep1, shape = ep2),
            dweibull(x, scale = ep1, shape = ep2),
            "������������� ��������")

# �������� ������������� ������ �� ������������ 

# ����������� ������ (������ ��������������� �����������)

set.seed(1946)
x <- sort(rweibull(100, 2, (1 + 1.21*rbinom(100, 1, 0.05)) ))
c(mean(x), sd(x))

# ��������-����������� ������: �������� ���������� ����������� �����������
par(mfrow = c(1,1))
qqnorm(x)
qqline(x)

# ����� ���������

z <- (x - mean(x))/sqrt(var(x))  #  �������������� �������
x.qq <- qqnorm(z, plot.it = FALSE)
x.qq <- lapply(x.qq, sort)
plot(x.qq, ylim = c(-2, 5), ylab = "Z-���������� �������", xlab = "�������� ����������� �������������")

library(boot)
# ��������� 999 ��������-������� (�.�. ��������� ������� �� 
# ����������� ������������� � ����������� ������� z)

# ������������ � ��������-���������� (bootstrap)
x.gen <- function(dat, mle) rnorm(length(dat))
x.qqboot <- boot(z, sort, R = 999, 
                 sim = "parametric",ran.gen = x.gen)
sapply(1:999,function(i) lines(x.qq$x, x.qqboot$t[i,],
                               type = "l", col = "grey"))
points (x.qq, pch = 20)
lines(c(-3, 3), c(-3, 3), col = "red", lwd = 2)

# ����������� ���������
x.env <- envelope(x.qqboot, level = 0.9)
lines(x.qq$x,x.env$point[1, ], lty = 4)
lines(x.qq$x,x.env$point[2, ], lty = 4)
lines(x.qq$x,x.env$overall[1, ], lty = 1)
lines(x.qq$x,x.env$overall[2, ], lty = 1)

# ����� ��������������� �������� �� ���������� car ����� � ������������� ����������
library(car)
qqPlot(x, dist = "norm", col = palette()[1], pch = 19,
       xlab="�������� ����������� �������������", 
       ylab="����������� ��������", 
       main="��������� ��������� ��� � ��")

# ���������� �������������� �������� ������������ ������������� ��������� ��
library(sm)
sm.density(x, model = "Normal", xlab = "������������� �������",
           ylab = "������� ��������� �������������")

# �������������� ����� �� ������������: ������������ � ���������, ����������
shapiro.test(x)
library(nortest)
ad.test(x)
cvm.test(x)
lillie.test(x)
sf.test(x)