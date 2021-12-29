# ������������� ������ (������������ � �������)

# ������ ��� ������ ���������� ������
library(gamair)
data(hubble)
str(hubble)

M <- lm(y ~ x - 1, data = hubble) # "-1" - ��� ���������� ���������� �����
summary(M)

# ������ ������� ��������� "�������"
(hub.const <- 76.581/3.09e19)
(age <- 1/hub.const)/(60^2*24*365)

# ������ ������������� ���������� ���������
CPI.df <- cbind(predict(M,interval ="conf"), predict(M,interval ="pred"))  
CPI.df <- CPI.df[,-4] 
colnames(CPI.df) <- c("Y_fit","CI_l","CI_u","PI_l","PI_u")
head(CPI.df)

par(mfrow = c(1, 1))
matplot(hubble$x, CPI.df, type = "l", 
        lwd = c(2, 1, 1, 1, 1), col = c(1, 2, 2, 4, 4),
        ylab = "��������, ��/�",xlab="����������, ���")
with(hubble, matpoints(x, y, pch = 20))

# ������ ������������� ����������, ��������������� ������
beta <- summary(M)$coefficients[1]
SE <- summary(M)$coefficients[2] # ����������� ������
SE
ci.lower <- beta - qt(0.975, df = 23)*SE
ci.upper <- beta + qt(0.975, df = 23)*SE
c(ci.lower, ci.upper)
Uni.upper <- 1/(ci.lower*60^2*24*365.25/3.09e19)
Uni.lower <- 1/(ci.upper*60^2*24*365.25/3.09e19)
c(Uni.lower, Uni.upper)

# ������ ������������� ���������� ��������� ����������
regr <- function(data, indices) {
  # ������ indices ����� ������������� �������� boot() 
  dat <- data[indices, ] 
  fit <- lm(y ~ -1 + x, data = dat)
  return(summary(fit)$coefficients[1])
} #�������������� ��� ����������� ����� �������� ���������

library(boot)
(results <- boot(data = hubble, statistic = regr, R = 1000))

plot(results)

quantile(results$t, c(0.025, 0.975))
(U.lower <- 1/(85.73249*60^2*24*365.25/3.09e19))
(U.upper <- 1/(67.07360*60^2*24*365.25/3.09e19))
# ����� �������� ��������� ������
boot.ci(results, type = "bca")

# ������ ������������� ���������� ��������� ������� ��������
library(arm)
simulations <- sim(M, 1000)
hist(simulations@coef, breaks = 30)
sd(simulations@coef)
quantile(simulations@coef, c(0.025, 0.975))

# ������ �������� ������
summary(M)
hubble$fit = fitted(M)

library(ggplot2)

p1 = ggplot(hubble, aes(x, y)) + geom_point() +
  geom_hline(aes(yintercept=mean(hubble$y)), color = "blue") +
  geom_segment(aes(x = x, y = y, xend = x, yend = mean(hubble$y))) +
  ggtitle("����� ����� ��������� TSS")

p2 = ggplot(hubble, aes(x, y)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_segment(aes(x = x, y = y, xend = x, yend = fit)) +
  ggtitle("����� ��������� �������� RSS")

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)


# ��������������� ����
x = 1:50
set.seed(200)
y = rnorm(50, 0.1 + 0.2*x + 0.035*(x^2), 10)

# �������� ������
M1 <- lm(y ~ x)
summary(M1)

# �������������� ������
M2 <- lm(y ~ x + I(x^2))
summary(M2)

par(mfrow = c(2, 2))
plot(M2)


# �������������� ���������

# ������ � ������
Y <- c(0.155, 0.15, 0.165, 0.115, 0.11, 0.16, 0.17, 0.355,
       0.43, 0.305, 0.315, 0.46, 0.545, 0.47, 0.45, 0.51, 0.525)
X <- c(1, 9, 11, 17, 18.5, 25, 36, 40.5, 42, 44, 45, 51, 54,
       55.5, 57, 61, 67)

# ��������� �������� 1-4 ��������
allModel <- lapply(1:4,function(k) lm(Y ~ poly(X, k, raw = TRUE)))
extract <- function(fit) {
  sigma <- summary(fit)$sigma  # ��������������������� ������
  R2.adj <- summary(fit)$adj.r.squared  # ����������������� ����������� R2 
  aic <- AIC(fit)           #  �������������� �IC-��������
  out <- data.frame(sigma = sigma, R2.adj = R2.adj, AIC = aic)
  return(out)    }

result <- lapply(allModel, extract)
result <- as.data.frame(matrix(unlist(result), nrow = 4, byrow = T))

# ���������� �������������������� ������ �����-���������
M.ErCV <- sapply(1:4,function(k) {
  n <- length(X) ; Err_S  <- 0 
  for(i in 1:n)    { Xcv <- X[-i]
  lm.temp <- lm( Y[-i] ~ poly(Xcv, k , raw = TRUE))
  YR <- predict(lm.temp, newdata=data.frame(Xcv=X))[i]
  Err_S <- Err_S + (Y[i] -  YR)^2  }  
  sqrt(Err_S/n) })

# ����� ����������� � ���� �������
result<-cbind(result, M.ErCV)
colnames(result) <- c("��. ����������", "R2Adj", "�������� AIC", "������ �����-���������")
rownames(result) <- c("������� 1 �������","������� 2 �������",
                      "������� 3 �������","������� 4 �������")
result

# ������������
par(mfrow = c(1, 1))
plot(X, Y, type = "p", pch = 22, bg = "yellow", 
     xlab = "���������� �� ����������", ylab = "������� ������ Lap94")
sapply(1:4, function(k) points(X,
                               predict(lm(Y ~ poly(X, k, raw = TRUE))),
                               type = "l", col = k, lwd = 2))
legend("topleft", c("k = 1", "k = 2","k = 3","k = 4"),
       col = 1:4, lwd = 2)


# ���������� ������������� ������

# ������
S1 <- c(3, 7.5,12.5,17.5,22.5,27.5)  
p1 <- c(50,23, 45 , 25, 2,  9)
log.ss1 <- nls(p1 ~ SSlogis(S1, phi1, phi2, phi3))
summary(log.ss1)

# ������������� ��������� ���������� ������
Rsquared <- 1 - var(residuals(log.ss1))/var(p1)

# ������������ �� ������ �������� �������
x <- 0:max(S1)
pr1 = predict(log.ss1, data.frame(S1 = x))

## �������, ��� ����������� ���������� � SSlogis ����� ��� "S1", 
## � ������� �� �������� ��� ������� � ������� ������������ ��������
### ��������� ������ ��������� �������� ��������������
se.fit <- sqrt(apply(attr(pr1,"gradient"), 1, 
                     function(mat) sum(vcov(log.ss1)*outer(mat, mat))))
PCI <- pr1 + outer(se.fit, qnorm(c(.5, .025, .975)))

# ����������� ����� �������
a <- coef(log.ss1)[1]*c(0.05, 0.5, 0.95)

# ����������� ����� �������
plx<-approx(pr1, x, xout = a)$y

matplot(x, PCI,type="l", xlab = "�������������, �/�",
        ylab = "���� ������������� � �����,%",
        lty = c(1,3,3), lwd = c(2,1,1), ylim = c(0,60))
matpoints(S1, p1, pch=20)
text(5, 5, bquote(R^2==.(round(Rsquared, 3))))
sapply(1:3, function(i)
  lines (c(0, plx[i], plx[i]), c(a[i], a[i], 0), lty = 2))

# ������������� ���������
load(file = "sleep_imp.Rdata")
M <- lm(Sleep ~ BodyWgt + BrainWgt + Span + Gest + Pred + Exp + Danger,
        data = sleep_imp3)
summary(M)
AIC(M)

# ������� ������������� ������ (��������������� ������ �� ������ AIC)
Mstep <- step(M, direction = "both")
summary(Mstep)
AIC(Mstep)

# ������������� ������ � ��������������

# �����-���������
library(lars) #��������, ����������� ���������� �������
Xmat <- as.matrix(sleep_nl[, -1])
M.las <- lars(Xmat, sleep_nl[, 1], type = "lasso")

par(mfrow = c(1, 2))
plot(M.las, plottype = "coefficients")
plot(M.las, plottype = "Cp")

# �������� ���������������� S ������ ������������ ���������
set.seed(0)
par(mfrow = c(1, 1))
r <- cv.lars(Xmat, sleep_nl[, 1]) 
(bestfrac <- r$index[which.min(r$cv)])

(las.coef <- predict(M.las, Xmat, s = bestfrac, 
                    type = "coefficient", mode = "fraction"))

# ������� ������
las.resid <- sleep_nl$Sleep - predict.lars(M.las, Xmat, s = bestfrac,
                                           type ="fit", mode = "fraction")$fit
rss.lasso <- sum(las.resid^2)/(nrow(sleep_nl) - 7)

# ��������� ���������
library(MASS)
M.ridge <- lm.ridge(Sleep ~ ., data = sleep_nl, lambda = seq(0,2,0.1))

plot(x = M.ridge$lambda, y = M.ridge$GCV, type = "o")

(lambda <- M.ridge$GCV[which.min(M.ridge$GCV)])

(M.ridge1 <- lm.ridge(Sleep ~ ., data = sleep_nl, lambda = lambda))

# ������������ ������
beta.M.ridge1 <- coef(M.ridge1)
m <- length(beta.M.ridge1)

# ������� ������
resid.ridge <- sleep_nl$Sleep - beta.M.ridge1[1] - 
  as.matrix(sleep_nl[,2:m])%*%beta.M.ridge1[2:m]

# ����� �������� ������� ��������� ���������
d <- svd(as.matrix(sleep_nl[, 2:m]))$d
(df <- nrow(sleep_nl) - sum(d^2/(lambda+d^2)))

# ������� ������� ����������
(rss.ridge <- sum(resid.ridge^2)/df)