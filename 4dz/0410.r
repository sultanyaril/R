# �������� ��������� ���������

# ������ ������� Motor Trend (1974)
data(mtcars)
dotchart(mtcars$mpg * 0.425144, labels = row.names(mtcars),
         main="���������� (� ��), ������� ��������� ���������� �� 1 �����",
         xlab="��������", cex = 0.8)
# ������������ ������ ��� ������� ������� � ������ + ��

# ��� ��������� ������� ��������� ������
x <- mtcars[order(mtcars$mpg), ]

# �������� �������� �������
x$cyl <- factor(x$cyl)

# ������ ���� ��� �����������
x$color[x$cyl==4] <- 1
x$color[x$cyl==6] <- 2
x$color[x$cyl==8] <- 3
dotchart(x$mpg, labels = row.names(x),
         groups = x$cyl, gcolor = "blue", pch = 16,
         main="������������� ��������� � 32 ������� �����������",
         xlab="����/������", cex = 0.8, color = x$color)
?mtcars
# � ������� ������� ?mtcars ������������ �� ���������� � ��������� ������
# �������� ����������� ������ ��� ������-���� ������� ������� 
x <- mtcars[order(mtcars$hp), ]
x$carb <- factor(x$carb)
cnt = 1
for (i in levels(factor(x$carb))) {
  x$color[x$carb==i] <- cnt
  cnt = cnt + 1
}

dotchart(x$hp, labels = row.names(x),
         groups = x$carb, gcolor = "blue", pch = 16,
         main="�������� � 32 ������� �����������",
         xlab="���������� ���", cex = 0.8, color = x$color)

# ���������� ��������� ���������
data(InsectSprays)
names(InsectSprays)

stripchart(InsectSprays$count ~ InsectSprays$spray,
           ylab = "���������� �������� ���������",
           xlab = "����������",
           vertical = TRUE,
           method = "jitter",
           jitter = 0.1,
           pch = 1, col = "purple")

# �������� jitter ������������ ��� �������� ��������� ������� � ������ ��� �������� ������������

# boxplot() ���������� ������ �������, �� � ������ �� stripchart() - ��� ����������

# �������� outline ���������� ������������� ������ ��������
boxplot(count ~ spray, 
        outline = FALSE, xlab = "�����������",
        ylab = "���������� �������� ���������",
        main = "������������� ������������",
        col = "wheat2", data = InsectSprays)

# �������� add ��������� ������� ����� �� ��� ������� ������
stripchart(count ~ spray, method="stack", 
           data = InsectSprays, add = TRUE,
           pch = 1, , lwd = 1.7, col = "purple", vertical = TRUE)

#����������� ��� ����������� ������

# ������ ���������� �������������
data(mtcars)

# ������� ��������������
mean(mtcars$mpg)

# �������
median(mtcars$mpg)

# ���������
var(mtcars$mpg)

# ����������� ����������
sd(mtcars$mpg)

# ����������� ��������
min(mtcars$mpg)

# ������������ ��������
max(mtcars$mpg)
max(mtcars$disp)

# ����������� ������ �������� - �������
SEmpg = sd(mtcars$mpg)/sqrt(length(mtcars$mpg))

# ��������
quantile(mtcars$mpg) #��������
quantile(mtcars$mpg, p = seq(0, 1, 0.1))

# ���������������� ������
IQR(mtcars$mpg)

# ������������� ������� ������ �������
library(moments)  #�������� ������ moments
kurtosis(mtcars$mpg, na.rm = TRUE)
skewness(mtcars$mpg, na.rm = TRUE)

# ������� ������� ������������� ��������
mtcars$mpg[3] <- NA

head(mtcars$mpg)
mean(mtcars$mpg)

# ����� ������� ��������� � � ������ ��������
mean(mtcars$mpg, na.rm = TRUE)

# ����� ������� ������� � ������ NA
length(mtcars$mpg)
# � ������ - ���
sum(!is.na(mtcars$mpg))

# ����� ��������� � ��������
which.min(mtcars$mpg)
which.max(mtcars$mpg)

# ����������� �� �������������� � ������ (�������� �����������)
rownames(mtcars)[which.min(mtcars$mpg)]
rownames(mtcars)[which.max(mtcars$mpg)]

# ������������� ������� apply:  ������� ����� ��������� � ������� � �������������� (0)
# � ������ (1) ��������� ������� 
tapply(X = mtcars$disp, INDEX = mtcars$am, FUN = mean)
# tapply(X = mtcars$disp, INDEX = list(mtcars$am, mtcars$vs), FUN = mean)
SE <- function(x) {sd(x)/sqrt(length(x))}
tapply(X = mtcars$disp, INDEX = mtcars$am, FUN = SE)

# ������������� ������� summary():
summary(mtcars)
summary(mtcars$mpg)

# vs � am - �������, ��� �� � ����������
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
is.factor(mtcars$vs)
is.factor(mtcars$am)
summary(mtcars)

# ����� Hmisc, ������� describe()
library("Hmisc")
describe(mtcars)

# ����� pastecs, ������� stat.desc():
library(pastecs)
stat.desc(mtcars)

# ����� psych, ������� describe.by() - ������ ����������
# ������������ ���������� ��� ������� ������ ���������� �������
install.packages("psych")
library(psych)
describeBy(mtcars, mtcars$am)

# ����� doBy, ������� summaryBy()
library(doBy)
summaryBy(mpg + wt ~ cyl + vs, data = mtcars,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

# ������������� �������� � ������
install.packages("outliers")
library(outliers)
set.seed(1234)
x = rnorm(10)

grubbs.test(x) #������������ ������, ��������� ����� ���������� ������������� ��� �������
grubbs.test(x,type=20) #2 ������� �� ����� ������
grubbs.test(x,type=11) #2 ������� �� ������ ������

dixon.test(x)
dixon.test(x,opposite=TRUE)
dixon.test(x,type=10)

# ���������� ���������

# ����������� � ��������������� ���������
#install.packages("VIM")
library(VIM)
data(sleep, package = "VIM")
head(sleep)

# ���������� ���������� �� ��������� ��� � 62 ������������� ������ �����
# ��������� ����������: ����������������� ��� �� ������������ (Dream),
# ��� ��� ���������� (NonD) � �� ����� (sleep)
# ��������������� ����������: ����� ���� (BodyWgt), ��� ����� (BrainWgt),
# ����������������� ����� (Span) � ����� ������������ (Gest).
# ������������� ����������: 5-������� ������ ������� ����������� �������� (Pred), ���� ������������ �� ����� ��� ��� (Exp): �� �������� ���� �� �����  ���� ��������� ������������, � ���������� ����� (Danger) �� ������  ���������� ���������� Pred � Exp

# ������ �����, � ������� ��� ����������� ��������
sleep[complete.cases(sleep), ]

# ������ �����, � ������� ���� ���� �� ���� ����������� ��������
sleep[!complete.cases(sleep), ]
sum(is.na(sleep$Dream))

#�������������� �������������� ����������
library(mice)
# ���� � ������� ������������� ����������� ���������: � ������ ������ ��������� ���,
# ����������� ����������� �� ����� �� ���������
# ������ ������� ��������� ����� ������� � ������ ������ �������� ������
# ��������� ������� � ����� ���������� � �������������� ���������� � ������ ������
md.pattern(sleep)

# �������� ������ �������������� � ��������� [0, 1] � ������������ �������� �������
# ����� ������� ������� �������� ������� ��������
# ����������� �������� ������������ ������� ������
matrixplot(sleep)
aggr(sleep)


#����������� ���������� ����� ����������
# ��������� ������� �� ���������� 1 � ������ ���������
x <- as.data.frame (abs (is.na(sleep)))
y <- x[, which(colSums(x) > 0)]
print(cor(y),4)
cor(sleep, y, use = "pairwise.complete.obs")

#���������� ����������� ��������, Multivariate Imputation by Chained Equations
imp <- mice(sleep, seed = 1234)
fit <- with(imp, lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)

# ���������� ��������� � ���������� ���������� ��� �������������
sleep_imp3 <- complete(imp, action = 3)
head(sleep_imp3)
sleep[!complete.cases(sleep_imp3), ]
save(sleep_imp3, file = "sleep_imp.Rdata")

