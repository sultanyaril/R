#������� �������� ��������

#�������� �������� "�������" � ������� ������� ������������
my.vector <- c(1, 2, 3, 4, 5)
# ����� ������������ � ����������� �������� "=", �� ������� "<-"

#����� ��������
my.vector

#������� �������� �������������������
S <- seq(1,7)
S

#��������� �������� ���� ������������������
S <- seq(from = 1, to = 7, by = 0.5)
S

#�������� ������� �� ������������� ��������
Text <- rep("test", 5)
Text

#������� ����������� ��������
v1 <- c(1, 2, 3)
v2 <- c(4, 5, 6)
V <- c(v1, v2)
V

#��������� ������
text.vect <- c("a", "b", "c")

#����������� � �������� �������� v1
new.vect <- c(v1, text.vect)

#����� ������ ������������� - ���������
new.vect

#�������� ��� ����
class(new.vect)

#���������� ��������� �������
y <- c(5, 3, 2, 6, 1)
y[3]
z <- c(0.5, 0.1, 0.6)
y[1]*z[3]
z[2] <- 0.3

#����� ���������� ���������
y[3:5]

y[c(1, 4)]

#�������� 1 � 4-�� ���������
y[-c(1, 4)]

#������������� �������� ��������� ��� ��������������� ������ ���������
y[y > 2]

#���������� ��������
sort(z) # �� ���������  decreasing = FALSE
sort(z, decreasing = TRUE)

#�������� ������
my.mat <- matrix(seq(1, 16), nrow = 4, ncol = 4)
my.mat

#��������� ������� ���������� �� ���������
my.mat <- matrix(seq(1, 16), nrow = 4, ncol = 4, byrow = TRUE)
my.mat

#��������� �������������� ���� ����� � ��������
rownames(my.mat) <- c("A", "B", "C", "D")
colnames(my.mat) <- c("E", "F", "G", "H")
my.mat

#���������� ������� ������� ������ ��������, ��� ��������� � ���
my.mat2 <- matrix(seq(1, 12), nrow = 4, ncol = 4, byrow = TRUE)
my.mat2

my.mat <- 1:16
my.mat
#��������� ����������� �������
dim(my.mat) <- c(4, 4)
my.mat

#����� �������, ������, ���������������
my.mat[, 4]
my.mat[1, ]
t(my.mat)
#������������ ��������� 1 � 4 �������� �������
my.mat[, 1]*my.mat[, 4]

#������������ ������� �� ��������� ��������
a <- c(1, 2, 3, 4)
b <- c(5, 6, 7, 8)
d <- c(9, 10, 11, 12)
e <- c(13, 14, 15, 16)

# ����������� �������� � �������
cbind(a, b, d, e)
rbind(a, b, d, e)

# ������� �������� � ��������������� ��������
treatment <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
class(treatment)

treatment <- factor(treatment, levels = c(0, 1))
class(treatment)
treatment
levels(treatment) <- c("no", "yes") # ���������������� ����� ������� �������
as.numeric(treatment) # ��������������� ����� � �������� ��������

#��������� ����� ������� ������� ��� ������ ������� gl()
my.fac = gl(2, 8, labels = c("Control", "Treatment"))
my.fac

#��������� �������� �������������� ���������� �� ������������ ���������
x <- c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6, 7)
cut(x, breaks = 3)
cut(x, breaks = 3,  labels = letters[1:3])
cut(x,breaks = quantile(x, c(0, .25, .50, .75, 1)), 
    labels = c("Q1","Q2","Q3","Q4"), include.lowest = TRUE) # �� ���������

#��� include.lowest � ������ �������� NA
cut(x,breaks = quantile(x, c(0, .25, .50, .75, 1)),labels = c("Q1","Q2","Q3","Q4"))


#��� ����������� �������: ���������,�������� � ���������� ��������
vector1 <- c("A", "B", "C")
vector2 <- seq(1, 3, 0.5)
vector3 <- c(FALSE, TRUE)

#����������� �������� � ������ � ������� ���������
my.list <- list(Text = vector1, Number = vector2, Logic = vector3)

# �������� ����������� ������
my.list
my.list$Text
my.list$Number
my.list$Logic

#���������� ��������� ������
my.list$Number[3:5]
my.list[[1]]
my.list[[1]][2]

#������� ����������� ��������� ������
str(my.list)

#�������� ������
city <- c("City1", "City1", "City2", "City2", "City3", "City3")
#�������� ����� � ���������� ����� ���� �� �������
��� <- c("Male", "Female", "Male", "Female", "Male", "Female")
number <- c(12450, 10345, 5670, 5800, 25129, 26000)
CITY <- data.frame(City = city, ��� = ���, Number = number) 

# �������� ��������� �������
str(CITY)

# �������� ����������� ������� CITY � �� ��������� ������������
CITY

CITY$���
CITY[, 2]
CITY["���"]
CITY$Number[1:3] 
CITY$Number[CITY$Number > 10000]
CITY$Number[CITY$��� == "Female"]

#��������� �� �� �������, �� � �������������� []
CITY[4, 3]
CITY[1:3, 3]
CITY[CITY$Number > 10000, 3]
CITY[CITY$��� == "Female", 3]


#�������� ����� ����������, �������� � �������
names(CITY)

#�������� ������ 3 ����� �������
head(CITY, n = 3)

#�������� ��������� 3 ����� �������
tail(CITY, n = 3)

#���������� ������
DF <- data.frame(X1 = c(1, 15, 1, 3), X2 = c(1, 0, 7, 0), X3 = c(1, 0, 1, 2),
                 X4 = c(7, 4, 41, 0), X5 = c(1, 0, 5, 3))
DF
row.names(DF) <- c("A","B","C","D")
DF

#DF1 - �������, ������� ������� ������������� �� �������� ����� ��������
DF1 <-  DF[ , rev(order(colSums(DF)))]
DF1

#DF2 - �������, ������ ������� ������������� � ����������
#������� �� 1 �������, ����� � ���������� �� �������
DF2 <- DF[order(DF$X1, -DF$X2), ]
DF2

#����������� ������
DF1 <- matrix(c(
  12,	22,	0,	1,	0,
  12,	23,	1,	3,	0,
  12,	24,	0,	0,	1),
  nrow = 3, ncol = 5, byrow = TRUE)
DF1

colnames(DF1) <- c("Y", "N", "A", "B", "C")
DF1

DF2 <- matrix(c(
  13,	22,	0,	1,	2,
  13,	23,	0,	3,	0,
  13,	24,	1,	0,	5),
  nrow = 3, ncol = 5, byrow = TRUE)
DF2

colnames(DF2) <- c("Y",	"N", "A", "B", "D")
DF2

DF1 <-as.data.frame(DF1)
DF1
DF2 <-as.data.frame(DF2)
DF2

cbind(DF1, DF2)

DF1[, names(DF2)[!(names(DF2) %in% names(DF1))]] <- NA
DF2[, names(DF1)[!(names(DF1) %in% names(DF2))]] <- NA
rbind(DF1, DF2)
#����� ����������� ������������� ���
merge(DF1, DF2, all = TRUE)

#������� ������ �� �������� � ������

#����� � �������� �������
Sys.time()

#���������� ��������� ��������� �� ��������� ������������� �������:
substr(as.character(Sys.time()), 1, 10)
substr(as.character(Sys.time()), 12, 19)
unclass(Sys.time())

#��������������� � ������ POSIXlt
date <- as.POSIXlt(Sys.time())
#�������� ������� POSIXlt
date$wday # ���� ������
date$yday # ��������� ����� ��� � ����
unlist(unclass(date))

#�������� ���������� ������ Date � �������� ��� ����
t1 <- as.POSIXlt("2021-09-01")
t2 <- as.POSIXlt("2021-09-13") 

#������� ����� ����� ������ 
t1 - t2

t3<-as.POSIXlt("2021-09-13 12:50:00")
t4<-as.POSIXlt("2021-09-13 14:15:30")
#������� ����� ����� ��������� ������� 
t4-t3

difftime("2021-09-13", "2021-09-01")
as.numeric(difftime("2021-09-13", "2021-09-01"))