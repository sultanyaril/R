# ������ ���������� ���� ��������� �������

# ������� ������������ ��������� ��� ������������� ���������� �������, �������� � ��������

dat1 <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
dat2 <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

# ����� ��. ������� cor()

# ��������� ������������ ������������� ������ � ��������� ���������� ���������� �������
shapiro.test(dat1)
shapiro.test(dat2)
cor.test(dat1, dat2)

# ���������� ��� ����������
shapiro.test(log (dat1))
shapiro.test(log(dat2))
cor.test(log(dat1), log(dat2))

# ����������� ���������� ��������
cor.test(dat1, dat2, method = "spearman")
cor.test(dat1, dat2, method = "spearman", alternative = "greater")
cor.test(dat1, dat2, method = "spearman", alternative = "less")

# ���������� ��� ����������
cor.test(log (dat1), log (dat2), method = "spearman")
cor.test(log (dat1), log (dat2), method = "spearman", alternative = "greater")
cor.test(log (dat1), log (dat2), method = "spearman", alternative = "less")

# �������� ����������� ���������� ��������
cor.test(dat1, dat2, method = "kendall")
cor.test(dat1, dat2, method = "kendall", alternative = "greater")
cor.test(dat1, dat2, method = "kendall", alternative = "less")

# ���������� ��� ����������
cor.test(log (dat1), log (dat2), method = "kendall")
cor.test(log (dat1), log (dat2), method = "kendall", alternative = "greater")
cor.test(log (dat1), log (dat2), method = "kendall", alternative = "less")

# �������� ������������ ����� ���������?

# �������� ��-������� ��� ������ �������������

mice <- matrix(c(13, 44, 25, 29), nrow = 2, byrow = TRUE,
               dimnames = list(c("�������", "������"),
                               c("����� � ���������", "�����")))
mice
# �������������� ������� �� ������ ��-������� �����
chisq.test(mice) 

# ������� ������������� ������������� �������
light <- c(12, 40, 45)
dark <- c(87, 34, 75)
very.dark <- c(3, 8, 2)
color.data <- matrix(c(light, dark, very.dark), nrow = 3,
                     dimnames = list(c("Pop1", "Pop2", "Pop3"),
                                     c("Light", "Dark", "Very dark")))
color.data

# ��� �������� ������ 5 � very.dark ����� � ������������� ��������������
chisq.test(color.data)

# ������ ���� ������
X <- matrix(c(1, 10, 8, 4), ncol = 2,
               dimnames = list(c("New England Journal of Medicine", "Lancet"),
                               c("����  ������", "���� �� ������")))
X
fisher.test(X) # �������, ��� ����� odds ratio (��������� ������)

# �������� ��������� (McNemar)

# ������ � "�������" �������
data <- read.table(header = TRUE, con <- textConnection("
 subject time result
       1  pre      0
       1 post      1
       2  pre      1
       2 post      1
       3  pre      0
       3 post      1
       4  pre      1
       4 post      0
       5  pre      1
       5 post      1
       6  pre      0
       6 post      1
       7  pre      0
       7 post      1
       8  pre      0
       8 post      1
       9  pre      0
       9 post      1
      10  pre      1
      10 post      1
      11  pre      0
      11 post      0
      12  pre      1
      12 post      1
      13  pre      0
      13 post      1
      14  pre      0
      14 post      0
      15  pre      0
      15 post      1 
"))

data
                   
library(reshape2)

# ����������� ������ � "�������" ������
data.wide <- dcast(data, subject ~ time, value.var = "result")
data.wide

# ���������� ������� ������������� �� ������
ct <- table(data.wide[, c("pre","post")])
ct

mcnemar.test(ct)
# ��� �������� �������� 
mcnemar.test(ct, correct = FALSE) 

# �������� �������-�������-������� (Cochran-Mantel-Haenszel)
drug <-
  array(c(11, 10, 25, 27,
          16, 22, 4, 10,
          14, 7, 5, 12,
          2, 1, 14, 16,
          6, 0, 11, 12,
          1, 0, 10, 10,
          1, 1, 4, 8,
          4, 6, 2, 1),
        dim = c(2, 2, 8),
        dimnames = list(
          Group = c("��������", "��������"),
          Response = c("�������", "��������"),
          Center = c("1", "2", "3", "4", "5", "6", "7", "8")))
drug

# ���� � ������� ��� �������� �� �������������, ������������� ����� � �.�.
mantelhaen.test(drug) 

library(reshape) # ��� ������� melt()
drug.df <- data.frame(melt(drug,
                            id=c("Center", "Group", "Response")))

library(ggplot2)
attach(drug.df)
drug.df



p <- ggplot(data = drug.df, aes(x = Center, y = value, fill = Response))+xlab("�������")+ylab("����")
p + geom_bar(stat = "identity", position = "dodge") + facet_grid(Group~.)
# ������������� ������ � "������" ��������
