#Примеры создания векторов

#Значения вносятся "вручную" с помощью функции конкатенации
my.vector <- c(1, 2, 3, 4, 5)
# Можно использовать и стандартный оператор "=", но принято "<-"

#Вывод значений
my.vector

#Функция создания последовательностей
S <- seq(1,7)
S

#Допустимо указание шага последовательности
S <- seq(from = 1, to = 7, by = 0.5)
S

#Создание вектора из повторяющихся значений
Text <- rep("test", 5)
Text

#Примеры объединения векторов
v1 <- c(1, 2, 3)
v2 <- c(4, 5, 6)
V <- c(v1, v2)
V

#Текстовый вектор
text.vect <- c("a", "b", "c")

#Объединение с числовым вектором v1
new.vect <- c(v1, text.vect)

#Новый вектор автоматически - текстовый
new.vect

#Проверим это явно
class(new.vect)

#Индексация элементов массива
y <- c(5, 3, 2, 6, 1)
y[3]
z <- c(0.5, 0.1, 0.6)
y[1]*z[3]
z[2] <- 0.3

#Вывод нескольких элементов
y[3:5]

y[c(1, 4)]

#Удаление 1 и 4-го элементов
y[-c(1, 4)]

#Использование условных выражений для автоматического выбора элементов
y[y > 2]

#Сортировка векторов
sort(z) # по умолчанию  decreasing = FALSE
sort(z, decreasing = TRUE)

#Создание матриц
my.mat <- matrix(seq(1, 16), nrow = 4, ncol = 4)
my.mat

#Изменение порядка заполнения по умолчанию
my.mat <- matrix(seq(1, 16), nrow = 4, ncol = 4, byrow = TRUE)
my.mat

#Изменение автоматических имен строк и столбцов
rownames(my.mat) <- c("A", "B", "C", "D")
colnames(my.mat) <- c("E", "F", "G", "H")
my.mat

#Заполнение матрицы меньшим числом значений, чем элементов в ней
my.mat2 <- matrix(seq(1, 12), nrow = 4, ncol = 4, byrow = TRUE)
my.mat2

my.mat <- 1:16
my.mat
#Изменение размерности вектора
dim(my.mat) <- c(4, 4)
my.mat

#Вывод столбца, строки, траспонирование
my.mat[, 4]
my.mat[1, ]
t(my.mat)
#Поэлементное умножение 1 и 4 столбцов матрицы
my.mat[, 1]*my.mat[, 4]

#Формирование матрицы из отдельных векторов
a <- c(1, 2, 3, 4)
b <- c(5, 6, 7, 8)
d <- c(9, 10, 11, 12)
e <- c(13, 14, 15, 16)

# Объединение векторов в матрицу
cbind(a, b, d, e)
rbind(a, b, d, e)

# Примеры создания и конвертирования факторов
treatment <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
class(treatment)

treatment <- factor(treatment, levels = c(0, 1))
class(treatment)
treatment
levels(treatment) <- c("no", "yes") # пользовательские метки уровней фактора
as.numeric(treatment) # конвертирование меток в числовые значения

#Генерация меток уровней фактора при помощи функции gl()
my.fac = gl(2, 8, labels = c("Control", "Treatment"))
my.fac

#Разбиение значений количественной переменной на ограниченные интервалы
x <- c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6, 7)
cut(x, breaks = 3)
cut(x, breaks = 3,  labels = letters[1:3])
cut(x,breaks = quantile(x, c(0, .25, .50, .75, 1)), 
    labels = c("Q1","Q2","Q3","Q4"), include.lowest = TRUE) # по квартилям

#Без include.lowest в выводе появится NA
cut(x,breaks = quantile(x, c(0, .25, .50, .75, 1)),labels = c("Q1","Q2","Q3","Q4"))


#Три разнотипных вектора: текстовые,числовые и логические значения
vector1 <- c("A", "B", "C")
vector2 <- seq(1, 3, 0.5)
vector3 <- c(FALSE, TRUE)

#Объединение векторов в список с именами компонент
my.list <- list(Text = vector1, Number = vector2, Logic = vector3)

# Просмотр содержимого списка
my.list
my.list$Text
my.list$Number
my.list$Logic

#Индексация элементов списка
my.list$Number[3:5]
my.list[[1]]
my.list[[1]][2]

#Функция определения структуры списка
str(my.list)

#Создание таблиц
city <- c("City1", "City1", "City2", "City2", "City3", "City3")
#Названия полей и переменных могут быть на русском
пол <- c("Male", "Female", "Male", "Female", "Male", "Female")
number <- c(12450, 10345, 5670, 5800, 25129, 26000)
CITY <- data.frame(City = city, Пол = пол, Number = number) 

# Просмотр структуры таблицы
str(CITY)

# Просмотр содержимого таблицы CITY и ее отдельных составляющих
CITY

CITY$Пол
CITY[, 2]
CITY["Пол"]
CITY$Number[1:3] 
CITY$Number[CITY$Number > 10000]
CITY$Number[CITY$Пол == "Female"]

#Повторяем те же команды, но с использованием []
CITY[4, 3]
CITY[1:3, 3]
CITY[CITY$Number > 10000, 3]
CITY[CITY$Пол == "Female", 3]


#Просмотр имене переменных, входящих в таблицу
names(CITY)

#Просмотр первых 3 строк таблицы
head(CITY, n = 3)

#Просмотр последних 3 строк таблицы
tail(CITY, n = 3)

#Сортировка таблиц
DF <- data.frame(X1 = c(1, 15, 1, 3), X2 = c(1, 0, 7, 0), X3 = c(1, 0, 1, 2),
                 X4 = c(7, 4, 41, 0), X5 = c(1, 0, 5, 3))
DF
row.names(DF) <- c("A","B","C","D")
DF

#DF1 - таблица, столбцы которой отсортированы по убыванию суммы значений
DF1 <-  DF[ , rev(order(colSums(DF)))]
DF1

#DF2 - таблица, строки которой отсортированы в восходящем
#порядке по 1 столбцу, затем в нисходящем по второму
DF2 <- DF[order(DF$X1, -DF$X2), ]
DF2

#Объединение таблиц
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
#Лучше объединение реализовывать так
merge(DF1, DF2, all = TRUE)

#Примеры работы со временем и датами

#Время в машинном формате
Sys.time()

#Извлечение отдельных элементов из машинного представления времени:
substr(as.character(Sys.time()), 1, 10)
substr(as.character(Sys.time()), 12, 19)
unclass(Sys.time())

#Конвертирование в формат POSIXlt
date <- as.POSIXlt(Sys.time())
#Элементы формата POSIXlt
date$wday # день недели
date$yday # прядковый номер дня в году
unlist(unclass(date))

#Создание переменных класса Date и операции над ними
t1 <- as.POSIXlt("2021-09-01")
t2 <- as.POSIXlt("2021-09-13") 

#Разница между двумя датами 
t1 - t2

t3<-as.POSIXlt("2021-09-13 12:50:00")
t4<-as.POSIXlt("2021-09-13 14:15:30")
#Разница между двумя моментами времени 
t4-t3

difftime("2021-09-13", "2021-09-01")
as.numeric(difftime("2021-09-13", "2021-09-01"))