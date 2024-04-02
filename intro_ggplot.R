
library(readxl)
library(ggplot2)


## Данные для визуализации 

fev <- read_excel("data/fev.xls", 
                  sheet = "tidy_data", 
                  col_names = TRUE, 
                  na = "NA", 
                  skip = 1 )


## Анализируем структуру данных

names(fev)
str(fev)

## Изменяем формат переменных
fev$Sex <- factor(fev$Sex)
fev$Smoker <- factor(fev$Smoker)


## Убираем из датафрейма неполные строки
fev[which(!complete.cases(fev)), ] 

fev <- fev[complete.cases(fev), ]



## Визуализация данных (первый заход)

ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point()



## убираем серый фон
ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point() + theme_bw()



## Меняем темы

ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point() + theme_classic()

ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point() + theme_minimal()


## Устанавливаем понравившуюся тему, как основную.
theme_set(theme_bw()) 

ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point()



## Изменяем подписи осей

ggplot(data = fev, aes(x = Age, y = FEV)) + 
  geom_point() + 
  labs(x = "Возраст", y = "Объем легких")



## Создаем верхний заголовок рисунка

ggplot(data = fev, aes(x = Age, y = FEV)) + 
  geom_point() + 
  labs(x = "Возраст", y = "Объем легких", 
       title = "Зависимость между \n возрастом и объемом легких")



## Делаем заголовок центральным

ggplot(data = fev, aes(x = Age, y = FEV)) + 
  geom_point() + 
  labs(x = "Возраст", y = "Объем легких", 
       title = "Зависимость между \n возрастом и объемом легких") + 
  theme(plot.title = element_text(hjust = 0.5))



## Меняем размер точек

ggplot(data = fev, aes(x = Age, y = FEV)) + 
  geom_point(size = 3) + 
  labs(x = "Возраст", y = "Объем легких", 
       title = "Зависимость между \n возрастом и объемом легких") + 
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = fev, aes(x = Age, y = FEV)) + 
  geom_point(size = 0.1) + 
  labs(x = "Возраст", y = "Объем легких", 
       title = "Зависимость между \n возрастом и объемом легких") + 
  theme(plot.title = element_text(hjust = 0.5))


## Меняем цвет и форму точек

ggplot(data = fev, aes(x = Age, y = FEV)) + 
  geom_point(color = "blue") + 
  labs(x = "Возраст", y = "Объем легких", 
       title = "Зависимость между \n возрастом и объемом легких") + 
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = fev, aes(x = Age, y = FEV)) + 
  geom_point(shape = 22, color = "red", fill = "yellow", size = 2) + 
  labs(x = "Возраст", y = "Объем легких", 
       title = "Зависимость между \n возрастом и объемом легких") + 
  theme(plot.title = element_text(hjust = 0.5))




## Сохраняем рисунок в файл ##

ggplot(data = fev, aes(x = Age, y = FEV)) + 
  geom_point(shape = 22, color = "red", fill = "yellow", size = 2) + 
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))


ggsave("figures/MyPicture.wmf", plot = last_plot())


## Рисунок-переменная

Plot_1 <- ggplot(data = fev, aes(x = Age, y = FEV)) + 
  geom_point(shape = 22, color = "red", fill = "yellow", size = 2)

Plot_1


Plot_1 + 
  labs(x = "Возраст", y = "Объем легких", 
       title = "Зависимость между \n возрастом и объемом легких") +
  theme(plot.title = element_text(hjust = 0.5))



Plot_2 <- Plot_1 +
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave("figures/MyPicture_2.wmf", plot = Plot_2)

## Эстетики (Aesthetics)

## Отражаем данные о поле с помощью цвета

Plot_1 <- ggplot(data = fev, aes(x = Age, y = FEV, color = Sex )) + 
  geom_point(size = 2) + 
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + 
  theme(plot.title = element_text(hjust = 0.5))

Plot_1




## Меняеем цвет на тот, который нам нравится

Plot_1 <- Plot_1 + scale_color_manual(values = c("pink","blue"))
Plot_1


## Меняеем положение легенды

Plot_1  + theme(legend.position =  "bottom")

Plot_1  + theme(legend.position =  "left")

Plot_1  + theme(legend.position =  c(0.1, 0.9)) 



## Отражаем данные о поле с помощью формы точек
Plot_1 <- ggplot(data = fev, aes(x = Age, y = FEV, shape = Sex, color = Sex )) +
  geom_point(size = 2) + 
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + 
  theme(plot.title = element_text(hjust = 0.5))

Plot_1



# В нашем датафрейме есть еще и данные о курении
# Если мы хотим выразить графиком одновременно данные по полу и по курению, то мы должны задать две разные эстетики

Plot_1 <- ggplot(data = fev, aes(x = Age, y = FEV, shape = Sex, color = Smoker )) + 
  geom_point(size = 2) + 
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + 
  theme(plot.title = element_text(hjust = 0.5))

Plot_1 




## Используем фасетирование 

Plot_1 + facet_wrap( ~ Smoker) 

Plot_1 + facet_grid(Sex ~ Smoker)



##Частотные распределения

ggplot(fev, aes(x = FEV)) + geom_histogram()

ggplot(fev, aes(x = FEV)) + geom_histogram(binwidth = 1)

ggplot(fev, aes(x = FEV)) + geom_histogram(binwidth = 0.1)

ggplot(fev, aes(x = FEV)) + stat_bin(geom = "bar") #Аналогично!

ggplot(fev, aes(x = FEV)) + stat_bin(geom = "bar", binwidth = 0.1)

ggplot(fev, aes(x = FEV)) + stat_bin(geom = "line", size = 1, color = "red")

ggplot(fev, aes(x = FEV)) + stat_bin(geom = "area", size = 1, color = "red")

ggplot(fev, aes(x = FEV)) + stat_density(geom = "area", size = 1, color = "red", fill = "blue")


ggplot(fev, aes(x = FEV)) + geom_histogram() + facet_wrap( ~ Sex)

ggplot(fev, aes(x = FEV)) + geom_histogram() + facet_wrap( ~ Sex, ncol = 1) 

ggplot(fev, aes(x = FEV, fill = Smoker)) + geom_histogram() + facet_wrap( ~ Sex, ncol = 1) 


ggplot(fev, aes(x = FEV, fill = Smoker)) + stat_density(geom = "area", size = 1, color = "red") + facet_wrap( ~ Sex, ncol = 1)



# Визуализация данных с использованием простейшей статистической обработки

## Боксплоты
ggplot(fev, aes(x = Smoker, y = FEV)) +
  geom_boxplot()


## Боксплоты для нескольких уровней группировки

## Добавляем данные по полу

ggplot(fev, aes(x = Smoker, y = FEV)) +
  geom_boxplot(aes(fill = Sex))


## Знакомимся с пакетом `dplyr`

library(dplyr)

## Конвейерная обработка данных

fev_summary <-
fev %>% 
  filter(Smoker == "Non") %>% 
  group_by(Age) %>% 
  summarize(Mean_FEV = mean(FEV))



## Строим столбчатую диаграмму

ggplot(fev_summary, aes(x = Age, y = Mean_FEV)) +
  geom_col(fill = "gray50") +
  labs(x = "Возраст", y = "Средний объем легких")


## Конвейерная обработка данных, которая сразу дает рисунок

fev %>% 
  filter(Smoker == "Non") %>% 
  group_by(Age) %>% 
  summarize(Mean_FEV = mean(FEV)) %>% 
    ggplot(aes(x = Age, y = Mean_FEV)) +
      geom_col(fill = "gray50") +
      labs(x = "Возраст", y = "Средний объем легких")


fev %>% 
  filter(Smoker == "Non") %>% 
  group_by(Age, Sex) %>%  # Добавили еще одну группирующую переменную
  summarize(Mean_FEV = mean(FEV)) %>% 
    ggplot(aes(x = Age, y = Mean_FEV)) +
      geom_col(fill = "gray50") +
      labs(x = "Возраст", y = "Средний объем легких") +
      facet_wrap(~Sex)


# Приводим на графике данные по варьированию, оцененному с помощью стандартного отклонения


fev %>% 
  filter(Smoker == "Non") %>% 
  group_by(Age, Sex) %>%  
  summarize(Mean_FEV = mean(FEV), SD = sd(FEV)) %>% 
    ggplot(aes(x = Age, y = Mean_FEV)) +
      geom_col(fill = "gray50") +
      geom_errorbar(aes(ymin = Mean_FEV - SD, ymax = Mean_FEV + SD), width = 0.2) + 
      labs(x = "Возраст", y = "Средний объем легких") +
      facet_wrap(~Sex)



fev %>% 
  group_by(Age, Sex, Smoker) %>%  
  summarize(Mean_FEV = mean(FEV), SD = sd(FEV)) %>% 
    ggplot(aes(x = Age, y = Mean_FEV)) +
      geom_col(aes(fill = Smoker)) +
      geom_errorbar(aes(ymin = Mean_FEV - SD, ymax = Mean_FEV + SD), width = 0.2) + 
      labs(x = "Возраст", y = "Средний объем легких") +
      facet_grid(Smoker~Sex) 



## Столбчатая диаграмма для двух групп

fev %>% 
  group_by(Sex) %>%  
  summarize(Mean_Height  = mean(Height), SD = sd(Height)) %>% 
    ggplot(aes(x = Sex, y = Mean_Height)) +
      geom_col() +
      geom_errorbar(aes(ymin = Mean_Height - SD, ymax = Mean_Height + SD), width = 0.2) +
      labs(x = "Пол", y = "Средний рост") 



## Графики с линиями тренда

ggplot(fev, aes(x = Age, y = FEV, color = Smoker)) + 
  geom_point() + 
  geom_smooth(method = "lm") +  
  facet_wrap( ~ Sex)


################ Шуточный пример ##############################

#Запустите код, расположенный между двумя линиями

#___________________________

circus <- function(n, p, cos2 = 0, sin2 =0, cos3 = 0, sin3 = 0){
  # n - number of points
  # p - period
  points <- data.frame(X=c(1:n), Y=c(1:n))
  factor <- points
  k <- 0
  for (i in 1:n){
    factor$X[i] <- (i-1)/p - k
    if ((i/p - trunc(i/p))==0) k <- k + 1
  }
  
  factor$Y <- factor$X
  
  for (i in 1:n){
    points$X[i] <- cos(2*3.14*factor$X[i]) + cos(cos2/4*3.14*factor$X[i]) + cos(cos3/4*3.14*factor$X[i])
    points$Y[i] <- sin(2*3.14*factor$Y[i]) + sin(sin2/4*3.14*factor$Y[i]) + sin(sin3/4*3.14*factor$Y[i])
  }
  return(points)
}

bill <- circus(100, 100, 10.7, 15, 0, 0)
bill2 <- circus(100, 100, 10.7, 15, 0, 0)
cock_head <- circus(100, 100, 15, 15, 0, 15)
cock_head$X <- cock_head$X +1.6
cock_head$Y <- cock_head$Y +1.1
cock_beard <- circus(100, 100, 1, 5, 1, 1)
cock_crest <- circus(100, 100, 15, 30, 20, 40)
cock_pupil <- circus(100, 100, 0, 0, 0, 0)

forest <- circus(100, 100, 15, 200, 15, 100)
fir <- data.frame(x = 3, y = seq(-3, 4, length.out = 100))
fir$xend <- seq(3, 5, length.out = 100)
fir$yend <- 4 - fir$xend


fir2 <- data.frame(x = 3, y = seq(4, -3, length.out = 100))
fir2$xend <- seq(3, -3, length.out = 100)
fir2$yend <- fir$yend


ray <- data.frame(x=3, y = 5, angle = runif(100, 0, 2*3.14), radius = rnorm(100, 1, 0.5))

stars <- data.frame(x = rnorm(30, 1, 5), y = rnorm(30, 11, 0.5) )

snow <- data.frame(x = rnorm(3000, 3, 10), y = rnorm(3000, -3, 0.1) )
snow$y[snow$y < -3] <-snow$y [snow$y < -3] + 0.3

ggplot() + geom_polygon(data = forest, aes(X*3, Y ), fill = "white", color = "black") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill="blue")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill="blue")) +
  xlab("") +
  ylab("") +
  ylim(-3, 12) +
  xlim(-7, 10) +
  geom_curve(data = fir, aes(x=x, y=y, xend = xend, yend = yend), curvature = -0, color = "green") +
  geom_curve(data = fir, aes(x=x, y=y +1, xend = xend, yend = yend + 1), curvature = -0, color = "darkgreen") +
  geom_curve(data = fir, aes(x=x, y=y -1, xend = xend, yend = yend - 1), curvature = -0, color = "darkgreen") +
  geom_curve(data = fir2, aes(x=x, y=y, xend = xend, yend = yend), curvature = -0, color = "darkgreen") +
  geom_curve(data = fir2, aes(x=x, y=y +1, xend = xend, yend = yend + 1), curvature = -0, color = "darkgreen") +
  geom_curve(data = fir2, aes(x=x, y=y -1, xend = xend, yend = yend - 1), curvature = -0, color = "darkgreen") +
  geom_spoke(data = ray, aes(x=x, y=y, angle = angle, radius = radius), color = "yellow") +
  geom_point(data  = stars, aes(x=x, y=y), color = "yellow", size=10, shape = "*") + geom_point(data  = snow, aes(x=x, y=y), color = "white") +
  geom_point(aes(x = rnorm(100,1,10), y=rnorm(100,2, 2)), shape=8, size=3, color="white") +
  geom_polygon(data = bill2, aes(X + 5, Y-0.5 +5), fill = "gold", color = "black") + geom_polygon(data = bill, aes(X+ 5, Y+5), fill = "gold", color = "black")  +
  geom_polygon(data = cock_head, aes(X+ 5, Y+5), fill = "orange", color = "black") +
  geom_polygon(data = cock_crest, aes(X*1.2 + 4+ 5, Y*1.2 +4+5), fill = "red", color = "black") +
  geom_polygon(data = cock_beard, aes(X/1.5+0.9+ 5, Y*1.2-3+5), fill = "red") +
  geom_polygon(data = cock_pupil, aes(X/4 + 1.6 + 5, Y/3 + 1.6+5), fill = "black") +
  geom_text(aes(x=0, y = 9), label = "Year of the roosteR", size = 10, color = "yellow") +
  geom_text(aes(x=0, y =7), label = "2017", size = 15, color = "white")
#___________________________




