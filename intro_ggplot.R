
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


