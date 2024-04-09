# "Домашняя работа по визуализации данных в ggplot2"

## Пакеты, которые необходимо устновить перед началом работы 

# install.packages("ggplot2")
# install.packages("cowplot")
# install.packages("readxl")

# Используя [файл с данными по гидрологическим и метеорологическим наблюдениям](data/hydrology_2022.xls) напишите код, который приведет к появлению следующих визуализаций. При этом надо стремиться к тому, чтобы ваши рисунки были макимально похожи на те, что приведены в задании. 

# **Переменные в данных**
#   
#   - *Date_Time*	Дата и время
# - *Month* Месяц	
# - *Air_T*	Температура воздуха
# - *S*	Соленость
# - *Wind* Направление ветра	
# - *Wave* Волнение моря в баллах	
# - *Water_T* Температура воды




library(readxl)
library(ggplot2)
library(cowplot)

hydr <- read_excel("data/hydrology_2022.xls", na = "NA")

hydr$Date <- as.POSIXct(hydr$Date_Time, format = "%d.%m.%Y %H:%M") # Этот код переводит даты в формат, понятный  для R.

hydr$Month <- factor(hydr$Month, levels = c("June", "July", "August")) # Этот код задает последовательность месяцев, иначе месяцы будут упорядочены по алфавиту

hydr$Month <- factor(hydr$Month, labels = c("Июнь", "Июль", "Август")) # Этот код позволяет задать русские обозначения для месяцев, которые в датасете закодированы на латинице.



## Рисунок 1

ggplot(hydr, aes(x = Date, y = S)) +
  geom_point() +
  theme_bw()+
  labs(x = "Дата", y = "Соленость")


## Рисунок 2

ggplot(hydr, aes(x = Month, y = Water_T)) +
  geom_boxplot() +
  theme_bw()+
  labs(x = "Месяц", y = "Температура воды")


## Рисунок 3

ggplot(hydr, aes(x = Water_T, y = S)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm") +
  theme_bw()+
  labs(x = "Температура воды", y = "Соленость")


## Рисунок 4 *

# Это все один рисунок. На панелях **A** и **B**  приведены средние значения, а "усами" отложены стандартные отклонения.  На панели **C** приведены исходные значения.  

library(dplyr)

df1 <-
  hydr %>% 
  group_by(Month) %>% 
  summarize(Air_T = mean(Air_T, na.rm = T), S = mean(S, na.rm = T), Water_T = mean(Water_T, na.rm = T)) 

df2 <- 
  hydr %>% 
  group_by(Month) %>% 
  summarize(Sd_Air_T = sd(Air_T, na.rm = T), Sd_S = sd(S, na.rm = T), Sd_Water_T = sd(Water_T, na.rm = T))

df3 <- merge(df1, df2) 


Pl1 <- 
  ggplot(df3, aes(x = Month, y = S)) +
  geom_col(fill = "blue") + 
  geom_errorbar(aes(ymin = S - Sd_S, ymax = S + Sd_S), width = 0.2) +
  theme_bw()+
  labs(x = "Месяц", y = "Соленость")


Pl2 <- 
  ggplot(df3, aes(x = Month, y = Air_T)) +
  geom_errorbar(aes(ymin = Air_T - Sd_Air_T, ymax = Air_T + Sd_Air_T), width = 0.2) +
  geom_point(color = "red", size = 4) + 
  theme_bw()+
  labs(x = "Месяц", y = "Температура воздуха")


Pl3 <- 
  ggplot(hydr, aes(x = Date, y = Water_T)) +
  geom_line() +
  theme_bw()+
  labs(x = "Дата", y = "Температура воды") 


library(cowplot)

plot_grid(Pl1, Pl2, Pl3, labels = "AUTO")


