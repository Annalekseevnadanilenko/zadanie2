#Даниленко Анна создайте модель множественной линейной регрессии потоков дневных потоков углекислого газа за весенний период 2013 года по данным измерений методом турбулентной пульсации

library("tidyverse")#

library("nycflights13") #

library("tidyr") #

library("stringr") #

library("dplyr") #

library("tibble") #

library("readr") #

setwd("C:/Users/any11/Documents") 

#читаем данные из файла eddypro, пропускаем первую строку и заменяем текстовые 'NA', пустые и сгенерированные пороговые значения на NA, игнорируем строки с "[" 

data = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("[")) 

data = data[-1,]# удаляю еще строку одну

data = data[data$DOY>=60 & data$DOY<=151 & year(data$date) == 2013, c(1:ncol(data))]#данные отбираем за весенний период(март-мая) на 2013 год

data=data[data$daytime == TRUE,] # отбираем данные дневных потоков

glimpse(data)# смотрим что получилось

data = select(data, -(roll)) # удаляем пустой и ненужный столбец

data = data %>% mutate_if(is.character, factor) #делаем преобразование строковых значений в факторные

names(data) = names(data) %>% str_replace_all("[!]","_emph_") %>% #Заменим специальные символы в названии стобцов на допустимые для переменных имена
  
#заменяем конфликтующие знаки колонок

str_replace_all("[?]","_quest_") %>%  
  
str_replace_all("[*]","_star_") %>%  
  
str_replace_all("[+]","_plus_") %>% 
  
str_replace_all("[-]","_minus_") %>% 
  
str_replace_all("[@]","_at_") %>% 
  
str_replace_all("[$]","_dollar_") %>%
  
str_replace_all("[#]","_hash_") %>% 
  
str_replace_all("[/]","_div_") %>% 
  
str_replace_all("[%]","_perc_") %>% 
  
str_replace_all("[&]","_amp_") %>% 
  
str_replace_all("[\\^]","_power_") %>% 
  
str_replace_all("[()]","_") 

glimpse(data)# смотрим результат 

data_numeric = data[,sapply(data,is.numeric) ] #выберем все переменные типа numeric

data_non_numeric = data[,!sapply(data,is.numeric) ]#все остальные переменные

cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% select(co2_flux) #создадим матрицу для корелляционного анализа и преобразовываем ее в таблиу с нужным нам столбцом потоки угликислого газа

vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude #выберем имена переменных (строк) с коэффициентом детерминации больше 0.1

formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep=""));formula #собераем переменные из вектора в формулу

#Создадим обучающую и тестирующую непересекающиеся выборки с помошью базового функционала для обучения и тестирования моделей

row_numbers = 1:length(data_numeric$co2_flux)

teach = sample(row_numbers, floor(length(data_numeric$co2_flux)*.7))

test = row_numbers[-teach]

teaching_tbl = data_numeric[teach,]#Обучающая выборка

testing_tbl = data_numeric[test,]#Тестирующая выборка

#МОДЕЛЬ 1

model = lm(formula, data = data);model #создаем модель линейной регрессии

coef(model)#коэффициенты

resid(model)#остатки

confint(model)#доверительный интервал

summary(model)#P-значения по модели

anova(model)#дисперсионный анализ

plot(model)#графическое представление модели

# МОДЕЛЬ 2

mod = lm(formula, data = data);mod

formula1 = co2_flux ~  (rand_err_H + LE + rand_err_co2_flux + h2o_flux +co2_molar_density + co2_mole_fraction + co2_mixing_ratio + sonic_temperature + air_temperature + es + T. + un_LE + un_co2_flux + un_h2o_flux + ts_var + co2_var + w.co2_cov + w.h2o_cov +co2 + co2.1)

summary(mod)#Получим информацию о моделе и коэффициенты

anova(mod)#Проанализируем переменные по значимости

anova(model, mod)#Сравним с предыдущей моделью, не ухудшилась ли она

plot(mod) #Выведем графики
