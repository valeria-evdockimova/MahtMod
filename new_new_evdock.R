rm(list=ls()) 
#"Создание моделей линейной регрессии дневных потоков паров воды газа за весенний период 2013 года по данным измерений методом турбулентной пульсации"
library("tidyverse")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")
## Читаем файл
tb1=read.csv("C:/Users/Валерия/Downloads/eddypro.csv", 
             skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
tb1
#удаляем первую строчку
tb1 = tb1[-1,]
tb1
#смотрим информацию по столбцам
glimpse(tb1)
names(tb1)
# удаляем из таблицы столбик ролл и ненужные столбикиm
tb1 = select(tb1, -(roll))
tb1<-tb1[,c(-1,-3,-9,-12,-15,-18,-21,-30,-35,-70,-88:-99)]
names(tb1)
# Преобразуем в факторы переменные типа char, которые содержат повторяющиеся значения
tb1 = tb1 %>% mutate_if(is.character, factor)
# Убираем проблему со знаками в переменных
names(tb1) =  str_replace_all(names(tb1), "[!]","_emph_")
names(tb1) = names(tb1) %>% 
  str_replace_all("[!]","_emph_") %>% 
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
glimpse(tb1)
sapply(tb1,is.numeric) 
#Оставляем только численные данные  
tb1_numeric = tb1[,sapply(tb1,is.numeric) ]
tb1_non_numeric = tb1[,!sapply(tb1,is.numeric) ]
# Приводим к типу logical колонку daytime
tb1$daytime = as.logical(tb1$daytime)
#Оставляем май , дневное время
tb1 = subset(tb1, as.Date(date) >= as.Date("2013-05-13") & as.Date(date) <= as.Date("2013-05-31") & daytime == T)
tb1
#Корреляцтонный анализ
cor_td = cor(tb1_numeric)
cor_td
#Избавляемся от всех строк, где есть хоть одно значение NA
cor_td = cor(drop_na(tb1_numeric)) %>% as.data.frame %>% select(h2o_flux)
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude
vars
##  Собираем все переменные из вектора с именнами переменных в одну формулу
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep=""))
formula
#сщздание обучающей выборки
row_numbers = 1:length(tb1$date) 
teach = sample(row_numbers, floor(length(tb1$date)*.7)) 
test = row_numbers[-teach] 
teaching_tb1_unq = tb1[teach,] 
testing_tb1_unq = tb1[test,] 
mod = lm(formula, data=tb1) 
mod 
coef(mod) 
resid(mod) 
confint(mod) 
summary(mod)
anova(mod)
#создание и анализ модели множественной регрессии
#с взаимодействием
mod1=lm(h2o_flux ~ (DOY  + H + LE + rand_err_LE + 
                      rand_err_co2_flux + h2o_flux + rand_err_h2o_flux + 
                      H_strg  + T. + un_Tau + un_H + un_LE + 
                      un_h2o_flux  +  h2o_var + 
                      w.ts_cov  + w.h2o_cov)^2,data=tb1)
coef(mod1) 
resid(mod1) 
confint(mod1)
summary(mod1)
anova(mod1)


mod2=lm(h2o_flux ~ (DOY  + H + LE + rand_err_LE + 
                      rand_err_co2_flux + h2o_flux + rand_err_h2o_flux + 
                      H_strg  + T. + un_Tau + un_H + un_LE + 
                      un_h2o_flux  +  h2o_var +  w.ts_cov  + w.h2o_cov)^2-DOY:un_H-DOY:w.ts_cov-DOY:w.h2o_cov-
          H:LE-H:rand_err_co2_flux-h2o_flux:H-H:H_strg-H:T.-H:un_Tau-
          H:un_H-H:un_LE-H:un_h2o_flux-H:h2o_var-H:w.ts_cov-H:w.h2o_cov-LE:rand_err_LE -
          h2o_flux:LE-LE:rand_err_h2o_flux -LE:H_strg-LE:T.-LE:un_H-LE:un_LE-
          LE:un_h2o_flux-LE:h2o_var -LE:w.h2o_cov-rand_err_LE:rand_err_co2_flux-
          h2o_flux:rand_err_LE-rand_err_LE:H_strg -rand_err_LE:rand_err_h2o_flux-
          rand_err_LE:T.-rand_err_LE:un_Tau-rand_err_LE:un_LE-rand_err_LE:un_h2o_flux-
          rand_err_LE:w.ts_cov-rand_err_LE:w.h2o_cov-rand_err_co2_flux:rand_err_h2o_flux-
          rand_err_co2_flux:H_strg-rand_err_co2_flux:T.-rand_err_co2_flux:un_Tau-rand_err_co2_flux:un_H-
          rand_err_co2_flux:h2o_var-rand_err_co2_flux:w.h2o_cov-h2o_flux:rand_err_h2o_flux-
          h2o_flux:H_strg -h2o_flux:T.-h2o_flux:un_H-h2o_flux:un_LE-h2o_flux:un_h2o_flux-
          h2o_flux:h2o_var-h2o_flux:w.h2o_cov-rand_err_h2o_flux:H_strg -rand_err_h2o_flux:T.-
          rand_err_h2o_flux:un_Tau-rand_err_h2o_flux:un_LE -rand_err_h2o_flux:un_h2o_flux-
          rand_err_h2o_flux:w.ts_cov-rand_err_h2o_flux:w.h2o_cov-H_strg:T.-H_strg:un_Tau-
          H_strg:un_H-H_strg:un_LE-H_strg:un_h2o_flux-H_strg:h2o_var-H_strg:w.ts_cov-
          H_strg:w.h2o_cov-T.:un_H-T.:un_LE-T.:un_h2o_flux-T.:h2o_var-T.:w.h2o_cov-
          un_Tau:un_H-un_Tau:h2o_var-un_H:w.ts_cov-un_H:w.h2o_cov-un_LE:un_h2o_flux-
          un_LE:h2o_var-un_LE:w.h2o_cov-un_h2o_flux:h2o_var-un_h2o_flux:w.h2o_cov-h2o_var:w.ts_cov-
          h2o_var:w.h2o_cov-w.ts_cov:w.h2o_cov,data=tb1)
coef(mod2) 
resid(mod2) 
confint(mod2)
summary(mod2)
anova(mod2)

mod3=lm(h2o_flux ~ (DOY  + H + LE + rand_err_LE + 
                      rand_err_co2_flux + h2o_flux + rand_err_h2o_flux + 
                      H_strg  + T. + un_Tau + un_H + un_LE + 
                      un_h2o_flux  +  h2o_var +  w.ts_cov  + w.h2o_cov)^2-DOY:un_H-DOY:w.ts_cov-DOY:w.h2o_cov-
          H:LE-H:rand_err_co2_flux-h2o_flux:H-H:H_strg-H:T.-H:un_Tau-
          H:un_H-H:un_LE-H:un_h2o_flux-H:h2o_var-H:w.ts_cov-H:w.h2o_cov-LE:rand_err_LE -
          h2o_flux:LE-LE:rand_err_h2o_flux -LE:H_strg-LE:T.-LE:un_H-LE:un_LE-
          LE:un_h2o_flux-LE:h2o_var -LE:w.h2o_cov-rand_err_LE:rand_err_co2_flux-
          h2o_flux:rand_err_LE-rand_err_LE:H_strg -rand_err_LE:rand_err_h2o_flux-
          rand_err_LE:T.-rand_err_LE:un_Tau-rand_err_LE:un_LE-rand_err_LE:un_h2o_flux-
          rand_err_LE:w.ts_cov-rand_err_LE:w.h2o_cov-rand_err_co2_flux:rand_err_h2o_flux-
          rand_err_co2_flux:H_strg-rand_err_co2_flux:T.-rand_err_co2_flux:un_Tau-rand_err_co2_flux:un_H-
          rand_err_co2_flux:h2o_var-rand_err_co2_flux:w.h2o_cov-h2o_flux:rand_err_h2o_flux-
          h2o_flux:H_strg -h2o_flux:T.-h2o_flux:un_H-h2o_flux:un_LE-h2o_flux:un_h2o_flux-
          h2o_flux:h2o_var-h2o_flux:w.h2o_cov-rand_err_h2o_flux:H_strg -rand_err_h2o_flux:T.-
          rand_err_h2o_flux:un_Tau-rand_err_h2o_flux:un_LE -rand_err_h2o_flux:un_h2o_flux-
          rand_err_h2o_flux:w.ts_cov-rand_err_h2o_flux:w.h2o_cov-H_strg:T.-H_strg:un_Tau-
          H_strg:un_H-H_strg:un_LE-H_strg:un_h2o_flux-H_strg:h2o_var-H_strg:w.ts_cov-
          H_strg:w.h2o_cov-T.:un_H-T.:un_LE-T.:un_h2o_flux-T.:h2o_var-T.:w.h2o_cov-
          un_Tau:un_H-un_Tau:h2o_var-un_H:w.ts_cov-un_H:w.h2o_cov-un_LE:un_h2o_flux-
          un_LE:h2o_var-un_LE:w.h2o_cov-un_h2o_flux:h2o_var-un_h2o_flux:w.h2o_cov-h2o_var:w.ts_cov-
          h2o_var:w.h2o_cov-w.ts_cov:w.h2o_cov-H:rand_err_LE-H:rand_err_h2o_flux-
          rand_err_LE:un_H-rand_err_LE:h2o_var-rand_err_co2_flux:w.ts_cov-rand_err_h2o_flux:un_H-
          rand_err_h2o_flux:h2o_var-T.:w.ts_cov -un_H:h2o_var,data=tb1)
coef(mod3) 
resid(mod3) 
confint(mod3)
summary(mod3)
anova(mod3)
mod4=lm(h2o_flux ~ (DOY  + H + LE + rand_err_LE + 
                      rand_err_co2_flux + h2o_flux + rand_err_h2o_flux + 
                      H_strg  + T. + un_Tau + un_H + un_LE + 
                      un_h2o_flux  +  h2o_var +  w.ts_cov  + w.h2o_cov)^2-DOY:un_H-DOY:w.ts_cov-DOY:w.h2o_cov-
          H:LE-H:rand_err_co2_flux-h2o_flux:H-H:H_strg-H:T.-H:un_Tau-
          H:un_H-H:un_LE-H:un_h2o_flux-H:h2o_var-H:w.ts_cov-H:w.h2o_cov-LE:rand_err_LE -
          h2o_flux:LE-LE:rand_err_h2o_flux -LE:H_strg-LE:T.-LE:un_H-LE:un_LE-
          LE:un_h2o_flux-LE:h2o_var -LE:w.h2o_cov-rand_err_LE:rand_err_co2_flux-
          h2o_flux:rand_err_LE-rand_err_LE:H_strg -rand_err_LE:rand_err_h2o_flux-
          rand_err_LE:T.-rand_err_LE:un_Tau-rand_err_LE:un_LE-rand_err_LE:un_h2o_flux-
          rand_err_LE:w.ts_cov-rand_err_LE:w.h2o_cov-rand_err_co2_flux:rand_err_h2o_flux-
          rand_err_co2_flux:H_strg-rand_err_co2_flux:T.-rand_err_co2_flux:un_Tau-rand_err_co2_flux:un_H-
          rand_err_co2_flux:h2o_var-rand_err_co2_flux:w.h2o_cov-h2o_flux:rand_err_h2o_flux-
          h2o_flux:H_strg -h2o_flux:T.-h2o_flux:un_H-h2o_flux:un_LE-h2o_flux:un_h2o_flux-
          h2o_flux:h2o_var-h2o_flux:w.h2o_cov-rand_err_h2o_flux:H_strg -rand_err_h2o_flux:T.-
          rand_err_h2o_flux:un_Tau-rand_err_h2o_flux:un_LE -rand_err_h2o_flux:un_h2o_flux-
          rand_err_h2o_flux:w.ts_cov-rand_err_h2o_flux:w.h2o_cov-H_strg:T.-H_strg:un_Tau-
          H_strg:un_H-H_strg:un_LE-H_strg:un_h2o_flux-H_strg:h2o_var-H_strg:w.ts_cov-
          H_strg:w.h2o_cov-T.:un_H-T.:un_LE-T.:un_h2o_flux-T.:h2o_var-T.:w.h2o_cov-
          un_Tau:un_H-un_Tau:h2o_var-un_H:w.ts_cov-un_H:w.h2o_cov-un_LE:un_h2o_flux-
          un_LE:h2o_var-un_LE:w.h2o_cov-un_h2o_flux:h2o_var-un_h2o_flux:w.h2o_cov-h2o_var:w.ts_cov-
          h2o_var:w.h2o_cov-w.ts_cov:w.h2o_cov-H:rand_err_LE-H:rand_err_h2o_flux-
          rand_err_LE:un_H-rand_err_LE:h2o_var-rand_err_co2_flux:w.ts_cov-rand_err_h2o_flux:un_H-
          rand_err_h2o_flux:h2o_var-T.:w.ts_cov -un_H:h2o_var-DOY-H-DOY:H -un_H-
          T.:un_Tau-un_Tau:w.ts_cov-un_H:un_LE-un_H:un_h2o_flux-LE:rand_err_co2_flux-
          LE:un_Tau-h2o_flux:w.ts_cov-un_LE:w.ts_cov-un_h2o_flux:w.ts_cov-
          LE:w.ts_cov-h2o_flux:rand_err_co2_flux-rand_err_co2_flux:un_LE-
          rand_err_co2_flux:un_h2o_flux-h2o_flux:un_Tau-un_Tau:un_LE-
          un_Tau:un_h2o_flux-un_Tau:w.h2o_cov-DOY:T.,data=tb1)
coef(mod4) 
resid(mod4) 
confint(mod2)
summary(mod4)
anova(mod4)
plot(mod4)
#оптимальная модель -4 получена,