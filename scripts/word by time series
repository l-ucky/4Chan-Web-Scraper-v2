# Prototype. I know it's shit.
library("ggplot2")
library("tidyverse")

data3 <- Sep_27_2023_06_01_19_PM %>% head(10)
data4 <- Sep_28_2023_06_01_20_PM %>% head(10)
data5 <- Sep_29_2023_06_01_17_PM %>% head(10)
data6 <- Sep_30_2023_06_01_15_PM %>% head(10)


data1 <- rbind(data3, data4, data5, data6)
data1
data2 <- data1 %>% 
  top_n(20) 


ggplot(data1, aes(x=date, y=n, color=word)) +
  geom_line()
