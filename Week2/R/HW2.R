library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)

garbage_data <- read.csv("../../garbage.csv")
popularity_data <- read_csv("../../population.csv")

str(garbage_data)
str(population_data)

#原本的欄位名稱用``還是有error, 所以用索引值表示
g <- select(garbage_data, -c(1, 9),
            time_period = 2,
            county = 3, 
            g_total = 4,
            g_garbage =  5,
            g_bulky = 6,
            g_recycle = 7,
            g_foodscrap = 8)

#改變資料型態(應該有更好的方法)
g$g_total <- as.numeric(gsub(",", "", g$g_total))
g$g_garbage <- as.numeric(gsub(",", "", g$g_garbage))
g$g_bulky <- as.numeric(gsub(",", "", g$g_bulky))
g$g_recycle <- as.numeric(gsub(",", "", g$g_recycle))
g$g_foodscrap <- as.numeric(gsub(",", "", g$g_foodscrap))

g_10612 <- filter(g, time_period == "106年 12月") %>%
  select(-c(1))

g_106 <- filter(g, time_period == "106年") %>%
  select(-c(1))

#跟garbage_data一樣, 有特殊符號的欄位名稱用索引值表示(或我懶得打)
p <- select(data.frame(population_data), -c(3, 7, 8, 9, 10),
            year = 年,
            county = 地區, 
            p_male = 年底人口數男性,
            p_female = 年底人口數女性,
            p_total = 年底人口數,
            p_under15 = 11,
            pr_under15 = 12,
            p_adult = 13,
            pr_adult = 14,
            p_over64 = 15,
            pr_over64 = 16,
            pr_dependency = 17) %>%
  filter(!grepl("區", county), !grepl("省", county))

#合併
merged_data <- merge(p, g_10612, by = "county")

#排序
merged_data <- merged_data[order(merged_data$p_total), ]
merged_data <- mutate(merged_data, `g_percapita` = (g_total/p_total)*1000)

#barplot
#garbage_ratio <- 
#  ggplot(data = g_106, aes(county, g_total)) +
#  geom_col() + stat_identity(position = "stack")
#
#print(garbage_ratio)

#scatterplot
garbage_dist <-  
  ggplot(data = merged_data, aes(x = p_total/1000, y = g_total, label = county)) +
  geom_point(aes(color = county)) +
  geom_text_repel(data = subset(merged_data, (p_total/1000 > 1000) | (p_total/1000 < 250))) +
  labs(title = "2017年12月各地區人口與垃圾生產量關係圖", 
       x = "年底人口數(千人)", y = "垃圾生產量(公噸)", color = "地區") +
  theme(plot.title = element_text(hjust = 0.5))


print(garbage_dist)


