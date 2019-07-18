library(dplyr)
library(readr)

garbage_data <- read.csv("../../garbage.csv")
population_data <- read_csv("../../population.csv")

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

p <- select(data.frame(population_data),
            p_total = 年底人口數,
            county = 地區) %>%
  filter(!grepl("區", county), !grepl("省", county))


#合併
merged_data <- merge(p, g_10612, by = "county")

#排序
merged_data <- merged_data[order(merged_data$p_total), ]
merged_data <- mutate(merged_data, g_percapita = (g_total/p_total)*1000)

View(merged_data)


plot(x = merged_data$p_total/1000, y = merged_data$g_total,
     xlab = "人口數(千人)", ylab = "垃圾生產量(公噸)",
     main = "2017年12月各地區人口與垃圾生產量關係圖")

plot(merged_data$p_total/1000, merged_data$g_percapita,
     xlab = "人口數(千人)",
     ylab = "單位垃圾生產量(公斤/人)",
     main = "2017年12月各地區人口與單位垃圾生產量關係圖")
