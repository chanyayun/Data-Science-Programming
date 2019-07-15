library(dplyr)
library(readr)
library(ggplot2)

garbage_data <- read.csv("garbage.csv")
popularity_data <- read_csv("popularity.csv")

str(garbage_data)
str(popularity_data)

g <- garbage_data %>% 
  filter(year == "106", month == "11") %>%
  select(county, GarbageGenerated) %>%
  rename(
    地區 = county, 
    `垃圾生產量(t)` = GarbageGenerated
  )

g$`垃圾生產量(t)` <- as.numeric(gsub(",", "", g$`垃圾生產量(t)`))

p <- data.frame(popularity_data) %>%
  filter(!grepl("區", 地區), !grepl("省", 地區)) %>%
  select(地區, 年底人口數)

merged_data <- merge(p, g, by = "地區")

#排序
merged_data <- merged_data[order(merged_data$年底人口數), ]

merged_data <- mutate(merged_data, `單位垃圾生產量(kg/人)` = (`垃圾生產量(t)`/年底人口數)*1000)

View(merged_data)


plot(x = merged_data$年底人口數/1000, y = merged_data$`垃圾生產量(t)`,
     xlab = "人口數(千人)", ylab = "垃圾生產量(公噸)",
     main = "2017年11月各地區人口與垃圾生產量關係圖")

plot(merged_data$年底人口數/1000, merged_data$`單位垃圾生產量(kg/人)`,
     xlab = "人口數(千人)",
     ylab = "單位垃圾生產量(公斤/人)",
     main = "2017年11月各地區人口與單位垃圾生產量關係圖")
