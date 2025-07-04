# 필요한 패키지 로딩
library(readr)   
library(dplyr)  
library(tibble)
library(ggplot2)
library(stringr)
library(tidyr)
library(readxl)
library(tidyverse)
library(scales)

SGG_nameMapping <- readxl::read_excel("SGG_nameMapping.xlsx", sheet = 'Sheet1', col_names = T)

tech_poten_bySGG_bytype <- readxl::read_excel("tech_potential.xlsx", sheet = '데이터', col_names = T)
market_poten_bySGG_bytype <- readxl::read_excel("market_potential.xlsx", sheet = '데이터', col_names = T)
population_bySGG <- readxl::read_excel("시군별 인구.xlsx", sheet = '데이터', col_names = T)
area_bySGG <- readxl::read_excel("시군별 면적.xlsx", sheet = '데이터', col_names = T)
PVgen_bySGG <- readxl::read_excel("PVGen_2023.xlsx", sheet = '데이터', col_names = T)

tech_poten_bySGG <- tech_poten_bySGG_bytype %>%
  group_by(SGG, class, units) %>% summarize(value = sum(value))

market_poten_bySGG <- market_poten_bySGG_bytype %>%
  group_by(SGG, class, units) %>% summarize(value = sum(value))


allData <- tech_poten_bySGG %>%
  left_join(market_poten_bySGG, by = c("SGG", "units")) %>%
  rename(techP = value.x, marketP = value.y) %>%
  select(-class.x, -class.y) %>%
  
  left_join(PVgen_bySGG, by = c("SGG", "units")) %>%
  rename(Gen = value) %>%
  select(-year) %>%
  
  mutate(utilRate = 100 * Gen / marketP) %>%
  
  left_join(population_bySGG, by = c("SGG")) %>%
  rename(pop = value) %>%
  select(-units.y) %>%
  
  left_join(area_bySGG, by = c("SGG")) %>%
  rename(area = value) %>%
  select(-units) %>%
  
  
  mutate(popDensity = pop / area, .before = area) %>%
  select( -units.x) %>%
  
  mutate(techP = techP / 1000,
         marketP = marketP / 1000,
         Gen = Gen / 1000)

allData_Total <- allData %>%
  mutate(province = "경기도") %>%
  group_by(province) %>% summarize(techP = sum(techP),
                                   marketP = sum(marketP),
                                   Gen = sum(Gen),
                                   pop = sum(pop),
                                   area = sum(area)) %>% ungroup() %>%
  mutate(utilRate = 100 * Gen / marketP,
         popDensity = pop / area) %>%
  rename(SGG = province) %>%
  select(SGG, techP, marketP, Gen, utilRate, pop, popDensity, area)

  
allData_wTotal <- allData_Total %>% bind_rows(allData) %>% select(-pop)
  

## Complete Table ##
allData_wTotal_forTable <- allData_wTotal %>%
  arrange(desc(marketP)) %>%
  left_join(SGG_nameMapping, by = c("SGG")) %>%
  mutate(SGG = SGG_eng) %>%
  select(-SGG_eng)

#write.csv(allData_wTotal_forTable, "./output/allData_wTotal_forTable.csv", fileEncoding = 'EUC-kr')
##################################################################################################################
################################################ Graph ###########################################################
##################################################################################################################
allData_forGraph <- allData_wTotal_forTable %>%
  filter(SGG != "Gyeonggi-do")
  
## (a) 기술잠재량 vs 행정면적 (순위 분포) ##

df_a <- allData_forGraph %>% arrange(desc(techP)) %>%
  mutate(SGG = factor(SGG, levels = SGG))

ggplot(df_a, aes(x = SGG)) +
  geom_bar(aes(y = techP / 1000), stat = "identity", fill = "#2166ac") +  # TWh 단위
  geom_point(aes(y = area / 18), color = "orangered", size = 10) +         # 보조축 조정
  scale_y_continuous(
    name = "Technical potential (TWh)",
    limits = c(0, 50),
    sec.axis = sec_axis(~ . * 18, name = "Administrative area (km²)", breaks = seq(0, 900, 150))
  ) +
  #labs(title = "(a) Ranked distribution of technical potential and administrative area", x = NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 50),   # x축 값
    axis.text.y = element_text(size = 50),                          # y축 값
    axis.title.y = element_text(size = 50),                         # 왼쪽 축 제목
    axis.title.y.right = element_text(size = 50),                  # 오른쪽 축 제목
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # 그래프 제목
  )


## (b) 기술잠재량 vs 행정면적 (산점도)
ggplot(allData_forGraph, aes(x = area, y = techP)) +
  geom_point(size = 2) +
  scale_x_log10(labels = scales::comma) +  # ✅ X축 log10
  scale_y_log10(labels = scales::comma) +  # ✅ Y축도 로그 스케일 그대로 유지
  labs(title = "(b) Relation between technical potential and administrative area",
       x = "Administrative area (km²)", y = "Technical potential (GWh)") +
  theme_bw()



### (c) 시장잠재량 vs 인구밀도 (순위 분포)    (1000 people/km²)
scaling_factor <- 15 / 8
df_c <- allData_forGraph %>% arrange(desc(marketP)) %>%
  mutate(SGG = factor(SGG, levels = SGG)) %>%
  mutate(popDensity = popDensity / 1000)

ggplot(df_c, aes(x = SGG)) +
  geom_bar(aes(y = marketP / 1000), stat = "identity", fill = "#4393c3") +  # MarketP: GWh → TWh
  geom_point(aes(y = popDensity / scaling_factor), color = "orangered", size = 10) +
  scale_y_continuous(
    name = "Market potential (TWh)",
    limits = c(0, 8),
    sec.axis = sec_axis(~ . * scaling_factor, name = "Population density",
                        breaks = seq(0, 15, 3))  # 선택적으로 눈금 조절
  ) +
  #labs(title = "(c) Ranked distribution of market potential and population density", x = NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 50),   # x축 값
    axis.text.y = element_text(size = 50),                          # y축 값
    axis.title.y = element_text(size = 50),                         # 왼쪽 축 제목
    axis.title.y.right = element_text(size = 50),                  # 오른쪽 축 제목
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # 그래프 제목
  )

## (d) 시장잠재량 vs 인구밀도 (산점도)
ggplot(allData_forGraph, aes(x = popDensity, y = marketP)) +
  geom_point(size = 2) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  labs(title = "(d) Relation between market potential and population density",
       x = "Population density (people/km²)", y = "Market potential (GWh)") +
  theme_bw()






##################################################################################################################
################################################ Correlation ###########################################################
##################################################################################################################

cor(allData_forGraph$techP, allData_forGraph$area, method = "pearson")
cor(allData_forGraph$marketP, allData_forGraph$popDensity, method = "pearson")

cor.test(allData_forGraph$techP, allData_forGraph$area, method = "pearson")
cor.test(allData_forGraph$marketP, allData_forGraph$popDensity, method = "pearson")


model1 <- lm(techp ~ area, data = allData_forGraph)
summary(model1)

model2 <- lm(marketp ~ popDensity, data = allData_forGraph)
summary(model2)
