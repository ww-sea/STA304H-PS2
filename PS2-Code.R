library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggsci)
library(tidyr)
library(purrr)
library(broom)
library(ggforce)
library(ggfortify)
library(patchwork)
library(forecast)
library(knitr)
gg_theme <-  theme(panel.background = element_blank(),
                   panel.grid.major.y = 
                     element_line(colour = "grey", linetype = 2),
                   plot.title = element_text(hjust = 0.5, size = 18),
                   legend.position = "top")


df_raw <- read_csv("gss.csv")
df <-  df_raw %>%
  mutate(education_n = case_when(
    education == "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)" ~ "Bachelor's degree",
    education == "College, CEGEP or other non-university certificate or di..." ~ "College",
    education == "High school diploma or a high school equivalency certificate" ~ "High school",
    education == "Less than high school diploma or its equivalent" ~ "Less than high school",
    education == "Trade certificate or diploma" ~ "Trade school",
    education == "University certificate or diploma below the bachelor's level" ~ "University or below",
    education == "University certificate, diploma or degree above the bach..." ~ "University or above"),
    income_family = factor(income_family, 
                           levels = c("Less than $25,000", " $25,000 to $49,999",
                                      "$50,000 to $74,999", "$75,000 to $99,999",
                                      "$100,000 to $ 124,999", "$125,000 and more")),
    pop_center_n = case_when(
      pop_center == "Larger urban population centres (CMA/CA)" ~ "Large urban",
      pop_center == "Rural areas and small population centres (non CMA/CA)" ~ "Small rural",
      pop_center == "Prince Edward Island" ~ "Prince Edward Island")) %>% 
  select(age, sex, feelings_life, total_children, place_birth_canada,
         pop_center_n, self_rated_health, self_rated_mental_health,
         language_home, income_family, education_n) %>%
  na.omit()

p1 <- df %>% ggplot(aes(x = age, y= ..density..)) +
  geom_histogram(fill = "steelblue", alpha = 0.6)+
  geom_density(col = "navy") +
  gg_theme +
  labs(x = "Age")

p2 <- df %>% group_by(sex) %>% summarise(Num = n()) %>% 
  ggplot(aes(x = 'Content', y = Num, fill = sex)) +
  geom_bar(stat = "identity", position = 'stack', width = 1)+ 
  coord_polar(theta = 'y') +
  labs(fill = "") +
  theme(axis.text = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        legend.position = "top") +
  scale_fill_d3()

p3 <- df %>% group_by(education_n) %>% summarise(Num = n()) %>% 
  ggplot(aes(x = 'Content', y = Num, fill = education_n)) +
  geom_bar(stat = "identity", position = 'stack', width = 1)+ 
  coord_polar(theta = 'y') +
  labs(fill = "") +
  theme(axis.text = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        legend.position = "right") +
  scale_fill_d3()


p4 <- df %>% group_by(income_family) %>% summarise(Num = n()) %>% 
  ggplot(aes(x = 'Content', y = Num, fill = income_family)) +
  geom_bar(stat = "identity", position = 'stack', width = 1)+ 
  coord_polar(theta = 'y') +
  labs(fill = "") +
  theme(axis.text = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        legend.position = "right") +
  scale_fill_d3()

p5 <- df %>% group_by(pop_center_n) %>% summarise(Num = n()) %>% 
  ggplot(aes(x = 'Content', y = Num, fill = pop_center_n)) +
  geom_bar(stat = "identity", position = 'stack', width = 1)+ 
  coord_polar(theta = 'y') +
  labs(fill = "") +
  theme(axis.text = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        legend.position = "top") +
  scale_fill_d3()
(p1) /
  (p2 + p5 + plot_layout(widths = c(1, 1))) / 
  (p4 + p3+ plot_layout(widths = c(1, 1)))

df <- df %>% mutate(age = ifelse(age >=80, NA, age)) %>% na.omit()
p1 <- df %>% ggplot(aes(x = age, y = feelings_life, col = sex)) +
  geom_point(alpha = 0.3) +
  gg_theme +
  labs(x = "Age", y = "Feeling life", col = "")+
  scale_fill_calc()

p2 <- df %>% ggplot(aes(x = education_n, y = feelings_life, fill = sex)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_calc() +
  gg_theme +
  labs(x = "Education", y = "Feeling life", fill = "")

p3 <- df %>% ggplot(aes(x = income_family, y = feelings_life, fill = sex)) +
  geom_boxplot() + 
  coord_flip() +
  scale_fill_calc() +
  gg_theme +
  labs(x = "Income family", y = "Feeling life", fill = "")

p4 <- df %>% ggplot(aes(x = place_birth_canada, y = feelings_life, fill = sex)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_calc() +
  gg_theme +
  labs(x = "Is birth in Canda", y = "Feeling life", fill = "")

p5 <- df %>% ggplot(aes(x = pop_center_n, y = feelings_life, fill = sex)) +
  geom_boxplot() + 
  coord_flip() +
  scale_fill_calc() +
  gg_theme +
  labs(x = "Population center", y = "Feeling life", fill = "")

p1 /(p2 + p3)/(p4 + p5)

df %>% ggplot(aes(x = feelings_life, y = ..density..)) +
  geom_histogram(fill = "steelblue") +
  geom_density(col = "navy") +
  gg_theme +
  labs(x = "Feeling life")

fit1 <- lm(feelings_life ~ sex + age + education_n + pop_center_n + place_birth_canada + income_family,
           data = df)
res1 <- summary(fit1)

anova(fit1)  %>% kable(caption = "Ordinary linear regression model results", digits = 2, align = "c")

autoplot(fit1) + gg_theme

fit2 <- glm(feelings_life ~ sex + age + education_n + pop_center_n + place_birth_canada + income_family,
            data = df, family = poisson())

tidy(fit2, exponentiate = T) %>% kable(digits = 2, align = "c", caption = "Poisson regression model results")

fit3 <- glm(feelings_life ~ pop_center_n + income_family,
            data = df, family = poisson())
tidy(fit3, exponentiate = T) %>% kable(digits = 2, align = "c", caption = "Optimized Poisson regression model results")

