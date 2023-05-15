library(tidyverse)
library(skimr)
library(ggplot2)

youth <- read.csv(file = "StudentRisk.csv", header = TRUE, na.strings = c(" ", "NA")) # nolint

youth_data <- select(youth, sex, grade, race, htinchx, wtpound)

youth_data <- drop_na(youth_data)

summary(youth_data)
skim(youth_data)

youth_data %>%
  dplyr::group_by(sex) %>%
  skim()

youth_data %>%
  dplyr::group_by(sex,race) %>%
  skim()

ggplot(youth_data, aes(x=sex,fill= sex)) + geom_bar()

ggplot(youth_data, aes(x=grade,fill= grade)) + geom_bar()

ggplot(youth_data, aes(x=race,fill= race)) + geom_bar()

ggplot(youth_data, aes(x = htinchx,fill = sex,))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~sex)

ggplot(youth_data, aes(x = htinchx, fill = sex,))+
  geom_density()+
  facet_wrap(~sex)

ggplot(youth_data, aes(x=sex, y=htinchx, fill=sex))+
  geom_boxplot()

ggplot(youth_data, aes(x = interaction(sex,race), y = wtpound, fill = sex)) + 
geom_boxplot()

s_plot <- ggplot(youth_data, aes(x = htinchx, y = wtpound))+
  geom_point(color = 'blue')+
  geom_smooth(method=lm, col='red')+
  scale_x_continuous("Height")+
  scale_y_continuous("Weight")

library(ggExtra)

ggMarginal(s_plot,type='boxplot')

