library(sasLM)
library(ggplot2)
library(tidyverse)

score2 %>% ggplot(aes(School, Y))+
  geom_jitter(aes(color = Class), size = 3, width = 0.05)

# 3 expressions of nested model (school is nested in class)
anova(lm(Y~School+School/Class, data = score2))

anova(lm(Y~School/Class, data = score2))

anova(lm(Y~School+Class %in% School, data = score2))

Fvalue = 114.667/46.833
Fvalue

1-pf(Fvalue, 2,3)


# Split plot design
head(rosemite)

anova(lm(Mite ~ Tree*Pos+Tree*Pos/Leaf, data = rosemite))

# Pos
Fvalue = 172.861/51.903
Fvalue

1-pf(Fvalue, 2, 4)

# Tree:Pos
Fvalue = 51.903/50.611
Fvalue

1-pf(Fvalue, 4, 9)
