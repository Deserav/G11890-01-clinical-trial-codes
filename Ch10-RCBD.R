library(tidyverse)
library(sasLM)
library(patchwork)


# Paired t-test
tirewear1 %>% pivot_wider(names_from = Tire, values_from = Wear)

# two sample t-test: bad
t.test(Wear~Tire, data=tirewear1)

# paired t-test: good
tirewear1W = tirewear1 %>% pivot_wider(names_from = Tire, values_from = Wear)
t.test(Pair(A,B)~1, data = tirewear1W)

# with anova
anova(lm(Wear~Car+Tire,data=tirewear1))

tirewear2 %>% pivot_wider(names_from = 'Tire', values_from = 'Wear')
anova(lm(Wear~Car+Tire, data = tirewear2))

#  CRD
anova(lm(Wear~Tire, data = tirewear2))

# two-factor design under RCBD
A = tirewear3 %>%
  ggplot(aes(Brand, Wear)) +
  geom_violin() +
  geom_boxplot(width = 0.3)+
  geom_point()
  
B = tirewear3 %>%
  ggplot(aes(Type, Wear)) +
  geom_violin() +
  geom_boxplot(width = 0.3)+
  geom_point()

A+B

tirewear3 %>% pivot_wider(names_from = 'Car', values_from = 'Wear')
anova(lm(Wear ~ Car + Brand*Type, data = tirewear3))

# Repeated Measures
defect2 %>% pivot_wider(names_from = 'Line', values_from = 'Defect')
anova(lm(Defect ~ Time + Line, defect2))

defect2 %>%
  mutate(Time = factor(as.character(Time),
                       levels = c("Morning", "Noon", "Afternoon"))) %>%
  ggplot(aes(Time, Defect)) + geom_point() +
  geom_line(aes(group = Line, color = Line))
