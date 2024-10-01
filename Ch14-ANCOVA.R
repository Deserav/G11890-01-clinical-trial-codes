library(tidyverse)
library(sasLM)
library(patchwork)

leprosy

A = leprosy %>% group_by(Drug) %>%
  summarize(Y = mean(Y))
B = leprosy %>% group_by(Drug) %>%
  summarize(a = summary(lm(Y~X))$coefficients[1],
            b = summary(lm(Y~X))$coefficients[2])

TEMP = coef(lm(Y~X+Drug, data = leprosy))

B = data.frame(Drug = A$Drug, b = rep(TEMP[2], 3),
               a = c(TEMP[1], sum(TEMP[c(1,3)]), sum(TEMP[c(1,4)])))

A1 = leprosy %>% ggplot(aes(X, Y)) +
  geom_point(aes(X, Y, color = Drug, size = 3)) +
  geom_hline(data = A, aes(yintercept = Y, color = Drug, linetype = 'dashed')) +
  geom_abline(data = B, aes(intercept = a, slope = b, color = Drug))

A2 = leprosy %>% ggplot(aes(X, Y)) +
  geom_point(aes(X, Y, color = Drug, size = 3)) +
  geom_hline(data = A, aes(yintercept = Y, color = Drug), linetype = 'dashed') +
  geom_smooth(method = 'lm', aes(X, Y, color = Drug), se = FALSE)

A1 + A2

# ANCOVA
library(car)
anova(lm(Y~Drug, leprosy)) # ANOVA
Anova(lm(Y~Drug, leprosy), type = 'III')

anova(lm(Y~Drug+X, leprosy)) # X effect adjustment
Anova(lm(Y~Drug+X, leprosy), type = 'III')

anova(lm(Y~Drug*X, leprosy)) # with interaction
Anova(lm(Y~Drug*X, leprosy), type = 'III')

# final model
summary(lm(Y~Drug+X, leprosy))

# With LSM
LSM(Y~Drug+X, leprosy, 'Drug', PLOT = T)

LSM(Y~Drug, leprosy, 'Drug', PLOT = T)
