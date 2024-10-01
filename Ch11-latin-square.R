library(tidyverse)
library(sasLM)

dynamite

# base R
anova(lm(Power~Company+Worker+Method, dynamite))

# sasLM
formlr = Power~Company+Worker+Method
LSM(formlr, dynamite, 'Method', adj = 'Tukey', PLOT = T)

PDIFF(formlr, dynamite, 'Method', adj = 'Tukey', PLOT = T)
