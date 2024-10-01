library(tidyverse)
library(sasLM)

# with baseR
ptensile

anova(lm(Y~Day+Pulp+Day*Pulp+Temp+Pulp*Temp, ptensile))

# Day
Fvalue = 38.778/9.069; Fvalue

1-pf(Fvalue, 2, 4)

# Pulp
Fvalue = 64.194/9.069; Fvalue

1-pf(Fvalue, 2, 4)

# Temp
Fvalue = 144.694/3.972; Fvalue

1-pf(Fvalue, 3, 18)

# Pulp*Temp
Fvalue = 12.528/3.972; Fvalue

1-pf(Fvalue, 6, 18)

# with package
library(lmerTest)

anova(lmer(Y~Day+Pulp*Temp+(1|Day:Pulp), ptensile))
