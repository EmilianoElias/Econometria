summary(lm(ceosal1$salary ~ ceosal1$roe))
summary(lm(ceosal1$salary ~ ceosal1$roe + ceosal1$sales))
summary(lm(ceosal1$salary ~ ceosal1$roe + ceosal1$sales + ceosal1$ros))

summary(lm(ceosal1$lsalary ~ ceosal1$roe))
summary(lm(ceosal1$lsalary ~ ceosal1$roe + ceosal1$lsales))
summary(lm(ceosal1$lsalary ~ ceosal1$roe + ceosal1$lsales + ceosal1$ros))

a <- anova(lm(ceosal1$lsalary ~ ceosal1$roe + ceosal1$lsales + ceosal1$ros))

b <- anova(lm(ceosal1$lsalary ~ ceosal1$ros + ceosal1$lsales + ceosal1$roe))

anova(lm(data$gdp_05 ~ data$capital + data$labor_force + data$secondary_educ + data$tfp + data$energy_productivity))