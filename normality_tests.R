# Normality tests

# dataset need to be imported !!

ks.test(df_eps_g_tau$residuals,pnorm, mean = 0, sd = 1)

qqnorm(df_eps_g_tau$residuals, datax = TRUE)
qqline(df_eps_g_tau$residuals, datax = TRUE)

## with ggplot2, but the qqline is missing
library(ggplot2)
qplot(data = df_eps_g_tau, sample = residuals)
qplot(residuals, data = df_eps_g_tau,geom = "density")