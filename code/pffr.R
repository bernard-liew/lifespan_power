library(refund)
library(dplyr)
library(tidyr)

df_pow <- readRDS("output/clean_dat.RDS")

df_hip <- df_pow %>%
  filter (joint == "hip") %>%
  mutate (subj = factor (subj)) %>%
  mutate (sex = factor (sex))


wide_df_hip <- pivot_wider(df_hip, names_from = cycle, values_from = power)
power <- wide_df_hip %>% dplyr::select(`1`:`100`) %>% as.matrix
wide_df_hip <- wide_df_hip %>% dplyr::select(subj:joint) %>% as.list()
wide_df_hip$power <- I(power)
wide_df_hip$t <- t <- 1:100
wide_df_hip$side <- as.factor(wide_df_hip$side)

########### Model with function random effect #############

# = every participant has its own functional intercept

# takes quite long
mod_fun <- pffr(power ~
                  s(subj, bs="re") +
                  s(age) +
                  s(speed) +
                  s(ht) +
                  s(strlen) +
                  sex +
                  side, yind = t, data = wide_df_hip
)

# does not work, bug in implementation
# plot(mod_fun)

summary(mod_fun)
# R-sq.(adj) =  0.856   Deviance explained = 85.9%

### Example for prediction intervals:

fake_newdata <- lapply(wide_df_hip[c("age", "sex", "speed", "ht", "strlen", "side")],
                       function(x) subset(x, wide_df_hip$subj=="HCC001"))
fake_newdata$subj <- factor(rep("HCC001", 2), levels=levels(wide_df_hip$subj))
fake_newdata$t <- 1:100
fake_newdata$power <- I(wide_df_hip$power[wide_df_hip$subj=="HCC001",])

pp <- predict(mod_fun, se.fit = T, newdata = fake_newdata,
              type = "terms")

matplot(t(pp$fit[[1]]), type="l", lty=1)
matplot(t(pp$fit[[1]] + 2*pp$se.fit[[1]]), type="l", add = T, lty=2)
matplot(t(pp$fit[[1]] - 2*pp$se.fit[[1]]), type="l", add = T, lty=2)

########### Model with scalar random effect #############

# = every participant has its own scalar intercept

mod_fun <- pffr(power ~
                  c(s(subj, bs="re")) +
                  s(age) +
                  s(speed) +
                  s(ht) +
                  s(strlen) +
                  sex +
                  side, yind = t, data = wide_df_hip
)

plot(mod_fun)

summary(mod_fun)
# R-sq.(adj) =  0.823   Deviance explained = 82.6%


########### Model without random effect #############

mod_fun <- pffr(power ~
                  #s(subj, bs="re") +
                  s(age) +
                  s(speed) +
                  s(ht) +
                  s(strlen) +
                  sex +
                  side, yind = t, data = wide_df_hip
)

plot(mod_fun)

summary(mod_fun)
# R-sq.(adj) =  0.807   Deviance explained = 80.9%
