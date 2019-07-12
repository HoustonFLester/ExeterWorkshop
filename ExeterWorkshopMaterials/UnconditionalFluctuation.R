####Description: Fluctuation data example
####Notes: These are generating the data and fitting the model that corresponds to unconditional 
####cont: fluctuation sort of ideas.

library(mvtnorm)
library(nlme)
library(tidyr)
library(dplyr)

#### Generating a AR(1) model without between person differences -----
#### R only example 
N <- 250
mean_vec_ar1_no_g <- matrix(data = 0, nrow = 6, ncol = 1)
c1 <- c(1, .8, .8^2, .8^3, .8^4, .8^5)
c2 <- c(.8, 1, .8, .8^2, .8^3, .8^4)
c3 <- c(.8^2, .8, 1, .8, .8^2, .8^3)
c4 <- c(.8^3, .8^2, .8, 1, .8, .8^2)
c5 <- c(.8^4, .8^3, .8^2, .8, 1, .8)
c6 <- c(.8^5, .8^4, .8^3, .8^2, .8, 1)


cov_mat_AR1_no_g <- matrix(data = c(c1, c2, c3, c4, c5, c6), nrow = 6, ncol = 6)

set.seed(5151)
ar1_no_g_dat <- rmvnorm(n = N, mean_vec_ar1_no_g, cov_mat_AR1_no_g)


#### Setting up the data for analysis -----
n_time <- 6
ar1_no_g_dat <- as.data.frame(ar1_no_g_dat)
names(ar1_no_g_dat)
ar1_no_g_dat$id <- 1:N
names(ar1_no_g_dat)[1:n_time] <- paste0("y_",1:n_time)

#### Wide to long ----
long_ar1_no_g_dat <- ar1_no_g_dat %>%
  gather(key = time_nom, value = long_y, y_1:y_6) %>%
  arrange(id) %>%
  separate(time_nom, into = c("Outcome", "Time_cat")) %>%
  mutate(Time_cont = as.numeric(Time_cat))

long_ar1_no_g_dat$id <- factor(long_ar1_no_g_dat$id)
long_ar1_no_g_dat$Time_cat <- factor(long_ar1_no_g_dat$Time_cat)
unc_tvarying <- long_ar1_no_g_dat

#### Alternative Covariance Structures ----

## RQ 5. 
unstructured <- gls(long_y ~ 1,
                    correlation = corSymm(form = ~ 1 | id),
                    weight = varIdent(form = ~ 1 | Time_cat), method = "ML", data = unc_tvarying)
summary(unstructured)
getVarCov(unstructured)

compound_symm <- gls(long_y ~ 1, 
                      correlation = corCompSymm(form = ~ 1 | id), method = "ML", data = unc_tvarying)
summary(compound_symm)
getVarCov(compound_symm)

AR1 <- gls(long_y ~ 1,
            correlation = corAR1(form = ~ 1 | id), method = "ML", data = unc_tvarying)
summary(AR1)
getVarCov(AR1)

AR5 <- gls(long_y ~ 1, 
           correlation = corARMA(form = ~1| id, p = 5, q = 0), method = "ML", data = unc_tvarying)
summary(AR5)

ARH5 <- gls(long_y ~ 1, 
           correlation = corARMA(form = ~1| id, p = 5, q = 0), weights = varIdent(form =~1|Time_cat),
           method = "ML", data = unc_tvarying)
summary(ARH5)

anova(unstructured, compound_symm, AR1, AR5, ARH5)


###Trying out a random intercept as well.
### Note normally figure out the between person difference stuff before messing with the within


Ar1_RI <- lme(long_y ~ 1, random = ~ 1|id, correlation = corAR1(), 
              method = "ML", data = unc_tvarying)

anova(AR1, Ar1_RI)
