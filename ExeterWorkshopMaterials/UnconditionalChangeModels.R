# Description: Unconditional Change Models (i.e., models with change over time where time is the only predictor)
# Notes: We will 
# Cont.   

install.packages("mvtnorm")
install.packages("lme4")
install.packages("lmerTest")
install.packages("nlme")
install.packages("dplyr")
install.packages("tidyr")


library(mvtnorm)
library(lme4)
library(lmerTest)
library(nlme)

#######Generating Data -----

Lev2_n <- 250
Lev1_n <- 10
Lev2_mean <- matrix(data = 0, nrow = 2, ncol = 1)
g_mat <- matrix(data = c(1, .3, .3, 1), nrow = 2, ncol = 2)

long_gamma00 <- 0 
long_gamma10 <- 1
long_gamma01 <- 1
long_gamma11 <- 1


set.seed(5189)
Lev2s <- rmvnorm(n = Lev2_n, Lev2_mean, g_mat)
treat <- rbinom(n = Lev2_n, size = 1, prob = .5)
time <- 0:9

set.seed(5146)

long_example_full <- NULL 
for(i in 1:Lev2_n){
  long_eti <- rnorm(n = Lev1_n, mean = 0, sd = 1)
  long_y_ti <- long_gamma00 + long_gamma01*treat[i] + (long_gamma10 + long_gamma11*treat[i] + Lev2s[i,2])*time + Lev2s[i,1] + long_eti
  long_example <- data.frame(y = long_y_ti, data_gen_gamma00 = long_gamma00, data_gen_gamma01 = long_gamma00,
                             data_gen_gamma10 = long_gamma10, data_gen_gamma11 = long_gamma11, treatment = treat[i],
                             time = time, person = i)
  long_example_full <- rbind(long_example_full , long_example)
}

######Fitting models to the generated data -----
unc_growth <- long_example_full

##RQs 1 and 2
Empty_long <- lm(y ~ 1 , data = unc_growth)
rand_int_long <- lmer(y ~ 1 + (1|person), data = unc_growth, REML = FALSE) #Sample size is large here so it should not matter much
# rand_int_long2 <- lme(y ~ 1, random = ~1|person, data = unc_growth, method = "ML")

summary(rand_int_long) 
ICC_long <- 27.65/(27.65 + 32.23) #46.18% so it seems reasonable 

anova(rand_int_long, Empty_long)
ICC_long


#####Adding in fixed and random effects of time. 
## RQ 3
fixed_linear_time  <- lmer(y ~ 1 + time  + (1|person), data = unc_growth, REML = FALSE)
# fixed_time2 <- lme(y ~ 1 time, random = ~1|person, data = unc_growth, method = "ML")
summary(fixed_linear_time)
summary(fixed_linear_time, ddf = "Satterthwaite")

## RQ 4
random_linear_time  <- lmer(y ~ 1 + time  + (1+ time |person), data = unc_growth, REML = FALSE)
# random_time2  <- lme(y ~ 1 + time, random = ~time|person, data = unc_growth, method = "ML")
summary(random_linear_time)
summary(random_time2)

anova(random_linear_time, fixed_linear_time)

###Quadratic Time ----
fixed_quad_time  <- lmer(y ~ 1 + time  + I(time^2) + (1 + time|person), data = unc_growth, REML = FALSE)
# fixed_quad_time2 <- lme(y ~ 1 + time + I(time^2), random = ~ time|person, data = unc_growth, method = "ML")
summary(fixed_quad_time)

random_quad_time  <- lmer(y ~ 1 + time + I(time^2) + (1+ time + I(time^2) |person),
                          data = unc_growth, REML = FALSE, 
                          control = lmerControl(optimizer ="Nelder_Mead")) # Do not make changes unless you
                                                                           # actually read the documentation.
summary(random_quad_time)
# random_quad_time2 <- lme(y ~ 1 + time + I(time^2), random = ~ time + I(time^2)|person, data = unc_growth, method = "ML")

anova(random_quad_time, fixed_quad_time)

#Comparing everything 
anova(rand_int_long, Empty_long, fixed_linear_time, random_linear_time, random_quad_time, fixed_quad_time)


####Cubic Time ----
#Ignore the fit for these because they exploded. 
fixed_cubic_time  <- lmer(y ~ 1 + time + I(time^2) + I(time^3)+ (1+ time + I(time^2) |person),
                          data = unc_growth, REML = FALSE, 
                          control = lmerControl(optimizer ="Nelder_Mead"))

random_cubic_time  <- lmer(y ~ 1 + time + I(time^2) + I(time^3) +
                             (1+ time + I(time^2) + I(time^3) |person),
                          data = unc_growth, REML = FALSE, 
                          control = lmerControl(optimizer ="Nelder_Mead")) 


#####Piecewise models ----
##Examples of how to answer RQ 6 and 7
set.seed(654)
N_people <- 500
dat_holder_list <- vector(mode = "list", length = N_people)

mean_vec_coefs <- c(0, 1, 5) # Means for random effects
G_matrix <- matrix(data = c(1, .3, .3, .3, 1, .3, .3, .3, 1), nrow = 3, ncol = 3) # Random effects covariance matrix
fixed_coefs <- c(0, 3, 0) # Fixed effects only 
coefs <- rmvnorm(n = N_people,mean_vec_coefs, G_matrix)
design_matrix_i <- data.frame(intercept = 1, slope12 = c(0,1,1,1,1), slope25 = c(0,0,1,2,3))
treatment <- rbinom(n = N_people, size = 1, prob = .5)

design_matrix_i <- as.matrix(design_matrix_i)

for(i in 1:N_people){
  id <- i
  B <- c(coefs[i,], fixed_coefs)
  main_eff <- treatment[i]
  slope12_int <- treatment[i]*design_matrix_i[,2]
  slope25_int <- treatment[i]*design_matrix_i[,3]
  design_matrix_i_full <- data.frame(design_matrix_i, main_eff, slope12_int, slope25_int)
  design_matrix_i_full <- as.matrix(design_matrix_i_full)
  y <- design_matrix_i_full%*%B + rnorm(n = 5, sd = 1)
  full_dat_i <- data.frame(intercept_coef = B[1], slope12_coef = B[2], slope25_coef = B[3], 
                           int_pred = design_matrix_i[,1], slope12_pred = design_matrix_i[,2],
                           slope25_pred = design_matrix_i[,3], treat_cond = main_eff, slope12_int = slope12_int, 
                           slope25_int = slope25_int, id = id, y = y)
  dat_holder_list[[i]] <- full_dat_i
}

fin_dat_pieces <- do.call(rbind, dat_holder_list)

fin_dat_pieces51 <- fin_dat_pieces %>%
  select(5:11)

names(fin_dat_pieces51) <- c("slope12", "slope25", "treat_cond", "slope12_int", "slope25_int", "id", "y")

fixed_model  <- lmer(y ~ 1 + slope12 + slope25 + treat_cond + slope12_int + slope25_int 
                           + (1|id), data = fin_dat_pieces51, REML = FALSE)
# fixed_model2 <- lme(y ~ 1 + slope12 + slope25 + treat_cond + slope12_int + slope25_int,
                      # random = ~1|id, data = fin_dat_pieces51, method = "ML")
summary(fixed_model)
# summary(fixed_model2)

####Adding the effect of treatment
random_model  <- lmer(y ~ 1 + slope12 + slope25 + treat_cond + slope12_int + slope25_int 
                     + (1 + slope12 + slope25|id), data = fin_dat_pieces51, REML = FALSE)

# random_model2  <- lme(y ~ 1 + slope12 + slope25 + treat_cond + slope12_int + slope25_int, 
                      # random = ~ 1 + slope12 + slope25|id, data = fin_dat_pieces51, method = "ML")

anova(fixed_model, random_model)



