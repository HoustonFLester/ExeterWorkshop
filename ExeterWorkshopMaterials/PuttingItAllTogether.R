####Description: Putting it all together with a real dataset
####Notes: These are the data used in the Bliese and 

# install.packages("multilevel")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("lme4")
# install.packages("nlme")
# install.packages("multilevel")
# install.packages("tidyr")
# install.packages("lavaan")
library(multilevel)
library(lavaan)


data(univbct)
set.seed(51)
univbct_50 <- sample(1:495, size = 50, replace = F) # do not reuse

#Taking a subset of the data to make a plot
univbct_50_dat <- univbct %>%
  filter(SUBNUM %in% univbct_50) %>%
  arrange(SUBNUM)

#Spaghetti plots of these data 
univbct_50_dat$time_factor <- factor(univbct_50_dat$TIME)
univbct_50_dat$SUBNUM_factor <- factor(univbct_50_dat$SUBNUM)

univbct$time_factor <- factor(univbct$TIME)
univbct$SUBNUM_factor <- factor(univbct$SUBNUM)



ggplot(data = univbct_50_dat, aes(x = time_factor, y = JSAT, colour = SUBNUM_factor)) +       
  geom_line(aes(group = SUBNUM_factor)) + geom_point() +
  ylab("Job Satisfaction") +
  xlab("Time") +
  theme(legend.title=element_blank())

###Identify the interesting individual

univbct_50_dat_wide <- univbct_50_dat %>%
  select(SUBNUM, TIME, JSAT) %>%
  spread(TIME, JSAT, sep = "") %>%
  arrange(TIME0)

View(univbct_50_dat_wide)


##Multivariate repeated Measures ANOVA. Functioning as the answer key.
MVRM_jsat <- gls(JSAT ~  time_factor, 
                 correlation = corSymm(form = ~ 1 | SUBNUM_factor),
                 weight = varIdent(form = ~ 1 | time_factor), data = univbct, na.action = na.omit, method = "ML")
summary(MVRM_jsat)
getVarCov(MVRM_jsat)


# RQ 1 and 2
ICC_jsat <- lme(JSAT ~ 1 , 
                     random =~ 1 | SUBNUM, method = "ML", data = univbct, na.action = na.omit)
summary(ICC_jsat)
getVarCov(ICC_jsat)
VarCorr(ICC_jsat)

###Step 1 try to get the trend over time correct.  
# RQ 3

  ##Random Intercept - fixed linear 

fixed_linear_RI <- lme(JSAT ~ 1 + TIME, 
                random =~ 1 | SUBNUM, data = univbct, na.action = na.omit, method = "ML")

summary(fixed_linear_RI)

# RQ 4
random_linear <- lme(JSAT ~ 1 + TIME, 
                       random =~ TIME | SUBNUM, method = "ML", data = univbct, na.action = na.omit)

summary(random_linear)


  ##Fixed Quadratic

fixed_quad_rlin <- lme(JSAT ~ 1 + TIME + I(TIME^2), 
                     random =~ TIME | SUBNUM, method = "ML", data = univbct, na.action = na.omit)
summary(fixed_quad_rlin)

###Data management for fitting a single piecewise slope for the difference between time points two and three

univbct2 <- univbct %>%
  mutate(p23 = if_else(TIME == 2 , 1, 0))

table(univbct2$TIME, univbct2$p23)

piece_RI <- lme(JSAT ~ 1 + p23, 
                       random =~ 1 | SUBNUM, method = "ML", data = univbct2, na.action = na.omit)
summary(piece_RI)

### I have never seen anyone do a model like this, but it should be okay. 
piece_Rlin <- lme(JSAT ~ 1 + p23, 
             random =~ p23 | SUBNUM, method = "ML", data = univbct2, na.action = na.omit)
summary(piece_Rlin)

anova(MVRM_jsat, ICC_jsat, fixed_linear_RI, random_linear, fixed_quad_rlin, piece_RI, piece_Rlin)

####Now looking to try out some alternative covariance structures (i.e., R matrix structures). 


####RQ 5
random_linear_alt <-lme(JSAT~TIME,random=~1+TIME|SUBNUM,
                        data=univbct,na.action=na.omit, 
                        control = lmeControl(opt = 'optim'), method = "ML")

random_linear_alt_AR1 <- update(random_linear_alt, correlation = corAR1())
VarCorr(random_linear_alt_AR1)

anova(random_linear_alt, random_linear_alt_AR1)


#This one is not estimable, because we have a random intercept. 
random_linear_alt_AR2 <- update(random_linear_alt, 
                                correlation = corARMA(form =~ 1|SUBNUM, p = 2))
summary(random_linear_alt_AR1)

########So now we have our best unconditional model 
random_linear_alt_AR1

random_linear_alt_AR1 <-lme(JSAT~TIME,random=~1+TIME|SUBNUM,
                        data=univbct,na.action=na.omit, correlation = corAR1(),
                        control = lmeControl(opt = 'optim'), method = "ML")

####Adding in a time-invariant predictor 

# RQ 6
univbct <- univbct %>%
  mutate(gender_c1 = GENDER - 1, READY_3.11 = READY - 3.11)

univbct2 <- univbct %>%
  group_by(SUBNUM) %>%
  mutate(person_READY = mean(READY, na.rm = T), t_vary_READY = READY - person_READY,
         Lev2_READY_c = person_READY - 3.11)



gender_mod <-lme(JSAT~ TIME + gender_c1,random=~1+TIME|SUBNUM,
                            data=univbct,na.action=na.omit, correlation = corAR1(),
                            control = lmeControl(opt = 'optim'), method = "ML")
summary(gender_mod)

# RQ 7
gender_int_mod <-lme(JSAT~ TIME + gender_c1 + TIME*gender_c1,random=~1+TIME|SUBNUM,
                 data=univbct,na.action=na.omit, correlation = corAR1(),
                 control = lmeControl(opt = 'optim'), method = "ML")

summary(gender_int_mod)

###Adding in a time-varying predictor ----

gender_int_mod <-lme(JSAT~ TIME + gender_c1 + TIME*gender_c1,random=~1+TIME|SUBNUM,
                     data=univbct,na.action=na.omit, correlation = corAR1(),
                     control = lmeControl(opt = 'optim'), method = "ML")

summary(gender_int_mod)


###Time-varying predictor smushed. 
tvar_int <-lme(JSAT~ TIME + gender_c1 + READY_3.11 + READY_3.11*TIME,random=~1+TIME|SUBNUM,
                     data=univbct,na.action=na.omit, correlation = corAR1(),
                     control = lmeControl(opt = 'optim'), method = "ML")

summary(tvar_int)

####Data management to unconflate the predictors (person mean centering)
####Do not actually use this approach unless there is not random growth in the predictor. 
#### These data do show change in the predictor (READY); so this analysis is inappropriate. 
#### However, I did want to show you how to do it. 
#### The approach below using lavaan is a more appropriate way. 
READY_fix_lin_ran_int <- lme(READY ~  TIME, 
                          random=~1|SUBNUM, data = univbct, na.action = na.omit, method = "ML")

READY_fix_lin_ran_slope <- lme(READY ~  TIME, 
                random=~1+TIME|SUBNUM, data = univbct, na.action = na.omit, method = "ML")

anova(READY_fix_lin_ran_int, READY_fix_lin_ran_slope) 
getVarCov(READY_MV)


tvar_main_unsmush <-lme(JSAT~ TIME + gender_c1 + t_vary_READY + Lev2_READY_c,random=~1+TIME|SUBNUM,
                data=univbct2,na.action=na.omit, correlation = corAR1(),
                control = lmeControl(opt = 'optim'), method = "ML")

summary(tvar_main_unsmush)


tvar_int_unsmush <-lme(JSAT~ TIME + gender_c1 + t_vary_READY + 
                          Lev2_READY_c + Lev2_READY_c*TIME ,random=~1+TIME|SUBNUM,
                        data=univbct2,na.action=na.omit, correlation = corAR1(),
                        control = lmeControl(opt = 'optim'), method = "ML")

summary(tvar_int_unsmush)

tvar_unsmush_complex <-lme(JSAT~ TIME + gender_c1 + t_vary_READY + 
                         Lev2_READY_c + Lev2_READY_c*TIME + Lev2_READY_c*t_vary_READY ,random=~1+TIME|SUBNUM,
                       data=univbct2,na.action=na.omit, correlation = corAR1(),
                       control = lmeControl(opt = 'optim'), method = "ML")

summary(tvar_unsmush_complex)

##Contextual Effects ----
tvar_main_unsmush <-lme(JSAT~ TIME + gender_c1 + t_vary_READY + Lev2_READY_c,random=~1+TIME|SUBNUM,
                        data=univbct2,na.action=na.omit, correlation = corAR1(),
                        control = lmeControl(opt = 'optim'), method = "ML")

summary(tvar_main_unsmush)
fixed.effects(tvar_main_unsmush)

contextual <-lme(JSAT~ TIME + gender_c1 + READY_3.11 + Lev2_READY_c,random=~1+TIME|SUBNUM,
                        data=univbct2,na.action=na.omit, correlation = corAR1(),
                        control = lmeControl(opt = 'optim'), method = "ML")

summary(contextual)


summary(tvar_int_unsmush)

####Actually use the SEM approach - It is not really different just easier to do if you familiar with SEM

###Data need to be in a wide format. 
View(univbct2) # We already have the wide variables, but we do need to remove the duplicates rows for them.
univbct2_wide <- univbct2[!duplicated(univbct2$SUBNUM),]

univbct2_wide <- univbct2_wide %>% 
  select(JOBSAT1, JOBSAT2, JOBSAT3, READY1, READY2, READY3, SUBNUM)

univbct2_wide <- univbct2_wide[,-7]

univbct2_wide$nmiss <- rowSums(is.na(univbct2_wide))

univbct2_wide_final <- univbct2_wide %>%
  filter(nmiss == 0)



SEM_approach <- ' i_sat =~ 1*JOBSAT1 + 1*JOBSAT2 + 1*JOBSAT3 
                  s_sat =~ 0*JOBSAT1 + 1*JOBSAT2 + 2*JOBSAT3
                  i_ready =~ 1*READY1 + 1*READY2 + 1*READY3
                  s_ready =~ 0*READY1 + 1*READY2 + 2*READY3
'

fit <- growth(SEM_approach, data = univbct2_wide_final)
summary(fit)





