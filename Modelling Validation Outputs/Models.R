library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(lme4)
library(nlme)
library(DHARMa)
library(tidyr)
library(ResourceSelection)
library(pROC)
library(car)
library(ggeffects)
library(performance)
library(MuMIn)

# Datasets ----

tf_d1 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/tf1.csv')
tf_d2 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/tf2.csv')
i_d1 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/i1.csv')
i_d1.1 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/i1_1.csv')
i_d2 = read_csv('/Volumes/FRASER USB/MRes/Data/2022/Validation/Model datasets/i2.csv')

# tf_d1 = filter(tf_d1, grepl('C61', `Animal ID`) == 'FALSE') # removed C61 as it is low quality throughout
# tf_d2 = filter(tf_d2, grepl('C61', `Animal ID`) == 'FALSE')
# i_d1 = filter(i_d1, grepl('C61', `Animal ID`) == 'FALSE') 
# i_d1.1 = filter(i_d1.1, grepl('C61', `Animal ID`) == 'FALSE')
# i_d2 = filter(i_d2, grepl('C61', `Animal ID`) == 'FALSE')

tf_d1$`Prey 1/0` = if_else(tf_d1$`Prey TRUE/FALSE` == TRUE, 1, 0)
tf_d2$`Prey 1/0` = if_else(tf_d2$`Prey TRUE/FALSE` == TRUE, 1, 0)

tf_d1$`Animal ID` = as.factor(tf_d1$`Animal ID`)

sum(i_d1.1$`Total prey in window`)

69 / 5399 # 1.3% post-buffer PCE out of window (false negative)
985 / 5399 # 18.2% pre-buffer PCE out of window (false negative)


# Modelling ------------------------------------------------------------------------

# TRUE/FALSE dataset 1 ---- 


# 1: Prey presence absence ~ window size ----


tf_m1 = glmmTMB(as.factor(`Prey 1/0`) ~ `Window size (s)` +(1|`Animal ID`), family = binomial(link = 'logit'), # could use clog-log as have many more 0s than 1s
            data = tf_d1)
summary(tf_m1)

df = data.frame('Window size (s)' = seq(0, 30, 0.01))
colnames(df)[colnames(df) == 'Window.size..s.'] = 'Window size (s)'
pred = predict(tf_m1, newdata = df, type = 'response', re.form = NA) # predict prey presence on test data using model

plot(simulateResiduals(tf_m1))

plot(x = tf_d1$`Window size (s)`, y = tf_d1$`Prey 1/0`) # visualisation of model predictions
lines(df$`Window size (s)`, pred)

plot(tf_m1)
AIC(tf_m1)

explained_deviance = 100 * ((tf_m1$null.deviance - tf_m1$deviance) / tf_m1$null.deviance) # psuedo r2 is 22%

roc_curve <- roc(tf_d1$`Prey 1/0`, fitted(tf_m1))
auc(roc_curve) # area under the curve = 0.82, good true positive and true negative rate
plot(roc_curve)



# 1.1: Prey presence absence ~ log(window size) ----

ggplot() + # much more linear distribution when log transformed
  geom_density(data = tf_d1, aes(x = log(`Window size (s)`))) +
  theme_classic()

tf_m1log = glmmTMB(`Prey 1/0` ~ log(`Window size (s)`) + (1|`Animal ID`), family = binomial(link = 'logit'),
               data = tf_d1)

plot(simulateResiduals(tf_m1log, n = 1000))

AIC(tf_m1, tf_m1log) # log model fits better
auc(roc(tf_d1$`Prey 1/0`, fitted(tf_m1log))) # 0.86


# model outputs

tf_m1log_pred = data.frame(`Window size (s)` = seq(min(tf_d1$`Window size (s)`),
                                                   max(tf_d1$`Window size (s)`),
                                                   length.out = 300))
colnames(tf_m1log_pred)[colnames(tf_m1log_pred) == 'Window.size..s.'] = 'Window size (s)'

tf_m1log_pred$pred = predict(tf_m1log, newdata = tf_m1log_pred, type = 'response', re.form = NA)

plot(x = tf_d1$`Window size (s)`, y = tf_d1$`Prey 1/0`) # visualisation of model predictions
lines(tf_m1log_pred$`Window size (s)`, tf_m1log_pred$pred)

summary(tf_m1log)
AIC(tf_m1log)

ggplot() + # model shape vs data distribution
  geom_density(data = tf_d1, aes(x = `Window size (s)`, ..scaled..), fill = 'grey') +
  geom_line(data = tf_m1log_pred, aes(x = `Window size (s)`, y = pred), col = 'red') +
  theme_classic()




# 2: Prey presence absence ~ mean VeDBA + SD VeDBA ----

ggplot() + # making sure not requiring tranformation
  geom_density(data = tf_d1, aes(x = `SD window VeDBA`, ..scaled..), fill = 'orange', alpha = 0.3) + 
  geom_density(data = tf_d1, aes(x = `Mean window VeDBA`, ..scaled..), fill = 'skyblue', alpha = 0.3) + 
  theme_classic()

# need to test additive vs interaction models
tf_m2_add = glmmTMB(`Prey 1/0` ~ scale(`SD window VeDBA`) + scale(`Mean window VeDBA`) + (1|`Animal ID`), family = binomial(link = 'logit'),
            data = tf_d1) 
tf_m2_int = glmmTMB(`Prey 1/0` ~ scale(`SD window VeDBA`) * scale(`Mean window VeDBA`) + (1|`Animal ID`), family = binomial(link = 'logit'),
                data = tf_d1) 

anova(tf_m2_add, tf_m2_int, test = "Chisq")  # significant difference in fit between additive and interaction models
AIC(tf_m2_add, tf_m2_int)  # interaction model is better - lower AIC, suggests high energy variability only matters when mean expenditure is high

tf_m2 = tf_m2_int

plot(simulateResiduals(tf_m2, n = 1000))

# check for multicollinearity

check_collinearity(tf_m2)

# model outputs

tf_m2_pred = expand.grid(`Mean window VeDBA` = seq(min(tf_d1$`Mean window VeDBA`),
                                                   max(tf_d1$`Mean window VeDBA`),
                                                   length.out = 100),
                        `SD window VeDBA` = seq(min(na.omit(tf_d1$`SD window VeDBA`)),
                                                  max(na.omit(tf_d1$`SD window VeDBA`)),
                                                  length.out = 100))
colnames(tf_m2_pred)[colnames(tf_m2_pred) == 'Mean.window.VeDBA'] = 'Mean window VeDBA'
colnames(tf_m2_pred)[colnames(tf_m2_pred) == 'SD.window.VeDBA'] = 'SD window VeDBA'

tf_m2_pred$pred = predict(tf_m2, newdata = tf_m2_pred, type = 'response', re.form = NA)

ggplot() +
  geom_tile(data = tf_m2_pred, aes(x = `Mean window VeDBA`, y = `SD window VeDBA`, fill = pred)) + 
  scale_fill_viridis_c(name = 'Probability
', limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  theme_minimal()

ggplot() +
  geom_contour_filled(data = tf_m2_pred, aes(x = `Mean window VeDBA`, y = `SD window VeDBA`, z = pred)) +
  labs(fill = 'Probability') +
  theme_minimal()

summary(tf_m2)


# mean vs sd vedba interaction plot

ggplot() +
  geom_point(data = tf_d1, aes(x = `Mean window VeDBA`, y = `SD window VeDBA`), alpha = 0.5, pch = 19) +
  facet_wrap(~ tf_d1$`Prey TRUE/FALSE`) +
  theme_classic()



# 3: Prey presence absence ~ window size + max window depth + mean VeDBA + SD VeDBA ----

tf_m3 = glmmTMB(`Prey 1/0` ~ `Mean window VeDBA` + `SD window VeDBA` + `Window size (s)` + `Max window depth` + (1|`Animal ID`:`Dive ID`),
                family = binomial,
                data = tf_d1)

summary(tf_m3)

plot(simulateResiduals(tf_m3, n = 1000))
check_collinearity(tf_m3)
acf(resid(tf_m3))
AIC(tf_m3)




# Model selection ----

generate_models <- function(response, predictors, random_effect) {
  n <- length(predictors)
  comb_list <- lapply(1:n, function(x) combn(predictors, x, simplify = FALSE))
  comb_list <- unlist(comb_list, recursive = FALSE)
  
  formulas <- lapply(comb_list, function(vars) {
    form <- paste(response, "~", paste(vars, collapse = " + "), "+ (1|", paste(random_effect, collapse = ':'), ")")
    as.formula(form)
  })
  
  # Add the null model formula
  null_formula <- as.formula(paste(response, "~ 1 + (1|", random_effect, ")"))
  formulas <- c(list(null_formula), formulas)  # Add null model to the beginning of the list
  
  return(formulas)
}


predictors = c('`Mean window VeDBA`', 
               '`SD window VeDBA`',
               '`Max window depth`',
               '`Window size (s)`')
response = c('`Prey 1/0`')
random_effect = c('`Animal ID`',
                  '`Dive ID`')

model_formulas = generate_models(response,
                                 predictors,
                                 random_effect)


model_list <- lapply(model_formulas, function(formula) {
  glmmTMB(formula, data = tf_d1, family = 'binomial')
})


model_table = data.frame('model_number' = seq(1, length(model_list)),
                         'formula' = NA,
                         'AIC' = NA)

for (i in 1:nrow(model_table)) {
  model_table$formula[i] = deparse(formula(model_list[[i]]))
  model_table$AIC[i] = AIC(model_list[[i]])
}


best_models = model_table[order(model_table$AIC, decreasing = FALSE),][1:5,]

best_models # best model is `Prey 1/0` ~ `Mean window VeDBA` + `SD window VeDBA` + `Max window depth` + `Window size (s)` + (1|`Animal ID`:`Dive ID`)

summary(model_list[[16]])

check_collinearity(model_list[[16]])



# TRUE/FALSE dataset 2 ----



# 1: Prey presence absence ~ time spent hunting ----


ggplot() + # fairly linear without transformation
  geom_density(data = tf_d2, aes(x = `Time spent hunting (s)`))


tf2_m1 = glm(`Prey 1/0` ~ `Time spent hunting (s)`, family = binomial(link = 'logit'),
             data = tf_d2)

AIC(tf2_m1) # 290
100 * ((tf2_m1$null.deviance - tf2_m1$deviance) / tf2_m1$null.deviance) # pseudo r2 is 61


# model outputs

tf2_m1_pred = data.frame(`Time spent hunting (s)` = seq(min(tf_d2$`Time spent hunting (s)`),
                                                           max(tf_d2$`Time spent hunting (s)`),
                                                           length.out = 300))
colnames(tf2_m1_pred)[colnames(tf2_m1_pred) == 'Time.spent.hunting..s.'] = 'Time spent hunting (s)'

tf2_m1_pred$pred = predict(tf2_m1, newdata = tf2_m1_pred, type = 'response')

ggplot() +
  geom_point(data = tf_d2, aes(x = `Time spent hunting (s)`, y = `Prey 1/0`), pch = 1, alpha = 0.5) +
  geom_line(data = tf2_m1_pred, aes(x = `Time spent hunting (s)`, y = pred), col = 'red') +
  ylab('') +
  theme_classic()

summary(tf2_m1)



# 2: Prey presence absence ~ max dive depth ----


ggplot() +
  geom_density(data = tf_d2, aes(x = `Max dive depth`, ..scaled..)) +
  theme_classic()

tf2_m2 = glm(`Prey 1/0` ~ `Max dive depth`, family = binomial(link = 'logit'),
             data = tf_d2)

AIC(tf2_m2) # 274
100 * ((tf2_m2$null.deviance - tf2_m2$deviance) / tf2_m2$null.deviance) # pseudo r2 is 63


# model outputs

tf2_m2_pred = data.frame(`Max dive depth` = seq(min(tf_d2$`Max dive depth`),
                                                        max(tf_d2$`Max dive depth`),
                                                        length.out = 300))
colnames(tf2_m2_pred)[colnames(tf2_m2_pred) == 'Max.dive.depth'] = 'Max dive depth'

tf2_m2_pred$pred = predict(tf2_m2, newdata = tf2_m2_pred, type = 'response')

ggplot() +
  geom_point(data = tf_d2, aes(x = `Max dive depth`, y = `Prey 1/0`), pch = 1, alpha = 0.5) +
  geom_line(data = tf2_m2_pred, aes(x = `Max dive depth`, y = pred), col = 'red') +
  ylab('') +
  theme_classic()

summary(tf2_m2)



# 2.1: Prey presence absence ~ time spent hunting + max dive depth (slightly stronger model) ----


tf2_m2.1 = glm(`Prey 1/0` ~ scale(`Time spent hunting (s)`) + scale(`Max dive depth`),
               family = binomial(link = 'logit'),
               data = tf_d2)

AIC(tf2_m2.1) # 226
100 * ((tf2_m2.1$null.deviance - tf2_m2.1$deviance) / tf2_m2.1$null.deviance) # pseudo r2 is 70

vif(tf2_m2.1) # not multicollinear (VIF = 1.23)


# model outputs

tf2_m2.1_pred = expand.grid(`Time spent hunting (s)` = seq(min(tf_d2$`Time spent hunting (s)`),
                                                   max(tf_d2$`Time spent hunting (s)`),
                                                   length.out = 100),
                           `Max dive depth` = seq(min(tf_d2$`Max dive depth`),
                                                          max(tf_d2$`Max dive depth`),
                                                          length.out = 100))
colnames(tf2_m2.1_pred)[colnames(tf2_m2.1_pred) == 'Time.spent.hunting..s.'] = 'Time spent hunting (s)'
colnames(tf2_m2.1_pred)[colnames(tf2_m2.1_pred) == 'Max.dive.depth'] = 'Max dive depth'

tf2_m2.1_pred$pred = predict(tf2_m2.1, newdata = tf2_m2.1_pred, type = 'response')


ggplot() +
  geom_tile(data = tf2_m2.1_pred, aes(x = `Time spent hunting (s)`, y = `Max dive depth`, fill = pred)) + 
  scale_fill_viridis_c(name = 'Probability
', limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  theme_minimal()

ggplot() +
  geom_contour_filled(data = tf2_m2.1_pred, aes(x = `Time spent hunting (s)`, y = `Max dive depth`, z = pred)) +
  labs(fill = 'Probability') +
  theme_minimal()

summary(tf2_m2.1)



# 3: Prey presence absence ~ dive duration ----


ggplot() + # log is most linear
  geom_density(data = tf_d2, aes(x = log(`Dive duration (s)`)))

tf2_m3 = glm(`Prey 1/0` ~ log(`Dive duration (s)`), family = binomial(link = 'logit'),
             data = tf_d2)

AIC(tf2_m3) # log has lower AIC (raw = 350, log = 319)
100 * ((tf2_m3$null.deviance - tf2_m3$deviance) / tf2_m3$null.deviance) # pseudo r2 is 57


# model outputs

tf2_m3_pred = data.frame('Dive duration (s)' = seq(min(tf_d2$`Dive duration (s)`), max(tf_d2$`Dive duration (s)`), length.out = 300))
colnames(tf2_m3_pred)[colnames(tf2_m3_pred) == 'Dive.duration..s.'] = 'Dive duration (s)'

tf2_m3_pred$pred = predict(tf2_m3, newdata = tf2_m3_pred, type = 'response')

plot(tf_d2$`Dive duration (s)`, tf_d2$`Prey 1/0`)
lines(tf2_m3_pred$`Dive duration (s)`, tf2_m3_pred$pred, add = TRUE)

summary(tf2_m3)



# model selection ----

predictors_d2 = c('`Time spent hunting (s)`', 
               '`Max dive depth`',
               '`Dive duration (s)`')
response_d2 = c('`Prey 1/0`')
random_effect_d2 = c('`Animal ID`',
                  '`Dive ID`')

model_formulas_d2 = generate_models(response_d2,
                                 predictors_d2,
                                 random_effect_d2)


model_list_d2 <- lapply(model_formulas_d2, function(formula) {
  glmmTMB(formula, data = tf_d2, family = 'binomial')
})


model_table_d2 = data.frame('model_number' = seq(1, length(model_list_d2)),
                         'formula' = NA,
                         'AIC' = NA)

for (i in 1:nrow(model_table_d2)) {
  model_table_d2$formula[i] = deparse(formula(model_list_d2[[i]]))
  model_table_d2$AIC[i] = AIC(model_list_d2[[i]])
}


best_models_d2 = model_table_d2[order(model_table_d2$AIC, decreasing = FALSE),][1:5,]

best_models_d2 # best model is `Prey 1/0` ~ `Time spent hunting (s)` + `Max dive depth` + `Dive duration (s)` + (1|`Animal ID`:`Dive ID`)
# look into model without time spent hunting (TDRs don't have this data)


# Intensity dataset 1 ----


# 1: Total prey in window ~ window size ----

# need to use glmmTMB package and truncated_nbinom2 family for this (zero-truncated negative binomial with quadratic parameterisation)
# double check with Marianna - this model needs revisiting

ggplot() +
  geom_density(data = i_d1.1, aes(x = `Total prey in window`))
ggplot() +
  geom_density(data = i_d1.1, aes(x = `Window size (s)`)) # should we sqrt transform? most linear this way

hist(i_d1.1$`Total prey in window`)

n = i_d1.1 %>% # need to assign weights
  group_by(`Animal ID`) %>%
  summarise(n = n())
i_d1.1 = left_join(i_d1.1, n, by = 'Animal ID')


i_m1 = glmmTMB(`Total prey in window` ~ `Window size (s)` + `Max window depth` + offset(sqrt(`Window size (s)`)),
                family = poisson,
                data = i_d1.1)

# plot(fitted(i_m1a), resid(i_m1a))
plot(DHARMa::simulateResiduals(i_m1, n = 1000))

# Extract residuals
residuals <- residuals(i_m1)

# Plot ACF of residuals
acf(residuals, main = "ACF of Residuals")


AIC(i_m1)



# 2: Total prey in window ~ mean VeDBA + SD VeDBA ----


ggplot() +
  geom_density(data = i_d1.1, aes(x = log(`Total prey in window`)))
ggplot() +
  geom_density(data = i_d1.1, aes(x = `Mean window VeDBA`))
ggplot() +
  geom_histogram(data = i_d1.1, aes(x = `SD window VeDBA`)) 

plot(i_d1.1$`Mean window VeDBA`, i_d1.1$`SD window VeDBA`)

# iterative polynomial test
test = data.frame('poly' = seq(1,11,1),
                  'AIC' = NA)

for (i in 1:nrow(test)) {
  test$AIC[i] = AIC(glmmTMB(`Total prey in window` ~ poly(`SD window VeDBA`, test$poly[i]) + poly(`Mean window VeDBA`, test$poly[i]) + (1|`Animal ID` + `Dive ID`), 
                            family = truncated_nbinom2(link = 'log'),
                            data = na.omit(i_d1.1)))
}
# best is probably poly = 2


i_m2 = glmmTMB(`Total prey in window` ~ poly(`SD window VeDBA`, 2) + poly(`Mean window VeDBA`, 2) + (1|`Animal ID` + `Dive ID`), 
               family = truncated_nbinom2(link = 'log'),
               data = na.omit(i_d1.1))
AIC(i_m2)
summary(i_m2)

plot(fitted(i_m2), residuals(i_m2, type = 'response'))
qqnorm(residuals(i_m2, type = 'response')); qqline(residuals(i_m2, type = 'response'), col = 'red')

plot(simulateResiduals(i_m2, n = 200))


i_m2_pred = expand.grid('SD window VeDBA' = seq(min(na.omit(i_d1.1)$`SD window VeDBA`), max(na.omit(i_d1.1)$`SD window VeDBA`), length.out = 100),
                        'Mean window VeDBA' = seq(min(na.omit(i_d1.1)$`Mean window VeDBA`), max(na.omit(i_d1.1)$`Mean window VeDBA`), length.out = 100))


i_m2_pred$pred = predict(i_m2, newdata = i_m2_pred, type = 'response', re.form = NA)

ggplot() +
  geom_contour_filled(data = i_m2_pred, aes(x = `Mean window VeDBA`, y = `SD window VeDBA`, z = pred)) +
  theme_minimal()




# 3: Mean VeDBA ~ prey type ----


i_m3 = glmmTMB(`Mean window VeDBA` ~ `Prey type` + (1|`Animal ID`),
               family = gaussian(link = 'identity'),
               data = i_d1)

AIC(i_m3)

plot(fitted(i_m3), residuals(i_m3, type = 'response'))
qqnorm(residuals(i_m3, type = 'response')); qqline(residuals(i_m3, type = 'response'), col = 'red') # fits really nice!


pred = ggpredict(i_m3, terms = 'Prey type')

plot(pred)



# 4: SD VeDBA ~ prey type ----


i_m4 = glmmTMB(`SD window VeDBA` ~ `Prey type` + (1|`Animal ID`),
               family = gaussian(link = 'identity'),
               data = i_d1)

AIC(i_m4)

plot(fitted(i_m4), residuals(i_m4, type = 'response'))
qqnorm(residuals(i_m4, type = 'response')); qqline(residuals(i_m4, type = 'response'), col = 'red') # not as good of a fit


pred = ggpredict(i_m4, terms = 'Prey type')

plot(pred)



# 5: Swarm TRUE/FALSE ~ SD VeDBA + mean VeDBA ----

i_d1_swarm = filter(i_d1, grepl('krill', `Prey type`, ignore.case = TRUE) == TRUE & is.na(`SD window VeDBA`) == FALSE)
i_d1_swarm$`Swarm TRUE/FALSE` = if_else(grepl('swarm', i_d1_swarm$`Prey type`, ignore.case = TRUE) == TRUE, 1, 0)


i_m5 = glmmTMB(`Swarm TRUE/FALSE` ~ `SD window VeDBA` + `Mean window VeDBA` + (1|`Animal ID`),
               family = binomial(link = 'logit'),
               data = i_d1_swarm)

plot(fitted(i_m5), residuals(i_m5, type = 'response'))
qqnorm(residuals(i_m5, type = 'response')); qqline(residuals(i_m5, type = 'response'), col = 'red')


i_m5_pred = expand.grid(`SD window VeDBA` = seq(min(i_d1_swarm$`SD window VeDBA`), max(i_d1_swarm$`SD window VeDBA`), length.out = 100),
                        `Mean window VeDBA` = seq(min(i_d1_swarm$`Mean window VeDBA`), max(i_d1_swarm$`Mean window VeDBA`), length.out = 100))

i_m5_pred$pred = predict(i_m5, newdata = i_m5_pred, type = 'response', re.form = NA)

ggplot() +
  geom_contour_filled(data = i_m5_pred, aes(x = `SD window VeDBA`, y = `Mean window VeDBA`, z = pred))




# Intensity dataset 2 ----


i_d2_filt = filter(i_d2, `Total prey` > 0)

ggplot() +
  geom_density(data = i_d2_filt, aes(x = sqrt(`Dive duration (s)`))) +
  theme_minimal()

i2_m1 = glmmTMB(`Total prey` ~ scale(`Time spent hunting (s)`) * scale(`Dive duration (s)`) + (1|`Animal ID`),
                family = truncated_nbinom2(link = 'log'),
                data = i_d2_filt)

AIC(i2_m1)
summary(i2_m1)

plot(fitted(i2_m1), residuals(i2_m1, type = 'response'))
qqnorm(residuals(i2_m1, type = 'response')); qqline(residuals(i2_m1, type = 'response'), col = 'red')

plot(simulateResiduals(i2_m1, n = 200))

