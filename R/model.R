library(tidyverse)
library(car)
library(corrplot)
library(performance)

ggplot(data = all_variables,
       aes(x = claimant_count_pct, y = life_expectancy, col = sex)) +
  geom_jitter() +
  geom_smooth()

# build linear regression model
model <- lm(life_expectancy ~ claimant_count_pct + sex + income_score_avg + education_score_avg + crime_score_avg + housing_score_avg + environment_score_avg,
            data = all_variables)

# check variance inflation factors
model_vif <- vif(model)
# claimant_count_pct      sex        income_score_avg         education_score_avg       crime_score_avg     housing_score_avg 
# 6.066737              1.500583             10.576264              4.127275              2.274874              1.128377 
# environment_score_avg 
# 1.243125

barplot(model_vif)

# income index has very high VIF (10.58) so try removing that variable

model2 <- lm(life_expectancy ~ claimant_count_pct + sex + education_score_avg + crime_score_avg + housing_score_avg + environment_score_avg,
            data = all_variables)

model2_vif <- vif(model2)
# claimant_count_pct      sex         education_score_avg       crime_score_avg     housing_score_avg environment_score_avg 
# 3.122283              1.257154              2.287439              2.139644              1.106388              1.217568 

# all below 5 so can continue

# view a summary of the model
summary(model2)

# Call:
#   lm(formula = life_expectancy ~ claimant_count_pct + sex + education_score_avg + 
#        crime_score_avg + housing_score_avg + environment_score_avg, 
#      data = all_variables)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -11.0881  -1.4117  -0.0852   1.2206  13.9330 
# 
# Coefficients:
#                         Estimate Std. Error  t value Pr(>|t|)    
#   (Intercept)           87.323737   0.074199 1176.878   <2e-16 ***
#   claimant_count_pct    -0.499840   0.018347  -27.243   <2e-16 ***
#   sex1                  -3.259024   0.043871  -74.286   <2e-16 ***
#   education_score_avg   -0.069677   0.001871  -37.242   <2e-16 ***
#   crime_score_avg       -0.358120   0.040827   -8.772   <2e-16 ***
#   housing_score_avg      0.049360   0.002405   20.524   <2e-16 ***
#   environment_score_avg -0.002730   0.001677   -1.628    0.104    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.247 on 13188 degrees of freedom
# Multiple R-squared:  0.6166,	Adjusted R-squared:  0.6164 
# F-statistic:  3535 on 6 and 13188 DF,  p-value: < 2.2e-16

# add the fitted model values to the original data as a column, so they can be compared
all_variables$life_expectancy.lm <- model2$fitted.values

# compared model life expectancy with actual life expectancy
ggplot(data = all_variables,
       aes(x = life_expectancy,
           y = life_expectancy.lm,
           col = sex)) +
  geom_point() +
  xlab("True life expectancy") +
  ylab("Model fitted life expectancy") +
  #geom_smooth(method = "lm", se = F)
  geom_abline(intercept = 0, 
              slope = 1,
              col = "black",
              linewidth = 1)

# check assumptions of linear regression

par(mfrow = c(2,2))
plot(model2)

# durbin watson test
durbinWatsonTest(model2)

# component and residual plots (checks for linear relationship)
crPlots(model2)

# various diagnostic visualisations
check_model(model2)

# repeat but remove outliers to improve normality of residuals

# start by plotting life expectancy and standard deviations
mean_life_exp <- mean(all_variables$life_expectancy, na.rm = T)
sd_life_exp <- sd(all_variables$life_expectancy, na.rm = T)

ggplot(all_variables) +
  geom_histogram(aes(x = life_expectancy)) +
  geom_vline(xintercept = mean_life_exp + sd_life_exp, 
             color = "green",   
             linetype = "dashed") +
  geom_vline(xintercept = mean_life_exp - sd_life_exp, 
             color = "green",   
             linetype = "dashed") +
  geom_vline(xintercept = mean_life_exp + 2*sd_life_exp, 
             color = "blue",   
             linetype = "dashed") +
  geom_vline(xintercept = mean_life_exp - 2*sd_life_exp, 
             color = "blue",   
             linetype = "dashed") +
  geom_vline(xintercept = mean_life_exp + 3*sd_life_exp, 
             color = "orange", 
             linetype = "dashed") +
  geom_vline(xintercept = mean_life_exp - 3*sd_life_exp, 
             color = "orange", 
             linetype = "dashed") +
  theme_classic()

# calculate z score

all_variables_filt <- all_variables |> 
  mutate(across(where(is.numeric), ~ (.-mean(., na.rm = TRUE)) / sd(., na.rm = TRUE), .names = "{.col}_zscore"))

all_variables_filt <- all_variables_filt |> 
  filter(
    between(claimant_count_pct_zscore, -2,2)| is.na(claimant_count_pct_zscore),
    between(income_score_avg_zscore, -2, 2)| is.na(income_score_avg_zscore),
    between(education_score_avg_zscore, -2, 2)| is.na(education_score_avg_zscore),
    between(housing_score_avg_zscore, -2,2)| is.na(housing_score_avg_zscore),
    between(environment_score_avg_zscore, -2,2)| is.na(environment_score_avg_zscore),
    between(crime_score_avg_zscore, -2,2)| is.na(crime_score_avg_zscore),
    between(life_expectancy_zscore, -2,2)| is.na(life_expectancy_zscore)
  )

# now can rerun model

model3 <- lm(life_expectancy ~ claimant_count_pct + sex + education_score_avg + crime_score_avg + housing_score_avg + environment_score_avg,
            data = all_variables_filt)

model3_vif <- vif(model3)
# claimant_count_pct                   sex   education_score_avg       crime_score_avg 
# 3.129791              1.262250              2.282972              2.131245 
# housing_score_avg environment_score_avg 
# 1.107280              1.214316 

# all below 5 so can continue

# view a summary of the model
summary(model3)

# Call:
#   lm(formula = life_expectancy ~ claimant_count_pct + sex + education_score_avg + 
#        crime_score_avg + housing_score_avg + environment_score_avg, 
#      data = all_variables_filt)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.2952 -1.2416 -0.0045  1.1932  8.5017 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)           87.455903   0.082281 1062.895  < 2e-16 ***
#   claimant_count_pct    -0.552732   0.025904  -21.338  < 2e-16 ***
#   sex1                  -2.952180   0.042180  -69.991  < 2e-16 ***
#   education_score_avg   -0.070130   0.002092  -33.526  < 2e-16 ***
#   crime_score_avg       -0.290728   0.044000   -6.607  4.1e-11 ***
#   housing_score_avg      0.040470   0.002614   15.484  < 2e-16 ***
#   environment_score_avg -0.007015   0.001928   -3.640 0.000274 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.886 on 10475 degrees of freedom
# Multiple R-squared:  0.5784,	Adjusted R-squared:  0.5781 
# F-statistic:  2395 on 6 and 10475 DF,  p-value: < 2.2e-16

check_model(model3)

# for each 1-point increase in claimant count percentage there is an associated 0.55 year decrease in life expectancy
# for each 1-point increase in crime IoD score there is an associated 0.29 year decrease in life expectancy

# compare model life expectancy with actual life expectancy
all_variables_filt$life_expectancy.lm <- model3$fitted.values

ggplot(data = all_variables_filt,
       aes(x = life_expectancy,
           y = life_expectancy.lm,
           col = sex)) +
  geom_point() +
  xlab("True life expectancy") +
  ylab("Model fitted life expectancy") +
  #geom_smooth(method = "lm", se = F)
  geom_abline(intercept = 0, 
              slope = 1,
              col = "black",
              linewidth = 1)

# filter out birmingham MSOAs
birmingham_MSOA <-  all_variables %>% 
  filter(grepl("Birmingham", MSOA11NM))

# predict life expectancy (will be the same as the one in the model so not a true prediction)
birmingham_MSOA$predicted_life_expectancy <- predict(model3, newdata = birmingham_MSOA)

birmingham_MSOA |> 
  ggplot(aes(x = life_expectancy, 
             y = predicted_life_expectancy)) +
  geom_point() +
  geom_abline(intercept = 0, 
              slope = 1,
              col = "red",
              linewidth = 1)
