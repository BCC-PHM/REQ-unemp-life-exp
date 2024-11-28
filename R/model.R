library(tidyverse)
library(car)
library(corrplot)

ggplot(data = all_variables,
       aes(x = life_expectancy, y = claimant_count_pct, col = sex)) +
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

model <- lm(life_expectancy ~ claimant_count_pct + sex + education_score_avg + crime_score_avg + housing_score_avg + environment_score_avg,
            data = all_variables)

model_vif <- vif(model)
# claimant_count_pct      sex         education_score_avg       crime_score_avg     housing_score_avg environment_score_avg 
# 3.122283              1.257154              2.287439              2.139644              1.106388              1.217568 

# all below 5 so can continue

# view a summary of the model
summary(model)

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
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)           87.323737   0.074199 1176.878   <2e-16 ***
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
all_variables$life_expectancy.lm <- model$fitted.values

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

summary(aov(model))
