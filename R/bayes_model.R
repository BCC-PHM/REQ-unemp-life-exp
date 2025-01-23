# Load packages
library(bayesrules)
library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(janitor)
library(broom.mixed)
library(data.table)


#########################################################
#just checking their distibution

ggplot(all_variables, aes(x=life_expectancy ))+
  geom_density()


ggplot(all_variables, aes(x=crime_score_avg))+
  geom_density()

ggplot(all_variables, aes(x=log(claimant_count_pct)))+
  geom_density()


ggplot(all_variables, aes(x=log(income_score_avg)))+
  geom_density()


ggplot(all_variables, aes(x=education_score_avg))+
  geom_density()


ggplot(all_variables, aes(x=housing_score_avg))+
  geom_density()


ggplot(all_variables, aes(x=environment_score_avg))+
  geom_density()

#########################################################
#just checking the their relationships 

ggplot(all_variables, aes(x = claimant_count_pct, y = life_expectancy)) + 
  geom_point(size = 0.5)


ggplot(all_variables, aes(x = crime_score_avg, y = life_expectancy)) + 
  geom_point(size = 0.5)

ggplot(all_variables, aes(x = education_score_avg, y = life_expectancy)) + 
  geom_point(size = 0.5)


ggplot(all_variables, aes(x = sex, y = life_expectancy)) + 
  geom_boxplot()

ggplot(all_variables, aes(x = income_score_avg, y = life_expectancy)) + 
  geom_point(size = 0.5)



#########################################################
# calculate z-score

all_variables1  = all_variables  %>% 
  mutate(across(where(is.numeric), ~ (.-mean(., na.rm = TRUE)) / sd(., na.rm = TRUE), .names = "{.col}_zscore"))

##if zscore> 3 = outliers, remove them by keeping zscore<3
##seems like there are extremely flat tails on life expectancy, i chose zscore >/<2 instead, the qqplot improved 
all_variables1<- all_variables1 %>%
  filter(
    
    between(claimant_count_pct_zscore, -2,2)| is.na(claimant_count_pct_zscore),
    between(income_score_avg_zscore, -2, 2)| is.na(income_score_avg_zscore),
    between(education_score_avg_zscore, -2, 2)| is.na(education_score_avg_zscore),
    between(housing_score_avg_zscore, -2,2)| is.na(housing_score_avg_zscore),
    between(environment_score_avg_zscore, -2,2)| is.na(environment_score_avg_zscore),
    between(crime_score_avg_zscore, -2,2)| is.na(crime_score_avg_zscore),
    between(life_expectancy_zscore, -2,2)| is.na(life_expectancy_zscore)
  )


#########################################################
##bayesian linear regression with weakly informative priors 

#try a simple model 

bayes_model_1= stan_glm(
  life_expectancy ~ claimant_count_pct, 
  data = all_variables1,
  family = gaussian,
  prior_intercept = normal(85,5, autoscale = TRUE),
  prior = normal(0,2.5,autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains=4, iter = 5000*2, seed = 8735468)

#posterior summary stats
tidy(bayes_model_1, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level =0.95)

pp_check(bayes_model_1)

#trace and density plot 
mcmc_trace(bayes_model_1, size=0.1)
mcmc_dens_overlay(bayes_model_1)



#########################################################

#try adding gender

bayes_model_2= stan_glm(
  life_expectancy ~ claimant_count_pct+sex, 
  data = all_variables1,
  family = gaussian,
  prior_intercept = normal(85,5, autoscale = TRUE),
  prior = normal(0,2.5,autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains=4, iter = 5000*2, seed = 8735468)

#posterior summary stats
tidy(bayes_model_2, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level =0.95)

pp_check(bayes_model_2)

#trace and density plot 
mcmc_trace(bayes_model_2, size=0.1)
mcmc_dens_overlay(bayes_model_2)


#########################################################

#adding all variables (except income)

bayes_model_3= stan_glm(
  life_expectancy ~ claimant_count_pct+sex+education_score_avg+housing_score_avg+environment_score_avg+crime_score_avg, 
  data = all_variables1,
  family = gaussian,
  prior_intercept = normal(85,5, autoscale = TRUE),
  prior = normal(0,2.5,autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains=4, iter = 5000*2, seed = 8735468)

#posterior summary stats
tidy(bayes_model_3, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level =0.95)

pp_check(bayes_model_3)


#trace and density plot 
mcmc_trace(bayes_model_3, size=0.1)
mcmc_dens_overlay(bayes_model_3)



##############################
#qq plot to check if residuals are normally distributed 

res = residuals(bayes_model_3)
qqnorm(res)
qqline(res)


hist(res, main = "Histogram of Residuals", xlab = "Residuals")

##############################
all_variables_sample = all_variables %>% 
  slice_sample(n=500)

#k-fold cross validation


set.seed(8735468)
prediction_summary_cv(model =bayes_model_1, data=all_variables_sample, k=10)
prediction_summary_cv(model =bayes_model_2, data=all_variables_sample, k=10)
prediction_summary_cv(model =bayes_model_3, data=all_variables_sample, k=10)


# mae mae_scaled within_50 within_95
# 1 1.615055  0.6103986     0.558      0.95
# mae mae_scaled within_50 within_95
# 2 1.487646  0.6329729     0.538      0.95
# mae mae_scaled within_50 within_95
# 3 1.365119  0.6171022     0.534     0.962


#model 3 is the best option in terms of prediction


##############################
#filter out birmingham data for prediction 
birmingham_prediciton_data = all_variables %>% 
  filter(grepl("Birmingham", MSOA11NM)) %>% 
  mutate(sex=as.numeric(as.character(sex)),
         ID = as.character(row_number()))

#store the bayes_model3 as a dataframe
#this will give us 20000 models
bayes_model_3_df = as.data.frame(bayes_model_3)

prediction_list <- list()

for (i in 1:nrow(birmingham_prediciton_data)){
  
  current_row = birmingham_prediciton_data[i,]
  
  prediction = bayes_model_3_df %>% 
    mutate( mu = `(Intercept)` +
              claimant_count_pct * current_row$claimant_count_pct +
              sex1 * current_row$sex +
              education_score_avg * current_row$education_score_avg +
              housing_score_avg * current_row$housing_score_avg +
              environment_score_avg * current_row$environment_score_avg +
              crime_score_avg * current_row$crime_score_avg,
           y_new = rnorm(20000, mean=mu,sd=sigma))
 
  prediction= prediction %>%
    summarise(median = quantile(y_new, 0.5, na.rm = TRUE),
              lower95CI = quantile(y_new, 0.025, na.rm = TRUE),
              upper95CI = quantile(y_new, 0.975, na.rm = TRUE)
  )
  prediction_list[[i]] <- prediction
  
}


prediciton =rbindlist(prediction_list) %>% 
  mutate(ID = as.character(row_number()))

#prediciton done

birmingham_prediciton_data = birmingham_prediciton_data %>% left_join(prediciton, by="ID")

# plot actual life exp against predicted life exp
birmingham_prediciton_data |> 
  ggplot(aes(x = life_expectancy, 
             y = median)) +
  geom_point() +
  geom_abline(intercept = 0, 
              slope = 1,
              col = "red",
              linewidth = 1)








