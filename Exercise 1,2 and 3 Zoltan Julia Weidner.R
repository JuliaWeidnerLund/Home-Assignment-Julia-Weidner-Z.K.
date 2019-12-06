############################################################################################################################
####Home Assignment Zoltan##################################################################################################
############################################################################################################################

#in this R script you find all three exercises for the home assignment (if it gets stuck, run smaller sections and not all at once)
#Exercise 1: line 10
#Exercise 2: line 390
#Exercise 3: line 548

####Exercise 1##############################################################################################################

####Preparations####

#loading packages
require(psych)
require(lm.beta)
require(dplyr)
require(gsheet)
require(car)
require(ggplot2)
require(rgl)
require(lsr)
require(tidyverse)
require(gridExtra)
#loading data
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ")
#loading error plotter function
error_plotter <- function(mod, col = "black", x_var = NULL){	
  mod_vars = as.character(mod$call[2])	
  data = as.data.frame(eval(parse(text = as.character(mod$call[3]))))	
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern ='~',mod_vars))-2)	
  x = substr(mod_vars, as.numeric(gregexpr(pattern ='~',mod_vars))+2, nchar(mod_vars))	
  
  data$pred = predict(mod)	
  
  if(x == "1" & is.null(x_var)){x = "response_ID"	
  data$response_ID = 1:nrow(data)} else if(x == "1"){x = x_var}	
  
  plot(data[,y] ~ data[,x], ylab = y, xlab = x)	
  abline(mod)	
  
  for(i in 1:nrow(data)){	
    clip(min(data[,x]), max(data[,x]), min(data[i,c(y,"pred")]), max(data[i,c(y,"pred")]))	
    abline(v = data[i,x], lty = 2, col = col)	
  }	
  
}

#loading function for tables
coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}




###Exploring the data###

#viewing the data
view(data_sample_1)
#looking at the structure of the data 
str(data_sample_1)
#getting a basic overview
summary(data_sample_1)
#looking at variables
who(TRUE)
#descriptive statistics
describe(data_sample_1)
#no NAs

###visualizing the data### (without sex)
windows()
data_sample_1 %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram()	

windows()
data_sample_1 %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram()

windows()
data_sample_1 %>% 	#noticable that one value is an outlier and below the possible values, wrong value: has to be removed
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram()	

windows()
data_sample_1 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram()	

windows()
data_sample_1 %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram()

windows()
data_sample_1 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva) +	
  geom_histogram()

#Found out that there are no NAs. 
#irregularity STAI-trait: ID_18 has value of 3.5 which does not exist. It must be 35.0 since the STAI can have values from 20 to 80.
#Transforming datasample by keeping old data sample and creating new one with changes
data_sample_2 <- data_sample_1 %>% 	
  mutate(STAI_trait = as.numeric(as.character(replace(STAI_trait, STAI_trait == 3.5 , 35.0))))

#The participant with the ID_15 is male and weights 38.8 kilogramms. Maybe a multivariate outlier, but also possible since it can be a very small man or a man with an eating disorder. 

#excluding negative houselhold income 
data_sample_2 <- data_sample_1 [!(data_sample_1$household_income < 0),]


#looking at the new data sample, checking if changes were successful 
view(data_sample_2)
summary(data_sample_2)	
describe(data_sample_2)

#visualizing
windows()
data_sample_2 %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram()	

windows()
data_sample_2 %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram()

windows()
data_sample_2 %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram()	

windows()
data_sample_2 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram()	

windows()
data_sample_2 %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram()

windows()
data_sample_2 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva) +	
  geom_histogram()


####Building models####

#Model 1: Age and Sex as predictors of pain
#Creating Model 1
mod_agesex <- lm(pain ~ age + sex, data = data_sample_2)	
mod_agesex

#Model 2:  age, sex, STAI, pain catastrophizing, mindfulness, and cortisol measures (model named after the first letters of the variables)
#Creating Model 2
mod_asspmc <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_2)
mod_asspmc

#looking at the models
summary(mod_agesex)
summary(mod_asspmc)

####testing the models####
#checking the mod_agesex
#checking for multivariate outliers by plotting and using cook's distance
#plotting
windows()
plot(pain ~ age, data=data_sample_2)

windows()
plot(pain ~ sex, data=data_sample_2)

#cook's distance
windows()
plot(mod_agesex, which = 4) 
#all datapoints that look like outliers have values under 1 in cook's distance and therefore are not outliers

#multivariate normally asumption
windows()
plot(mod_agesex, which=2)
#points fit roughly on straight line

#skew and curtosis
describe(residuals(mod_agesex))#everything between +2 or - 2 is okay, since the values are 0.2 for skew and 0.3 for kurtosis, it is okay

#linearity of residuals
residualPlots(mod_agesex)

#histogram
windows()
hist(residuals(mod_agesex), breaks = 20) #dirstributed normally I guess

#shapiro wilks #noemality of residuals
residual_agesex<-residuals(mod_agesex)
hist(residual_agesex)
shapiro.test(residual_agesex)#not significant so normally distributed, p=.6195

#ncv test for homogenity (tests heterogenity)
ncvTest(mod_agesex) #value: p=.54359
#not significant, so not heterogen so homogen

#multicolienarity
vif(mod_agesex)
#values above 3 are correlated too  much
#both values under 3


#testing mod_asspmc
#checking this model 
#checking for multivariate outliers
windows()
plot(pain ~ age, data=data_sample_2)

windows()
plot(pain ~ sex, data=data_sample_2)

windows()
plot(pain ~ STAI_trait, data=data_sample_2)

windows()
plot(pain ~ pain_cat, data=data_sample_2)

windows()
plot(pain ~ STAI_trait, data=data_sample_2)

windows()
plot(pain ~ cortisol_serum, data=data_sample_2)

windows()
plot(pain ~ cortisol_saliva, data=data_sample_2)

#cook's distance
windows()
plot(mod_asspmc, which = 4) 
#all dataponts that look like outliers have values under 1 in cook's distance and therefore are not outliers

#multivariate normally asumption
windows()
plot(mod_asspmc, which=2)
#fit roughly on straight line

#skew and curtosis
describe(residuals(mod_asspmc))#everything between +2 or - 2 is okay, since the values are -0.03 for skew and -0.33 for kurtosis, it is okay

#linearity
residualPlots(mod_asspmc) #not violated #p=0.450

#histogram
windows()
hist(residuals(mod_asspmc), breaks = 20) #dirstributed normally I guess

#shapiro wilks #noemality of residuals
residual_asspmc<-residuals(mod_asspmc)
hist(residual_asspmc)
shapiro.test(residual_asspmc)#not significant so normally distributed, p=.836

#ncv test for homogenity (tests heterogenity)
ncvTest(mod_asspmc) #value: .52663
#not significant, so not heterogen so homogen

#multicolienarity
vif(mod_asspmc)
#values above 3 are correlated too  much
#serum:4.83 and saliva: 5.22, one has to be removed, serum has better value and therefore is kept

#mod_asspmc1 with serum
mod_asspmc2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_2)
mod_asspmc2

#testing the model again
#testing mod_asspmc2

#checking for multivariate outliers
windows()
plot(pain ~ age, data=data_sample_2)

windows()
plot(pain ~ sex, data=data_sample_2)

windows()
plot(pain ~ STAI_trait, data=data_sample_2)

windows()
plot(pain ~ pain_cat, data=data_sample_2)

windows()
plot(pain ~ STAI_trait, data=data_sample_2)

windows()
plot(pain ~ cortisol_serum, data=data_sample_2)


#cook's distance
windows()
plot(mod_asspmc2, which = 4) 
#all dataponts that look like outliers have values under 1 in cook's distance and therefore are not outliers

#multivariate normally asumption
windows()
plot(mod_asspmc2, which=2)
#fit roughly on straight line

#skew and curtosis
describe(residuals(mod_asspmc2))#everything between +2 or - 2 is okay, since the values are -0.1 for skew and -0.07 for kurtosis, it is okay

#linearity
residualPlots(mod_asspmc2) #not violated

#histogram
windows()
hist(residuals(mod_asspmc2), breaks = 20) #dirstributed normally I guess

#shapiro wilks #noemality of residuals
residual_asspmc2<-residuals(mod_asspmc2)
hist(residual_asspmc2)
shapiro.test(residual_asspmc2)#not significant so normally distributed, p=.995

#ncv test for homogenity (tests heterogenity)
ncvTest(mod_asspmc2) #value: .909
#not significant, so not heterogen so homogen

#multicolienarity
vif(mod_asspmc2)

#Comparing model fit with AIC function for both models #the lower the better
#model 1:
AIC(mod_agesex)#value:573.1962
#model 2:
AIC(mod_asspmc2)#value:515.392
#Since the models have a difference in AIC thats bigger than 2, they differ significantly regarding their model fit. Since the second model has a lowe AIC, we choose this one.
#since they differ more than two, its significant, the lower number fits better, so the second model is significanty better than the first

#Since the models are nested (mod_asspmc contains all the variables that mod_agesex contains), an ANOVA is appropriate
#Conducting ANOVA
anova(mod_agesex,mod_asspmc2)#values: F:19.481, DF: 4, p<.001 #but this cant be taken right now because we have not changed the model yet
#F is significant, which means that the models significantly differ in terms of their residual errors

#decided for mod_asspmc2 as the better model

#report statistics of both models
summary(mod_agesex)
summary(mod_asspmc)


##adjusted R²:0.1275 (mod_agesex) ,0.4416 (mod_asspmc)

#show models
mod_agesex
mod_asspmc2

 







####Exercise 2##########################################################################################################

#creating the other researcher's model

mod_otherresearcher <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income , data = data_sample_2)

#re-run the data and model diagnostics, as there are new variables in the model
windows()
plot(pain ~ age, data=data_sample_2)

windows()
plot(pain ~ sex, data=data_sample_2)

windows()
plot(pain ~ STAI_trait, data=data_sample_2)

windows()
plot(pain ~ pain_cat, data=data_sample_2)

windows()
plot(pain ~ mindfulness, data=data_sample_2)

windows()
plot(pain ~ cortisol_serum, data=data_sample_2)

windows()
plot(pain ~ weight, data=data_sample_2)

windows()
plot(pain ~ IQ, data=data_sample_2)

windows()
plot(pain ~ household_income, data=data_sample_2)

#cook's distance
windows()
plot(mod_otherresearcher, which = 4) 
#all dataponts that look like outliers have values under 1 in cook's distance and therefore are not outliers 

#multivariate normally asumption
windows()
plot(mod_otherresearcher, which=2)
#all points fit roughly on a straight line

#skew and curtosis
describe(residuals(mod_otherresearcher))#everything between +2 or - 2 is okay, since the values are -0.14 for skew and -0.13 for kurtosis, it is okay

#linearity
residualPlots(mod_otherresearcher)

#histogram
windows()
hist(residuals(mod_otherresearcher), breaks = 20) #dirstributed normally I guess

#shapiro wilks #noemality of residuals
residual_otherresearcher<-residuals(mod_otherresearcher)
hist(residual_otherresearcher)
shapiro.test(residual_otherresearcher)#not significant so normally distributed, p=.7021

#heterogenity or homogenity
ncvTest(mod_otherresearcher) #p=.91

#multicolienarity
vif(mod_otherresearcher) #all under 3 so it is okay, no problematic amount of collinearity

#backwards model coefficient table
coef_table(mod_otherresearcher)


#run backward regression
mod_otherresearcher_back <- step(mod_otherresearcher, direction = "backward") #put all predictors into it and take away step by step until we have best model
#we use different p value: 0.1 so it is less strict than 0.05

#Run a new regression model now only using the predictors that were retained in the end of the backward regression, and save this model in a new R object. We will refer to this model as the “backward model”.
mod_otherresearcher_back #what stayed after backward regression
#creating "backward model"
mod_backward <- lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + weight, data = data_sample_2)
summary(mod_backward)
#model diagnostics for backward model
#outliers
windows()
plot(mod_backward, which = 4) 
#multivariate normally asumption
windows()
plot(mod_backward, which=2)
#points roughly on straight line

#skew and curtosis
describe(residuals(mod_backward))#everything between +2 or - 2 is okay, since the values are -0.15 for skew and -0.2 for kurtosis, it is okay

#histogram
windows()
hist(residuals(mod_backward), breaks = 20) #dirstributed normally I guess

#shapiro wilks #noemality of residuals
residual_backward<-residuals(mod_backward)
hist(residual_backward)
shapiro.test(residual_backward)#not significant so normally distributed, p=.9045

#heterogenity or homogenity
ncvTest(mod_backward) #p=.919

#multicolienarity
vif(mod_backward)#nothing over 3

#coeficient table
coef_table(mod_backward)

#compare model that went into backward regression to backward regression
AIC(mod_otherresearcher) #515.5384
AIC(mod_backward) #512.8281
#backward better

#saving model from the end of exercise 1 in new model
mod_theorybased <- mod_asspmc2 
#Compare the backward model and the theory-based model based on AIC (and using the anova() function if appropriate) only appropriate to run anova if they are nested
AIC(mod_backward) #value:512.8281
AIC(mod_theorybased) #value:515.392
#difference is bigger than 2, so the models differ significantly from eachother. There is a lower value in mod_backward so this one is the better one.
#looking at models to see if they are nested and then conclude if an anova is appropriate
#they are not nested, therefore an anova would not be appropriate

#After this, you decide to put the two models to the test on some new data

data_sample_new = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")

#checking for errors in general
summary(data_sample_new)
describe(data_sample_new)
str(data_sample_new)
view(data_sample_new)
#everything okay witht the dataset
#do predictions on pain using the regression models or equations of the backward model and the theory-based model which were “trained” on data file 1
#(IMPORTANT: do not fit the regression models on data file 2 (don’t re-train your models), just use the regression equations that you derived based on data file 1. These regression equations should be applied on the new data (data file 2), to predict pain.)

#Compare the predicted values with the actual pain ratings. Which model was able to predict the actual pain ratings in data file 2 better?

#checking the model on the new data saple (data_sample_new)
# calculate predicted values 	
pred_data_sample_new <- predict(mod_theorybased, data_sample_new)	
pred_data_sample_new_back <- predict(mod_backward, data_sample_new)
pred_data_sample_new
pred_data_sample_new_back

#calculating sum of squared residuals 	
RSS_test = sum((data_sample_new[,"pain"] - pred_data_sample_new)^2)	
RSS_test_back = sum((data_sample_new[,"pain"] - pred_data_sample_new_back)^2)	
RSS_test	#value 229.5327
RSS_test_back	#value 233.1865
#error is smalleer in the theorybased model

#coefficient tables
coef_table(mod_theorybased)
coef_table(mod_backward)



#####################################################################################################################
####Exercise 3#######################################################################################################
#####################################################################################################################

#also loaded the packages I already loaded before in case I open R and only want to run something from this exercise
require(psych)
require(tidyverse)
require(car)
require(gridExtra)
require(olsrr)
require(ggfortify)
require(lme4)
require(lm.beta) 
require(r2glmm) 
require(MuMIn) 
require(influence.ME) 
require(lattice)

#loading data sample 3
data_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv ")
#loading data sample 4
data_4 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv")

#eyeballing data
view(data_3)
view(data_4)

#running the descriptive statistics to get to know more about the data
str(data_3)
describe(data_3)
summary(data_3)

str(data_4)
describe(data_4)
summary(data_4)

#solving problems with inconsistency in data_3:
#changing the"Female" into "female" in data_3
data_3_new <- data_3%>%mutate(sex = droplevels(replace(sex, sex=="Female","female")))

#removing the values of the person with ID nr 195 from the data set because of the wrong score in mindfulness which is over 6 
data_3_new <- data_3_new[-c(195),]

#looking at the new data set data_3_new to see if the changes were successful
view(data_3_new)
str(data_3_new)
describe(data_3_new)
summary(data_3_new)

#in data set 4 there are negative household incomes. I still chose to keep them because this variable is not important for the analyses here and otherwise I would lose the other values too if I excluded the participants.

#creating a random intercept model (mod_rnd_int) based on data_3_new
mod_rnd_int = lmer(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1|hospital), data = data_3_new)

####running model diagnostics for mod_rnd_int####
#searching for potential outliers
#creating functions
influence_observation = influence(mod_rnd_int, obs = T)$alt.fixed
influence_group = influence(mod_rnd_int, group = "hospital")$alt.fixed

#creating plots
windows ()
data_plot_influence = as_tibble(influence_group) %>% gather(colnames(influence_group),value = coefficient, key = predictor)
data_plot_influence %>% ggplot() + aes(x = 1, y = coefficient, group = predictor) + geom_violin() + facet_wrap(~predictor, scales = "free")

#checking normality assumption
windows()
qqmath(mod_rnd_int, id = 0.05)
#the points roughly fit on a straight line so the normality assumption is fullfilled
windows()
qqmath(ranef(mod_rnd_int))

#checking linearity assumption
windows()
plot(mod_rnd_int, arg = "pearson")

windows()
data_3_new %>% ggplot() + aes(x = pain, y = residuals(mod_rnd_int)) +    
  geom_point()

windows()
data_3_new %>% ggplot() + aes(x = age, y = residuals(mod_rnd_int)) +    
  geom_point()

windows()
data_3_new %>% ggplot() + aes(x = sex, y = residuals(mod_rnd_int)) +    
  geom_point()

windows()
data_3_new %>% ggplot() + aes(x = STAI_trait, y = residuals(mod_rnd_int)) +    
  geom_point()

windows()
data_3_new %>% ggplot() + aes(x = pain_cat, y = residuals(mod_rnd_int)) +    
  geom_point()

windows()
data_3_new %>% ggplot() + aes(x = mindfulness, y = residuals(mod_rnd_int)) +    
  geom_point()

windows()
data_3_new %>% ggplot() + aes(x = cortisol_serum, y = residuals(mod_rnd_int)) +    
  geom_point()


#homescedasticity
windows()
plot(mod_rnd_int, arg = "pearson")
#if there is a recognisable funnel shape, then the assumption is violated. Because there is no funnel shape in this case it is fine.

#we have to check for cross clusters as well
homosced_mod = lm(residuals(mod_rnd_int)^2 ~ hospital, data = data_3_new)
summary(homosced_mod)
#p value not significant so it is fine #F(9,189)= 1.022, p=.424
#far away from significance so I decided not to check zyklon plot as well (this should be enough on its own)

#checking for muticolinerity
windows()
pairs.panels(data_3_new[, c("age", "sex", "STAI_trait", "pain_cat","mindfulness","cortisol_serum")], col = "red", lm = T)
#all correlations are low (under .8 which is the most typical cutoff(.5 is more conservative)) so there is no multicolinearity

####done with model diagnostics, looks like everything is good####

#calculating model coefficients and confidence intervalls of the coefficients on all fixed predictors
#model coefficients
summary(mod_rnd_int)
mod_rnd_int
#confidence intervalls
confint(mod_rnd_int)

#comparing them to the to the model from exercise 1 (mod_asspmc2)
summary(mod_asspmc2)
mod_asspmc2
confint(mod_asspmc2) #report in table


#variance explained by the fixed effect predictors using marginal R²
r2beta(mod_rnd_int, method = "nsj", data = data_3_new)

#variance explained by the fixed and random effect terms combined using conditional R² (in this function also marginal R² again)
r.squaredGLMM(mod_rnd_int)

#Now compute the variance explained by the model on data file 4. You can do this by using the formula we learned in class: 1-(RSS/TSS).
#data_4:
pred_4 <- predict(mod_rnd_int,data_4,allow.new.levels=T)#allowed new levels because of warning message "new levels detected in data"	
pred_4 #this line shows the results

#RSS
#RSS fromula from exercise 11 brought error and did not work properly (value bigger than TSS), therefore searched for alternative:
RSS = sum((data_4$pain - pred_4)^2)
RSS#value:323.557
#create mod mean for TSS function later
mod_mean <- lm(pain ~ 1, data = data_4)

#TSS (found in exercise 11)
TSS = sum((data_4$pain - predict(mod_mean))^2)
TSS#value:466.8226

#formula 1-RSS/TSS, danach vergleichen (Exercise 11 page 11)
var_explained <- 1-(323.557/466.8226)
var_explained#result: 0.3068952 ~ 0.31

#building new linear mixed model with only the strongest predictor
#chosing the one with the highest beta value
mod_rnd_int #cortisol serum has value of 0.46720, so this one is chosen to be included in the model
mod_most_influential = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_3_new)#allowing both random intercept and random slope
mod_most_influential

data_3_new = data_3_new %>% mutate(pred_4 = predict(mod_most_influential))

#plot with regression line for each hospital
windows()
int_plot = data_3_new %>% ggplot() + aes(y = cortisol_serum,
           x = pain, color = hospital) + geom_point(size = 4) + geom_smooth(method = "lm",se = F, fullrange = TRUE)
int_plot

#comparing model fits using cAIC
#does not work, says it does not find funtion cAIC
#tried instead:
CAICF(mod_rnd_int) #761.8305
CAICF(mod_most_influential) #736.1703 
#anova possible because models are nested
anova(mod_rnd_int, mod_most_influential)#significant
#mod_most_influential is better

