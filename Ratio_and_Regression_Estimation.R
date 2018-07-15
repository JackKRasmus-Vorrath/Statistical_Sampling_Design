#For T. Johnson's tutorial on Ratio, Domain, and Regression Estimation and Calibration:
#Cf. https://rpubs.com/trjohns/survey-ratioreg

#imports
library(survey)
library(DAAG)

#check the data frame
head(DAAG::medExpenses)
#The sample size is 33 (i.e., # of families)

#duplicate data frame
medExp <- DAAG::medExpenses


#TASK DESCRIPTION and DATA FRAME PREPARATION

#In this ratio estimation, the daily medical expenses per person is calculated using
  #total medical expenses to the total # of people in the community
  #R = tau_y / tau_x = sum(i:N)y_i / sum(i:N)x_i
  #where y_i is the daily medical expenses of a family, and x_i is the family size

#Population Size from which a sample of 33 is taken (for fpc)
medExp$N <- 600

#create ID variable for PSUs (i.e., each sampled family)
medExp$Family <- seq.int(nrow(medExp))

#reorder columns
medExp <- medExp[,c('Family','familysize','expenses','N')]

#check the data frame
head(medExp)



#ESTIMATING A RATIO

#Using SRS, the estimate of ratio is:
  #r = y_hat/x_hat
  #y_hat = (1/n) * sum(i:N)y_i
  #x_hat = (1/n) * sum(i:N)x_i

#The estimated variance is:
  #V_hat(r) = (1 - n/N) * (1/x_hat^2) * ((s_r)^2)/n
  #where ((s_r)^2) = (1/(n-1)) * sum(i:n)(y_i - r*x_i)^2


#instantiate sampling design object
medExp.srs <- svydesign(id = ~1, data = medExp, fpc = ~N)

#ratio estimation of daily medical expenses per person
svyratio(~expenses, ~familysize, design = medExp.srs)
#the estimated daily medical expenses per person is $2.15, with SE=$0.25



#RATIO ESTIMATOR OF TOTAL

#Using SRS, the ratio estimators for mu_y and tau_y are:
  #mu_y-hat = r*mu_x
  #tau_y-hat = r*tau_x
#where r = y_hat / x_hat


#Supposing 2700 total people in a community of N=600 households,
  #familysize can be used as an auxiliary variable, with:
  #tau_x = 2700
  #mu_x = 2700/600 = 4.5

#ratio estimator for tau_y
predict(svyratio(~expenses, ~familysize, design = medExp.srs), total=2700)
#the estimated total daily expenses of the community is $5811.15, with SE=$681.54


#compare with the total estimator N*y_hat, which does not use the auxiliary variable (familysize)
svytotal(~expenses, design = medExp.srs)
#Here, the estimated total daily expenses of the community is $4813.30, with SE=$288.67
  
#Note that the SE is smaller for this estimator:
  #the correlation between expenses and the auxiliary variable, familysize, is weak
  #a ratio estimator requires a strong correlation between the target and auxiliary variables

#visualization of the relationship between target and auxiliary variables
with(medExp, plot(familysize, expenses))
with(medExp, abline(0, mean(expenses)/mean(familysize)))

#R-squared calculation of the relationship between target and auxiliary variable (familysize)
ratio.lm <- lm(familysize ~ expenses, data = medExp)
summary(ratio.lm)$r.squared
#the R-squared value is 0.42


#A ratio estimator of mu_y can be calculated from the ratio estimator of tau_y
  #mu_y-hat = tau_y-hat / N

#For mu_y, divide by N the estimate and SE output by the ratio estimator of tau_y
tau_y <- predict(svyratio(~expenses, ~familysize, design = medExp.srs), total=2700)
tau_y$total[1,1] / 600
tau_y$se[1,1] / 600
#The estimated average daily medical expenses per household (mu_y-hat) is $9.69, with SE=$1.14



#CALIBRATION FOR REGRESSION ESTIMATORS

#calibrate() reweights the sample to incorporate info about the auxiliary variables
  #svymean() and svytotal() can be applied to calibrated samples

#built-in sample data
head(trees)

#duplicate data frame
trees_df <- trees


#The target variable is Total (Tau_v) and Mean (Mu_v) Volume
  #Girth and Height are auxiliary variables
  #Known totals for Girth and Height are T_g = 14000 and T_h = 77000


#Population Size from which the SRS of size n = 31 is taken (for fpc)
trees_df$N <- 1000

#instantiate sampling design object
trees_df.srs <- svydesign(id = ~1, data = trees_df, fpc = ~N)

#ratio estimation of total Volume using Girth as auxiliary variable
predict(svyratio(~Volume, ~Girth, design = trees_df.srs), total = 14000)
#The estimated total Volume is 31882.64 in^3 with SE=1811.89 in^3


#calibrate() can perform the same calculation as above
  #the intercept is dropped from the formula using '-1'
  #the (proportional) variance argument is used to specify the structure of y_i to x_i


#instantiate calibrated sample object
trees.cal <- calibrate(trees_df.srs, formula = ~Girth - 1, population = 14000, variance = 1)

#ratio estimation of total Volume (Tau_v) using the calibrated sample
svytotal(~Volume, design = trees.cal)

#ratio estimation of mean Volume (Mu_v) using the calibrated sample
svymean(~Volume, design = trees.cal)


#Weights (w_i) are calibrated to the known total of the auxiliary variable T_g
  #where T_g = sum(1:n)w_i*x_i

#checking calibrated calculation of the known value, total Girth (T_g) = 14000
sum(weights(trees.cal)*trees_df$Girth)

#The estimate of the target Volume (Tau_v) is calculated using these weights
  #where Tau_v = sum(i:n)w_i*y_i

#checking calibrated estimation of total Volume (Tau_v) = 31882.64
sum(weights(trees.cal)*trees_df$Volume)



#Regression estimators with intercepts can produce better results
  #Note the poor linear fit below:
with(trees_df, plot(Girth, Volume))
with(trees_df, abline(0, mean(Volume)/mean(Girth)))

#instantiate calibrated sample object using regression estimation, with intercept of N = 1000
  #slope and intercept are estimated assuming a constant variance
    #NB: unlike ratio estimators, no variance argument need be passed
  #N and T_g are passed to the population argument
trees.cal_int <- calibrate(trees_df.srs, formula = ~Girth, 
                           population = c(`(Intercept)` = 1000, Girth = 14000))

#regression estimator of total Volume, using intercept
svytotal(~Volume, trees.cal_int)

#Calibration satisfies the following weight constraints:
  #N = sum(i:n)w_i*x_i0 = sum(i:n)w_i
    #where x_i0 = 1 for all trees
  #T_g = sum(i:n)w_i*x_ig
    #where x_ig is the Girth of the ith tree

#checking calibrated calculation of the known value of population size, N = 1000
sum(weights(trees.cal_int))

#checking calibrated calculation of the known value T_g = 14000
sum(weights(trees.cal_int)*trees_df$Girth)



#Generalized regression estimators using multiple auxiliary variables may further reduce SE
trees.cal_gre <- calibrate(trees_df.srs, formula = ~Girth + Height,
                           population = c(`(Intercept)` = 1000, Girth = 14000, Height = 77000))

#The formula argument implies the following prediction model to calibrate weights b_g and b_h:
  #y_i = a + b_g*x_ig + b_h*x_ih
  #where a = N = 1000
  #X_ig and x_ih are the Girth and Height of the ith tree
  #The known values of T_g = 14000, and T_h = 77000 are passed to the population argument

#generalized regression estimator of total Volume, using auxiliary variables Girth and Height
svytotal(~Volume, trees.cal_gre)






