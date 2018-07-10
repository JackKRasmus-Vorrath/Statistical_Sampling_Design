#For T. Johnson's tutorial on Ratio, Domain, and Regression Estimation and Calibration:
#Cf. https://rpubs.com/trjohns/survey-ratioreg

library(survey)

#create dataframe
df <- setNames(data.frame(matrix(ncol = 4, nrow = 12)), c('ID', 'Area', 'Num_Grouse', 'Pop_Total_Bush '))

#df of length 12, specifying Area in hectares per sampled bush,
#   the Number of Grouse in each sampled bush,
#   and the population total number of bushes
df$ID <- 1:12
df$Area <- c(8.9, 2.7, 6.6, 20.6, 3.7, 4.1, 25.8, 1.8, 20.1, 14.0, 10.1, 8.0)
df$Num_Grouse <- c(24, 3, 10, 36, 8, 8, 60, 5, 35, 34, 18, 22)
df$Pop_Total_Bush <- 248



#simple inflation estimator (using formula t_y = N*y-hat)
simp_inf_est <- 248*mean(df$Num_Grouse)
simp_inf_est

#ratio estimator (using formula t_yr = t_x * (y-hat / x-hat))
rat_est <- 3015*(mean(df$Num_Grouse)/mean(df$Area))
rat_est



#survey design with finite population correction, specifying N,
#   i.e., the population total number of bushes from which the sample n=12 was drawn
bushes.srs <- svydesign(id = ~1, data = df, fpc = ~Pop_Total_Bush)


#Mean number of grouse per hectare
svyratio(~Num_Grouse, ~Area, design = bushes.srs)



#estimated total number of grouse (without using auxiliary variable for size, i.e., total hectares)
svytotal(~Num_Grouse, design = bushes.srs)

#estimated total number of grouse (using auxiliary variable for size, i.e., total hectares)
predict(svyratio(~Num_Grouse, ~Area, design = bushes.srs), total = 3015)



#NB: a strong correlation between the target (Num_Grouse) and the auxiliary variable (Area)
#   results in lower SE when using the ratio estimator!
with(df, plot(Area, Num_Grouse))
with(df, abline(0, mean(Num_Grouse)/mean(Area)))



# convert design to one using Jack-Knife estimator of variance
#NB: must specify type 'JK1' for unstratified designs!
bushes.jknf <- as.svrepdesign(bushes.srs, type="JK1")

#re-estimate total number of grouse (using auxiliary variable for size, i.e., total hectares)
predict(svyratio(~Num_Grouse, ~Area, design = bushes.jknf), total = 3015)
