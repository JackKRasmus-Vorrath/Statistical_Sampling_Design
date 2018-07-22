setwd('C:/Users/jkras/Desktop')

library(readxl)
library(stratification)
library(sampling)
library(PracTools)
library(survey)
library(gridExtra)
library(ggplot2)
library(Hmisc)

#read individual sheet to dataframe object
df <- readxl::read_excel('projectData.xlsx',sheet='projectData')
head(df)

#Summary Statistics########################################################

summary(df)
nrow(df)
sum(df$inventory)
mean(df$inventory)
sd(df$inventory)

Hmisc::describe(df)

#Histogram of inventory, excluding values above 100K
ggplot(df,aes(x=inventory)) + geom_histogram(binwidth=1000,aes(fill=..count..)) + 
  xlab('Inventory Value') + ylab('Count') + ggtitle('Counts of Inventory Value: Excluding Values > 100K') +
  xlim(c(0, 100000)) + ylim(c(0,450))

#Histogram of inventory, excluding values above 1M
ggplot(df,aes(x=inventory)) + geom_histogram(binwidth=1000,fill='blue',alpha=0.6) + 
  xlab('Inventory Value') + ylab('Count') + ggtitle('Counts of Inventory Value: Excluding Values > 1M') +
  xlim(c(0, 1000000)) + ylim(c(0,450))




################################################################################################################################

#Using highest valued Inventory records as the certainty stratum
#   CF. https://enablement.acl.com/helpdocs/analytics/13/user-guide/en-us/Content/da_sampling_data/classical_variables_sampling.htm#navlink-5

cert_vec <- list(top_certainty_vec <- c(which(df$inventory==max(df$inventory))),
                 top5_certainty_vec <- df[order(df$inventory, decreasing=T)[1:5],]$coID,
                 top10_certainty_vec <- df[order(df$inventory, decreasing=T)[1:10],]$coID,
                 top20_certainty_vec <- df[order(df$inventory, decreasing=T)[1:20],]$coID,
                 top50_certainty_vec <- df[order(df$inventory, decreasing=T)[1:50],]$coID,
                 top80_certainty_vec <- df[order(df$inventory, decreasing=T)[1:80],]$coID,
                 top90_certainty_vec <- df[order(df$inventory, decreasing=T)[1:90],]$coID,
                 top100_certainty_vec <- df[order(df$inventory, decreasing=T)[1:100],]$coID,
                 top110_certainty_vec <- df[order(df$inventory, decreasing=T)[1:110],]$coID,
                 top120_certainty_vec <- df[order(df$inventory, decreasing=T)[1:120],]$coID,
                 top150_certainty_vec <- df[order(df$inventory, decreasing=T)[1:150],]$coID,
                 top200_certainty_vec <- df[order(df$inventory, decreasing=T)[1:200],]$coID)

#NB: Must define cert_vec in global environment before passing to minCV() function below. For common scoping issues:
#    CF. https://stackoverflow.com/questions/28601959/error-object-not-found-in-nested-functions



#Generalized Lavallee-Hidiroglou Method -- applying Kozak's (2004) Algorithm###################################################
#   CF. https://www.rdocumentation.org/packages/stratification/versions/2.2-5/topics/strata.LH



#Function Optimizing the number of sampled strata and the best performing certainty stratum from the vector list above#########

minCV <- function(lo_Ls, hi_Ls, cert_vec){
  CV.list<-numeric()
  min.CV <- 999
  min.i <- 99
  min.j <- 99
  
  for (i in lo_Ls:hi_Ls){
    for (j in 1:length(cert_vec)){
      k <- tryCatch(stratification::strata.LH(x=df$`inventory`, n=500, Ls=i, alloc=c(0.5,0,0.5), 
                              certain=cert_vec[j], model="none", algo="Kozak")$RRMSE,
                    warning=function(w){
                      message("Warning: LH Algo failed. Some sampled strata < mininum Nh") 
                      NA})
      if(is.na(k)){
        next
      }
      cat('Coefficient of Variation (RRMSE): ', k, '\n')
      cat('Selected Number of Strata: ', i, '\n')
      cat('Index of Certainty Stratum: ', j, '\n')
      
      CV.list <- append(CV.list, k)
      cat('All Coefficients of Variation: \n', CV.list, '\n')
      
      min_val <- min(CV.list, na.rm=TRUE)
      if (min.CV > min_val){
        min.CV <- min_val
        min.i <- i
        min.j <- j
      }
    }
  }
  result_list <- list('Minimum_CV'=min.CV, 'Best_LS'=min.i, 'Best_CertStratum'=min.j)
  return(result_list)
}


#Implement Stratification Optimization########################################################################################

best_results <- minCV(5, 10, cert_vec)
best_results$Minimum_CV
best_results$Best_LS
best_results$Best_CertStratum


#Best Stratification Configuration: Lavallee-Hidiroglou (1998) Method and Kozak's (2004) Algorithm############################

Optim_Config <- stratification::strata.LH(x=df$`inventory`, n=500, Ls=10, alloc=c(0.5,0,0.5), 
                             certain=cert_vec[9], model='none', algo='Kozak')

print(Optim_Config)
plot(Optim_Config)


#Performance Comparison: Gunning and Horgan (2004) Geometric Stratification Method

#############################################################################################################################

Optim_Geo <- stratification::strata.geo(x=df$`inventory`, n=500, Ls = 8, alloc = c(0.5, 0, 0.5), 
                        certain=cert_vec[9], model="none")

print(Optim_Geo)
plot(Optim_Geo)




#Apply Optimum Stratification to Sampling and Estimation######################################################################

#Identify boundary separating highest-valued certainty stratum from other strata
options(digits=15)
min(df[order(df$inventory, decreasing=T)[1:110],]$inventory)
#returns 1372514.035

#Assign Strata
df$Stratum <- NA
df$Stratum[df$`inventory` < 36703.82] <- 1
df$Stratum[(df$`inventory` < 71655.62) & (df$`inventory` >= 36703.82)] <- 2
df$Stratum[(df$`inventory` < 106848.40) & (df$`inventory` >= 71655.62)] <- 3
df$Stratum[(df$`inventory` < 151104.80) & (df$`inventory` >= 106848.40)] <- 4
df$Stratum[(df$`inventory` < 209954.79) & (df$`inventory` >= 151104.80)] <- 5
df$Stratum[(df$`inventory` < 295186.09) & (df$`inventory` >= 209954.79)] <- 6
df$Stratum[(df$`inventory` < 413551.11) & (df$`inventory` >= 295186.09)] <- 7
df$Stratum[(df$`inventory` < 614205.40) & (df$`inventory` >= 413551.11)] <- 8
df$Stratum[(df$`inventory` < 974662.93) & (df$`inventory` >= 614205.40)] <- 9
df$Stratum[(df$`inventory` < 1372514.04) & (df$`inventory` >= 974662.93)] <- 10
df$Stratum[(df$`inventory` < 105379553.91) & (df$`inventory` >= 1372514.04)] <- 11

#Check if any NAs in Stratum column
any(is.na(df$Stratum))

#Assign column for Finite Population Correction
df$Num <- NA
df$Num[df$Stratum == 1] <- 4346
df$Num[df$Stratum == 2] <- 1047
df$Num[df$Stratum == 3] <- 1341
df$Num[df$Stratum == 4] <- 851
df$Num[df$Stratum == 5] <- 723
df$Num[df$Stratum == 6] <- 424
df$Num[df$Stratum == 7] <- 361
df$Num[df$Stratum == 8] <- 248
df$Num[df$Stratum == 9] <- 176
df$Num[df$Stratum == 10] <- 135
df$Num[df$Stratum == 11] <- 110

#Check if any NAs in df
any(is.na(df))

#Specify vector of population stratum sizes, appending certainty stratum
Nh <- append(Optim_Config$Nh, 110)

#Specify vector of population stratum standard deviations, appending 1 for certainty stratum
Sh <- append(sqrt(Optim_Config$varh), 1)

#Neyman Allocation to determine strata sizes in sample of size n = 500
PracTools::strAlloc(n.tot=500, Nh=Nh, Sh=Sh, alloc='neyman')
#returns sample stratum sizes of (100, 38, 43, 39, 41, 39, 43, 50, 60, 47, 0)

#Run the allocation again for a sample of size 499, reserving 1 unit for the certainty stratum
Nh <- Optim_Config$Nh
Sh <- sqrt(Optim_Config$varh)
PracTools::strAlloc(n.tot=499, Nh=Nh, Sh=Sh, alloc='neyman')
#returns sample stratum sizes of (101, 37, 43, 39, 41, 38, 43, 50, 60, 47)

#adjusted Neyman Allocation, including certainty stratum
Ney_Alloc <- c(101, 37, 43, 39, 41, 38, 43, 50, 60, 47, 1)





#PASS 1 OF STRATIFIED SAMPLING ESTIMATION of MEAN and TOTAL
#############################################################################################################################

#Set Random Sampling Seed
set.seed(222)

#Sampling using Neyman Allocation
Ney_Samp_1 <- sampling::strata(df, stratanames='Stratum', size=Ney_Alloc, method="srswor")

#Drop duplicated column
Ney_Samp_1 <- Ney_Samp_1[, !duplicated(colnames(Ney_Samp_1))]

#Rename ID column to match original data frame, for merging
colnames(Ney_Samp_1)[2] <- "coID"
Ney_Samp_1_merged <- merge(x = df, y = Ney_Samp_1)

#sort by Stratum
Ney_Samp_1_merged <- Ney_Samp_1_merged[order(Ney_Samp_1_merged$Stratum),]

#Neyman stratified design object
mydesign_1 <- survey::svydesign(id = ~1, strata = ~Stratum, data = Ney_Samp_1_merged, fpc = ~Num)

#Adjust for single-PSU certainty stratum
options(survey.lonely.psu = "certainty")

#Neyman stratified mean estimate and SE
survey::svymean(~`inventory`, design = mydesign_1)
#Returns:                 mean         SE
#         inventory 169756.4553944  3452.97697

#Confidence interval of the mean estimate
confint(survey::svymean(~`inventory`, design = mydesign_1))
#Returns:                 2.5 %        97.5 %
#         inventory 162988.744885283  176524.165903571

#DOES NOT CONTAIN TRUE VALUE of 179774.106017914################

#Neyman stratified estimate of total and SE
survey::svytotal(~`inventory`, design = mydesign_1)
#Returns:                total         SE
#         inventory  1657162517.56  33707961.22347

#Confidence interval of the estimate of total
confint(survey::svytotal(~`inventory`, design = mydesign_1))
#Returns:                2.5 %           97.5 %
#         inventory  1591096127.57013  1723228907.55066

#DOES NOT CONTAIN TRUE VALUE of 1754954822.94687#########




#PASS 2 OF STRATIFIED SAMPLING ESTIMATION of MEAN and TOTAL
#############################################################################################################################

#Set Random Sampling Seed
set.seed(223)

#Sampling using Neyman Allocation
Ney_Samp_2 <- sampling::strata(df, stratanames='Stratum', size=Ney_Alloc, method="srswor")

#Drop duplicated column
Ney_Samp_2 <- Ney_Samp_2[, !duplicated(colnames(Ney_Samp_2))]

#Rename ID column to match original data frame, for merging
colnames(Ney_Samp_2)[2] <- "coID"
Ney_Samp_2_merged <- merge(x = df, y = Ney_Samp_2)

#sort by Stratum
Ney_Samp_2_merged <- Ney_Samp_2_merged[order(Ney_Samp_2_merged$Stratum),]

#Neyman stratified design object
mydesign_2 <- survey::svydesign(id = ~1, strata = ~Stratum, data = Ney_Samp_2_merged, fpc = ~Num)

#Adjust for single-PSU certainty stratum
options(survey.lonely.psu = "certainty")

#Neyman stratified mean estimate and SE
survey::svymean(~`inventory`, design = mydesign_2)
#Returns:                 mean         SE
#         inventory  171029.233252   3328.92537

#Confidence interval of the mean estimate
confint(survey::svymean(~`inventory`, design = mydesign_2))
#Returns:                2.5 %           97.5 %
#         inventory  164504.659423391  177553.807080663

#DOES NOT CONTAIN TRUE VALUE of 179774.106017914################

#Neyman stratified estimate of total and SE
survey::svytotal(~`inventory`, design = mydesign_2)
#Returns:                total          SE
#         inventory  1669587375.006  32496969.44308

#Confidence interval of the estimate of total
confint(survey::svytotal(~`inventory`, design = mydesign_2))
#Returns:               2.5 %           97.5 %
#         inventory  1605894485.29115  1733280264.72143

#DOES NOT CONTAIN TRUE VALUE of 1754954822.94687################




#PASS 3 OF STRATIFIED SAMPLING ESTIMATION of MEAN and TOTAL
#############################################################################################################################

#Set Random Sampling Seed
set.seed(224)

#Sampling using Neyman Allocation
Ney_Samp_3 <- sampling::strata(df, stratanames='Stratum', size=Ney_Alloc, method="srswor")

#Drop duplicated column
Ney_Samp_3 <- Ney_Samp_3[, !duplicated(colnames(Ney_Samp_3))]

#Rename ID column to match original data frame, for merging
colnames(Ney_Samp_3)[2] <- "coID"
Ney_Samp_3_merged <- merge(x = df, y = Ney_Samp_3)

#sort by Stratum
Ney_Samp_3_merged <- Ney_Samp_3_merged[order(Ney_Samp_3_merged$Stratum),]

#Neyman stratified design object
mydesign_3 <- survey::svydesign(id = ~1, strata = ~Stratum, data = Ney_Samp_3_merged, fpc = ~Num)

#Adjust for single-PSU certainty stratum
options(survey.lonely.psu = "certainty")

#Neyman stratified mean estimate and SE
survey::svymean(~`inventory`, design = mydesign_3)
#Returns:               mean         SE
#         inventory  170941.7207444  3432.95716

#Confidence interval of the mean estimate
confint(survey::svymean(~`inventory`, design = mydesign_3))
#Returns:               2.5 %          97.5 %
#         inventory  164213.248356339  177670.193132409

#DOES NOT CONTAIN TRUE VALUE of 179774.106017914################

#Neyman stratified estimate of total and SE
survey::svytotal(~`inventory`, design = mydesign_3)
#Returns:               total             SE
#         inventory  1668733077.907  33512527.76587

#Confidence interval of the estimate of total
confint(survey::svytotal(~`inventory`, design = mydesign_3))
#Returns:               2.5 %           97.5 %
#         inventory  1603049730.45458  1734416425.35857

#DOES NOT CONTAIN TRUE VALUE of 1754954822.94687################




#PASS 4 OF STRATIFIED SAMPLING ESTIMATION of MEAN and TOTAL
#############################################################################################################################

#Set Random Sampling Seed
set.seed(225)

#Sampling using Neyman Allocation
Ney_Samp_4 <- sampling::strata(df, stratanames='Stratum', size=Ney_Alloc, method="srswor")

#Drop duplicated column
Ney_Samp_4 <- Ney_Samp_4[, !duplicated(colnames(Ney_Samp_4))]

#Rename ID column to match original data frame, for merging
colnames(Ney_Samp_4)[2] <- "coID"
Ney_Samp_4_merged <- merge(x = df, y = Ney_Samp_4)

#sort by Stratum
Ney_Samp_4_merged <- Ney_Samp_4_merged[order(Ney_Samp_4_merged$Stratum),]

#Neyman stratified design object
mydesign_4 <- survey::svydesign(id = ~1, strata = ~Stratum, data = Ney_Samp_4_merged, fpc = ~Num)

#Adjust for single-PSU certainty stratum
options(survey.lonely.psu = "certainty")

#Neyman stratified mean estimate and SE
survey::svymean(~`inventory`, design = mydesign_4)
#Returns:                mean         SE
#         inventory  180661.3660204  4638.56949

#Confidence interval of the mean estimate
confint(survey::svymean(~`inventory`, design = mydesign_4))
#Returns:                2.5 %            97.5 %
#         inventory  171569.936881626  189752.795159095

#CONTAINS TRUE VALUE of 179774.106017914################

#Neyman stratified estimate of total and SE
survey::svytotal(~`inventory`, design = mydesign_4)
#Returns:                total             SE
#         inventory  1763616255.091  45281715.35415

#Confidence interval of the estimate of total
confint(survey::svytotal(~`inventory`, design = mydesign_4))
#Returns:                2.5 %             97.5 %
#         inventory  1674865723.83843  1852366786.34309

#CONTAINS TRUE VALUE of 1754954822.94687################




#PASS 5 OF STRATIFIED SAMPLING ESTIMATION of MEAN and TOTAL
#############################################################################################################################

#Set Random Sampling Seed
set.seed(226)

#Sampling using Neyman Allocation
Ney_Samp_5 <- sampling::strata(df, stratanames='Stratum', size=Ney_Alloc, method="srswor")

#Drop duplicated column
Ney_Samp_5 <- Ney_Samp_5[, !duplicated(colnames(Ney_Samp_5))]

#Rename ID column to match original data frame, for merging
colnames(Ney_Samp_5)[2] <- "coID"
Ney_Samp_5_merged <- merge(x = df, y = Ney_Samp_5)

#sort by Stratum
Ney_Samp_5_merged <- Ney_Samp_5_merged[order(Ney_Samp_5_merged$Stratum),]

#Neyman stratified design object
mydesign_5 <- survey::svydesign(id = ~1, strata = ~Stratum, data = Ney_Samp_5_merged, fpc = ~Num)

#Adjust for single-PSU certainty stratum
options(survey.lonely.psu = "certainty")

#Neyman stratified mean estimate and SE
survey::svymean(~`inventory`, design = mydesign_5)
#Returns:               mean             SE
#         inventory  188972.1127207  4634.39056

#Confidence interval of the mean estimate
confint(survey::svymean(~`inventory`, design = mydesign_5))
#Returns:               2.5 %              97.5 %
# q       inventory  179888.874126264  198055.351315211

#DOES NOT CONTAIN TRUE VALUE of 179774.106017914################

#Neyman stratified estimate of total and SE
survey::svytotal(~`inventory`, design = mydesign_5)
#Returns:                total            SE
#         inventory  1844745764.38   45240920.6795

#Confidence interval of the estimate of total
confint(survey::svytotal(~`inventory`, design = mydesign_5))
#Returns:                  2.5 %           97.5 %
#         inventory  1756075189.22059  1933416339.53909

#DOES NOT CONTAIN TRUE VALUE of 1754954822.94687#########



#PASS 6 OF STRATIFIED SAMPLING ESTIMATION of MEAN and TOTAL -- NO LONELY PSU CERTAINTY STRATUM ADJUSTMENT
############################################################################################################################

#Run the allocation again for a sample of size 498, reserving 2 units for the certainty stratum
Nh <- Optim_Config$Nh
Sh <- sqrt(Optim_Config$varh)
PracTools::strAlloc(n.tot=498, Nh=Nh, Sh=Sh, alloc='neyman')
#returns sample stratum sizes of (101, 37, 43, 39, 41, 38, 42, 50, 60, 47)

#adjusted Neyman Allocation, including certainty stratum
Ney_Alloc_2 <- c(101, 37, 43, 39, 41, 38, 43, 50, 60, 47, 2)



#Set Random Sampling Seed
set.seed(227)

#Sampling using Neyman Allocation
Ney_Samp_6 <- sampling::strata(df, stratanames='Stratum', size=Ney_Alloc_2, method="srswor")

#Drop duplicated column
Ney_Samp_6 <- Ney_Samp_6[, !duplicated(colnames(Ney_Samp_6))]

#Rename ID column to match original data frame, for merging
colnames(Ney_Samp_6)[2] <- "coID"
Ney_Samp_6_merged <- merge(x = df, y = Ney_Samp_6)

#sort by Stratum
Ney_Samp_6_merged <- Ney_Samp_6_merged[order(Ney_Samp_6_merged$Stratum),]

#Neyman stratified design object
mydesign_6 <- survey::svydesign(id = ~1, strata = ~Stratum, data = Ney_Samp_6_merged, fpc = ~Num)

#Neyman stratified mean estimate and SE
survey::svymean(~`inventory`, design = mydesign_6)
#Returns:                mean           SE
#         inventory  185135.8284971  4675.96438

#Confidence interval of the mean estimate
confint(survey::svymean(~`inventory`, design = mydesign_6))
#Returns:                  2.5 %           97.5 %
#         inventory  175971.106721974  194300.550272168

#CONTAINS TRUE VALUE of 179774.106017914################

#Neyman stratified estimate of total and SE
survey::svytotal(~`inventory`, design = mydesign_6)
#Returns:                  total             SE
#         inventory   1807295957.788   45646764.26414

#Confidence interval of the estimate of total
confint(survey::svytotal(~`inventory`, design = mydesign_6))
#Returns:                  2.5 %              97.5 %
#         inventory   1717829943.81991   1896761971.7569

#CONTAINS TRUE VALUE of 1754954822.94687#################




#PASS 7 OF STRATIFIED SAMPLING ESTIMATION of MEAN and TOTAL -- NO LONELY PSU CERTAINTY STRATUM ADJUSTMENT
############################################################################################################################


#Set Random Sampling Seed
set.seed(228)

#Sampling using Neyman Allocation
Ney_Samp_7 <- sampling::strata(df, stratanames='Stratum', size=Ney_Alloc_2, method="srswor")

#Drop duplicated column
Ney_Samp_7 <- Ney_Samp_7[, !duplicated(colnames(Ney_Samp_7))]

#Rename ID column to match original data frame, for merging
colnames(Ney_Samp_7)[2] <- "coID"
Ney_Samp_7_merged <- merge(x = df, y = Ney_Samp_7)

#sort by Stratum
Ney_Samp_7_merged <- Ney_Samp_7_merged[order(Ney_Samp_7_merged$Stratum),]

#Neyman stratified design object
mydesign_7 <- survey::svydesign(id = ~1, strata = ~Stratum, data = Ney_Samp_7_merged, fpc = ~Num)

#Neyman stratified mean estimate and SE
survey::svymean(~`inventory`, design = mydesign_7)
#Returns:                  mean              SE
#         inventory   179848.508501     4643.21721

#Confidence interval of the mean estimate
confint(survey::svymean(~`inventory`, design = mydesign_7))
#Returns:                  2.5 %              97.5 %
#         inventory   170747.969987538   188949.047014456

#CONTAINS TRUE VALUE of 179774.106017914################

#Neyman stratified estimate of total and SE
survey::svytotal(~`inventory`, design = mydesign_7)
#Returns:                 total             SE
#         inventory   1755681139.987   45327086.45115

#Confidence interval of the estimate of total
confint(survey::svytotal(~`inventory`, design = mydesign_7))
#Returns:                   2.5 %             97.5 %
#         inventory   1666841683.01835   1844520596.95512

#CONTAINS TRUE VALUE of 1754954822.94687#################




#PASS 8 OF STRATIFIED SAMPLING ESTIMATION of MEAN and TOTAL -- NO LONELY PSU CERTAINTY STRATUM ADJUSTMENT
############################################################################################################################


#Set Random Sampling Seed
set.seed(229)

#Sampling using Neyman Allocation
Ney_Samp_8 <- sampling::strata(df, stratanames='Stratum', size=Ney_Alloc_2, method="srswor")

#Drop duplicated column
Ney_Samp_8 <- Ney_Samp_8[, !duplicated(colnames(Ney_Samp_8))]

#Rename ID column to match original data frame, for merging
colnames(Ney_Samp_8)[2] <- "coID"
Ney_Samp_8_merged <- merge(x = df, y = Ney_Samp_8)

#sort by Stratum
Ney_Samp_8_merged <- Ney_Samp_8_merged[order(Ney_Samp_8_merged$Stratum),]

#Neyman stratified design object
mydesign_8 <- survey::svydesign(id = ~1, strata = ~Stratum, data = Ney_Samp_8_merged, fpc = ~Num)

#Neyman stratified mean estimate and SE
survey::svymean(~`inventory`, design = mydesign_8)
#Returns:                   mean         SE
#         inventory   184075.5593905  4733.82151

#Confidence interval of the mean estimate
confint(survey::svymean(~`inventory`, design = mydesign_8))
#Returns:                  2.5 %              97.5 %
#         inventory   174797.439716593   193353.679064313

#CONTAINS TRUE VALUE of 179774.106017914################

#Neyman stratified estimate of total and SE
survey::svytotal(~`inventory`, design = mydesign_8)
#Returns:                 total             SE
#         inventory   1796945610.77    46211565.60562

#Confidence interval of the estimate of total
confint(survey::svytotal(~`inventory`, design = mydesign_8))
#Returns:                 2.5 %           97.5 %
#         inventory  1706372606.51338  1887518615.02582

#CONTAINS TRUE VALUE of 1754954822.94687#################




#PASS 9 OF STRATIFIED SAMPLING ESTIMATION of MEAN and TOTAL -- NO LONELY PSU CERTAINTY STRATUM ADJUSTMENT
############################################################################################################################


#Set Random Sampling Seed
set.seed(230)

#Sampling using Neyman Allocation
Ney_Samp_9 <- sampling::strata(df, stratanames='Stratum', size=Ney_Alloc_2, method="srswor")

#Drop duplicated column
Ney_Samp_9 <- Ney_Samp_9[, !duplicated(colnames(Ney_Samp_9))]

#Rename ID column to match original data frame, for merging
colnames(Ney_Samp_9)[2] <- "coID"
Ney_Samp_9_merged <- merge(x = df, y = Ney_Samp_9)

#sort by Stratum
Ney_Samp_9_merged <- Ney_Samp_9_merged[order(Ney_Samp_9_merged$Stratum),]

#Neyman stratified design object
mydesign_9 <- survey::svydesign(id = ~1, strata = ~Stratum, data = Ney_Samp_9_merged, fpc = ~Num)

#Neyman stratified mean estimate and SE
survey::svymean(~`inventory`, design = mydesign_9)
#Returns:                  mean          SE
#         inventory   182583.5832636  5040.97741

#Confidence interval of the mean estimate
confint(survey::svymean(~`inventory`, design = mydesign_9))
#Returns:                  2.5 %             97.5 %
#         inventory  172703.449097735   192463.717429423

#CONTAINS TRUE VALUE of 179774.106017914################

#Neyman stratified estimate of total and SE
survey::svytotal(~`inventory`, design = mydesign_9)
#Returns:                 total             SE
#         inventory  1782380939.819   49210021.45333

#Confidence interval of the estimate of total
confint(survey::svytotal(~`inventory`, design = mydesign_9))
#Returns:                 2.5 %             97.5 %
#         inventory  1685931070.09209   1878830809.54603

#CONTAINS TRUE VALUE of 1754954822.94687#################




#PASS 10 OF STRATIFIED SAMPLING ESTIMATION of MEAN and TOTAL -- NO LONELY PSU CERTAINTY STRATUM ADJUSTMENT
############################################################################################################################


#Set Random Sampling Seed
set.seed(231)

#Sampling using Neyman Allocation
Ney_Samp_10 <- sampling::strata(df, stratanames='Stratum', size=Ney_Alloc_2, method="srswor")

#Drop duplicated column
Ney_Samp_10 <- Ney_Samp_10[, !duplicated(colnames(Ney_Samp_10))]

#Rename ID column to match original data frame, for merging
colnames(Ney_Samp_10)[2] <- "coID"
Ney_Samp_10_merged <- merge(x = df, y = Ney_Samp_10)

#sort by Stratum
Ney_Samp_10_merged <- Ney_Samp_10_merged[order(Ney_Samp_10_merged$Stratum),]

#Neyman stratified design object
mydesign_10 <- survey::svydesign(id = ~1, strata = ~Stratum, data = Ney_Samp_10_merged, fpc = ~Num)

#Neyman stratified mean estimate and SE
survey::svymean(~`inventory`, design = mydesign_10)
#Returns:                 mean           SE
#         inventory  170758.0515275  3455.03629

#Confidence interval of the mean estimate
confint(survey::svymean(~`inventory`, design = mydesign_10))
#Returns:                  2.5 %             97.5 %
#         inventory  163986.304831325   177529.798223672

#DOES NOT CONTAIN TRUE VALUE of 179774.106017914###########

#Neyman stratified estimate of total and SE
survey::svytotal(~`inventory`, design = mydesign_10)
#Returns:                  total            SE
#         inventory   1666940099.011   33728064.2754

#Confidence interval of the estimate of total
confint(survey::svytotal(~`inventory`, design = mydesign_10))
#Returns:                  2.5 %            97.5 %
#         inventory   1600834307.7634   1733045890.25948

#DOES NOT CONTAIN TRUE VALUE of 1754954822.94687############



#############################################################################################################################

#PERFORMANCE COMPARISON with UNSTRATIFIED SIMPLE RANDOM SAMPLING
#############################################################################################################################

#Set Random Sampling Seed
set.seed(1)

#SRS of Size 500
srs_df <- df[sample(nrow(df), 500), ]

#SRS design object
mydesign_srs <- survey::svydesign(id = ~1, data = srs_df)

#SRS mean estimate and SE
survey::svymean(~`inventory`, design = mydesign_srs)
#Returns:                     mean           SE
#         inventory   314438.5186579    203292.38226

#confidence interval of SRS mean estimate
confint(survey::svymean(~`inventory`, design = mydesign_srs))
#Returns:                     2.5 %         97.5 %
#         inventory  -84007.2288961298  712884.266211934

#HUGE UNINFORMATIVE CONFIDENCE INTERVAL CONTAINS TRUE VALUE of 179774.106017914###

#SRS estimate of total and SE
survey::svytotal(~`inventory`, design = mydesign_srs)
#Returns:                   total             SE
#         inventory  157219259.329      101646191.1282

#Confidence interval of SRS estimate of total
confint(survey::svytotal(~`inventory`, design = mydesign_srs))
#Returns:                   2.5 %           97.5 %
#         inventory  -42003614.4480649   356442133.105967

#HUGE UNINFORMATIVE CONFIDENCE INTERVAL STILL DOES NOT CONTAIN TRUE VALUE of 1754954822.94687###





#VISUALIZE TABLE OF RESULTS########################################################################

val_df <- setNames(data.frame(matrix(ncol = 8, nrow = 11)), 
                   c('Mean_Estimate','Mean_SE','Mean_95%_CI','Includes_True_Mean','Total_Estimate','Total_SE','Total_95%_CI','Includes_True_Total'))

rownames(val_df) <- c(paste('Stratified_Test',1:5), paste('Stratified_No_Lonely_PSU',1:5), 'SRS')

val_df[,1] <- c(169756.46, 171029.23, 170941.72, 180661.37, 188972.11, 
                185135.83, 179848.51, 184075.56, 182583.58, 170758.05,
                314438.52)

val_df[,2] <- c(3452.98, 3328.93, 3432.96, 4638.57, 4634.39, 
                4675.96, 4643.22, 4733.82, 5040.98, 3455.04,
                203292.38)

val_df[,3] <- c('[162988.75, 176524.17]','[164504.66, 177553.81]','[164213.25, 177670.19]','[171569.94, 189752.80]','[179888.87, 198055.35]',
                '[175971.11, 194300.55]','[170747.97, 188949.05]','[174797.44, 193353.68]','[172703.45, 192463.72]','[163986.30, 177529.80]',
                '[-84007.23, 712884.27]')

val_df[,4] <- c('NO','NO','NO','YES','NO',
                'YES','YES','YES','YES','NO',
                'YES*')

val_df[,5] <- c(1657162517.56, 1669587375.01, 1668733077.91, 1763616255.09, 1844745764.38,
                1807295957.79, 1755681139.99, 1796945610.77, 1782380939.82, 1666940099.01,
                157219259.33)

val_df[,6] <- c(33707961.22, 32496969.44, 33512527.77, 45281715.35, 45240920.68,
                45646764.26, 45327086.45, 46211565.61, 49210021.45, 33728064.28,
                101646191.13)

val_df[,7] <- c('[1591096127.57, 1723228907.55]','[1605894485.29, 1733280264.72]','[1603049730.45, 1734416425.36]','[1674865723.84, 1852366786.34]','[1756075189.22, 1933416339.54]',
                '[1717829943.82, 1896761971.76]','[1666841683.02, 1844520596.96]','[1706372606.51, 1887518615.03]','[1685931070.09, 1878830809.55]','[1600834307.76. 1733045890.26]',
                '[-42003614.45, 356442133.11]')

val_df[,8] <- c('NO','NO','NO','YES','NO',
                'YES','YES','YES','YES','NO',
                'NO')


val_tbl <- tableGrob(val_df)

grid.arrange(val_tbl)
