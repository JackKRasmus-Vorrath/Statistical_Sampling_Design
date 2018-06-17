library(readxl)
library(PracTools)
library(sampler)
library(survey)
library(sampling)

#read individual sheets to dataframe objects
df_samp <- read_excel('lab4Dat_2.xlsx',sheet='data')
#head(df_samp)


#compute strata standard deviations
aggregate(df_samp$Sales, by=list(df_samp$Strata), FUN=sd)

#perform Neyman Allocation to determine strata sample sizes
Nh <- c(228, 101, 30, 10, 6)
Sh <- c(58.87, 92.71, 163.29, 744.35, 1213.75)
strAlloc(n.tot = 50, Nh = Nh, Sh = Sh, alloc = "neyman")


#substitute sum of the Measure of Size for the product of stratum size and std
df_MOS <- aggregate(df_samp$MOS, by=list(df_samp$Strata), FUN=sum)

#sum of MOS feature
sum(df_MOS$x)

#MOS per stratum / total MOS
df_MOS$MOS_Proportion <- df_MOS$x / sum(df_MOS$x)
#head(df_MOS)

#multiplying by 50 to identify sample sizes of MOS-driven stratification
df_MOS$Sample_Size <- round(df_MOS$MOS_Proportion * 50)
#head(df_MOS)

#NB: there are only 10 and 6 samples in the upper two strata!
#adjusted, taking all the records from the upper strata,
#and redistributing the rest (with Neyman allocation on
#whatever number of samples are left over the remaining strata,
#using MOS again as the standard deviation proxy)

#perform MOS-driven Neyman Allocation to adjust strata sample sizes
Nh <- c(228, 101, 30)
Sh <- c(8973.055, 14123.365, 9041.752)
strAlloc(n.tot = 34, Nh = Nh, Sh = Sh, alloc = "neyman")

#appending adjusted MOS-driven Neyman Allocation vector
df_MOS$Sample_Size_Adj <- c(19, 13, 2, 10, 6)
#head(df_MOS)


#create finite population correction column, detailing population strata sizes
df_samp$Num <- NA

df_samp$Num[df_samp$Strata == 1] <- 228
df_samp$Num[df_samp$Strata == 2] <- 101
df_samp$Num[df_samp$Strata == 3] <- 30
df_samp$Num[df_samp$Strata == 4] <- 10
df_samp$Num[df_samp$Strata == 5] <- 6

#proportional allocation sample
prop_samp <- ssamp(df_samp, n = 50, strata = Strata)
prop_samp

#a single-PSU stratum makes no contribution to the variance 
#(for multistage sampling it makes no contribution at that level of sampling)
#options(survey.lonely.psu="certainty")

#the data for the single-PSU stratum are centered at the sample grand mean 
#rather than the stratum mean. This is conservative.
#stratum contribution to the variance is taken to be the average of all 
#the strata with more than one PSU
options(survey.lonely.psu="adjust")

#alternatively, one could place two samples in whichever strata are
#allocated only 1 unit, and then perform the same proportional allocation
#once again on whatever number of samples are left over the remaining strata

#determine strata sample sizes to adjust Proportional Allocation
Nh <- c(228, 101, 30)
Sh <- c(58.87, 92.71, 163.29)
strAlloc(n.tot = 46, Nh = Nh, Sh = Sh, alloc = "prop")

#adjusted Proportional Allocation
prop_alloc_adjusted <- c(29,13,4,2,2)


#proportional allocation sample
prop_samp <- sampling:::strata(df_samp, stratanames='Strata', size=prop_alloc_adjusted, method="srswor")
#prop_samp

#create finite population correction column, detailing population strata sizes
prop_samp$Num <- NA

prop_samp$Num[prop_samp$Strata == 1] <- 228
prop_samp$Num[prop_samp$Strata == 2] <- 101
prop_samp$Num[prop_samp$Strata == 3] <- 30
prop_samp$Num[prop_samp$Strata == 4] <- 10
prop_samp$Num[prop_samp$Strata == 5] <- 6

#rename column to match df_samp data frame, for merging below
colnames(prop_samp)[2] <- "CoID"
prop_merged <- merge(x = df_samp, y = prop_samp)

#sort based on Strata and CoID
prop_merged <- prop_merged[order(prop_merged$Strata),]
prop_merged <- prop_merged[order(prop_merged$CoID),]

#drop duplicated Stratum column
prop_merged <- subset(prop_merged, select=-c(Stratum))
prop_merged


#stratified design object
mydesign <- svydesign(id = ~1, strata = ~Strata, data = prop_merged, fpc = ~Num)

#stratified mean estimate and SE
svymean(~Sales, design = mydesign)

#confidence interval of mean estimate
confint(svymean(~Sales, design = mydesign))

#estimate of population total
svytotal(~Sales, design = mydesign)

#confidence interval of population total
confint(svytotal(~Sales, design = mydesign))


#Original Neyman Allocation vector
#NB: sums to 51, not 50, and there are only 6 samples in stratum 5!
neyman_alloc <- c(16,11,6,9,9)

#adjust Neyman Allocation to determine strata sample sizes
Nh <- c(228, 101, 30)
Sh <- c(58.87, 92.71, 163.29)
strAlloc(n.tot = 35, Nh = Nh, Sh = Sh, alloc = "neyman")

#adjusted, taking all the records from the upper stratum,
#and redistributing the rest (with Neyman allocation on
#whatever number of samples are left over the remaining strata)
neyman_alloc_adjusted <- c(17,12,6,9,6)


#Neyman allocation sample
neyman_samp <- sampling:::strata(df_samp, stratanames='Strata', size=neyman_alloc_adjusted, method="srswor")

#create finite population correction column, detailing population strata sizes
neyman_samp$Num <- NA

neyman_samp$Num[neyman_samp$Strata == 1] <- 228
neyman_samp$Num[neyman_samp$Strata == 2] <- 101
neyman_samp$Num[neyman_samp$Strata == 3] <- 30
neyman_samp$Num[neyman_samp$Strata == 4] <- 10
neyman_samp$Num[neyman_samp$Strata == 5] <- 6

#rename column to match df_samp data frame, for merging below
colnames(neyman_samp)[2] <- "CoID"
neyman_merged <- merge(x = df_samp, y = neyman_samp)

#sort based on Strata and CoID
neyman_merged <- neyman_merged[order(neyman_merged$Strata),]
neyman_merged <- neyman_merged[order(neyman_merged$CoID),]

#drop duplicated Stratum column
neyman_merged <- subset(neyman_merged, select=-c(Stratum))
neyman_merged

#stratified design object
mydesign <- svydesign(id = ~1, strata = ~Strata, data = neyman_merged, fpc = ~Num)

#stratified mean estimate and SE
svymean(~Sales, design = mydesign)

#confidence interval of mean estimate
confint(svymean(~Sales, design = mydesign))

#estimate of population total
svytotal(~Sales, design = mydesign)

#confidence interval of population total
confint(svytotal(~Sales, design = mydesign))





#design effect estimation
#svymean(~Sales, design = mydesign, deff = TRUE)

#domain means by strata
#svyby(~Sales, by = ~Graduate, design = mydesign, FUN = svymean)

#domain totals by strata
#svyby(~Sales, by = ~Graduate, design = mydesign, FUN = svytotal)

