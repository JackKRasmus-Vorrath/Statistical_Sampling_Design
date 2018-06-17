#Exercise 1: SRS size calculation

#95% CI for sample mean
zstar <- qnorm(.975)

#population standard deviation
sigma <- 75

#acceptable Margin-of-Error
E <- 2

#sample size calculation
n <- (zstar^2 * sigma^2)/ E^2

#population size
N <- 10000

#finite population correction
nstar <- (n*N)/(n - 1 + N)



#Exercise 2: Estimates of Proportions

library(PracTools)
library(sampler)
library(survey)
library(sampling)
library(data.table)

#create dataframe
df <- setNames(data.frame(matrix(ncol = 3, nrow = 1100000)), c("ID", "Waited", "Stratum"))

#df of length 1.1M, with 300K, 400K, and 400K records in each stratum
df$ID <- 1:1100000
df$Stratum <- c(rep('1',300000),rep('2',400000),rep('3',400000))

#assign 'Waited' factor to 20% subset of Stratum 1 by matching ID in the first 300K rows
#assign False (Not-Waited) condition to remainder
sample_df_1 <- df[ sample( which(df$Stratum=='1'), round(0.2*length(which(df$Stratum=='1')))), ]
setDT(df)[ID %in% sample_df_1$ID,  Waited := 1 ]
df$Waited[1:300000][is.na(df$Waited[1:300000])] <- FALSE
#df[299999:300001,]

#assign 'Waited' factor to 40% subset of Stratum 2 by matching ID in the subsequent 400K rows
#assign False (Not-Waited) condition to remainder
sample_df_2 <- df[ sample( which(df$Stratum=='2'), round(0.4*length(which(df$Stratum=='2')))), ]
setDT(df)[ID %in% sample_df_2$ID,  Waited := 1 ]
df$Waited[300001:700000][is.na(df$Waited[300001:700000])] <- FALSE
#df[699990:700005,]

#assign 'Waited' factor to 60% subset of Stratum 3 by matching ID in the final 400K rows
#assign False (Not-Waited) condition to remainder
sample_df_3 <- df[ sample( which(df$Stratum=='3'), round(0.6*length(which(df$Stratum=='3')))), ]
setDT(df)[ID %in% sample_df_3$ID,  Waited := 1 ]
df$Waited[700001:1100000][is.na(df$Waited[700001:1100000])] <- FALSE
#df[1099990:1100000,]

#convert Boolean factor to numeric for calculation of proportion (mean of 0s and 1s)
df$Waited <- as.numeric(df$Waited)
#mean(as.numeric(df$Waited))



#Simple Random Sample of Size 400
srs_df <- df[sample(nrow(df), 400), ]

#sample design object
mydesign <- svydesign(id = ~1, data = srs_df)

#SRS mean estimate and SE
svymean(~Waited, design = mydesign)

#confidence interval of mean estimate
confint(svymean(~Waited, design = mydesign))



#Proportionally Allocated Stratified Sample of Size 400

#determine strata sample sizes to adjust Proportional Allocation
Nh <- c(300000, 400000, 400000)
strAlloc(n.tot = 400, Nh = Nh, alloc = "prop")

#adjusted Proportional Allocation
prop_alloc_adjusted <- c(110,145,145)

#proportional allocation sample
prop_samp <- sampling:::strata(df, stratanames='Stratum', size=prop_alloc_adjusted, method="srswor")
#prop_samp

#create finite population correction column, detailing population strata sizes
prop_samp$Num <- NA

prop_samp$Num[prop_samp$Stratum == 1] <- 300000
prop_samp$Num[prop_samp$Stratum == 2] <- 400000
prop_samp$Num[prop_samp$Stratum == 3] <- 400000

#drop duplicated column
prop_samp <- prop_samp[, !duplicated(colnames(prop_samp))]

#rename column to match df data frame, for merging below
colnames(prop_samp)[2] <- "ID"
prop_merged <- merge(x = df, y = prop_samp)

#sort based on Strata and CoID
prop_merged <- prop_merged[order(prop_merged$Stratum),]
prop_merged <- prop_merged[order(prop_merged$ID),]


#proportionally stratified design object
mydesign <- svydesign(id = ~1, strata = ~Stratum, data = prop_merged, fpc = ~Num)

#proportionally stratified mean estimate and SE
svymean(~Waited, design = mydesign)

#confidence interval of mean estimate
confint(svymean(~Waited, design = mydesign))



#Neyman Allocated Stratified Sample of Size 400

#compute strata standard deviations
aggregate(df$Waited, by=list(df$Stratum), FUN=sd)

#perform Neyman Allocation to determine strata sample sizes
Nh <- c(300000, 400000, 400000)
Sh <- c(0.4000007, 0.4898986, 0.4898986)
strAlloc(n.tot = 400, Nh = Nh, Sh = Sh, alloc = "neyman")

#adjusted Neyman Allocation
neyman_alloc_adjusted <- c(94,153,153)

#Neyman allocation sample
neyman_samp <- sampling:::strata(df, stratanames='Stratum', size=neyman_alloc_adjusted, method="srswor")
#neyman_samp

#create finite population correction column, detailing population strata sizes
neyman_samp$Num <- NA

neyman_samp$Num[neyman_samp$Stratum == 1] <- 300000
neyman_samp$Num[neyman_samp$Stratum == 2] <- 400000
neyman_samp$Num[neyman_samp$Stratum == 3] <- 400000

#drop duplicated column
neyman_samp <- neyman_samp[, !duplicated(colnames(neyman_samp))]

#rename column to match df data frame, for merging below
colnames(neyman_samp)[2] <- "ID"
neyman_merged <- merge(x = df, y = neyman_samp)

#sort based on Strata and CoID
neyman_merged <- neyman_merged[order(neyman_merged$Stratum),]
neyman_merged <- neyman_merged[order(neyman_merged$ID),]


#Neyman stratified design object
mydesign <- svydesign(id = ~1, strata = ~Stratum, data = neyman_merged, fpc = ~Num)

#Neyman stratified mean estimate and SE
svymean(~Waited, design = mydesign)

#confidence interval of mean estimate
confint(svymean(~Waited, design = mydesign))








#sample(df[1:300000,], size=60000, replace = FALSE)
#subset(df, ID %in% sample_df_1$ID)

#library(devtools)
#source_gist("https://gist.github.com/mrdwab/6424112", filename = "stratified.R")
#stratified(df, group=df$Stratum, size = c(1 = 60000, 2 = 160000, 3 = 240000))

#rndid <- with(df, ave(df$Stratum[1:300000], FUN=function(x) {sample.int(length(60000))}))
#df[rndid<=3,]

#x <- rep( c('1','2','3'), 1100000*c(0.1,0.2,0.65,0.05) )