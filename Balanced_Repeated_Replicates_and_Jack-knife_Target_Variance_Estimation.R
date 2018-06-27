library(survey)

#create dataframe
df <- setNames(data.frame(matrix(ncol = 6, nrow = 16)), 
               c('Obs','Stratum','PSU','Case','y','w'))

#df values
df$Obs <- 1:16
df$Stratum <- c(rep(1,4), rep(2,4), rep(3,4), rep(4,4))
df$PSU <- c(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2)
df$Case <- c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2)
df$y <- c(0.58, 0.48, 0.42, 0.57, 0.39, 0.46, 0.50, 0.21, 
          0.39, 0.47, 0.44, 0.43, 0.64, 0.55, 0.47, 0.50)
df$w <- c(1,2,2,2,1,2,2,1,1,2,1,1,1,1,2,2)

#create finite population correction column, detailing population strata sizes
#df$fpc <- NA

#df$fpc[df$Stratum == 1] <- 4
#df$fpc[df$Stratum == 2] <- 4
#df$fpc[df$Stratum == 3] <- 4
#df$fpc[df$Stratum == 4] <- 4

#stratified design object
mydesign <- svydesign(id = ~1, strata = ~Stratum, data = df, weights = ~w)



# convert to BRR replicate weights
mydesign.brr <- as.svrepdesign(mydesign, type="BRR")

# convert to JKn weights with finite population correction
mydesign.jknf <- as.svrepdesign(mydesign, type="JKn")



#BRR mean estimate and SE
svymean(~y, design = mydesign.brr)

#BRR confidence interval of mean estimate
confint(svymean(~y, design = mydesign.brr))



#JKNF mean estimate and SE
svymean(~y, design = mydesign.jknf)

#JKNF confidence interval of mean estimate
confint(svymean(~y, design = mydesign.jknf))



#BRR variance estimate
svyvar(~y, design = mydesign.brr)

#JKNF variance estimate
svyvar(~y, design = mydesign.jknf)


