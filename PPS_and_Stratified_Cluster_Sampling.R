#For T.R. Johnson's detailed tutorial, 
#Cf. https://rpubs.com/trjohns/survey-cluster

library(survey)
data(api)

#checking sample dataframes
head(apiclus1)
head(apiclus2)
head(apisrs)
head(apistrat)

head(apipop)

#dimensions of sample dataframes
dim(apiclus1)
dim(apiclus2)
dim(apisrs)
dim(apistrat)

#dimensions of population data frame
dim(apipop)

#One Stage Cluster Sampling
api.onestage <- svydesign(id = ~dnum, data = apiclus1, fpc = ~fpc)
summary(api.onestage)

unique(weights(api.onestage))

#TWo Stage Cluster Sampling
api.twostage <- svydesign(id = ~dnum + snum, data = apiclus2, fpc = ~fpc1 + fpc2)
summary(api.twostage)

unique(weights(api.twostage))

#mean estimations
svymean(~enroll, design = api.onestage)

svymean(~enroll, design = api.twostage, na.rm = TRUE)

#retrieving records with NAs
with(apiclus2, sname[is.na(enroll)])

#total estimations
svytotal(~enroll, design = api.onestage)

svytotal(~enroll, design = api.twostage, na.rm = TRUE)


#alternative estimator of total using mu * Population Size
tmp <- svymean(~enroll, design = api.onestage)

#estimate
tmp[1] * 6194

#standard error
SE(tmp) * 6194

#confidence interval
confint(tmp) * 6194


#PPS Sampling
#adding sampling weights
apiclus1$w <- NA
for (i in unique(apiclus1$dnum)) {
  apiclus1$w[apiclus1$dnum == i] <- 6194/(15 * sum(apiclus1$dnum == i))
}


#Stratified Cluster Sampling Design

library(SDaA)

#merging and creating features for teacher IDs and stratum sizes
teacher.sample <- merge(teachers, teachmi, by = c("dist", "school"))

teacher.sample$teacher <- 1:nrow(teacher.sample)
teacher.sample$N <- NA
teacher.sample$N[teacher.sample$dist == "sm/me"] <- 66
teacher.sample$N[teacher.sample$dist == "large"] <- 245

#instantiating svydesign object
#PSU and SSU identifiers passed as formula to ID argument
#only PSUs were stratified; no SSU stratum identifier is passed to the strata argument
#feature N indicates the number of schools in the stratum
#feature popteach indicates the number of teachers in the school for a given teacher
teacher.design <- svydesign(id = ~school + teacher, data = teacher.sample, strata = ~dist + 
                              NULL, fpc = ~N + popteach)
summary(teacher.design)

#removing PSU with only one element
options(survey.lonely.psu = "remove")

#estimation of total
svytotal(~hrwork, design = teacher.design, na.rm = TRUE)

#estimation of mean
svymean(~hrwork, design = teacher.design, na.rm = TRUE)

#estimation of total by district stratum identifier
svyby(~hrwork, by = ~dist, design = teacher.design, FUN = svytotal, na.rm = TRUE)

#estimation of mean by district stratum identifier
svyby(~hrwork, by = ~dist, design = teacher.design, FUN = svymean, na.rm = TRUE)


