setwd("~/Documents/NEW MAC/Office/Brock/MBE/ECON 5P04/OGWANG ASSIGNMENT")

install.packages("plm")
install.packages("dynlm")
install.packages("psych")
library(dynlm)
library(psych)
library(plm)

df<-read.csv("CharitableGiving_Data.csv", header = TRUE)
CharitableGiving_Data<-df
df<-pdata.frame(CharitableGiving_Data)

ind<- cbind(CharitableGiving_Data$Individual, CharitableGiving_Data$time)

plm<-plm(charity~income+price+age+ms+deps, data = df, model = "pooling")
summary(plm)
plm_within<-plm(charity~income+price+age+ms+deps, data = df, model = "within")
summary(plm_within)
plm_random<-plm(charity~income+price+age+ms+deps, data = df, model = "random")
summary(plm_random)

install.packages("foreign")
library(foreign)
write.dta(df, "dataframe.dta")

hau_1<-phtest(plmwithin, plmrandom)
hau_1