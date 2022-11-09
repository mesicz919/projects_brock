setwd("~/Documents/NEW MAC/Office/Brock/MBE/ECON 5P08/PROJECT")

install.packages("dynlm")
install.packages("car")
install.packages("psych")
install.packages("ggplot2")
install.packages("forecast")
install.packages("fpp2")
library(dynlm)
library(car)
library(psych)
library("ggplot2")
library("forecast")
library("fpp2")

df<-read.csv("PROJECT DATA.csv", header = TRUE)

#plot GDP components
GDP_exp<- df$GDP_exp
CONS_exp<-df$CONS_exp
INV_exp<-df$INV_exp
GOVT_exp<-df$GOVT_exp
EXP_exp<-df$EXP_exp
IM_exp<-df$IM_exp
GDP_exp<- ts(GDP_exp,start=c(1988,1), end=c(2018,1))
CONS_exp<- ts(CONS_exp,start=c(1988,1), end=c(2018,1))
INV_exp<- ts(INV_exp,start=c(1988,1), end=c(2018,1))
GOVT_exp<- ts(GOVT_exp,start=c(1988,1), end=c(2018,1))
EXP_exp<- ts(EXP_exp,start=c(1988,1), end=c(2018,1))
IM_exp<- ts(IM_exp,start=c(1988,1), end=c(2018,1))
GDP_exp<-window(GDP_exp,start=c(1988,1), end=c(2018,1)) 
autoplot(GDP_exp) + autolayer(GDP_exp, series="UK GDP exp.") +
  autolayer(CONS_exp, series="UK cons. exp.") +
  autolayer(INV_exp, series="UK inv. exp.") +
  autolayer(GOVT_exp, series="UK gov't exp.") +
  autolayer(EXP_exp, series="UK export exp.") +
  autolayer(IM_exp, series="UK import exp.") +
  ggtitle("UK GDP and Components (expenditure approach), 1988-2018") + xlab("Year") + ylab("£ (in millions)")

  
#plot log GDP components
log_GDP_exp<- df$log_GDP_exp
log_CONS_exp<-df$log_CONS_exp
log_INV_exp<-df$log_INV_exp
log_GOVT_exp<-df$log_GOVT_exp
log_EXP_exp<-df$log_EXP_exp
log_IM_exp<-df$log_IM_exp
log_GDP_exp<- ts(log_GDP_exp,start=c(1988,1), end=c(2018,1))
log_CONS_exp<- ts(log_CONS_exp,start=c(1988,1), end=c(2018,1))
log_INV_exp<- ts(log_INV_exp,start=c(1988,1), end=c(2018,1))
log_GOVT_exp<- ts(log_GOVT_exp,start=c(1988,1), end=c(2018,1))
log_EXP_exp<- ts(log_EXP_exp,start=c(1988,1), end=c(2018,1))
log_IM_exp<- ts(log_IM_exp,start=c(1988,1), end=c(2018,1))
log_GDP_exp<-window(log_GDP_exp,start=c(1988,1), end=c(2018,1)) 
autoplot(log_GDP_exp) + autolayer(log_GDP_exp, series="UK GDP exp.") +
  autolayer(log_CONS_exp, series="UK cons. exp.") +
  autolayer(log_INV_exp, series="UK inv. exp.") +
  autolayer(log_GOVT_exp, series="UK gov't exp.") +
  autolayer(log_EXP_exp, series="UK export exp.") +
  autolayer(log_IM_exp, series="UK import exp.") +
  ggtitle("UK Log-GDP and Components (expenditure approach), 1988-2018") + xlab("Year") + ylab("£ (in millions)")


#plot CA
Current_Account<- df$Current_Account
Current_Account<- ts(Current_Account,start=c(1988,1), end=c(2018,1))

#plot TB
Trade_Balance<- df$Trade_Balance
Trade_Balance<- ts(Trade_Balance,start=c(1988,1), end=c(2018,1))

#plot NIIP
NIIP<-df$NIIP
NIIP<- ts(NIIP,start=c(1988,1), end=c(2018,1))

Current_Account<-window(Current_Account,start=c(1988,1), end=c(2018,1)) 
autoplot(Current_Account) + autolayer(Current_Account, series="UK Current Account") + 
  autolayer(Trade_Balance, series="UK Trade Balance") + autolayer(NIIP, series="UK NIIP") + 
  ggtitle("UK Balance of Payments Data, 1990-2018") + xlab("Year") + ylab("£ (in millions)") + guides(colour=guide_legend(title="Variables"))

#plot CA as % of GDP
Current_Account_GDP<-df$Current_Account_GDP
Current_Account_GDP<- ts(Current_Account_GDP,start=c(1988,1), end=c(2018,1))

#plot TB as % of GDP
Trade_Balance_GDP<- df$Trade_Balance_GDP
Trade_Balance_GDP<- ts(Trade_Balance_GDP,start=c(1988,1), end=c(2018,1))

Current_Account_GDP<-window(Current_Account_GDP,start=c(1988,1), end=c(2018,1)) 
autoplot(Current_Account_GDP) + autolayer(Current_Account_GDP, series="Current Account (as % of GDP)") + autolayer(Trade_Balance_GDP, series="Trade Balance (as % of GDP)") + 
  ggtitle("UK Current Acccount & Trade Balance (as % of GDP), 1988-2018") + 
  xlab("Year") + ylab("£ (in millions)") + guides(colour=guide_legend(title="Variables"))


#plot trade balance w/ EU and non -EU (missing data)
Trade_EU<-df$Trade_EU
Trade_EU<- ts(Trade_EU,start=c(1988,1), end=c(2018,1))
Trade_non_EU<-df$Trade_non_EU
Trade_non_EU<- ts(Trade_non_EU,start=c(1988,1), end=c(2018,1))
Trade_EU<-window(Trade_EU,start=c(1999,1), end=c(2018,1)) 
autoplot(Trade_EU) + autolayer(Trade_EU, series="Trade Balance w/ EU countries") + autolayer(Trade_non_EU, series="Trade Balance w/ non-EU countries") + 
  ggtitle("UK Trade Balance (EU vs.Non-EU countries), 1990-2018") + 
  xlab("Year") + ylab("Trade Balance (in millions of pounds)") + guides(colour=guide_legend(title="Bilateral Trade"))

#Plot w/ major trading partners
TB_US<-df$Trade.Balance.US
TB_US<- ts(TB_US,start=c(1988,1), end=c(2018,1))
TB_DEU<-df$Trade.Balance.Germany
TB_DEU<- ts(TB_DEU,start=c(1988,1), end=c(2018,1))
TB_NED<-df$Trade.Balance.Netherlands
TB_NED<- ts(TB_NED,start=c(1988,1), end=c(2018,1))
TB_FRA<-df$Trade.Balance.France
TB_FRA<- ts(TB_FRA,start=c(1988,1), end=c(2018,1))
TB_CHINA<-df$Trade.Balance.China
TB_CHINA<- ts(TB_CHINA,start=c(1988,1), end=c(2018,1))

US_trade<-window(TB_US,start=c(1988,1), end=c(2018,1)) 
autoplot(TB_US) + autolayer(TB_FRA, series="Trade Balance w/ France") + autolayer(TB_DEU, series="Trade Balance w/ Germany") +
  autolayer(TB_NED, series="Trade Balance w/ Netherlands") + autolayer(TB_CHINA, series="Trade Balance w/ China") + 
  autolayer(TB_US, series="Trade Balance w/ US") +
  ggtitle("UK Trade Balance with Major Trading Partners, 1990-2018") + 
  xlab("Year") + ylab("Trade Balance (in millions of pounds)") + guides(colour=guide_legend(title="Trade Partners"))

#Hp-filter

Detrended_Current_Account<-df$Detrended_Current_Account
Detrended_Trade_Balance<-df$Detrended_Trade_Balance
trend_GDP_exp_HP<-df$trend_GDP_exp_HP
trend_CONS_exp_HP<-df$trend_CONS_exp_HP
trend_INV_exp_HP<-df$trend_INV_exp_HP
trend_GOVT_exp_HP<-df$trend_GOVT_exp_HP
trend_EXP_exp_HP<-df$trend_EXP_exp_HP	
trend_IM_exp_HP<-df$trend_IM_exp_HP
Detrended_Current_Account<- ts(Detrended_Current_Account,start=c(1988,1), end=c(2018,1))
Detrended_Trade_Balance<- ts(Detrended_Trade_Balance,start=c(1988,1), end=c(2018,1))
trend_GDP_exp_HP<- ts(trend_GDP_exp_HP,start=c(1988,1), end=c(2018,1))
trend_CONS_exp_HP<- ts(trend_CONS_exp_HP,start=c(1988,1), end=c(2018,1))
trend_INV_exp_HP<- ts(trend_INV_exp_HP,start=c(1988,1), end=c(2018,1))
trend_GOVT_exp_HP<- ts(trend_GOVT_exp_HP,start=c(1988,1), end=c(2018,1))
trend_EXP_exp_HP<- ts(trend_EXP_exp_HP,start=c(1988,1), end=c(2018,1))
trend_IM_exp_HP<- ts(trend_IM_exp_HP,start=c(1988,1), end=c(2018,1))
trend_GDP_exp_HP<-window(trend_GDP_exp_HP,start=c(1988,1), end=c(2018,1)) 
autoplot(trend_GDP_exp_HP) +
  autolayer(trend_GDP_exp_HP, series="Trend Component GDP") +
  autolayer(trend_CONS_exp_HP, series="Trend Component cons.") +
  autolayer(trend_INV_exp_HP, series="Trend Component inv.") +
  autolayer(trend_GOVT_exp_HP, series="Trend Component gov't") +
  autolayer(trend_EXP_exp_HP, series=" Trend Component exp.") +
  autolayer(trend_IM_exp_HP, series="Trend Component imports") +
  ggtitle("HP Filter Detrending: GDP and Components (trend component)") + xlab("Year") + ylab("£ (in millions)")

autoplot(Detrended_Current_Account) + autolayer(Detrended_Current_Account, series="Detrended Current Account") +
  autolayer(Detrended_Trade_Balance, series="Detrended Trade Balance") +
  ggtitle("HP Filter Detrending: CA and TB, 1988-2018") + xlab("Year") + ylab("£ (in millions)")

cyc_GDP_exp_HP<-df$cyc_GDP_exp_HP
cyc_CONS_exp_HP<-df$cyc_CONS_exp_HP
cyc_INV_exp_HP<-df$cyc_INV_exp_HP
cyc_GOVT_exp_HP<-df$cyc_GOVT_exp_HP
cyc_EXP_exp_HP<-df$cyc_EXP_exp_HP
cyc_IM_exp_HP<-df$cyc_IM_exp_HP
cyc_GDP_exp_HP<- ts(cyc_GDP_exp_HP,start=c(1988,1), end=c(2018,1))
cyc_CONS_exp_HP<- ts(cyc_CONS_exp_HP,start=c(1988,1), end=c(2018,1))
cyc_INV_exp_HP<- ts(cyc_INV_exp_HP,start=c(1988,1), end=c(2018,1))
cyc_GOVT_exp_HP<- ts(cyc_GOVT_exp_HP,start=c(1988,1), end=c(2018,1))
cyc_EXP_exp_HP<- ts(cyc_EXP_exp_HP,start=c(1988,1), end=c(2018,1))
cyc_IM_exp_HP<- ts(cyc_IM_exp_HP,start=c(1988,1), end=c(2018,1))
cyc_GDP_exp_HP<-window(cyc_GDP_exp_HP,start=c(1988,1), end=c(2018,1)) 
autoplot(cyc_GDP_exp_HP) +
  autolayer(cyc_GDP_exp_HP, series="Cyc. Component GDP") +
  autolayer(cyc_CONS_exp_HP, series="Cyc. Component cons.") +
  autolayer(cyc_INV_exp_HP, series="Cyc. Component inv.") +
  autolayer(cyc_GOVT_exp_HP, series="Cyc. Component gov't") +
  autolayer(cyc_EXP_exp_HP, series=" Cyc. Component exp.") +
  autolayer(cyc_IM_exp_HP, series="Cyc. Component imports") +
  ggtitle("HP Filter Detrending: GDP and Components (cyclical component)") + xlab("Year") + ylab("£ (in millions)")

#log-detrending

#CA
Detrended_Current_Account<-df$Detrended_Current_Account
TREND<-df$TREND
Time<-df$Time
model1<-lm(Detrended_Current_Account ~ TREND)
summary(model1)
residuals<-resid(model1)
residuals<- ts(residuals,start=c(1988,1), end=c(2018,1))

#TB
Detrended_Trade_Balance<-df$Detrended_Trade_Balance
model2<-lm(Detrended_Trade_Balance ~ TREND)
summary(model2)
residuals2<-resid(model2)
residuals2<- ts(residuals2,start=c(1988,1), end=c(2018,1))

#GDP
log_GDP_exp<-df$log_GDP_exp
model3<-lm(log_GDP_exp ~ TREND)
summary(model3)
residuals3<-resid(model3)
residuals3<- ts(residuals3,start=c(1988,1), end=c(2018,1))

residuals<-window(residuals,start=c(1988,1), end=c(2018,1)) 
autoplot(residuals) + autolayer(residuals, series="Cyclical Component of UK Current Account") +
  autolayer(residuals2, series="Cyclical Component of UK Trade Balance") + autolayer(residuals3, series="Cyclical Component of UK GDP") +
  ggtitle("Log-Linear Detrending: UK CA, TB, and GDP, 1990-2018") + 
  xlab("Year") + ylab("£ (in millions)") + guides(colour=guide_legend(title="Variables"))


#CONS
log_CONS_exp<-df$log_CONS_exp
model4<-lm(log_CONS_exp ~ TREND)
summary(model4)
residuals4<-resid(model4)
residuals4<- ts(residuals4,start=c(1988,1), end=c(2018,1))

#INV
log_INV_exp<-df$log_INV_exp
model5<-lm(log_INV_exp ~ TREND)
summary(model5)
residuals5<-resid(model5)
residuals5<- ts(residuals5,start=c(1988,1), end=c(2018,1))

#GOVT
log_GOVT_exp<-df$log_GOVT_exp
model6<-lm(log_GOVT_exp ~ TREND)
summary(model6)
residuals6<-resid(model6)
residuals6<- ts(residuals6,start=c(1988,1), end=c(2018,1))

#EXP
log_EXP_exp<-df$log_EXP_exp
model7<-lm(log_EXP_exp ~ TREND)
summary(model7)
residuals7<-resid(model7)
residuals7<- ts(residuals7,start=c(1988,1), end=c(2018,1))

#IM
log_IM_exp<-df$log_IM_exp
model8<-lm(log_IM_exp ~ TREND)
summary(model8)
residuals8<-resid(model8)
residuals8<- ts(residuals8,start=c(1988,1), end=c(2018,1))

residuals4<-window(residuals4,start=c(1990,1), end=c(2018,1)) 
autoplot(residuals4) + autolayer(residuals4, series="Cyclical Consumption") + 
  autolayer(residuals5, series="Cyclical Investment") +
  autolayer(residuals6, series="Cyclical Gov't Spending") + autolayer(residuals7, series="Cyclical UK Exports") +
  autolayer(residuals8, series="Cyclical UK Imports") + ggtitle("Log-Linear Detrending: Cyclical Components of UK GDP") + 
  xlab("Year") + ylab("£ (in millions)") + guides(colour=guide_legend(title="Components of GDP"))

#Quadratic log-detrending

#CA
TREND_2<-df$TREND^2
TREND_2
model9<-lm(Detrended_Current_Account ~ TREND + TREND_2)
summary(model9)
residuals9<-resid(model9)
residuals9<- ts(residuals9,start=c(1988,1), end=c(2018,1))

#TB
model10<-lm(Detrended_Trade_Balance ~ TREND + TREND_2)
summary(model10)
residuals10<-resid(model10)
residuals10<- ts(residuals10,start=c(1988,1), end=c(2018,1))

#GDP
model11<-lm(log_GDP_exp ~ TREND + TREND_2)
summary(model11)
residuals11<-resid(model11)
residuals11<- ts(residuals11,start=c(1988,1), end=c(2018,1))

residuals9<-window(residuals9,start=c(1988,1), end=c(2018,1)) 
autoplot(residuals9) + autolayer(residuals9, series="Cyclical Component of UK Current Account") +
  autolayer(residuals10, series="Cyclical Component of UK Trade Balance") + autolayer(residuals11, series="Cyclical Component of UK GDP") +
  ggtitle("Log-Quadratic Detrending: UK CA, TB, and GDP, 1990-2018") + 
  xlab("Year") + ylab("£ (in millions)") + guides(colour=guide_legend(title="Variables"))

#CONS
model12<-lm(log_CONS_exp ~ TREND + TREND_2)
summary(model12)
residuals12<-resid(model12)
residuals12<- ts(residuals12,start=c(1988,1), end=c(2018,1))

#INV
model13<-lm(log_INV_exp ~ TREND + TREND_2)
summary(model13)
residuals13<-resid(model13)
residuals13<- ts(residuals13,start=c(1988,1), end=c(2018,1))

#GOVT
model14<-lm(log_GOVT_exp ~ TREND + TREND_2)
summary(model14)
residuals14<-resid(model14)
residuals14<- ts(residuals14,start=c(1988,1), end=c(2018,1))

#EXP
model15<-lm(log_EXP_exp ~ TREND + TREND_2)
summary(model15)
residuals15<-resid(model15)
residuals15<- ts(residuals15,start=c(1988,1), end=c(2018,1))

#IM
model16<-lm(log_IM_exp ~ TREND + TREND_2)
summary(model16)
residuals16<-resid(model16)
residuals16<- ts(residuals16,start=c(1988,1), end=c(2018,1))

residuals12<-window(residuals12,start=c(1990,1), end=c(2018,1)) 
autoplot(residuals12) + autolayer(residuals12, series="Cyclical Consumption") + 
  autolayer(residuals13, series="Cyclical Investment") +
  autolayer(residuals14, series="Cyclical Gov't Spending") + autolayer(residuals15, series="Cyclical UK Exports") +
  autolayer(residuals16, series="Cyclical UK Imports") + ggtitle("Log-Quadratic Detrending: Cyclical Components of UK GDP") + 
  xlab("Year") + ylab("£ (in millions)") + guides(colour=guide_legend(title="Components of GDP"))

#TREND COMPONENTS
trend_CA_log_det<-df$trend_CA_log_det
trend_TB_log_det<-df$trend_TB_log_det
trend_GDP_log_det<-df$trend_GDP_log_det
trend_CONS_log_det<-df$trend_CONS_log_det
trend_INV_log_det<-df$trend_INV_log_det
trend_GOVT_log_det<-df$trend_GOVT_log_det
trend_EXP_log_det<-df$trend_EXP_log_det	
trend_IM_log_det<-df$trend_IM_log_det
trend_CA_log_det<- ts(trend_CA_log_det,start=c(1988,1), end=c(2018,1))
trend_TB_log_det<- ts(trend_TB_log_det,start=c(1988,1), end=c(2018,1))
trend_GDP_log_det<- ts(trend_GDP_log_det,start=c(1988,1), end=c(2018,1))
trend_CONS_log_det<- ts(trend_CONS_log_det,start=c(1988,1), end=c(2018,1))
trend_INV_log_det<- ts(trend_INV_log_det,start=c(1988,1), end=c(2018,1))
trend_GOVT_log_det<- ts(trend_GOVT_log_det,start=c(1988,1), end=c(2018,1))
trend_EXP_log_det<- ts(trend_EXP_log_det,start=c(1988,1), end=c(2018,1))
trend_IM_log_det<- ts(trend_IM_log_det,start=c(1988,1), end=c(2018,1))
trend_GDP_log_det<-window(trend_GDP_log_det,start=c(1988,1), end=c(2018,1)) 
autoplot(trend_GDP_log_det) +
  autolayer(trend_GDP_log_det, series="Trend Component GDP") +
  autolayer(trend_CONS_log_det, series="Trend Component cons.") +
  autolayer(trend_INV_log_det, series="Trend Component inv.") +
  autolayer(trend_GOVT_log_det, series="Trend Component gov't") +
  autolayer(trend_EXP_log_det, series=" Trend Component exp.") +
  autolayer(trend_IM_log_det, series="Trend Component imports") +
  ggtitle("Log-Linear Detrending: GDP and Components (trend component)") + xlab("Year") + ylab("£ (in millions)")

trend_CA_log_det<-window(trend_CA_log_det,start=c(1988,1), end=c(2018,1)) 
autoplot(trend_CA_log_det) + autolayer(trend_CA_log_det, series="Trend Component of UK Current Account") +
  autolayer(trend_TB_log_det, series="Trend Component of UK Trade Balance") +
  ggtitle("Log-Linear Detrending: UK CA, TB, and GDP (trend component)") + 
  xlab("Year") + ylab("£ (in millions)") + guides(colour=guide_legend(title="Variables"))

trend_CA_quad<-df$trend_CA_quad
trend_TB_quad<-df$trend_TB_quad
trend_GDP_quad<-df$trend_GDP_quad
trend_CONS_quad<-df$trend_CONS_quad
trend_INV_quad<-df$trend_INV_quad
trend_GOVT_quad<-df$trend_GOVT_quad
trend_EXP_quad<-df$trend_EXP_quad	
trend_IM_quad<-df$trend_IM_quad
trend_CA_quad<- ts(trend_CA_quad,start=c(1988,1), end=c(2018,1))
trend_TB_quad<- ts(trend_TB_quad,start=c(1988,1), end=c(2018,1))
trend_GDP_quad<- ts(trend_GDP_quad,start=c(1988,1), end=c(2018,1))
trend_CONS_quad<- ts(trend_CONS_quad,start=c(1988,1), end=c(2018,1))
trend_INV_quad<- ts(trend_INV_quad,start=c(1988,1), end=c(2018,1))
trend_GOVT_quad<- ts(trend_GOVT_quad,start=c(1988,1), end=c(2018,1))
trend_EXP_quad<- ts(trend_EXP_quad,start=c(1988,1), end=c(2018,1))
trend_IM_quad<- ts(trend_IM_quad,start=c(1988,1), end=c(2018,1))
trend_GDP_quad<-window(trend_GDP_quad,start=c(1988,1), end=c(2018,1)) 
autoplot(trend_GDP_quad) +
  autolayer(trend_GDP_quad, series="Trend Component GDP") +
  autolayer(trend_CONS_quad, series="Trend Component cons.") +
  autolayer(trend_INV_quad, series="Trend Component inv.") +
  autolayer(trend_GOVT_quad, series="Trend Component gov't") +
  autolayer(trend_EXP_quad, series=" Trend Component exp.") +
  autolayer(trend_IM_quad, series="Trend Component imports") +
  ggtitle("Log-Quadtratic Detrending: GDP and Components (trend comp.)") + xlab("Year") + ylab("£ (in millions)")

trend_CA_quad<-window(trend_CA_quad,start=c(1988,1), end=c(2018,1)) 
autoplot(trend_CA_quad) + autolayer(trend_CA_quad, series="Trend Component of UK Current Account") +
  autolayer(trend_TB_quad, series="Trend Component of UK Trade Balance") +
  ggtitle("Log-Quadratic Detrending: UK CA, TB, and GDP, 1990-2018") + 
  xlab("Year") + ylab("£ (in millions)") + guides(colour=guide_legend(title="Variables"))

