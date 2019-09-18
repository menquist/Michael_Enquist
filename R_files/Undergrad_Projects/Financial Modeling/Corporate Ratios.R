library(quantmod)
library(knitr)
library(dplyr)
library(reshape)

TM


finStatements <- getFin('DRD', auto.assign = FALSE) 


BS <- viewFin(finStatements,"BS", "Q")
IS <- viewFin(finStatements,"IS", "Q")
CF <- viewFin(finStatements,"CF", "Q")
a <- viewFin(finStatements,"BS", "A")
b <- viewFin(finStatements,"IS", "A")
C.F <- viewFin(finStatements,"CF", "A")
e <- viewFin(finStatements,"BS", "A")
f <- viewFin(finStatements,"IS", "A")
###share price
q <- yahooQF("Last Trade (Price Only)")                
qq <- getQuote("finStatements", what = q)
c <- yahooQF("EBITDA")
d <- getQuote("finStatements", what = c)
str(a)
head(a)

### Melting a
a2 <- melt(BS)
a2
### Metling b
b1 <- melt(IS)
b1
### Melting C
c1 <- melt(CF)
c1


### Melting a
a2 <- melt(BS)
a2
### Metling b
b1 <- melt(IS)
b1
### Melting C
c1 <- melt(CF)
c1

###Total Assets
a3 <- filter(a2, Var1 == "Total Assets")
a3
TotalAsset_Rate <- 100*(a3$value[1] - a3$value[2])/a3$value[2]
TotalAsset_Rate <- NULL
for(i in 1:nrow(a3)) TotalAsset_Rate[i] <- 100*(a3$value[i] - a3$value[1+i])/a3$value[1+i]
ggplot(a3, aes(y = value, x = Var2)) + geom_point(size = 3, colour = "blue") + theme_bw()
TotalAsset_Rate
### for the altman Total Assets
Total_Assets <- a3$value



#### Total Equity
a4 <- filter(a2, Var1 == "Total Equity")
a4
TOTALEQUITY_Rate <- 100*(a4$value[1] - a4$value[2])/a4$value[2]
TOTALEQUITY_Rate <- NULL
for(i in 1:nrow(a4)) TOTALEQUITY_Rate[i] <- 100*(a4$value[i] - a4$value[1+i])/a4$value[1+i]
TOTALEQUITY_Rate
ggplot(a4, aes(y = value, x = Var2)) + geom_point(size = 3, colour = "blue") + theme_bw()
plot(TOTALEQUITY_Rate)

#### Total DEBT
a5 <- filter(a2, Var1 == "Total Debt")
a5
TOTALDEBT_Rate <- 100*(a5$value[1] - a5$value[2])/a5$value[2]
TOTALDEBT_Rate <- NULL
for(i in 1:nrow(a5)) TOTALDEBT_Rate[i] <- 100*(a5$value[i] - a5$value[1+i])/a5$value[1+i]
TOTALDEBT_Rate
plot(TOTALDEBT_Rate)
ggplot(a5, aes(y = value, x = Var2)) + geom_point(size = 3, colour = "blue") + theme_bw()

#### Total Liabilites
a6 <- filter(a2, Var1 == "Total Liabilities")
a6
TOTALLiabilites_Rate <- 100*(a6$value[1] - a6$value[2])/a6$value[2]
TOTALLiabilites_Rate <- NULL
for(i in 1:nrow(a6)) TOTALLiabilites_Rate[i] <- 100*(a6$value[i] - a6$value[1+i])/a6$value[1+i]
TOTALLiabilites_Rate
plot(TOTALLiabilites_Rate)
ggplot(a6, aes(y = value, x = Var2)) + geom_point(size = 3, colour = "blue") + theme_bw()

#### for the altman Total Liabilities
Total_Liabilities  <- a6$value



#################### Debt Equity

DEBTEQUITY <- filter(a2, Var1 %in% c("Total Debt", "Total Equity")) %>%
  dcast(Var2 ~ Var1)
colnames(DEBTEQUITY) <- c("date", "debt", "equity")
DEBTEQUITY <- mutate(DEBTEQUITY, DE = debt/equity)
DEBTEQUITY
DE_rates <- 100*(DEBTEQUITY$DE[1] - DEBTEQUITY$DE[2])/DEBTEQUITY$DE[2]
DE_rates <- NULL
for(i in 1:nrow(DEBTEQUITY)) DE_rates[i] <- 100*(DEBTEQUITY$DE[i] - DEBTEQUITY$DE[1+i])/DEBTEQUITY$DE[1+i]
DE_rates
TotalAsset_Rate
TOTALEQUITY_Rate
TOTALDEBT_Rate
TOTALLiabilites_Rate

#### Total Revenue 
Total.Revenue <- filter(b1, Var1 == "Total Revenue") 
TR_rate <- 100*(Total.Revenue$value[1] - Total.Revenue$value[2])/Total.Revenue$value[2]
TR_rate <- NULL
for(i in 1:nrow(Total.Revenue)) TR_rate[i] <- 100*(Total.Revenue$value[i] - Total.Revenue$value[1+i])/Total.Revenue$value[1+i]
TR_rate
revenue.rates <- cbind(Total.Revenue,DE_rates)



############# bind all the Rate of Changes
Rate.of.Change <- cbind(DEBTEQUITY,DE_rates,
                        TotalAsset_Rate,
                        TOTALEQUITY_Rate,
                        TOTALDEBT_Rate,
                        TOTALLiabilites_Rate,
                        TR_rate)
Rate.of.Change
ggplot(DEBTEQUITY, aes(y = DE, x = date)) + geom_point(size = 3, colour = "blue") + theme_bw()


###############################################################ENTERPRISE VALUE


qq
qqq <- qq$Last
qqq
####
enterprise <- filter(a2, Var1 %in%  c("Total Debt", "Minority Interest", "Cash & Equivalents","Preferred Stock - Non Redeemable, Net"
                                      ,"Total Common Shares Outstanding" )) %>%
  dcast(Var2 ~ Var1)
colnames(enterprise) <- c( "date", "cash", "debt", "minority","stock", "total" )

ev.fun <- function(x) {
  if(sum(is.na(enterprise$minority)) == 0 & sum(is.na(enterprise$stock)) == 0) {
    EV <- mutate(x, EV = (total*qqq)+debt+minority+stock-cash)
  } else {
    if(sum(is.na(enterprise$minority)) == 0 & sum(is.na(enterprise$stock)) > 0) {
      EV <- mutate(x, EV = (total*qqq)+debt-cash) 
    } else{
      if(sum(is.na(enterprise$minority)) > 0 & sum(is.na(enterprise$stock)) == 0) {
        EV <- mutate(x, EV = (total*qqq)+debt+stock-cash)
      } else {
        EV <- mutate(x, EV = (total*qqq)+debt-cash)
      }
    }
  }
  
  return(EV)
}

Enterprise_Value <- enterprise_out$EV
Enterprise_Value
enterprise_out <- ev.fun(enterprise)
enterprise_out




############################################################################ EBITDA


c

ebitda <- as.numeric(substr(d$EBITDA, 1, 4))
Ebitda <- ebitda*10000000
Ebitda
################################################################### EBITDA Margin



b

b2 <- filter(b1, Var1 == "Revenue")
b2
b3 <- b2$value*1000
b3
b4 <- Ebitda
ebitdamargin <- b4/b3
ebitdamargin
##################################################################### Gross Profit Margin

Gross <- filter(b1, Var1 %in% c("Gross Profit", "Revenue")) %>%
  dcast(Var2 ~ Var1)
colnames(Gross) <- c("date", "gross", "revenue")
Grossprofit <- mutate(Gross, GP = gross/revenue)
Grossprofit
GP_rate <- 100*(Grossprofit$GP[1] - Grossprofit$GP[2])/Grossprofit$GP[2]
GP_rates <- NULL
for(i in 1:nrow(Grossprofit)) GP_rates[i] <- 100*(Grossprofit$GP[i] - Grossprofit$GP[1+i])/Grossprofit$GP[1+i]
GP_rates
#####################################################################Short term debt riskiness
ltdebt <- filter(a2, Var1 %in% c("Total Long Term Debt", "Total Debt")) %>%
  dcast(Var2 ~ Var1)
colnames(ltdebt) <- c("date", "ldebt", "totdebt")
ltd_ratio <- mutate(ltdebt, ltd = ldebt/totdebt)
ltd_ratio
ltd_rates <- 100*(ltd_ratio$ltd[1] - ltd_ratio$ltd[2])/ltd_ratio$ltd[2]
ltd_rates <- NULL
for(i in 1:nrow(ltd_ratio)) ltd_rates[i] <- 100*(ltd_ratio$ltd[i] - ltd_ratio$ltd[1+i])/ltd_ratio$ltd[1+i]
ltd_rates
######################################################################EBIT MArgin
EBIT <- filter(b1, Var1 %in% c("Revenue", "Operating Income")) %>%
  dcast(Var2 ~ Var1)
colnames(EBIT) <- c("date", "revenue", "optincome")
EBIT_Margin <- mutate(EBIT, EbitM = optincome/revenue)
EBIT_Margin 
ebitM_rates  <- 100*(EBIT_Margin$EbitM[1] - EBIT_Margin$EbitM[2])/EBIT_Margin$EbitM[2]
ebitM_rates <- NULL
for(i in 1:nrow(EBIT_Margin)) ebitM_rates[i] <- 100*(EBIT_Margin$EbitM[i] - EBIT_Margin$EbitM[1+i])/EBIT_Margin$EbitM[1+i]
ebitM_rates

#################### account payable
acccounts.payable <- filter(a2, Var1 %in% c("Accounts Payable")) %>%
  dcast(Var2 ~ Var1) 
colnames(acccounts.payable) <- c("date", "acccounts.payable")

############################################################################Net Income Margin
NI_Margin <- filter(b1, Var1 %in% c("Revenue", "Net Income")) %>%
  dcast(Var2 ~ Var1)
colnames(NI_Margin) <- c("date", "revenue", "netincome")
NI_mar <- mutate(NI_Margin, NIM = netincome/revenue)
NI_mar 
NI_rates  <- 100*(NI_mar$NIM[1] - NI_mar$NIM[2])/NI_mar$NIM[2]
NI_rates <- NULL
for(i in 1:nrow(NI_mar)) NI_rates[i] <- 100*(NI_mar$NIM[i] - NI_mar$NIM[1+i])/NI_mar$NIM[1+i]
NI_rates


########################################################################### ROIC 


e1 <- melt(e)
f1 <- melt(f)
head(b1)
e1
head(f1)
### average net debt
net_debt <- filter(a2, Var1 %in% c("Cash & Equivalents", "Notes Payable/Short Term Debt", "Long Term Debt" , "Total Equity")) %>%
  dcast(Var2 ~ Var1)
colnames(net_debt) <- c("date", "cash", "stdebt", "ltdebt", "Equity")
Net_debt <- mutate(net_debt, NB = stdebt+ltdebt-cash)
Net_debt
NET_Debt <- Net_debt$NB
ave_net_debt <- mean(NET_Debt)
equity <- net_debt$Equity

##ebit
ebit <- filter(b1, Var1 == "Operating Income")
ebit_value <- ebit$value 



#### ROIC
ROIC <- ebit_value/(ave_net_debt+equity)
ROIC
str(ROIC)


#### HAVING PROBLEMS HERE!!!!
ROIC_rates  <- 100*(ROIC[1] - ROIC[2])/ROIC[2]
ROIC_rates <- NULL
for(i in 1:nrow(ROIC)) ROIC_rates[i] <- 100*(ROIC[i] - ROIC[1+i])/ROIC[1+i]
ROIC_rates


##################################################################### ROE
NI <- filter(b1, Var1 == "Net Income")
NI
ave_equity <- mean(equity)
ave_equity
ROE <- (NI$value)/ave_equity
ROE

###################################################################### Shareholders Equity

Shareholders_Equity <- filter(a2, Var1 %in% c("Total Assets", "Total Liabilities")) %>%
  dcast(Var2 ~ Var1)

colnames(Shareholders_Equity) <- c("X2", "TA", "TL")
ShareholdersEquity <- mutate(Shareholders_Equity, EQ = TA - TL)
SHEQ <- ShareholdersEquity$EQ
SHEQ

##################################################################### Working capital

Working_capital <- filter(a2, Var1 %in% c("Total Current Assets", "Total Current Liabilities")) %>%
  dcast(Var2 ~ Var1)
colnames(Working_capital) <- c("X2", "TCA", "TCL")
Workingcapital <- mutate(Working_capital, WC = TCA - TCL)
WCAP <- Workingcapital$WC

Current_TCLoverTCA <- mutate(Working_capital, cacl = TCL/TCA)
Current_Liabilites_over_Assets <- Current_TCLoverTCA$cacl
WCAP
Current_TCLoverTCA
Current_Liabilites_over_Assets
################################################################## EBIT
PRE_EBIT <- filter(b1, Var1 %in% c("Income Before Tax")) %>%
  dcast(Var2 ~ Var1)
EBIT <- PRE_EBIT$`Income Before Tax`

################################################################## Sales
PRE_Sales <- filter(b1, Var1 %in% c("Revenue")) %>%
  dcast(Var2 ~ Var1)
Sales <- PRE_Sales$Revenue
Sales
################################################################## Total Current Assets
PRE_TCA <- filter(a2, Var1 %in% c("Total Current Assets")) %>%
  dcast(Var2 ~ Var1)
Total_Current_Assets <- PRE_TCA$`Total Current Assets`
Total_Current_Assets

####################################################################### Operating Cash Flow/Sales Ratio
OP_CF <- filter(c1, Var1 %in% c("Cash from Operating Activities")) %>% 
  dcast(Var2 ~ Var1)
OP_CF_Sales <- cbind(OP_CF,Sales[1:4]) 
colnames(OP_CF_Sales) <- c("Date", "CF.OP", "Sales")
OP.CF.Sales <-   mutate(OP_CF_Sales, OP.CF.Sales= CF.OP/Sales )
Operating_Cash_Flow.Sales_Ratio <- OP.CF.Sales$OP.CF.Sales

################################################################# Retained Earnings (Accumulated Deficit)
PRE_Retained <- filter(a2, Var1 %in% c("Retained Earnings (Accumulated Deficit)")) %>%
  dcast(Var2 ~ Var1)
Retained_Earnings <- PRE_Retained$`Retained Earnings (Accumulated Deficit)`
Retained_Earnings

Pre_Total_Liabilities <- filter(a2, Var1 %in% c("Total Liabilities")) %>%
  dcast(Var2 ~ Var1)
Total_Liabilities <- Pre_Total_Liabilities$`Total Liabilities`
Total_Liabilities
##### Altman Z-scores below 1.23 indicate vulner- ability to bankruptcy, 
####scores between 1.23 and 2.90 are a gray area, and scores above 2.90 are considered safe.

Altman_Table <- cbind(Total_Current_Assets, Sales, EBIT, WCAP,
                      SHEQ, Total_Liabilities, Total_Assets, Retained_Earnings,Current_Liabilites_over_Assets)

Altman_Equation <-  (z = 3.1*(EBIT/Total_Assets) + (Sales/Total_Current_Assets) + .42*(SHEQ/Total_Liabilities)
       + .85*(Retained_Earnings/Total_Assets) + .72*(WCAP/Total_Assets) + Current_Liabilites_over_Assets)

Risk_to_Default <- cbind(Altman_Table,Altman_Equation)
Risk_to_Default 

Overall <- cbind(Risk_to_Default,Rate.of.Change,ROE,GP_rates,ltd_rates,
                 Operating_Cash_Flow.Sales_Ratio, NI_rates, TR_rate)

View(Overall)

BIIB <- Overall
### Create file for under or top performing to run stocktrend
write.csv(BIIB, "Corporate Financials/BIIB.07.05.2016.csv")


######################## Annual Report

### Melting annual BS
Annual.BS <- melt(e)
Annual.BS

### Metling Annual IS
Annual.IS<- melt(f)
Annual.IS
### Melting Annual CF
Annual.CF <- melt(C.F)
Annual.CF

