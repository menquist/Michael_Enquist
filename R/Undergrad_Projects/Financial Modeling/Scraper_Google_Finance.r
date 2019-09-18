# symbolsd and insert your working directory.
setwd("  ## INSERT YOUR WD HERE ##  ")
wd <- getwd()

## install.packages("quantmod")


# Load the library
library(quantmod)



# Choose Companies  

# Create a vector of companies whose symbolsancials you want (via their stock symbol)
# Read in ticker symbols by using the NASDAQ.csv file 
symbols <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="", stringsAsFactors=FALSE)$V1


# Creating a Trycatch function for quantmod's getsymbols() function with error handling functionality:
getsymbols_HANDLED <- function (symb) {
  return(tryCatch(getFin(symb, auto.assign = "FALSE"), error=function(e) NULL)) 
}

# Next, use lapply (the "apply" function specific to Statementss) 
symbols.f <- lapply(symbols, getsymbols_HANDLED) 


# Match Names and Remove Nulls 
# Apply the names from your original vector
names(symbols.f) <- symbols

# Dump out a vector of all the names of NULLs, and  remove them
symbols.n <- names(symbols.f[sapply(symbols.f, is.null)]) 
symbols.f <- symbols.f[!sapply(symbols.f, is.null)]

# Check size
length(symbols.n)
length(symbols.f)
length(symbols.n) + length(symbols.f)

# Create Data-Frames 
# Start with the Income Statement

IS_Statements <- lapply(names(symbols.f), function (x) assign(paste(x, "IS", sep = "_A"), as.data.frame(symbols.f[[x]][[1]][[2]]) ))
names(IS_Statements) <- names(symbols.f)
IS_Statements <- lapply(IS_Statements, rev)

# Repeat these steps for the Balance Sheet
BS_Statements <- lapply(names(symbols.f), function (x) assign(paste(x, "BS", sep = "_A"), as.data.frame(symbols.f[[x]][[2]][[2]]) ))
names(BS_Statements) <- names(symbols.f)
BS_Statements <- lapply(BS_Statements, rev)

# Repeat these steps for the Cash Flow Statement
CF_Statements <- lapply(names(symbols.f), function (x) assign(paste(x, "CF", sep = "_A"), as.data.frame(symbols.f[[x]][[3]][[2]]) ))
names(CF_Statements) <- names(symbols.f)
CF_Statements <- lapply(CF_Statements, rev)

#### Quarterly Statments
IS_Statements1 <- lapply(names(symbols.f), function (x) assign(paste(x, "IS", sep = "_Q"), as.data.frame(symbols.f[[x]][[1]][[1]]) ))
names(IS_Statements1) <- names(symbols.f)
IS_Statements1 <- lapply(IS_Statements1, rev)

# Repeat these steps for the Balance Sheet
BS_Statements1 <- lapply(names(symbols.f), function (x) assign(paste(x, "BS", sep = "_Q"), as.data.frame(symbols.f[[x]][[2]][[1]]) ))
names(BS_Statements1) <- names(symbols.f)
BS_Statements1 <- lapply(BS_Statements1, rev)

# Repeat these steps for the Cash Flow Statement
CF_Statements1 <- lapply(names(symbols.f), function (x) assign(paste(x, "CF", sep = "_Q"), as.data.frame(symbols.f[[x]][[3]][[1]]) ))
names(CF_Statements1) <- names(symbols.f)
CF_Statements1 <- lapply(CF_Statements1, rev)

#### merge Statements

#IS_Statements <-  mapply(c, IS_Statements, IS_Statements1, SIMPLIFY=FALSE)
#BS_Statements <-  mapply(c, BS_Statements, BS_Statements1, SIMPLIFY=FALSE)



# Create and export csv file for each statement

#create an Financial Statements folder and set wd
dir.create("Financial Statements")
setwd(paste(wd,"/Financial Statements", sep=""))

# Use the Statements and create/export csv file
lapply(1:length(IS_Statements), function(x) write.csv(IS_Statements[[x]],file = paste(paste(names(IS_Statements[x]),"Annual_IS", sep ="_"), ".csv")))
lapply(1:length(BS_Statements), function(x) write.csv(BS_Statements[[x]],file = paste(paste(names(BS_Statements[x]),"Annual_BS", sep ="_"), ".csv")))
lapply(1:length(CF_Statements), function(x) write.csv(CF_Statements[[x]],file = paste(paste(names(IS_Statements[x]),"Annual_CF", sep ="_"), ".csv")))

lapply(1:length(IS_Statements1), function(x) write.csv(IS_Statements1[[x]],file = paste(paste(names(IS_Statements1[x]),"Quarterly_IS", sep ="_"), ".csv")))
lapply(1:length(BS_Statements1), function(x) write.csv(BS_Statements1[[x]],file = paste(paste(names(BS_Statements1[x]),"Quarterly_BS", sep ="_"), ".csv")))
lapply(1:length(CF_Statements1), function(x) write.csv(CF_Statements1[[x]],file = paste(paste(names(IS_Statements1[x]),"Quarterly_CF", sep ="_"), ".csv")))

#Reset working directory
setwd(wd)

