############################################################
###                 Simple R quantmod Scraper            ###
############################################################

# By Matt Lunkes

#### Overview: ######################################## ####

# Set-up  

# Step 1: Choose Companies
# Step 2: Get Financials
# Step 3: Match Names and Remove Nulls
# Step 4: Create Data-Frames
# Step 5: Iterate Through & Export csv Versions


# Set-up  ############################################# ####
############################################################

## R  #<-- remove the '##' & call this if you're starting up from a terminal
# Find and insert your working directory.
setwd("  ## INSERT YOUR WD HERE ##  ")
wd <- getwd()

# If you need to, install quantmod via by removing '##' and running the below code --v
## install.packages("quantmod")

# It might ask you to select a CRAN mirror when installing packages.
# If so, remove the belove '##' and run code, and then rerun the install.packages("quantmod") command
## chooseCRANmirror(0)  #<-- Choose '49' for USA (IA)

# Load the library
library(quantmod)
# Ready to go!


# Step 1: Choose Companies  ########################### ####
############################################################


# Create a vector of companies whose financials you want (via their stock symbol)
fin <- as.character(Volatility_Table$ticker[100:105])

# ^-- check spellings of symbols above; the vector will load, but it will cause errors in everything else below


# Step 2: Get Financials  ############################# ####
############################################################

# First, augment quantmod's getFin() function with error handling functionality:
# NOTE: you need the auto.assign=FALSE parameter, otherwise the environment gets messed up
getFin_HANDLED <- function (symb) {
  return(tryCatch(getFin(symb, auto.assign = "FALSE"), error=function(e) NULL)) 
}

# Next, use lapply (the "apply" function specific to lists) to mass-apply getFinancials (from the quantmod library)
fin.f <- lapply(fin, getFin_HANDLED) 

str(fin.)
# Step 3: Match Names and Remove Nulls ################ ####
############################################################

# For some reason, auto.assign drops the top-level names (Company names) in the fin.f list
# Apply the names from your original vector
names(fin.f) <- fin

# Dump out a vector of all the names of NULLs (tickers w/o financials), and then remove them
fin.n <- names(fin.f[sapply(fin.f, is.null)]) #<-- (just in case we have to see what got dumped out)
fin.f <- fin.f[!sapply(fin.f, is.null)]

# Check size
length(fin.n)
length(fin.f)
length(fin.n) + length(fin.f)

# Save fin.f before going any further
save(fin.f,file = "fin_f.RData")


# Step 4: Create Data-Frames  ######################### ####
############################################################

# quantmod returns financials in a 2-level list, and then we in turn wrapped each company in another list "fin.f" via the lapply()
# the general schema is fin.f [["Company"]] [["Financial Statement"]] [["Quarter or Annual"]]
# The "Financial Statment" level maps to 1 = "Income Statment", 2 = "Balance Sheet", 3 = "Cash Flow Statement"
# The "Q or A" level maps to 1 = "end of Quarter", 2 = "Annual"
# Thus, calling >fin.f[[2]][[1]][[2]]  would print out the 2nd Company's (2nd from our original 'fin' vector) Year-End Income Statement

# Start with the Income Statement
# Iterate through, create data frames, and add them to financial statement specic lists
# Also, pass names to List Elements.
IS_list <- lapply(names(fin.f), function (x) assign(paste(x, "IS", sep = "_A"), as.data.frame(fin.f[[x]][[1]][[2]]) ))
names(IS_list) <- names(fin.f)

# Google financial statements are presented in a weird order, w/ years going right to left
# Re-sort each financial in order to put it in proper order
IS_list <- lapply(IS_list, rev)

# Repeat these steps for the Balance Sheet
BS_list <- lapply(names(fin.f), function (x) assign(paste(x, "BS", sep = "_A"), as.data.frame(fin.f[[x]][[2]][[2]]) ))
names(BS_list) <- names(fin.f)
BS_list <- lapply(BS_list, rev)

# Repeat these steps for the Cash Flow Statement
CF_list <- lapply(names(fin.f), function (x) assign(paste(x, "CF", sep = "_A"), as.data.frame(fin.f[[x]][[3]][[2]]) ))
names(CF_list) <- names(fin.f)
CF_list <- lapply(CF_list, rev)

#### Quarterly
IS_list1 <- lapply(names(fin.f), function (x) assign(paste(x, "IS", sep = "_Q"), as.data.frame(fin.f[[x]][[1]][[1]]) ))
names(IS_list1) <- names(fin.f)

# Google financial statements are presented in a weird order, w/ years going right to left
# Re-sort each financial in order to put it in proper order
IS_list1 <- lapply(IS_list1, rev)

# Repeat these steps for the Balance Sheet
BS_list1 <- lapply(names(fin.f), function (x) assign(paste(x, "BS", sep = "_Q"), as.data.frame(fin.f[[x]][[2]][[1]]) ))
names(BS_list1) <- names(fin.f)
BS_list1 <- lapply(BS_list1, rev)

# Repeat these steps for the Cash Flow Statement
CF_list1 <- lapply(names(fin.f), function (x) assign(paste(x, "CF", sep = "_Q"), as.data.frame(fin.f[[x]][[3]][[1]]) ))
names(CF_list1) <- names(fin.f)
CF_list1 <- lapply(CF_list1, rev)

#### merge lists

#IS_list <-  mapply(c, IS_list, IS_list1, SIMPLIFY=FALSE)
#BS_list <-  mapply(c, BS_list, BS_list1, SIMPLIFY=FALSE)



# Step 5: Iterate Through & Export csv Versions  ###### ####
############################################################

# First, create an Exports folder and set wd
dir.create("Exports")
setwd(paste(wd,"/Exports", sep=""))

# Use the lists of data frames you made in Step 4 to create/export csv versions
lapply(1:length(IS_list), function(x) write.csv(IS_list[[x]],file = paste(paste(names(IS_list[x]),"Annual_IS", sep ="_"), ".csv")))
lapply(1:length(BS_list), function(x) write.csv(BS_list[[x]],file = paste(paste(names(BS_list[x]),"Annual_BS", sep ="_"), ".csv")))
lapply(1:length(CF_list), function(x) write.csv(CF_list[[x]],file = paste(paste(names(IS_list[x]),"Annual_CF", sep ="_"), ".csv")))

lapply(1:length(IS_list1), function(x) write.csv(IS_list1[[x]],file = paste(paste(names(IS_list1[x]),"Quarterly_IS", sep ="_"), ".csv")))
lapply(1:length(BS_list1), function(x) write.csv(BS_list1[[x]],file = paste(paste(names(BS_list1[x]),"Quarterly_BS", sep ="_"), ".csv")))
lapply(1:length(CF_list1), function(x) write.csv(CF_list1[[x]],file = paste(paste(names(IS_list1[x]),"Quarterly_CF", sep ="_"), ".csv")))


# Reset working directory
setwd(wd)


############################################################
###                          End                         ###
############################################################