library(pdftools)
setwd("~/Downloads/chatbotdata/")
filenames <- list.files(pattern="*.pdf",all.files = T)
txt = lapply(filenames, pdf_text)
n.obs = sapply(txt, length)
seq.max = seq_len(max(n.obs))
mat <- t(sapply(txt,"[",i= seq.max))
mat <- data.frame(mat)
#Text = gsub(', [^A-za-z]+', ' ',do.call(paste,c(mat,sep=' ')))
#mat <- data.frame(Text, stringsAsFactors = FALSE)
#mat$Text <- gsub('\\NA', "", mat$Text)
x <- colnames(mat)
mat$Title <- as.factor(filenames)
mat$section <- "Stats"
library(reshape)
long <- melt(mat, id.vars = c("Title","section"))
long$variable <- sub("^X","",long$variable)
colnames(long)[3] <- "Page Number"
colnames(long)[4] <- "Text"
long <- long[!(is.na(long$Text) | long$Text==""), ]

long$`Page Number` <- as.numeric(long$`Page Number`)
long <- long[order(long$Title, (long$`Page Number`), decreasing =F),]

library(tidyr)
library(qdap)
n.obsd = sapply(long$Text, sent_detect_nlp)
n.obs = sapply(n.obsd,length)
seq.max = seq_len(max(n.obs))
mat1 = t(sapply(n.obsd,"[", i = seq.max))

mat1 = data.frame(mat1)
colnames(mat1)
mat1$Title <-  long$Title
mat1$section <- long$`Page Number`

long <- melt(mat1, id.vars = c("Title","section"))
long$variable <- sub("^X","",long$variable)
colnames(long)[3] <- "Page Number"
colnames(long)[4] <- "Text"
long$`Page Number` <- as.numeric(long$`Page Number`)
#long <- long[order(long$section, (long$`Page Number`), decreasing =F),]
long <- long[order(long[,c(1)], (long[,c(2)]), decreasing =F),]

long <- long[!(is.na(long$Text) | long$Text==" "),]

long$Characters <- nchar(gsub("[^A-z]","",long$Text))
long$Characters <- as.numeric(long$Characters)
library(dplyr)
long = long %>% 
  filter(Characters > 50)
library(rJava)
#library(XLConnect)

#creating an Excel workbook. Both .xls and .xlsx file formats can be used.
#wb <- loadWorkbook("Stat Learn summary.xlsx", create = TRUE)

#creating sheets within an Excel workbook
#createSheet(wb, name = "documents")

#writing into sheets within an Excel workbook : 
#writing ChickWeight data frame into chickSheet
#writeWorksheet(wb, long, sheet = "documents")

#saving a workbook to an Excel file :
#saves a workbook to the corresponding Excel file and writes the file to disk.
#saveWorkbook(wb)
library(xlsx)

#wb <- createWorkbook()
#sheet1 <- createSheet(wb, sheetName = 'documents')
setwd("~/Slack-AI-ChatBot/Chatbot")
write.xlsx2(long, "Chatbot-dat.xlsx", sheetName = 'documents', append = FALSE)

#str(long)
