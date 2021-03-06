## Author: Michael Creegan ##
## Date: March 4, 2021 ##

###############################################
### FOMC Sentiment Analysis & Insights / NLP ##
###############################################


##import libraries##
#install.packages("xlsx", dependencies = TRUE)
#install.packages("rtools", dependencies = TRUE)
#library(hms)

library(dplyr)
library(SentimentAnalysis)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)
library(rlang)
library(tidyverse)
library(tidytext)
library(xlsx)
library(RCurl)
library(XML)
library(kableExtra)
library(tm)
library(ngram)
library(wordcloud)
library(ggridges)
library(gridExtra)
library(rcompanion)
library(ggcorrplot)
library(caret)
library(e1071)
library(R.utils)
library(DT)
library(lattice)
library(kernlab)
library(mlbench)
library(caretEnsemble)
library(nnet)
library(LiblineaR)
library(knitr)

##import FOMC statements from 2007 - 2021##

links<-c("https://www.federalreserve.gov/newsevents/pressreleases/monetary20210127a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20200129a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20200303a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20200315a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20200323a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20200429a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20200610a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20200729a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20200916a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20201105a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20201216a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20190130a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20190320a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20190501a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20190619a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20190731a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20190918a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20191030a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20191211a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20180131a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20180321a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20180502a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20180613a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20180801a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20180926a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20181108a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20181219a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20170201a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20170315a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20170503a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20170614a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20170726a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20170920a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20171101a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20171213a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20160127a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20160316a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20160427a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20160615a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20160727a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20160921a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20161102a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20161214a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20150128a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20150318a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20150429a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20150617a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20150729a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20150917a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20151028a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20151216a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20140129a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20140319a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20140430a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20140618a.htm", 
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20140730a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20140917a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20141029a.htm",  
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20141217a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20130130a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20130320a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20130501a.htm", 
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20130619a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20130731a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20130918a.htm",  
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20131030a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20131218a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20120125a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20120313a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20120425a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20120620a.htm", 
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20120801a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20120913a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20121024a.htm",  
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20121212a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20110126a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20110315a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20110427a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20110622a.htm", 
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20110809a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20110921a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20111102a.htm",  
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20111213a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20100127a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20100316a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20100428a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20100623a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20100810a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20100921a.htm",  
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20101103a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20101214a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20090128a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20090318a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20090429a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20090624a.htm", 
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20090812a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20090923a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20091104a.htm",  
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20091216a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20080122b.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20080130a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20080318a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20080430a.htm", 
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20080625a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20080805a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20080916a.htm",  
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20081008a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20081029a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20081216b.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20070131a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20070321a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20070509a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20070618a.htm", 
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20070807a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20070817b.htm",  
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20070918a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20071031a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20071211a.htm"
)
length(links)


##Prepare metadata for extraction and create dataframe##
##extract year of publication from statement release date, create data frame w date and URL ##

statement.dates<-NULL
year<-NULL
for(i in seq(from=1, to=length(links))) {
  statement.dates[i]<-(str_extract(links[i],"[[:digit:]]+"))
  year[i]<-substr(statement.dates[i],1,4)
}
reports<-data.frame(year,statement.dates, links)

# Convert factors to characters
reports %<>% mutate_if(is.factor, as.character)%>% arrange(statement.dates)


## Data extraction via web-scraping ##
## loop through statement links and scrape contents from Fed website ##
## Discard fluff from content like prelim paragraphs ##

statement.content<-NULL
statement.length<-NULL
for(i in seq(from=1, to=length(reports$links))) {
  stm.url<-getURL(reports$links[i])
  stm.tree<-htmlTreeParse(stm.url,useInternal=TRUE )
  stm.tree.parse<-unlist(xpathApply(stm.tree, path="//p", fun=xmlValue))
  n<-(which(!is.na(str_locate(stm.tree.parse, "release")))+1)[1]
  l<-length(stm.tree.parse)-1
  # Condense separate paragraphs into one element per statement date
  reports$statement.content[i]<-paste(stm.tree.parse[n:l], collapse = "")
  # Remove line breaks
  reports$statement.content[i]<-gsub("\r?\n|\r"," ",reports$statement.content[i])
  #reports$statement.content[i]<-gsub("\\.+\\;+\\,+","",reports$statement.content[i])
  # Count number of characters per statement
  reports$statement.length[i]<-nchar(reports$statement.content[i])
  #reports$statement.length[i]<-wordcount(reports$statement.content[i], sep = " ", count.function = sum)
}

# Create R data object
saveRDS(reports, file = "fomc_data.rds")

#data cleansing#
# Correct the date for one statement, because the URL is not in sync with the actual date inside the statement content
reports$statement.dates[match(c("20070618"),reports$statement.dates)]<-"20070628"

##merge fomc dataset with reports dataset##
d4<-readRDS(file = "fomc_data.rds")
dim(d4)
str(d4)

classificationFile = "https://raw.githubusercontent.com/completegraph/DATA607FINAL/master/Code/Classification_FOMC_Statements.csv"
cls = read_csv(classificationFile , col_types = cols( Date = col_character() ) )
cls %>% rename( Economic.Growth = "Economic Growth", Employment.Growth = "Employment Growth", Medium.Term.Rate = "Medium Term Rate", Policy.Rate = "Policy Rate") -> cls
str(cls)

#write.csv(reports, "Classification_FOMC_Statements.csv", row.names = FALSE)
