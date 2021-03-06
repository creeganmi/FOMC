## Author: Michael Creegan ##
## Date: March 4, 2021 ##

###############################################
### FOMC Sentiment Analysis & Insights / NLP ##
###############################################

library(magrittr)


read.table(
  text = system("openssl ciphers -v", intern=TRUE) %>% 
    gsub("[[:alpha:]]+=", "", .)
) %>% 
  setNames(
    c("ciphername", "protoccol_version", "key_exchange", "authentication", 
      "symmetric_encryption_method", "message_authentication_code")
  )

##import libraries##

library(dplyr)
library(SentimentAnalysis)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)
library(rlang)
library(tidyverse)
library(tidytext)
#library(xlsx)
library(readxl)
library(openxlsx)
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

##import FOMC statements from 2007 - forward##


links<-c("https://www.federalreserve.gov/newsevents/pressreleases/monetary20190130a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20190320a.htm",
         "https://www.federalreserve.gov/newsevents/pressreleases/monetary20190501a.htm",
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

library(httr)
#httr_config <- config(ssl_cipher_list = "DEFAULT@SECLEVEL=1")
#res <- with_config(config = httr_config, GET(reports$links[2]))

statement.content<-NULL
statement.length<-NULL
for(i in seq(from=1, to=length(reports$links))) {
  stm.url<-content(GET(reports$links[i]))
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

str(reports)

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

#merge the FOMC data and classification data#

d4 %>% inner_join( cls , by = c("statement.dates" = "Date")) %>%
  mutate( date_mdy = mdy(Date2)) %>%
  select(Index, 
         year ,
         statement.dates, 
         links, 
         statement.content, 
         statement.length ,
         date_mdy,
         Economic.Growth,
         Employment.Growth,
         Inflation,
         Medium.Term.Rate,
         Policy.Rate ) -> mgData
str(mgData)

mgData %>% select( Index, date_mdy, Economic.Growth, Employment.Growth, Inflation, Medium.Term.Rate, Policy.Rate) %>% kable() %>% kable_styling(bootstrap_options = c("hover", "striped")) %>%
  scroll_box(width = "90%", height = "300px")

## export the merged data frame! ##
rds_filename = "fomc_merged_data_v2.rds"
saveRDS(mgData, file = rds_filename)


## EXPLORATORY ANALYSIS ##
##Analyze FOMC statement word lengths and frequency##

# Compute total statement length per year by aggregating across individual statements
yearly.length<-reports%>% group_by(year) %>% summarize(words.per.year=sum(statement.length))
yearly.length

ggplot(yearly.length, aes(x=yearly.length$year,y=yearly.length$words.per.year))+
  geom_bar(stat="identity",fill="darkblue", colour="black") + 
  coord_flip()+xlab("Year")+ylab("Statement Length")

sample<-reports%>%filter(reports$statement.dates=="20140319")
sample[,4]


str_count(sample, pattern="inflation")

p<-ggplot(reports, aes(x=year,y=statement.length))+
  geom_point(stat="identity",color=statement.dates)+
  scale_fill_brewer(palette="Pastel1")+
  theme(legend.position="right")+xlab("Year") + ylab("Length of Statement")

p

p + ggplot2::annotate("text", x = 4,y = 5000, 
                      label = "Bernanke", family="serif", fontface="bold", 
                      colour="blue", size=4)+
  ggplot2::annotate("text", x=10, y=5500, label="Yellen", family="serif", fontface="bold", 
                    colour="darkred",size=4)+
  ggplot2::annotate("text", x=13, y=3600, label="Powell", family="serif", 
                    fontface="bold", colour="black",size=4)+
  ggplot2::annotate("segment", x = 0, xend = 8.1, y = 2700, yend = 6500, colour = "blue", 
                    size=1, arrow=arrow(ends="both"))+
  ggplot2::annotate("segment", x = 8.1, xend = 12.1, y = 6500, yend = 3200, 
                    colour = "darkred", size=1, arrow=arrow(ends="both"))+
  ggplot2::annotate("segment", x = 12.1, xend = 14, y = 3200, yend = 3200,
                    colour = "black", size=1, arrow=arrow(ends="both"))
