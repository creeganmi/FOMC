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

##custom stop words like "committee" that do not provide insight into sentiment##

words<-c("committee", "ben", "geithner", "bernanke", 
         "timothy", "hoenig", "thomas", "donald", "kevin", "mishkin", 
         "kroszner", "kohn", "charles", "frederic")

lexicon<-c("Custom")
my.stop_words<-data.frame(words, lexicon)
colnames(my.stop_words)<-c("word","lexicon")
new.stop_words <- rbind(my.stop_words, stop_words)
new.stop_words$word<-as.character(new.stop_words$word)
new.stop_words$lexicon<-as.character(new.stop_words$lexicon)
head(new.stop_words)

##cleanse data## remove punctuaction, white space, stop words, etc...##
report.words<-reports %>%
  mutate(date = statement.dates, year = year, text= statement.content) %>%
  unnest(text) %>% unnest_tokens(word, text) %>%
  mutate(word = stripWhitespace(gsub("[^A-Za-z ]"," ",word))) %>% 
  filter(word != "") %>% filter(word != " ") %>%
  anti_join(new.stop_words)%>% 
  count(date, year, word, sort = TRUE)%>% 
  mutate(frequency = n) %>% 
  select(date, year, word, frequency)

# Verify the count for the word "inflation" during the statements published in 2007 
report.words%>%filter(year=='2007', word=='inflation')

# Rank most frequent words by year
f_text<-report.words%>% group_by(year,word) %>% 
  summarize(total=sum(frequency))%>%
  arrange(year,desc(total),word)%>% 
  mutate(rank=row_number())%>%
  ungroup() %>% 
  arrange(rank,year)

# Select the top 10 ranked words per year
topWords <- f_text %>% 
  filter(rank<11)%>%
  arrange(year,rank)

print(topWords)

# Graph top 10 most frequent words by year
gg <- ggplot(head(topWords, 130), aes(y=total,x=reorder(word,rank))) + 
  geom_col(fill="#27408b") +
  facet_wrap(~year,scales="free", ncol=3)+ 
  coord_flip()+theme_ridges(font_size=11) + 
  labs(x="",y="",title="Most Frequent Words in FOMC Statements grouped by years (2007 - 2019)")

gg

##explore economic attributes##
mgData<-readRDS(file = "fomc_merged_data_v2.rds")

gEcon <- ggplot(data=mgData, aes(x=Economic.Growth, fill=Economic.Growth)) + 
  geom_bar() + theme(legend.position = "none")

gEmp  <- ggplot(data=mgData, aes(x=Employment.Growth, fill=Employment.Growth)) + 
  geom_bar() +  theme(legend.position = "none")

gInf  <- ggplot(data=mgData, aes(x=Inflation, fill=Inflation)) + 
  geom_bar() + theme(legend.position = "none")

gRate <- ggplot(data=mgData, aes(x=Medium.Term.Rate, fill=Medium.Term.Rate)) + 
  geom_bar() + theme(legend.position = "none")

gPolicy <- ggplot(data=mgData, aes(x=Policy.Rate, fill=Policy.Rate)) + 
  geom_bar() + theme(legend.position = "none")

grid.arrange(gEcon, gEmp, gInf, gRate, gPolicy, ncol=3, nrow=2 )

##correlation/covariation of attributes##

mgData %>% select(Economic.Growth:Policy.Rate) -> catData  # categorical data
cv = matrix(rep(0,25), nrow=5, ncol=5)
for(idx in 1:5){
  for(jdx in 1:5){
    cv[idx,jdx] = cramerV(catData[,idx], catData[,jdx])
  }
}
rownames( cv ) = colnames(catData)
colnames( cv ) = colnames(catData)
ggcorrplot(cv, lab=TRUE, ggtheme = ggplot2::theme_classic(), colors=c("violet", "white", "lightgreen")) +
  ggtitle("CramerV Matrix", subtitle="Classification Attributes Comparison")

##time series##

DGS10<-read.csv("https://raw.githubusercontent.com/DataScienceAR/Cuny-Assignments/master/Data-607/Data-Sets/DGS10.csv",stringsAsFactors = FALSE)
str(DGS10)

DGS10$DATE<- as_date(DGS10$DATE)
DGS10$DGS10<-as.numeric(DGS10$DGS10)

# Analysis of 10-Year Treasury Constant Maturity Rate

ggplot(data = DGS10)+
  aes(x=DATE,y=`DGS10`)+
  geom_line(size=.98,color="steelblue")+
  labs(x="Date",y="Percent",title="10 Year Constant Maturity Rate")+
  theme(panel.background = element_rect(fill = "white"))


# Analysis of Russell 3000Â® Total Market Index
RU3000TR<-read.csv("https://raw.githubusercontent.com/DataScienceAR/Cuny-Assignments/master/Data-607/Data-Sets/RU3000TR.csv",stringsAsFactors = FALSE)
str(RU3000TR)
RU3000TR$DATE<- as_date(RU3000TR$DATE)
RU3000TR$RU3000TR<-as.numeric(RU3000TR$RU3000TR)

ggplot(data = RU3000TR)+
  aes(x=DATE,y=`RU3000TR`)+
  geom_line(size=.98,color="steelblue")+
  labs(x="Date",y="Percent",title="Russell 3000Â® Total Market Index")+
  theme(panel.background = element_rect(fill = "white"))

# Analysis of Russell 1000Â® Total Market Index

RU1000TR<-read.csv("https://raw.githubusercontent.com/DataScienceAR/Cuny-Assignments/master/Data-607/Data-Sets/RU1000TR.csv",stringsAsFactors = FALSE)
str(RU1000TR)

RU1000TR$DATE<- as_date(RU1000TR$DATE)
RU1000TR$RU1000TR<-as.numeric(RU1000TR$RU1000TR)

ggplot(data = RU1000TR)+
  aes(x=DATE,y=`RU1000TR`)+
  geom_line(size=.98,color="steelblue")+
  labs(x="Date",y="Percent",title="Russell 1000Â® Total Market Index")+
  theme(panel.background = element_rect(fill = "white"))

# Analysis of Federal Funds Target Range

FEDTARGET<-read.csv("https://raw.githubusercontent.com/DataScienceAR/Cuny-Assignments/master/Data-607/Data-Sets/FEDTARGET.csv",stringsAsFactors = FALSE)
str(FEDTARGET)

FEDTARGET$DATE<- as.Date(strptime(FEDTARGET$DATE,format="%m/%d/%Y"),format="%Y-%m-%d")
FEDTARGET$Percent<-as.numeric(FEDTARGET$Percent)

ggplot(data = FEDTARGET)+
  aes(x=DATE,y=`Percent`,color=Type)+
  geom_line(size=.98)+
  labs(x="Date",y="Percent",title="Federal Funds Target Range")+
  theme(panel.background = element_rect(fill = "white"))


##Text Classification##
fomc_data <-readRDS(file = "fomc_merged_data_v2.rds")
head(select(fomc_data, Index,year,statement.dates,statement.length,date_mdy,
            Employment.Growth,Economic.Growth,Inflation,Medium.Term.Rate,Policy.Rate))

#ramdomize data#

set.seed(1234567)
fomc_Rand <- fomc_data[sample(nrow(fomc_data)),]
customStopWords <- c("the federal open market committee", "committee")

fomc_dataX <- fomc_Rand %>% 
  mutate(statement.content = tolower(statement.content))%>%
  mutate(statement.content = str_replace_all(statement.content, customStopWords, ""))

# form a corpus
corpus <- VCorpus(VectorSource(fomc_dataX$statement.content))
# Remove Punctuation
corpus <- tm_map(corpus, content_transformer(removePunctuation))
# Remove numbers
corpus <- tm_map(corpus, removeNumbers)
# Convert to lower case
corpus <- tm_map(corpus, content_transformer(tolower))
# Remove stop words
corpus <- tm_map(corpus, content_transformer(removeWords), stopwords("english"))
##Stemming
corpus <- tm_map(corpus, stemDocument)
# Remove Whitespace
corpus <- tm_map(corpus, stripWhitespace)
# Create Document Term Matrix
dtm <- DocumentTermMatrix(corpus)
# handle sparsity
corpusX <- removeSparseTerms(dtm, 0.30)
# convert to matrix
data_matrix <- as.matrix(corpusX)

#medium term rate classification#

mRate <- data_matrix
# attach the 'medium.term.rate' column
mRate_matrix <- cbind(mRate, fomc_dataX$Medium.Term.Rate)
# rename it to 'tone'
colnames(mRate_matrix)[ncol(mRate_matrix)] <- "tone"
# convert to data frame
mRateData <- as.data.frame(mRate_matrix)
# convert 'tone' to lower case and make it a factor column as well
mRateData$tone <- as.factor(tolower(mRateData$tone))

#partition into test and train data#
mRate_n <- nrow(mRateData)
mRateTrainVolume <- round(mRate_n * 0.7)
set.seed(314)
mRateTrainIndex <- sample(mRate_n, mRateTrainVolume)
mRateTrain <- mRateData[mRateTrainIndex,]
mRateTest <- mRateData[-mRateTrainIndex,]

names(mRateTrain)
##need to work on this model##
mRateModel <- train(tone ~., data = mRateTrain, method = 'svmLinear3')


##Sentiment Analysis##

fomcStatements <-readRDS(file = "fomc_merged_data_v2.rds") %>% 
  select(statement.dates, statement.content)

fomcX <- fomcStatements %>% 
  mutate(date = statement.dates, year = as.numeric(str_extract(statement.dates,'\\d{4}')),
         text= statement.content)%>%   
  select(date, year, text)

# Sentiment analysis with Loughran-Mcdonald dictionary

sentiment <- analyzeSentiment(fomcX$text, language = "english", aggregate = fomcX$year,
                              removeStopwords = TRUE, stemming = TRUE,
                              rules=list("SentimentLM"=list(ruleSentiment,
                                                            loadDictionaryLM())))

summary(sentiment)

# Table showing breakdown of Sentiments

table(convertToDirection(sentiment$SentimentLM))

# Line plot to visualize the evolution of sentiment scores. 

plotSentiment(sentiment, xlab="Tone")

Sentiment<-data.frame(fomcX$date,fomcX$year,sentiment$SentimentLM,
                      convertToDirection(sentiment$SentimentLM))

names(Sentiment)<-(c("FOMC_Date","FOMC_Year","Sentiment_Score","Sentiment"))
str(Sentiment)

# Change the date format to Ymd
Sentiment$FOMC_Date<- ymd(Sentiment$FOMC_Date)
Sentiment$FOMC_Year<- as.numeric(Sentiment$FOMC_Year)
str(Sentiment)

# Distribution of Sentiment Score for period of 2007 to 2019

ggplot(Sentiment,aes(x=Sentiment_Score))+
  geom_histogram(binwidth =.0125,color="black",fill="lightblue")+
  labs(x="Setiment Score",y="Frequency",title="Sentiment Score Distribution from 2007 to 2019")+
  theme(panel.background = element_rect(fill = "white"))

# Sentiment Score Trend

ggplot(data = Sentiment)+
  aes(x=FOMC_Date,y=Sentiment_Score)+
  geom_line(size=.98,color="firebrick")+
  labs(x="FOMC Date",y="Sentiment Score",title="Sentiment Score trend over the period of 2007 to 2019")+
  theme(panel.background = element_rect(fill = "white"))

# Scatter plot of score vs Date (Grouped)

ggplot(Sentiment,aes(x=FOMC_Date,y=Sentiment_Score,color=Sentiment))+
  geom_point()+
  labs(x="FOMC Date",y="Sentiment Score",title="Sentiments spread over the period of 2007 to 2019")+
  theme(panel.background = element_rect(fill = "white"))

# Exporting data frame to RDS
## Changing the Date format
Sentiment$FOMC_Date<-format(Sentiment$FOMC_Date, format = "%Y%m%d")
## Exporting to .RDS
saveRDS(Sentiment,"SentimentDF.rds")

##Financial Impact of Sentiment##

#load all data frames
mgData<-readRDS(file = "fomc_merged_data_v2.rds")
sData <- readRDS( file = "SentimentDF.rds")
file_fred_ru1000tr = "https://raw.githubusercontent.com/completegraph/DATA607FINAL/master/DATA/FRED_RU1000TR.csv"
ru1000tr = read_csv(file_fred_ru1000tr, 
                    col_types = cols(DATE=col_character(), 
                                     RU1000TR = col_double() ) )

# Generate a lubridate date column to join with the FOMC data.
# -----------------------------------------------------------------
ru1000tr %>% mutate( date_mdy = lubridate::ymd( DATE ) )-> ruData
#z_ru_daily = (RU1000TR - mean(RU1000TR, na.rm=TRUE))/sd(RU1000TR, na.rm = TRUE )
#  Second, join the data:
#  Since this is a 2-way inner join, we start with the FOMC statement data
#  and join it to the sentiment data by date string (yyyymmdd)
# -------------------------------------------------------------------------
mgData %>% inner_join(sData, by = c( "statement.dates" = "FOMC_Date")) -> msData
#  Join the sentiment-FOMC data to the Russell 1000 Index data from FRED
#  Make sure to add a Z-score for each of the time series: sentiment and Rusell index values
#     Save the raw data and normalized data by FOMC data.
# ----------------------------------------------------------------------------------
msEQdata = msData %>% left_join(ruData, by = c("date_mdy" = "date_mdy") ) %>% 
  select( date_mdy, Sentiment_Score, RU1000TR ) %>%
  mutate( z_ru_fomc = (RU1000TR - mean(RU1000TR, na.rm = TRUE) ) / sd( RU1000TR, na.rm=TRUE ) ,
          z_sentiment = ( Sentiment_Score - mean( Sentiment_Score, na.rm = TRUE) ) / 
            sd( Sentiment_Score, na.rm=TRUE) )

msEQdata %>% mutate( logEquity = log(RU1000TR) ) %>%
  mutate( z_logEquity = ( logEquity - mean(logEquity) )/ sd( logEquity ) ) -> msEQdata

msEQdata %>%  kable() %>% scroll_box(width="100%", height="200px")

#Sentiment vs Russell 1000
ggplot() + 
  geom_line(data=msEQdata, aes(x=date_mdy, y=Sentiment_Score) , color = "red" ) +
  geom_line(data=msEQdata, aes(x=date_mdy, y=RU1000TR), color="green") +
  ggtitle("Sentiment vs. Russell 1000 Equity Level", subtitle="Not usable without fixes")

ggplot() + 
  geom_line(data=msEQdata, aes(x=date_mdy, y=z_sentiment) , color = "red" ) +
  geom_line(data=msEQdata, aes(x=date_mdy, y=z_ru_fomc), color="green") +
  ggtitle("Scaled Sentiment vs. Scaled Equity Index", subtitle = "Nearly There...")

ggplot() + 
  geom_line(data=msEQdata, aes(x=date_mdy, y=z_sentiment) , color = "red" ) +
  geom_line(data=msEQdata, aes(x=date_mdy, y=z_logEquity), color="green") +
  ggtitle("Scaled-Sentiment vs. Scaled Log Equity Price")


