# course: CS5200 Database Management Systems
# semester: Spring 2023
# title: "Load XML"
# author: Xuebao Zhao(002108354), Yinan Hu(002108083)
# date: Apr 15 2023
library(RCurl)
library(xml2)
library(XML)
library(dplyr)
library(RSQLite)

# Load the xml file

# local method
# xmlFile <- "pubmed-tfm-xml/pubmed22n0001-tf.xml"
# xmlDom <- xmlParse(xmlFile, validate=T)

# url method
url <- "http://raw.githubusercontent.com/daisytoo/CS5200-Practicum-xml/main/pubmed22n0001-tf.xml"
xml <- read_xml(url,validate = T)
xmlDom <- xmlParse(xml)

root <- xmlRoot(xmlDom)
n = xmlSize(root)

# Create tables
# Connect to SQLite
dbcon <- dbConnect(RSQLite::SQLite(), "sqlite.db")
dbExecute(dbcon, "PRAGMA foreign_keys = ON")


# create Journals table
sql <- "DROP TABLE IF EXISTS Journals"
dbExecute(dbcon, sql)

sql <- paste0(
  "CREATE TABLE Journals (",
  "issn TEXT,",
  "title TEXT,",
  "isoAbbr TEXT,",
  "PRIMARY KEY (issn)",
  ")"
)
dbExecute(dbcon, sql)


# create Authors table
sql <- "DROP TABLE IF EXISTS Authors"
dbExecute(dbcon, sql)

sql <- paste0(
  "CREATE TABLE Authors (",
  "aid INTEGER,",
  "lastName TEXT,",
  "foreName TEXT,",
  "initials TEXT,",
  "suffix TEXT,",
  "PRIMARY KEY (aid)",
  ")"
)
dbExecute(dbcon, sql)

# create Articles table
sql <- "DROP TABLE IF EXISTS Articles"
dbExecute(dbcon, sql)

sql <- paste0(
  "CREATE TABLE Articles (",
  "pmid INTEGER,",
  "journal TEXT,",
  "articleTitle TEXT,",
  "volume INTEGER, ",
  "issue INTEGER,",
  "year TEXT,",
  "month TEXT,",
  "day TEXT,",
  "PRIMARY KEY (pmid),",
  "FOREIGN KEY (journal) REFERENCES Journals(issn)",
  ")"
)

dbExecute(dbcon, sql)

# create AuthorShips table
sql <- "DROP TABLE IF EXISTS AuthorShips"
dbExecute(dbcon, sql)

sql <- paste0(
  "CREATE TABLE AuthorShips (",
  "asid INTEGER,",
  "aid INTEGER,",
  "pmid INTEGER,",
  "PRIMARY KEY (asid),",
  "FOREIGN KEY (aid) REFERENCES Authors(aid),",
  "FOREIGN KEY (pmid) REFERENCES Articles(pmid)",
  ")"
)

# Store XML in tables
# Create data frames
author_df <- data.frame(aid = integer(),
                        lastName = character(), 
                        foreName = character(), 
                        initials = character(), 
                        suffix = character())
article_df <- data.frame(pmid = character(), 
                         journal = character(),
                         articleTitle = character(),
                         volume = integer(),
                         issue = integer(), 
                         year = character(), 
                         month = character(), 
                         day = character())
authorship_df <- data.frame(asid = integer(),
                            aid = integer(), 
                            pmid = integer())
journal_df <- data.frame(issn = character(),
                         title = character(), 
                         isoAbbr = character())
# Month conversion scheme
mon_dic <- c("Jan"="01", "Feb"="02", "Mar"="03", "Apr" = "04", "May" = "05", "Jun" = "06", "Jul" = "07", "Aug" = "08", "Sep" = "09", "Oct" = "10", "Nov" = "11", "Dec" = "12")


# Populate dataframes

# Journals dataframe
xpath_journal <- "./PubDetails/Journal/*"
xpath_journalOne <- "./PubDetails/Journal"
xpath_journalIssue <- "./PubDetails/Journal/JournalIssue"
xpath_pubDate <- "./PubDetails/Journal/JournalIssue/PubDate"

for (i in 1:n){
  node <- root[[i]]
  journal_attr <- xpathSApply(node, xpath_journal)
  journalOne_attr <- xpathSApply(node, xpath_journalOne)
  journalIssue_attr <- xpathSApply(node, xpath_journalIssue)
  pubDate_attr <- xpathSApply(node, xpath_pubDate)
  
  if (xmlName(journal_attr[[1]]) == "ISSN"){
    issn <- xmlValue(journal_attr[[1]]) 
    title <- xmlValue(journal_attr[[3]])
    isoAbbr <- xmlValue(journal_attr[[4]])
    
    journal_df <- rbind(journal_df, data.frame(issn = issn, title = title, isoAbbr = isoAbbr))
  }
}

# Remove duplicate items based on ISSN
journal_df <- journal_df %>% distinct(issn, .keep_all = TRUE)
# Insert a placeholder entry with ISSN 0000-0000 and unknown information for journals that have missing ISSN values
# for related articles whose corresponding journals have missing ISSN
journal_df <- rbind(journal_df, data.frame(issn = "0000-0000",title = "unknown", isoAbbr = "unknown"))
# head(journal_df, 10)

# Authors
xpath_author <- "./PubDetails/AuthorList/*"
for (i in 1:n){
  node <- root[[i]]
  author_attr <- xpathSApply(node, xpath_author)
  author_length <- length(author_attr)
  if(author_length > 0){
    for (j in 1:author_length){
      # Set missing suffix or initials to "None"
      if (length(author_attr[[j]]['Initials']) >0){
        initial <- xmlValue(author_attr[[j]]['Initials'][[1]])
      }
      else{
        initial <- "None"
      }
      if (length(author_attr[[j]]['Suffix']) >0){
        suffix <- xmlValue(author_attr[[j]]['Suffix'][[1]])
      }
      else{
        suffix <- "None"
      }
      # Discard the author that has both missing lastname and missing forename
      if (length(author_attr[[j]]['LastName']) > 0) {
        lastName <- xmlValue(author_attr[[j]]['LastName'][[1]])
      }
      else {
        lastName <- "None"
      }
      if (length(author_attr[[j]]['ForeName']) > 0) {
        foreName <- xmlValue(author_attr[[j]]['ForeName'][[1]])
      }
      else {
        foreName <- "None"
      }
      # Discard the author that has both missing lastname and missing forename
      author_df <- rbind(author_df, data.frame(lastName = lastName, foreName = foreName, initial = initial, suffix = suffix))
    }
  }
}

# Remove duplicate items based on the lastname and forename of authors
author_df <- author_df %>% distinct(lastName, foreName, .keep_all = TRUE)
# Fill out the identifier "aid" in author dateframe
author_df$aid <- seq(1, nrow(author_df))
author_df <- author_df[,c(5,1,2,3,4)]

# print(head(author_df, 20))

# Articles
xpath_articleTitle <- "./PubDetails/ArticleTitle"
for (i in 1:n){
  node <- root[[i]]
  pmid <- as.numeric(xmlAttrs(node))
  
  journal_attr <- xpathSApply(node, xpath_journal)
  journalOne_attr <- xpathSApply(node, xpath_journalOne)
  journalIssue_attr <- xpathSApply(node, xpath_journalIssue)
  pubDate_attr <- xpathSApply(node, xpath_pubDate)
  
  if (xmlName(journal_attr[[1]]) == "ISSN"){
    issn <- xmlValue(journal_attr[[1]]) 
    
    if (length(journalIssue_attr[[1]]['Volume']) > 0){
      volume <- xmlValue(journalIssue_attr[[1]]['Volume'][[1]])
    }
    else {
      volume <- NA
    }
    if (length(journalIssue_attr[[1]]['Issue']) > 0){
      issue <- xmlValue(journalIssue_attr[[1]]['Issue'][[1]])
    }
    else {
      issue <- NA
    }
    # Devise a conversion scheme and convert all dates to the scheme. Missing Month and Missing Date set to "00", Missing Year set to "0000"
    # MedlineDate

    # Year, Month and Day
    if (length(pubDate_attr[[1]]['Year']) > 0){
      year <- xmlValue(pubDate_attr[[1]]['Year'][[1]])
    }
    if (length(pubDate_attr[[1]]['Month']) > 0){
      month <- as.character(mon_dic[xmlValue(pubDate_attr[[1]]['Month'][[1]])])
    }
    else{
      month <- "00"
    }
    if (length(pubDate_attr[[1]]['Day']) > 0){
      day <- xmlValue(pubDate_attr[[1]]['Day'][[1]])
    }
    else{
      day <- "00"
    }
    if (length(pubDate_attr[[1]]['MedlineDate']) > 0){
      medlineDate <- xmlValue(pubDate_attr[[1]]['MedlineDate'][[1]])
      year <- as.character(substr(medlineDate,1,4))
      month <- as.character(mon_dic[substr(medlineDate,6,8)])
      day <- "00"
    }
  }
  else{
    # Relate articles whose corresponding journals have missing ISSN to the dummy journal with ISSN "0000-0000"
    issn <- "0000-0000"
  }
  
  articleTitle <- xmlValue(xpathSApply(node, xpath_articleTitle))
  article_df <- rbind(article_df, data.frame(pmid = pmid, journal = issn,
                                             volume = volume, issue = issue,
                                             year = year, month = month,
                                             day = day, articleTitle = articleTitle))
}
# print(head(article_df, 5))

# Authorships
for (i in 1:n){
  node <- root[[i]]
  pmid <- as.numeric(xmlAttrs(node))
  author_attr <- xpathSApply(node, xpath_author)
  author_length <- length(author_attr)
  # If the article doesn't have a corresponding author in its AuthorList, then set the "aid" as NA
  if(author_length <= 0){
    authorship_df  <- rbind(authorship_df, data.frame(aid = NA, pmid = pmid))
  }
  else{
    for(j in 1:author_length){
      # Discard the author that has both missing lastname and missing forename
      if (length(author_attr[[j]]['LastName']) > 0) {
        lastName <- xmlValue(author_attr[[j]]['LastName'][[1]])
      }
      else {
        lastName <- "None"
      }
      if (length(author_attr[[j]]['ForeName']) > 0) {
        foreName <- xmlValue(author_attr[[j]]['ForeName'][[1]])
      }
      else {
        foreName <- "None"
      }
      aid <- author_df[author_df$lastName == lastName & author_df$foreName == foreName,]$aid
      # Populate the authorship dataframe based on the pmid of the articles and forename, lastname of the authors in the author lists
      authorship_df  <- rbind(authorship_df, data.frame(aid = aid, pmid = pmid))
    }
  }
}

# Remove duplicate items based on the pmid and aid
authorship_df <- authorship_df %>% distinct(pmid, aid, .keep_all = TRUE)
# Fill out the identifier "asid" in authorship dateframe
authorship_df$asid <- seq(1, nrow(authorship_df))
authorship_df <- authorship_df[,c(3,1,2)]

# print(head(authorship_df, 20))

# Write data to database
dbWriteTable(dbcon,"Journals", journal_df, overwrite = T)
dbWriteTable(dbcon,"Authors", author_df, overwrite = T)
dbWriteTable(dbcon,"Articles", article_df, overwrite = T)
dbWriteTable(dbcon,"Authorships", authorship_df, overwrite = T)

# Test stored data in database
# query test
dbGetQuery(dbcon, "select * from Journals limit 100;")
dbGetQuery(dbcon, "select * from Authors limit 100;")
dbGetQuery(dbcon, "select * from Articles limit 100;")
dbGetQuery(dbcon, "select * from Authorships limit 100;")

# Disconnect
dbDisconnect(dbcon)