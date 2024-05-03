# course: CS5200 Database Management Systems
# semester: Spring 2023
# title: "Load DataWarehouse"
# author: Xuebao Zhao(002108354), Yinan Hu(002108083)
# date: Apr 15 2023

library(RMySQL)
library(RSQLite)
library(dplyr)
warnings('off')
# Create a local MySQL database and connect to it
db_user <- '5200'

# delare proper credentials
db_password <- 'password'
db_name <- '5200_P1'

db_host <- 'localhost'
db_port <- 3306
dbcon <-  dbConnect(MySQL(), user = db_user, password = db_password,
                    dbname = db_name, host = db_host, port = db_port)

# Drop table if exists
sql <- "DROP TABLE IF EXISTS JournalFact"
dbExecute(dbcon, sql)
sql <- "DROP TABLE IF EXISTS TimeDimension"
dbExecute(dbcon, sql)

# Create TimeDimension table
sql <- "CREATE TABLE TimeDimension(
          tid INTEGER PRIMARY KEY AUTO_INCREMENT,
          year INTEGER NOT NULL,
          month INTEGER NOT NULL
        )"
dbExecute(dbcon, sql)

# Create JournalFact table
sql <- "CREATE TABLE JournalFact(
          jfid INTEGER PRIMARY KEY AUTO_INCREMENT,
          issn TEXT NOT NULL,
          tid INTEGER NOT NULL,
          numOfArticle INTEGER NOT NULL,
          numOfAuthor INTEGER NOT NULL,
          FOREIGN KEY (tid) REFERENCES TimeDimension(tid)
        )"
dbExecute(dbcon, sql)

# Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), "sqlite.db")

# Retrieve the necessary data from the Articles table in the SQLite database
query_retrieve_data <- "
  SELECT DISTINCT year, month
    FROM Articles;
"
distinct_year_month <- dbGetQuery(con, query_retrieve_data)

# Insert the retrieved data into the TimeDimension table in the MySQL database
query_insert_data <- "
  INSERT INTO TimeDimension (year, month)
  VALUES (
"
for (i in 1:nrow(distinct_year_month)) {
  if (is.na(distinct_year_month$month[i]))
    distinct_year_month$month[i] <- 0
  
  final_insert <- paste0(query_insert_data, distinct_year_month$year[i], ", ", distinct_year_month$month[i],");")
  dbExecute(dbcon, final_insert)
}

# Populate JournalFact table
query_journal_fact <- "
  WITH JournalYearMonth AS (
    SELECT j.issn, j.title, ar.year, ar.month, ar.pmid
    FROM Journals j
    JOIN Articles ar ON j.issn = ar.journal
  ),
  YearMonthID AS (
    SELECT DISTINCT year, month
    FROM Articles
  ),
  ArticleCount AS (
    SELECT issn, year, month, COUNT(pmid) as numOfArticle
    FROM JournalYearMonth
    GROUP BY issn, year, month
  ),
  UniqueAuthorCount AS (
    SELECT j.issn, j.year, j.month, COUNT(DISTINCT ash.aid) as numOfAuthor
    FROM JournalYearMonth j
    JOIN AuthorShips ash ON j.pmid = ash.pmid
    GROUP BY j.issn, j.year, j.month
  )
  SELECT ac.issn AS issn, ym.year AS year, ym.month AS month, ac.numOfArticle AS numOfArticle, uac.numOfAuthor AS numOfAuthor
  FROM ArticleCount ac
  JOIN UniqueAuthorCount uac ON ac.issn = uac.issn AND ac.year = uac.year AND ac.month = uac.month
  JOIN YearMonthID ym ON ac.year = ym.year AND ac.month = ym.month;
"
journal_data <- dbGetQuery(con, query_journal_fact)

#=================Hashmap=================
# Retrieve the necessary data from the Articles table in the SQLite database
query_retrieve_data <- "
  SELECT tid, year, month
    FROM TimeDimension;
"
td <- dbGetQuery(dbcon, query_retrieve_data)

# Create a new column 'year_month' to be used as a key in the hashmap
td$year_month <- paste(td$year, td$month, sep = "-")

# Create a named list (hashmap) with the 'year_month' column as keys and 'tid' column as values
year_month_hashmap <- as.list(setNames(td$tid, td$year_month))

n <- nrow(journal_data)
for (i in 1:n) {
  currdate <- paste(journal_data$year[i], journal_data$month[i], sep = "-")
  currdate <- gsub("-0", "-", currdate)
  journal_data$year[i] <- year_month_hashmap[[currdate]]
}
journal_data$month <- NULL
colnames(journal_data)[colnames(journal_data) == "year"] <- "tid"
journal_data$tid <- as.numeric(journal_data$tid)

for (i in 1:n) {
  query <- sprintf("INSERT INTO JournalFact (issn, tid, numOfArticle, numOfAuthor) VALUES ('%s', %d, %d, %d)",
                   journal_data$issn[i], journal_data$tid[i], journal_data$numOfArticle[i], journal_data$numOfAuthor[i])
  dbExecute(dbcon, query)
}

# Add yearly data to the TimeDimension table
query_add_yearly_data <- "
  INSERT INTO TimeDimension (year, month)
  SELECT DISTINCT year, 13
  FROM TimeDimension;
"
dbExecute(dbcon, query_add_yearly_data)

# Add quarterly data to the TimeDimension table
query_add_quarterly_data <- "
  INSERT INTO TimeDimension (year, month)
  SELECT DISTINCT year, 14
  FROM TimeDimension
  WHERE month >= 1 AND month <= 3
  UNION
  SELECT DISTINCT year, 15
  FROM TimeDimension
  WHERE month >= 4 AND month <= 6
  UNION
  SELECT DISTINCT year, 16
  FROM TimeDimension
  WHERE month >= 7 AND month <= 9
  UNION
  SELECT DISTINCT year, 17
  FROM TimeDimension
  WHERE month >= 10 AND month <= 12;
"
dbExecute(dbcon, query_add_quarterly_data)

# Aggregate yearly data
query_yearly <- "
INSERT INTO JournalFact (issn, tid, numOfArticle, numOfAuthor)
SELECT issn, t_year.tid, SUM(numOfArticle) as total_numOfArticle, SUM(numOfAuthor) as total_numOfAuthor
FROM JournalFact jf
JOIN TimeDimension td ON jf.tid = td.tid
JOIN TimeDimension t_year ON td.year = t_year.year AND t_year.month = 13
GROUP BY issn, t_year.tid;
"
dbExecute(dbcon, query_yearly)

# Aggregate quarterly data

# Quarter1
query_quarter1 <- "
INSERT INTO JournalFact (issn, tid, numOfArticle, numOfAuthor)
SELECT issn, t_quarter.tid, SUM(numOfArticle) as total_numOfArticle, SUM(numOfAuthor) as total_numOfAuthor
FROM JournalFact jf
JOIN TimeDimension td ON jf.tid = td.tid
JOIN TimeDimension t_quarter ON td.year = t_quarter.year AND 
    (t_quarter.month = 14 AND td.month BETWEEN 1 AND 3)
GROUP BY issn, t_quarter.tid;
"
dbExecute(dbcon, query_quarter1)

# Quarter2
query_quarter2 <- "
INSERT INTO JournalFact (issn, tid, numOfArticle, numOfAuthor)
SELECT issn, t_quarter.tid, SUM(numOfArticle) as total_numOfArticle, SUM(numOfAuthor) as total_numOfAuthor
FROM JournalFact jf
JOIN TimeDimension td ON jf.tid = td.tid
JOIN TimeDimension t_quarter ON td.year = t_quarter.year AND 
    (t_quarter.month = 15 AND td.month BETWEEN 4 AND 6)
GROUP BY issn, t_quarter.tid;
"
dbExecute(dbcon, query_quarter2)

# Quarter3
query_quarter3 <- "
INSERT INTO JournalFact (issn, tid, numOfArticle, numOfAuthor)
SELECT issn, t_quarter.tid, SUM(numOfArticle) as total_numOfArticle, SUM(numOfAuthor) as total_numOfAuthor
FROM JournalFact jf
JOIN TimeDimension td ON jf.tid = td.tid
JOIN TimeDimension t_quarter ON td.year = t_quarter.year AND 
    (t_quarter.month = 16 AND td.month BETWEEN 7 AND 9)
GROUP BY issn, t_quarter.tid;
"
dbExecute(dbcon, query_quarter3)

# Quarter4
query_quarter4 <- "
INSERT INTO JournalFact (issn, tid, numOfArticle, numOfAuthor)
SELECT issn, t_quarter.tid, SUM(numOfArticle) as total_numOfArticle, SUM(numOfAuthor) as total_numOfAuthor
FROM JournalFact jf
JOIN TimeDimension td ON jf.tid = td.tid
JOIN TimeDimension t_quarter ON td.year = t_quarter.year AND 
    (t_quarter.month = 17 AND td.month BETWEEN 10 AND 12)
GROUP BY issn, t_quarter.tid;
"
dbExecute(dbcon, query_quarter4)

#====================Test========================
#What the are number of articles published in every journal in 1976 and 1978
sql_query1<- "
SELECT jf.issn, SUM(jf.numOfArticle) as total_numOfArticle
FROM JournalFact jf
JOIN TimeDimension td ON jf.tid = td.tid
WHERE td.year IN (1976, 1978)
GROUP BY jf.issn;
"

articles_data <- dbGetQuery(dbcon, sql_query1)
print(articles_data)

dbDisconnect(con)
dbDisconnect(dbcon)