#Read in data

df.master <- read.csv('/Users/markbowren/Documents/uiowa/core/data/master.csv')

#Add leading zeros to id numbers with 3 digits

df.master.1digit.ids <- subset(df.master, RedID < 10)

df.master.2digit.ids <- subset(df.master, RedID < 100 & RedID >= 10)

df.master.3digit.ids <- subset(df.master, RedID < 1000 & RedID >= 100)

df.master.4digit.ids <- subset(df.master, RedID >= 1000)

df.master.1digit.ids$RedID <- paste0(0, 0, 0, df.master.1digit.ids$RedID)

df.master.2digit.ids$RedID <- paste0(0, 0, df.master.2digit.ids$RedID)

df.master.3digit.ids$RedID <- paste0(0, df.master.3digit.ids$RedID)

df.master <- rbind(df.master.1digit.ids, df.master.2digit.ids, df.master.3digit.ids, df.master.4digit.ids)

#Parse down based on inclusion/exclusion criteria

#Only stroke, TBI, or tumor resection (account for spelling errors in documentation)

df.core <- subset(df.master,
                  ETIOLOGY == "Encephalitis" | ETIOLOGY == "ENCEPHALITIS" | ETIOLOGY == "Head Trauma" | ETIOLOGY == "HEAD TRAUMA" | ETIOLOGY == "HEAD Trrauma" | ETIOLOGY == "resection" | ETIOLOGY == "Resection" | ETIOLOGY == "RESECTION" | ETIOLOGY == "RESECTION " | ETIOLOGY == "SAH" | ETIOLOGY == "Stroke" | ETIOLOGY == "STROKE" | ETIOLOGY == "STROKE ")

#Specify subetiologies

df.core <- subset(df.core,
                  SUBETIOLOGY == 'closed' | SUBETIOLOGY == 'Closed' | SUBETIOLOGY == 'CLOSED' | SUBETIOLOGY == 'HEMORRHAGE' | SUBETIOLOGY == 'hemorrhagic' | SUBETIOLOGY == 'Hemorrhagic' | SUBETIOLOGY == 'HEMORRHAGIC' | SUBETIOLOGY == 'HSE' | SUBETIOLOGY == 'Ischemic' | SUBETIOLOGY == 'ISCHEMIC' | SUBETIOLOGY == 'Penetrating' | SUBETIOLOGY == 'PENETRATING' | SUBETIOLOGY == 'tumor' | SUBETIOLOGY == 'TUMOR')

#Sort by ID

df.core <- df.core[order(df.core$RedID), ]

#Set scan dates to missing values when the associated type of scan is not MRI (TYPE = V or M) or CT (TYPE = C or T)

df.core.scan.1 <- subset(df.core, select = c('RedID', 'SCAN1', 'TYPE1'))

df.core.scan.1 <- df.core.scan.1[order(df.core.scan.1$RedID), ]

table(df.core.scan.1$TYPE1)

df.core.scan.1$SCAN1[df.core.scan.1$TYPE1 == 'E'] <- ''

df.core.scan.1$SCAN1[df.core.scan.1$TYPE1 == 'O'] <- ''

df.core.scan.1$SCAN1[df.core.scan.1$TYPE1 == 'P'] <- ''

df.core.scan.1$SCAN1[df.core.scan.1$TYPE1 == 'R'] <- ''

df.core.scan.1$SCAN1[df.core.scan.1$TYPE1 == 'S'] <- ''

df.core.scan.2 <- subset(df.core, select = c('RedID', 'SCAN2', 'TYPE2'))

df.core.scan.2 <- df.core.scan.2[order(df.core.scan.2$RedID), ]

table(df.core.scan.2$TYPE2)

df.core.scan.2$SCAN2[df.core.scan.2$TYPE2 == 'E'] <- ''

df.core.scan.2$SCAN2[df.core.scan.2$TYPE2 == 'O'] <- ''

df.core.scan.2$SCAN2[df.core.scan.2$TYPE2 == 'P'] <- ''

df.core.scan.2$SCAN2[df.core.scan.2$TYPE2 == 'R'] <- ''

df.core.scan.2$SCAN2[df.core.scan.2$TYPE2 == 'S'] <- ''

df.core.scan.2$SCAN2[df.core.scan.2$TYPE2 == 'X'] <- ''

df.core.scan.3 <- subset(df.core, select = c('RedID', 'SCAN3', 'TYPE3'))

df.core.scan.3 <- df.core.scan.3[order(df.core.scan.3$RedID), ]

table(df.core.scan.3$TYPE3)

df.core.scan.3$SCAN3[df.core.scan.3$TYPE3 == 'E'] <- ''

df.core.scan.3$SCAN3[df.core.scan.3$TYPE3 == 'O'] <- ''

df.core.scan.3$SCAN3[df.core.scan.3$TYPE3 == 'P'] <- ''

df.core.scan.3$SCAN3[df.core.scan.3$TYPE3 == 'R'] <- ''

df.core.scan.3$SCAN3[df.core.scan.3$TYPE3 == 'S'] <- ''

df.core.scan.4 <- subset(df.core, select = c('RedID', 'SCAN4', 'TYPE4'))

df.core.scan.4 <- df.core.scan.4[order(df.core.scan.4$RedID), ]

table(df.core.scan.4$TYPE4)

df.core.scan.4$SCAN4[df.core.scan.4$TYPE4 == 'E'] <- ''

df.core.scan.4$SCAN4[df.core.scan.4$TYPE4 == 'O'] <- ''

df.core.scan.4$SCAN4[df.core.scan.4$TYPE4 == 'P'] <- ''

df.core.scan.4$SCAN4[df.core.scan.4$TYPE4 == 'R'] <- ''

df.core.scan.4$SCAN4[df.core.scan.4$TYPE4 == 'S'] <- ''

df.core.scan.5 <- subset(df.core, select = c('RedID', 'SCAN5', 'TYPE5'))

df.core.scan.5 <- df.core.scan.5[order(df.core.scan.5$RedID), ]

table(df.core.scan.5$TYPE5)

df.core.scan.5$SCAN5[df.core.scan.5$TYPE5 == 'E'] <- ''

df.core.scan.5$SCAN5[df.core.scan.5$TYPE5 == 'O'] <- ''

df.core.scan.5$SCAN5[df.core.scan.5$TYPE5 == 'P'] <- ''

df.core.scan.5$SCAN5[df.core.scan.5$TYPE5 == 'R'] <- ''

df.core.scan.5$SCAN5[df.core.scan.5$TYPE5 == 'S'] <- ''

df.core.scan.6 <- subset(df.core, select = c('RedID', 'SCAN6', 'TYPE6'))

df.core.scan.6 <- df.core.scan.6[order(df.core.scan.6$RedID), ]

table(df.core.scan.6$TYPE6)

df.core.scan.6$SCAN6[df.core.scan.6$TYPE6 == 'E'] <- ''

df.core.scan.6$SCAN6[df.core.scan.6$TYPE6 == 'O'] <- ''

df.core.scan.6$SCAN6[df.core.scan.6$TYPE6 == 'P'] <- ''

df.core.scan.6$SCAN6[df.core.scan.6$TYPE6 == 'R'] <- ''

df.core.scan.6$SCAN6[df.core.scan.6$TYPE6 == 'S'] <- ''

#Create a dataframe of all dates together so you can take max value as most recent scan (which we assume is the scan used for the analysis)

df.core.scan.dates <- cbind(df.core.scan.1, df.core.scan.2, df.core.scan.3, df.core.scan.4, df.core.scan.5, df.core.scan.6)

df.core.scan.dates <- df.core.scan.dates[order(df.core.scan.dates$RedID), ]

df.core.scan.dates <- subset(df.core.scan.dates, select = c('RedID', 'SCAN1', 'SCAN2', 'SCAN3', 'SCAN4', 'SCAN5', 'SCAN6'))

df.core.scan.dates <- df.core.scan.dates[order(df.core.scan.dates$RedID), ]

df.core.scan.dates <- df.core.scan.dates[,-1]

df.core.scan.dates$max.date <- apply(df.core.scan.dates, 1, max)

df.core$scan.date <- df.core.scan.dates$max.date

#Create a function to test missing values

percent.missing <- function (x){
  sum(is.na(x)/length(x) * 100)
}

df.core[df.core == ''] <- NA

percent.missing(df.core$RedID)

percent.missing(df.core$scan.date)

nrow(df.core)

#Create a column for each test that is difference between administration date and scan date (negatives indicate scan after test which is bad)

df.core$date.diff.tmta <- as.numeric(as.Date(df.core$ef.date) - as.Date(df.core$scan.date))

df.core$date.diff.tmtb <- as.numeric(as.Date(df.core$ef.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.r.sim <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.3.sim <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.iv.sim <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.r.inf <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.3.inf <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.iv.inf <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.r.cod <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.3.cod <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.iv.cod <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.r.bd <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.3.bd <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.iv.bd <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.r.dig <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.3.dig <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.iv.dig <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.r.ari <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.3.ari <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wais.iv.ari <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wrat.r <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.wrat.iv <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.bnt <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.rey1 <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.rey2 <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.rey3 <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.rey4 <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.rey5 <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.reyr <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.reyh <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.cftc <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.cftr <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.vrtc <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.vrte <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.face <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.jlo <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.bdi <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.bai <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.mmpid <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.mmpihs <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.mmpihy <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.mmpipd <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.mmpipa <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.mmpipt <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.mmpisc <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.mmpima <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

df.core$date.diff.mmpisi <- as.numeric(as.Date(df.core$core.date) - as.Date(df.core$scan.date))

#Create a date column for every test

df.core$tmta.date <- df.core$ef.date

df.core$tmtb.date <- df.core$ef.date

df.core$wais.r.sim.date <- df.core$core.date

df.core$wais.3.sim.date <- df.core$core.date

df.core$wais.iv.sim.date <- df.core$core.date

df.core$wais.r.inf.date <- df.core$core.date

df.core$wais.3.inf.date <- df.core$core.date

df.core$wais.iv.inf.date <- df.core$core.date

df.core$wais.r.cod.date <- df.core$core.date

df.core$wais.3.cod.date <- df.core$core.date

df.core$wais.iv.cod.date <- df.core$core.date

df.core$wais.r.bd.date <- df.core$core.date

df.core$wais.3.bd.date <- df.core$core.date

df.core$wais.iv.bd.date <- df.core$core.date

df.core$wais.r.dig.date <- df.core$core.date

df.core$wais.3.dig.date <- df.core$core.date

df.core$wais.iv.dig.date <- df.core$core.date

df.core$wais.r.dig.date <- df.core$core.date

df.core$wais.3.dig.date <- df.core$core.date

df.core$wais.iv.dig.date <- df.core$core.date

df.core$wrat.r.date <- df.core$core.date

df.core$wrat.iv.date <- df.core$core.date

df.core$bnt.date <- df.core$core.date

df.core$rey1.date <- df.core$core.date

df.core$rey2.date <- df.core$core.date

df.core$rey3.date <- df.core$core.date

df.core$rey4.date <- df.core$core.date

df.core$rey5.date <- df.core$core.date

df.core$reyr.date <- df.core$core.date

df.core$reyh.date <- df.core$core.date

df.core$cftc.date <- df.core$core.date

df.core$cftr.date <- df.core$core.date

df.core$vrtc.date <- df.core$core.date

df.core$vrte.date <- df.core$core.date

df.core$face.date <- df.core$core.date

df.core$jlo.date <- df.core$core.date

df.core$bdi.date <- df.core$core.date

df.core$bai.date <- df.core$core.date

df.core$mmpid.date <- df.core$core.date

df.core$mmpihs.date <- df.core$core.date

df.core$mmpihy.date <- df.core$core.date

df.core$mmpipd.date <- df.core$core.date

df.core$mmpipa.date <- df.core$core.date

df.core$mmpipt.date <- df.core$core.date

df.core$mmpisc.date <- df.core$core.date

df.core$mmpima.date <- df.core$core.date

df.core$mmpisi.date <- df.core$core.date

#Create a vector IDs and remove duplicates

id.list <- df.core$RedID

id.list <- unique(id.list)

length(id.list)

#For loop to create a list populated with all of the dataframes for each subject indexed by ID

subject.dfs <- list()

for(i in id.list){
  subject.dfs[[i]] <- as.data.frame(df.core[df.core$RedID == i,])
}

#Subset the subject.dfs list of dataframes for rows in each dataframe that are considered post scan for the core part of the battery (i.e., core.date) as defined by the above function is.postScan(). Create a new list subject.dfs.core.
#I reference rey.5 because I needed a date diff vector and all of the tests in the core part of the csv file have the same pattern of dates and thus the same pattern of diff dates, they are interchangeable. 

subject.dfs.core <- list()

for(i in id.list){
  subject.dfs.core[[i]] <- unique(subset(subject.dfs[[i]], date.diff.rey5 >= -5000000))
}

#Remove IDs from id.list that have nrow = 0

for(i in id.list){
  if(nrow(subject.dfs.core[[i]]) == 0){
    id.position <- match(i, id.list)
    id.list <- id.list[-id.position]
  }
}

id.list.core <- id.list

length(id.list.core)

#Select most contemporaneous tests

#WAIS R Similarities

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.r.sim), ]
  subject.dfs.core[[i]]$wais.r.sim.final <- NA
  subject.dfs.core[[i]]$wais.r.sim.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.r.sim.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.r.sim.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.R.SIM)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.R.SIM) == 'FALSE'){
      subject.dfs.core[[i]]$wais.r.sim.final <- rep(subject.dfs.core[[i]][j,]$WAIS.R.SIM, length(subject.dfs.core[[i]]$wais.r.sim.final))
      subject.dfs.core[[i]]$wais.r.sim.date.final <- rep(subject.dfs.core[[i]][j,]$wais.r.sim.date, length(subject.dfs.core[[i]]$wais.r.sim.date.final))
      subject.dfs.core[[i]]$date.diff.wais.r.sim.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.r.sim, length(subject.dfs.core[[i]]$date.diff.wais.r.sim.final))
    } else {
      subject.dfs.core[[i]]$wais.r.sim.final <- NA
      subject.dfs.core[[i]]$wais.r.sim.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.r.sim.final <- NA
    }
    j <- j + 1
  }
}

#WAIS 3 Similarities

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.3.sim), ]
  subject.dfs.core[[i]]$wais.3.sim.final <- NA
  subject.dfs.core[[i]]$wais.3.sim.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.3.sim.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.3.sim.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.3.SIM)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.3.SIM) == 'FALSE'){
      subject.dfs.core[[i]]$wais.3.sim.final <- rep(subject.dfs.core[[i]][j,]$WAIS.3.SIM, length(subject.dfs.core[[i]]$wais.3.sim.final))
      subject.dfs.core[[i]]$wais.3.sim.date.final <- rep(subject.dfs.core[[i]][j,]$wais.3.sim.date, length(subject.dfs.core[[i]]$wais.3.sim.date.final))
      subject.dfs.core[[i]]$date.diff.wais.3.sim.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.3.sim, length(subject.dfs.core[[i]]$date.diff.wais.3.sim.final))
    } else {
      subject.dfs.core[[i]]$wais.3.sim.final <- NA
      subject.dfs.core[[i]]$wais.3.sim.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.3.sim.final <- NA
    }
    j <- j + 1
  }
}

#WAIS IV Similarities

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.iv.sim), ]
  subject.dfs.core[[i]]$wais.iv.sim.final <- NA
  subject.dfs.core[[i]]$wais.iv.sim.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.iv.sim.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.iv.sim.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.IV.SIM)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.IV.SIM) == 'FALSE'){
      subject.dfs.core[[i]]$wais.iv.sim.final <- rep(subject.dfs.core[[i]][j,]$WAIS.IV.SIM, length(subject.dfs.core[[i]]$wais.iv.sim.final))
      subject.dfs.core[[i]]$wais.iv.sim.date.final <- rep(subject.dfs.core[[i]][j,]$wais.iv.sim.date, length(subject.dfs.core[[i]]$wais.iv.sim.date.final))
      subject.dfs.core[[i]]$date.diff.wais.iv.sim.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.iv.sim, length(subject.dfs.core[[i]]$date.diff.wais.iv.sim.final))
    } else {
      subject.dfs.core[[i]]$wais.iv.sim.final <- NA
      subject.dfs.core[[i]]$wais.iv.sim.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.iv.sim.final <- NA
    }
    j <- j + 1
  }
}

#WAIS R Information

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.r.inf), ]
  subject.dfs.core[[i]]$wais.r.inf.final <- NA
  subject.dfs.core[[i]]$wais.r.inf.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.r.inf.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.r.inf.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.R.INF)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.R.INF) == 'FALSE'){
      subject.dfs.core[[i]]$wais.r.inf.final <- rep(subject.dfs.core[[i]][j,]$WAIS.R.INF, length(subject.dfs.core[[i]]$wais.r.inf.final))
      subject.dfs.core[[i]]$wais.r.inf.date.final <- rep(subject.dfs.core[[i]][j,]$wais.r.inf.date, length(subject.dfs.core[[i]]$wais.r.inf.date.final))
      subject.dfs.core[[i]]$date.diff.wais.r.inf.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.r.inf, length(subject.dfs.core[[i]]$date.diff.wais.r.inf.final))
    } else {
      subject.dfs.core[[i]]$wais.r.inf.final <- NA
      subject.dfs.core[[i]]$wais.r.inf.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.r.inf.final <- NA
    }
    j <- j + 1
  }
}

#WAIS 3 Information

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.3.inf), ]
  subject.dfs.core[[i]]$wais.3.inf.final <- NA
  subject.dfs.core[[i]]$wais.3.inf.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.3.inf.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.3.inf.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.3.INF)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.3.INF) == 'FALSE'){
      subject.dfs.core[[i]]$wais.3.inf.final <- rep(subject.dfs.core[[i]][j,]$WAIS.3.INF, length(subject.dfs.core[[i]]$wais.3.inf.final))
      subject.dfs.core[[i]]$wais.3.inf.date.final <- rep(subject.dfs.core[[i]][j,]$wais.3.inf.date, length(subject.dfs.core[[i]]$wais.3.inf.date.final))
      subject.dfs.core[[i]]$date.diff.wais.3.inf.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.3.inf, length(subject.dfs.core[[i]]$date.diff.wais.3.inf.final))
    } else {
      subject.dfs.core[[i]]$wais.3.inf.final <- NA
      subject.dfs.core[[i]]$wais.3.inf.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.3.inf.final <- NA
    }
    j <- j + 1
  }
}

#WAIS IV Information

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.iv.inf), ]
  subject.dfs.core[[i]]$wais.iv.inf.final <- NA
  subject.dfs.core[[i]]$wais.iv.inf.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.iv.inf.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.iv.inf.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.IV.INF)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.IV.INF) == 'FALSE'){
      subject.dfs.core[[i]]$wais.iv.inf.final <- rep(subject.dfs.core[[i]][j,]$WAIS.IV.INF, length(subject.dfs.core[[i]]$wais.3.inf.final))
      subject.dfs.core[[i]]$wais.iv.inf.date.final <- rep(subject.dfs.core[[i]][j,]$wais.iv.inf.date, length(subject.dfs.core[[i]]$wais.iv.inf.date.final))
      subject.dfs.core[[i]]$date.diff.wais.iv.inf.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.iv.inf, length(subject.dfs.core[[i]]$date.diff.wais.iv.inf.final))
    } else {
      subject.dfs.core[[i]]$wais.iv.inf.final <- NA
      subject.dfs.core[[i]]$wais.iv.inf.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.iv.inf.final <- NA
    }
    j <- j + 1
  }
}

#WAIS R Coding

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.r.cod), ]
  subject.dfs.core[[i]]$wais.r.cod.final <- NA
  subject.dfs.core[[i]]$wais.r.cod.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.r.cod.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.r.cod.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.R.D.SYM)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.R.D.SYM) == 'FALSE'){
      subject.dfs.core[[i]]$wais.r.cod.final <- rep(subject.dfs.core[[i]][j,]$WAIS.R.D.SYM, length(subject.dfs.core[[i]]$wais.r.cod.final))
      subject.dfs.core[[i]]$wais.r.cod.date.final <- rep(subject.dfs.core[[i]][j,]$wais.r.cod.date, length(subject.dfs.core[[i]]$wais.r.cod.date.final))
      subject.dfs.core[[i]]$date.diff.wais.r.cod.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.r.cod, length(subject.dfs.core[[i]]$date.diff.wais.r.cod.final))
    } else {
      subject.dfs.core[[i]]$wais.r.cod.final <- NA
      subject.dfs.core[[i]]$wais.r.cod.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.r.cod.final <- NA
    }
    j <- j + 1
  }
}

#WAIS 3 Coding

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.3.cod), ]
  subject.dfs.core[[i]]$wais.3.cod.final <- NA
  subject.dfs.core[[i]]$wais.3.cod.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.3.cod.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.3.cod.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.3.DIG.SYM.CODING)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.3.DIG.SYM.CODING) == 'FALSE'){
      subject.dfs.core[[i]]$wais.3.cod.final <- rep(subject.dfs.core[[i]][j,]$WAIS.3.DIG.SYM.CODING, length(subject.dfs.core[[i]]$wais.3.cod.final))
      subject.dfs.core[[i]]$wais.3.cod.date.final <- rep(subject.dfs.core[[i]][j,]$wais.3.cod.date, length(subject.dfs.core[[i]]$wais.3.cod.date.final))
      subject.dfs.core[[i]]$date.diff.wais.3.cod.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.3.cod, length(subject.dfs.core[[i]]$date.diff.wais.3.cod.final))
    } else {
      subject.dfs.core[[i]]$wais.3.cod.final <- NA
      subject.dfs.core[[i]]$wais.3.cod.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.3.cod.final <- NA
    }
    j <- j + 1
  }
}

#WAIS IV Coding

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.iv.cod), ]
  subject.dfs.core[[i]]$wais.iv.cod.final <- NA
  subject.dfs.core[[i]]$wais.iv.cod.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.iv.cod.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.iv.cod.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.IV.CD)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.IV.CD) == 'FALSE'){
      subject.dfs.core[[i]]$wais.iv.cod.final <- rep(subject.dfs.core[[i]][j,]$WAIS.IV.CD, length(subject.dfs.core[[i]]$wais.iv.cod.final))
      subject.dfs.core[[i]]$wais.iv.cod.date.final <- rep(subject.dfs.core[[i]][j,]$wais.iv.cod.date, length(subject.dfs.core[[i]]$wais.iv.cod.date.final))
      subject.dfs.core[[i]]$date.diff.wais.iv.cod.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.iv.cod, length(subject.dfs.core[[i]]$date.diff.wais.iv.cod.final))
    } else {
      subject.dfs.core[[i]]$wais.iv.cod.final <- NA
      subject.dfs.core[[i]]$wais.iv.cod.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.iv.cod.final <- NA
    }
    j <- j + 1
  }
}

#WAIS R Block Design

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.r.bd), ]
  subject.dfs.core[[i]]$wais.r.bd.final <- NA
  subject.dfs.core[[i]]$wais.r.bd.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.r.bd.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.r.bd.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.R.BLOCK)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.R.BLOCK) == 'FALSE'){
      subject.dfs.core[[i]]$wais.r.bd.final <- rep(subject.dfs.core[[i]][j,]$WAIS.R.BLOCK, length(subject.dfs.core[[i]]$wais.r.bd.final))
      subject.dfs.core[[i]]$wais.r.bd.date.final <- rep(subject.dfs.core[[i]][j,]$wais.r.bd.date, length(subject.dfs.core[[i]]$wais.r.bd.date.final))
      subject.dfs.core[[i]]$date.diff.wais.r.bd.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.r.bd, length(subject.dfs.core[[i]]$date.diff.wais.r.bd.final))
    } else {
      subject.dfs.core[[i]]$wais.r.bd.final <- NA
      subject.dfs.core[[i]]$wais.r.bd.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.r.bd.final <- NA
    }
    j <- j + 1
  }
}

#WAIS 3 Block Design

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.3.bd), ]
  subject.dfs.core[[i]]$wais.3.bd.final <- NA
  subject.dfs.core[[i]]$wais.3.bd.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.3.bd.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.3.bd.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.3.BLOCK.DESIGN)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.3.BLOCK.DESIGN) == 'FALSE'){
      subject.dfs.core[[i]]$wais.3.bd.final <- rep(subject.dfs.core[[i]][j,]$WAIS.3.BLOCK.DESIGN, length(subject.dfs.core[[i]]$wais.3.bd.final))
      subject.dfs.core[[i]]$wais.3.bd.date.final <- rep(subject.dfs.core[[i]][j,]$wais.3.bd.date, length(subject.dfs.core[[i]]$wais.3.bd.date.final))
      subject.dfs.core[[i]]$date.diff.wais.3.bd.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.3.bd, length(subject.dfs.core[[i]]$date.diff.wais.3.bd.final))
    } else {
      subject.dfs.core[[i]]$wais.3.bd.final <- NA
      subject.dfs.core[[i]]$wais.3.bd.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.3.bd.final <- NA
    }
    j <- j + 1
  }
}

#WAIS IV Block Design

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.iv.bd), ]
  subject.dfs.core[[i]]$wais.iv.bd.final <- NA
  subject.dfs.core[[i]]$wais.iv.bd.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.iv.bd.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.iv.bd.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.IV.BD)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.IV.BD) == 'FALSE'){
      subject.dfs.core[[i]]$wais.iv.bd.final <- rep(subject.dfs.core[[i]][j,]$WAIS.IV.BD, length(subject.dfs.core[[i]]$wais.iv.bd.final))
      subject.dfs.core[[i]]$wais.iv.bd.date.final <- rep(subject.dfs.core[[i]][j,]$wais.iv.bd.date, length(subject.dfs.core[[i]]$wais.iv.bd.date.final))
      subject.dfs.core[[i]]$date.diff.wais.iv.bd.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.iv.bd, length(subject.dfs.core[[i]]$date.diff.wais.iv.bd.final))
    } else {
      subject.dfs.core[[i]]$wais.iv.bd.final <- NA
      subject.dfs.core[[i]]$wais.iv.bd.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.iv.bd.final <- NA
    }
    j <- j + 1
  }
}

#WAIS R Digit Span

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.r.dig), ]
  subject.dfs.core[[i]]$wais.r.dig.final <- NA
  subject.dfs.core[[i]]$wais.r.dig.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.r.dig.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.r.dig.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.R.DIG)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.R.DIG) == 'FALSE'){
      subject.dfs.core[[i]]$wais.r.dig.final <- rep(subject.dfs.core[[i]][j,]$WAIS.R.DIG, length(subject.dfs.core[[i]]$wais.r.dig.final))
      subject.dfs.core[[i]]$wais.r.dig.date.final <- rep(subject.dfs.core[[i]][j,]$wais.r.dig.date, length(subject.dfs.core[[i]]$wais.r.dig.date.final))
      subject.dfs.core[[i]]$date.diff.wais.r.dig.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.r.dig, length(subject.dfs.core[[i]]$date.diff.wais.r.dig.final))
    } else {
      subject.dfs.core[[i]]$wais.r.dig.final <- NA
      subject.dfs.core[[i]]$wais.r.dig.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.r.dig.final <- NA
    }
    j <- j + 1
  }
}

#WAIS 3 Digit Span

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.3.dig), ]
  subject.dfs.core[[i]]$wais.3.dig.final <- NA
  subject.dfs.core[[i]]$wais.3.dig.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.3.dig.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.3.dig.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.3.DIG)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.3.DIG) == 'FALSE'){
      subject.dfs.core[[i]]$wais.3.dig.final <- rep(subject.dfs.core[[i]][j,]$WAIS.3.DIG, length(subject.dfs.core[[i]]$wais.3.dig.final))
      subject.dfs.core[[i]]$wais.3.dig.date.final <- rep(subject.dfs.core[[i]][j,]$wais.3.dig.date, length(subject.dfs.core[[i]]$wais.3.dig.date.final))
      subject.dfs.core[[i]]$date.diff.wais.3.dig.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.3.dig, length(subject.dfs.core[[i]]$date.diff.wais.3.dig.final))
    } else {
      subject.dfs.core[[i]]$wais.3.dig.final <- NA
      subject.dfs.core[[i]]$wais.3.dig.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.3.dig.final <- NA
    }
    j <- j + 1
  }
}

#WAIS IV Digit Span

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.iv.dig), ]
  subject.dfs.core[[i]]$wais.iv.dig.final <- NA
  subject.dfs.core[[i]]$wais.iv.dig.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.iv.dig.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.iv.dig.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.IV.DS)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.IV.DS) == 'FALSE'){
      subject.dfs.core[[i]]$wais.iv.dig.final <- rep(subject.dfs.core[[i]][j,]$WAIS.IV.DS, length(subject.dfs.core[[i]]$wais.iv.dig.final))
      subject.dfs.core[[i]]$wais.iv.dig.date.final <- rep(subject.dfs.core[[i]][j,]$wais.iv.dig.date, length(subject.dfs.core[[i]]$wais.iv.dig.date.final))
      subject.dfs.core[[i]]$date.diff.wais.iv.dig.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.iv.dig, length(subject.dfs.core[[i]]$date.diff.wais.iv.dig.final))
    } else {
      subject.dfs.core[[i]]$wais.iv.dig.final <- NA
      subject.dfs.core[[i]]$wais.iv.dig.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.iv.dig.final <- NA
    }
    j <- j + 1
  }
}

#WAIS R Arithmetic

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.r.ari), ]
  subject.dfs.core[[i]]$wais.r.ari.final <- NA
  subject.dfs.core[[i]]$wais.r.ari.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.r.ari.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.r.ari.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.R.ARITH)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.R.ARITH) == 'FALSE'){
      subject.dfs.core[[i]]$wais.r.ari.final <- rep(subject.dfs.core[[i]][j,]$WAIS.R.ARITH, length(subject.dfs.core[[i]]$wais.r.ari.final))
      subject.dfs.core[[i]]$wais.r.ari.date.final <- rep(subject.dfs.core[[i]][j,]$wais.r.ari.date, length(subject.dfs.core[[i]]$wais.r.ari.date.final))
      subject.dfs.core[[i]]$date.diff.wais.r.ari.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.r.ari, length(subject.dfs.core[[i]]$date.diff.wais.r.ari.final))
    } else {
      subject.dfs.core[[i]]$wais.r.ari.final <- NA
      subject.dfs.core[[i]]$wais.r.ari.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.r.ari.final <- NA
    }
    j <- j + 1
  }
}

#WAIS 3 Arithmetic

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.3.ari), ]
  subject.dfs.core[[i]]$wais.3.ari.final <- NA
  subject.dfs.core[[i]]$wais.3.ari.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.3.ari.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.3.ari.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.3.ARITH)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.3.ARITH) == 'FALSE'){
      subject.dfs.core[[i]]$wais.3.ari.final <- rep(subject.dfs.core[[i]][j,]$WAIS.3.ARITH, length(subject.dfs.core[[i]]$wais.3.dig.final))
      subject.dfs.core[[i]]$wais.3.ari.date.final <- rep(subject.dfs.core[[i]][j,]$wais.3.dig.date, length(subject.dfs.core[[i]]$wais.3.dig.date.final))
      subject.dfs.core[[i]]$date.diff.wais.3.ari.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.3.dig, length(subject.dfs.core[[i]]$date.diff.wais.3.dig.final))
    } else {
      subject.dfs.core[[i]]$wais.3.ari.final <- NA
      subject.dfs.core[[i]]$wais.3.ari.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.3.ari.final <- NA
    }
    j <- j + 1
  }
}

#WAIS IV Arithmetic

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wais.iv.ari), ]
  subject.dfs.core[[i]]$wais.iv.ari.final <- NA
  subject.dfs.core[[i]]$wais.iv.ari.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wais.iv.ari.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wais.iv.ari.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WAIS.IV.ARI)){
    if(is.na(subject.dfs.core[[i]][j,]$WAIS.IV.ARI) == 'FALSE'){
      subject.dfs.core[[i]]$wais.iv.ari.final <- rep(subject.dfs.core[[i]][j,]$WAIS.IV.ARI, length(subject.dfs.core[[i]]$wais.iv.ari.final))
      subject.dfs.core[[i]]$wais.iv.ari.date.final <- rep(subject.dfs.core[[i]][j,]$wais.iv.ari.date, length(subject.dfs.core[[i]]$wais.iv.ari.date.final))
      subject.dfs.core[[i]]$date.diff.wais.iv.ari.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wais.iv.ari, length(subject.dfs.core[[i]]$date.diff.wais.iv.ari.final))
    } else {
      subject.dfs.core[[i]]$wais.iv.ari.final <- NA
      subject.dfs.core[[i]]$wais.iv.ari.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wais.iv.ari.final <- NA
    }
    j <- j + 1
  }
}

#WRAT R Word Reading

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wrat.r), ]
  subject.dfs.core[[i]]$wrat.r.final <- NA
  subject.dfs.core[[i]]$wrat.r.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wrat.r.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wrat.r.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WRAT.R.READSTAN)){
    if(is.na(subject.dfs.core[[i]][j,]$WRAT.R.READSTAN) == 'FALSE'){
      subject.dfs.core[[i]]$wrat.r.final <- rep(subject.dfs.core[[i]][j,]$WRAT.R.READSTAN, length(subject.dfs.core[[i]]$wrat.r.final))
      subject.dfs.core[[i]]$wrat.r.date.final <- rep(subject.dfs.core[[i]][j,]$wrat.r.date, length(subject.dfs.core[[i]]$wrat.r.date.final))
      subject.dfs.core[[i]]$date.diff.wrat.r.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wrat.r, length(subject.dfs.core[[i]]$date.diff.wrat.r.final))
    } else {
      subject.dfs.core[[i]]$wrat.r.final <- NA
      subject.dfs.core[[i]]$wrat.r.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wrat.r.final <- NA
    }
    j <- j + 1
  }
}

#WRAT IV Word Reading

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.wrat.iv), ]
  subject.dfs.core[[i]]$wrat.iv.final <- NA
  subject.dfs.core[[i]]$wrat.iv.date.final <- NA
  subject.dfs.core[[i]]$date.diff.wrat.iv.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$wrat.iv.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$WRAT.4.READ)){
    if(is.na(subject.dfs.core[[i]][j,]$WRAT.4.READ) == 'FALSE'){
      subject.dfs.core[[i]]$wrat.iv.final <- rep(subject.dfs.core[[i]][j,]$WRAT.4.READ, length(subject.dfs.core[[i]]$wrat.iv.final))
      subject.dfs.core[[i]]$wrat.iv.date.final <- rep(subject.dfs.core[[i]][j,]$wrat.iv.date, length(subject.dfs.core[[i]]$wrat.iv.date.final))
      subject.dfs.core[[i]]$date.diff.wrat.iv.final <- rep(subject.dfs.core[[i]][j,]$date.diff.wrat.iv, length(subject.dfs.core[[i]]$date.diff.wrat.iv.final))
    } else {
      subject.dfs.core[[i]]$wrat.iv.final <- NA
      subject.dfs.core[[i]]$wrat.iv.date.final <- NA
      subject.dfs.core[[i]]$date.diff.wrat.iv.final <- NA
    }
    j <- j + 1
  }
}

#Boston Naming Test

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.bnt), ]
  subject.dfs.core[[i]]$bnt.final <- NA
  subject.dfs.core[[i]]$bnt.date.final <- NA
  subject.dfs.core[[i]]$date.diff.bnt.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$bnt.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$BOS.NAM.RAW)){
    if(is.na(subject.dfs.core[[i]][j,]$BOS.NAM.RAW) == 'FALSE'){
      subject.dfs.core[[i]]$bnt.final <- rep(subject.dfs.core[[i]][j,]$BOS.NAM.RAW, length(subject.dfs.core[[i]]$bnt.final))
      subject.dfs.core[[i]]$bnt.date.final <- rep(subject.dfs.core[[i]][j,]$rey1.date, length(subject.dfs.core[[i]]$bnt.date.final))
      subject.dfs.core[[i]]$date.diff.bnt.final <- rep(subject.dfs.core[[i]][j,]$date.diff.bnt, length(subject.dfs.core[[i]]$date.diff.bnt.final))
    } else {
      subject.dfs.core[[i]]$bnt.final <- NA
      subject.dfs.core[[i]]$bnt.date.final <- NA
      subject.dfs.core[[i]]$date.diff.bnt.final <- NA
    }
    j <- j + 1
  }
}

#Rey 1

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.rey1), ]
  subject.dfs.core[[i]]$rey1.final <- NA
  subject.dfs.core[[i]]$rey1.date.final <- NA
  subject.dfs.core[[i]]$date.diff.rey1.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$rey1.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$REY.1)){
    if(is.na(subject.dfs.core[[i]][j,]$REY.1) == 'FALSE'){
      subject.dfs.core[[i]]$rey1.final <- rep(subject.dfs.core[[i]][j,]$REY.1, length(subject.dfs.core[[i]]$rey1.final))
      subject.dfs.core[[i]]$rey1.date.final <- rep(subject.dfs.core[[i]][j,]$rey1.date, length(subject.dfs.core[[i]]$rey1.date.final))
      subject.dfs.core[[i]]$date.diff.rey1.final <- rep(subject.dfs.core[[i]][j,]$date.diff.rey1, length(subject.dfs.core[[i]]$date.diff.rey1.final))
    } else {
      subject.dfs.core[[i]]$rey1.final <- NA
      subject.dfs.core[[i]]$rey1.date.final <- NA
      subject.dfs.core[[i]]$date.diff.rey1.final <- NA
    }
    j <- j + 1
  }
}

#Rey 2

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.rey2), ]
  subject.dfs.core[[i]]$rey2.final <- NA
  subject.dfs.core[[i]]$rey2.date.final <- NA
  subject.dfs.core[[i]]$date.diff.rey2.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$rey2.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$REY.2)){
    if(is.na(subject.dfs.core[[i]][j,]$REY.2) == 'FALSE'){
      subject.dfs.core[[i]]$rey2.final <- rep(subject.dfs.core[[i]][j,]$REY.2, length(subject.dfs.core[[i]]$rey2.final))
      subject.dfs.core[[i]]$rey2.date.final <- rep(subject.dfs.core[[i]][j,]$rey2.date, length(subject.dfs.core[[i]]$rey2.date.final))
      subject.dfs.core[[i]]$date.diff.rey2.final <- rep(subject.dfs.core[[i]][j,]$date.diff.rey2, length(subject.dfs.core[[i]]$date.diff.rey2.final))
    } else {
      subject.dfs.core[[i]]$rey2.final <- NA
      subject.dfs.core[[i]]$rey2.date.final <- NA
      subject.dfs.core[[i]]$date.diff.rey2.final <- NA
    }
    j <- j + 1
  }
}

#Rey 3

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.rey3), ]
  subject.dfs.core[[i]]$rey3.final <- NA
  subject.dfs.core[[i]]$rey3.date.final <- NA
  subject.dfs.core[[i]]$date.diff.rey3.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$rey3.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$REY.3)){
    if(is.na(subject.dfs.core[[i]][j,]$REY.3) == 'FALSE'){
      subject.dfs.core[[i]]$rey3.final <- rep(subject.dfs.core[[i]][j,]$REY.3, length(subject.dfs.core[[i]]$rey3.final))
      subject.dfs.core[[i]]$rey3.date.final <- rep(subject.dfs.core[[i]][j,]$rey3.date, length(subject.dfs.core[[i]]$rey3.date.final))
      subject.dfs.core[[i]]$date.diff.rey3.final <- rep(subject.dfs.core[[i]][j,]$date.diff.rey3, length(subject.dfs.core[[i]]$date.diff.rey3.final))
    } else {
      subject.dfs.core[[i]]$rey3.final <- NA
      subject.dfs.core[[i]]$rey3.date.final <- NA
      subject.dfs.core[[i]]$date.diff.rey3.final <- NA
    }
    j <- j + 1
  }
}

#Rey 4

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.rey4), ]
  subject.dfs.core[[i]]$rey4.final <- NA
  subject.dfs.core[[i]]$rey4.date.final <- NA
  subject.dfs.core[[i]]$date.diff.rey4.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$rey4.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$REY.4)){
    if(is.na(subject.dfs.core[[i]][j,]$REY.4) == 'FALSE'){
      subject.dfs.core[[i]]$rey4.final <- rep(subject.dfs.core[[i]][j,]$REY.4, length(subject.dfs.core[[i]]$rey4.final))
      subject.dfs.core[[i]]$rey4.date.final <- rep(subject.dfs.core[[i]][j,]$rey4.date, length(subject.dfs.core[[i]]$rey4.date.final))
      subject.dfs.core[[i]]$date.diff.rey4.final <- rep(subject.dfs.core[[i]][j,]$date.diff.rey4, length(subject.dfs.core[[i]]$date.diff.rey4.final))
    } else {
      subject.dfs.core[[i]]$rey4.final <- NA
      subject.dfs.core[[i]]$rey4.date.final <- NA
      subject.dfs.core[[i]]$date.diff.rey4.final <- NA
    }
    j <- j + 1
  }
}

#Rey 5

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.rey5), ]
  subject.dfs.core[[i]]$rey5.final <- NA
  subject.dfs.core[[i]]$rey5.date.final <- NA
  subject.dfs.core[[i]]$date.diff.rey5.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$rey5.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$REY.5)){
    if(is.na(subject.dfs.core[[i]][j,]$REY.5) == 'FALSE'){
      subject.dfs.core[[i]]$rey5.final <- rep(subject.dfs.core[[i]][j,]$REY.5, length(subject.dfs.core[[i]]$rey5.final))
      subject.dfs.core[[i]]$rey5.date.final <- rep(subject.dfs.core[[i]][j,]$rey5.date, length(subject.dfs.core[[i]]$rey5.date.final))
      subject.dfs.core[[i]]$date.diff.rey5.final <- rep(subject.dfs.core[[i]][j,]$date.diff.rey5, length(subject.dfs.core[[i]]$date.diff.rey5.final))
    } else {
      subject.dfs.core[[i]]$rey5.final <- NA
      subject.dfs.core[[i]]$rey5.date.final <- NA
      subject.dfs.core[[i]]$date.diff.rey5.final <- NA
    } 
    j <- j + 1
  }
}

#Rey Recall

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.reyr), ]
  subject.dfs.core[[i]]$reyr.final <- NA
  subject.dfs.core[[i]]$reyr.date.final <- NA
  subject.dfs.core[[i]]$date.diff.reyr.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$reyr.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$REY.RECALL)){
    if(is.na(subject.dfs.core[[i]][j,]$REY.RECALL) == 'FALSE'){
      subject.dfs.core[[i]]$reyr.final <- rep(subject.dfs.core[[i]][j,]$REY.RECALL, length(subject.dfs.core[[i]]$reyr.final))
      subject.dfs.core[[i]]$reyr.date.final <- rep(subject.dfs.core[[i]][j,]$reyr.date, length(subject.dfs.core[[i]]$reyr.date.final))
      subject.dfs.core[[i]]$date.diff.reyr.final <- rep(subject.dfs.core[[i]][j,]$date.diff.reyr, length(subject.dfs.core[[i]]$date.diff.reyr.final))
    } else {
      subject.dfs.core[[i]]$reyr.final <- NA
      subject.dfs.core[[i]]$reyr.date.final <- NA
      subject.dfs.core[[i]]$date.diff.reyr.final <- NA
    }
    j <- j + 1
  }
}

#Rey Recognition Hits

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.reyh), ]
  subject.dfs.core[[i]]$reyh.final <- NA
  subject.dfs.core[[i]]$reyh.date.final <- NA
  subject.dfs.core[[i]]$date.diff.reyh.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$reyh.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$REY.RECOG.HIT)){
    if(is.na(subject.dfs.core[[i]][j,]$REY.RECOG.HIT) == 'FALSE'){
      subject.dfs.core[[i]]$reyh.final <- rep(subject.dfs.core[[i]][j,]$REY.RECOG.HIT, length(subject.dfs.core[[i]]$reyh.final))
      subject.dfs.core[[i]]$reyh.date.final <- rep(subject.dfs.core[[i]][j,]$reyh.date, length(subject.dfs.core[[i]]$reyh.date.final))
      subject.dfs.core[[i]]$date.diff.reyh.final <- rep(subject.dfs.core[[i]][j,]$date.diff.reyh, length(subject.dfs.core[[i]]$date.diff.reyh.final))
    } else {
      subject.dfs.core[[i]]$reyh.final <- NA
      subject.dfs.core[[i]]$reyh.date.final <- NA
      subject.dfs.core[[i]]$date.diff.reyh.final <- NA
    }
    j <- j + 1
  }
}

#Complex Figure Copy

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.cftc), ]
  subject.dfs.core[[i]]$cftc.final <- NA
  subject.dfs.core[[i]]$cftc.date.final <- NA
  subject.dfs.core[[i]]$date.diff.cftc.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$cftc.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$CONS.CFT.RAW)){
    if(is.na(subject.dfs.core[[i]][j,]$CONS.CFT.RAW) == 'FALSE'){
      subject.dfs.core[[i]]$cftc.final <- rep(subject.dfs.core[[i]][j,]$CONS.CFT.RAW, length(subject.dfs.core[[i]]$cftc.final))
      subject.dfs.core[[i]]$cftc.date.final <- rep(subject.dfs.core[[i]][j,]$cftc.date, length(subject.dfs.core[[i]]$cftc.date.final))
      subject.dfs.core[[i]]$date.diff.cftc.final <- rep(subject.dfs.core[[i]][j,]$date.diff.cftc, length(subject.dfs.core[[i]]$date.diff.cftc.final))
    } else {
      subject.dfs.core[[i]]$cftc.final <- NA
      subject.dfs.core[[i]]$cftc.date.final <- NA
      subject.dfs.core[[i]]$date.diff.cftc.final <- NA
    }
    j <- j + 1
  }
}

#Complex Figure Recall

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.cftr), ]
  subject.dfs.core[[i]]$cftr.final <- NA
  subject.dfs.core[[i]]$cftr.date.final <- NA
  subject.dfs.core[[i]]$date.diff.cftr.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$cftr.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$COM.FIG.RECALL)){
    if(is.na(subject.dfs.core[[i]][j,]$COM.FIG.RECALL) == 'FALSE'){
      subject.dfs.core[[i]]$cftr.final <- rep(subject.dfs.core[[i]][j,]$COM.FIG.RECALL, length(subject.dfs.core[[i]]$cftr.final))
      subject.dfs.core[[i]]$cftr.date.final <- rep(subject.dfs.core[[i]][j,]$cftr.date, length(subject.dfs.core[[i]]$cftr.date.final))
      subject.dfs.core[[i]]$date.diff.cftr.final <- rep(subject.dfs.core[[i]][j,]$date.diff.cftr, length(subject.dfs.core[[i]]$date.diff.cftr.final))
    } else {
      subject.dfs.core[[i]]$cftr.final <- NA
      subject.dfs.core[[i]]$cftr.date.final <- NA
      subject.dfs.core[[i]]$date.diff.cftr.final <- NA
    }
    j <- j + 1
  }
}

#Benton Visuospatial Retention Test - Correct score

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.vrtc), ]
  subject.dfs.core[[i]]$vrtc.final <- NA
  subject.dfs.core[[i]]$vrtc.date.final <- NA
  subject.dfs.core[[i]]$date.diff.vrtc.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$vrtc.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$BENT.RET.CORR)){
    if(is.na(subject.dfs.core[[i]][j,]$BENT.RET.CORR) == 'FALSE'){
      subject.dfs.core[[i]]$vrtc.final <- rep(subject.dfs.core[[i]][j,]$BENT.RET.CORR, length(subject.dfs.core[[i]]$vrtc.final))
      subject.dfs.core[[i]]$vrtc.date.final <- rep(subject.dfs.core[[i]][j,]$vrtc.date, length(subject.dfs.core[[i]]$vrtc.date.final))
      subject.dfs.core[[i]]$date.diff.vrtc.final <- rep(subject.dfs.core[[i]][j,]$date.diff.vrtc, length(subject.dfs.core[[i]]$date.diff.vrtc.final))
    } else {
      subject.dfs.core[[i]]$vrtc.final <- NA
      subject.dfs.core[[i]]$vrtc.date.final <- NA
      subject.dfs.core[[i]]$date.diff.vrtc.final <- NA
    }
    j <- j + 1
  }
}

#Benton Visuospatial Retention Test - Errors score

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.vrte), ]
  subject.dfs.core[[i]]$vrte.final <- NA
  subject.dfs.core[[i]]$vrte.date.final <- NA
  subject.dfs.core[[i]]$date.diff.vrte.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$vrte.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$BENT.RET.ERR)){
    if(is.na(subject.dfs.core[[i]][j,]$BENT.RET.ERR) == 'FALSE'){
      subject.dfs.core[[i]]$vrte.final <- rep(subject.dfs.core[[i]][j,]$BENT.RET.ERR, length(subject.dfs.core[[i]]$vrte.final))
      subject.dfs.core[[i]]$vrte.date.final <- rep(subject.dfs.core[[i]][j,]$vrte.date, length(subject.dfs.core[[i]]$vrte.date.final))
      subject.dfs.core[[i]]$date.diff.vrte.final <- rep(subject.dfs.core[[i]][j,]$date.diff.vrte, length(subject.dfs.core[[i]]$date.diff.vrte.final))
    } else {
      subject.dfs.core[[i]]$vrte.final <- NA
      subject.dfs.core[[i]]$vrte.date.final <- NA
      subject.dfs.core[[i]]$date.diff.vrte.final <- NA
    }
    j <- j + 1
  }
}

#Benton Facial Recognition (but it really measures discrimination) Test

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.face), ]
  subject.dfs.core[[i]]$face.final <- NA
  subject.dfs.core[[i]]$face.date.final <- NA
  subject.dfs.core[[i]]$date.diff.face.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$face.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$PER.FAC.REC)){
    if(is.na(subject.dfs.core[[i]][j,]$PER.FAC.REC) == 'FALSE'){
      subject.dfs.core[[i]]$face.final <- rep(subject.dfs.core[[i]][j,]$PER.FAC.REC, length(subject.dfs.core[[i]]$face.final))
      subject.dfs.core[[i]]$face.date.final <- rep(subject.dfs.core[[i]][j,]$face.date, length(subject.dfs.core[[i]]$face.date.final))
      subject.dfs.core[[i]]$date.diff.face.final <- rep(subject.dfs.core[[i]][j,]$date.diff.face, length(subject.dfs.core[[i]]$date.diff.face.final))
    } else {
      subject.dfs.core[[i]]$face.final <- NA
      subject.dfs.core[[i]]$face.date.final <- NA
      subject.dfs.core[[i]]$date.diff.face.final <- NA
    }
    j <- j + 1
  }
}

#JLO

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.jlo), ]
  subject.dfs.core[[i]]$jlo.final <- NA
  subject.dfs.core[[i]]$jlo.date.final <- NA
  subject.dfs.core[[i]]$date.diff.jlo.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$jlo.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$PER.JUDG)){
    if(is.na(subject.dfs.core[[i]][j,]$PER.JUDG) == 'FALSE'){
      subject.dfs.core[[i]]$jlo.final <- rep(subject.dfs.core[[i]][j,]$PER.JUDG, length(subject.dfs.core[[i]]$jlo.final))
      subject.dfs.core[[i]]$jlo.date.final <- rep(subject.dfs.core[[i]][j,]$jlo.date, length(subject.dfs.core[[i]]$jlo.date.final))
      subject.dfs.core[[i]]$date.diff.jlo.final <- rep(subject.dfs.core[[i]][j,]$date.diff.jlo, length(subject.dfs.core[[i]]$date.diff.jlo.final))
    } else {
      subject.dfs.core[[i]]$jlo.final <- NA
      subject.dfs.core[[i]]$jlo.date.final <- NA
      subject.dfs.core[[i]]$date.diff.jlo.final <- NA
    }
    j <- j + 1
  }
}

#Trails A

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.tmta), ]
  subject.dfs.core[[i]]$tmta.final <- NA
  subject.dfs.core[[i]]$tmta.date.final <- NA
  subject.dfs.core[[i]]$date.diff.tmta.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$tmta.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$T.MT.A)){
    if(is.na(subject.dfs.core[[i]][j,]$T.MT.A) == 'FALSE'){
      subject.dfs.core[[i]]$tmta.final <- rep(subject.dfs.core[[i]][j,]$T.MT.A, length(subject.dfs.core[[i]]$tmta.final))
      subject.dfs.core[[i]]$tmta.date.final <- rep(subject.dfs.core[[i]][j,]$tmta.date, length(subject.dfs.core[[i]]$tmta.date.final))
      subject.dfs.core[[i]]$date.diff.tmta.final <- rep(subject.dfs.core[[i]][j,]$date.diff.tmta, length(subject.dfs.core[[i]]$date.diff.tmta.final))
    } else {
      subject.dfs.core[[i]]$tmta.final <- NA
      subject.dfs.core[[i]]$tmta.date.final <- NA
      subject.dfs.core[[i]]$date.diff.tmta.final <- NA
    }
    j <- j + 1
  }
}

#Trails B

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.tmtb), ]
  subject.dfs.core[[i]]$tmtb.final <- NA
  subject.dfs.core[[i]]$tmtb.date.final <- NA
  subject.dfs.core[[i]]$date.diff.tmtb.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$tmtb.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$T.MT.B)){
    if(is.na(subject.dfs.core[[i]][j,]$T.MT.B) == 'FALSE'){
      subject.dfs.core[[i]]$tmtb.final <- rep(subject.dfs.core[[i]][j,]$T.MT.B, length(subject.dfs.core[[i]]$tmtb.final))
      subject.dfs.core[[i]]$tmtb.date.final <- rep(subject.dfs.core[[i]][j,]$tmtb.date, length(subject.dfs.core[[i]]$tmtb.date.final))
      subject.dfs.core[[i]]$date.diff.tmtb.final <- rep(subject.dfs.core[[i]][j,]$date.diff.tmtb, length(subject.dfs.core[[i]]$date.diff.tmtb.final))
    } else {
      subject.dfs.core[[i]]$tmtb.final <- NA
      subject.dfs.core[[i]]$tmtb.date.final <- NA
      subject.dfs.core[[i]]$date.diff.tmtb.final <- NA
    }
    j <- j + 1
  }
}

#BDI

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.bdi), ]
  subject.dfs.core[[i]]$bdi.final <- NA
  subject.dfs.core[[i]]$bdi.date.final <- NA
  subject.dfs.core[[i]]$date.diff.bdi.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$bdi.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$BDI)){
    if(is.na(subject.dfs.core[[i]][j,]$BDI) == 'FALSE'){
      subject.dfs.core[[i]]$bdi.final <- rep(subject.dfs.core[[i]][j,]$BDI, length(subject.dfs.core[[i]]$bdi.final))
      subject.dfs.core[[i]]$bdi.date.final <- rep(subject.dfs.core[[i]][j,]$bdi.date, length(subject.dfs.core[[i]]$bdi.date.final))
      subject.dfs.core[[i]]$date.diff.bdi.final <- rep(subject.dfs.core[[i]][j,]$date.diff.bdi, length(subject.dfs.core[[i]]$date.diff.bdi.final))
    } else {
      subject.dfs.core[[i]]$bdi.final <- NA
      subject.dfs.core[[i]]$bdi.date.final <- NA
      subject.dfs.core[[i]]$date.diff.bdi.final <- NA
    }
    j <- j + 1
  }
}

#BAI

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.bai), ]
  subject.dfs.core[[i]]$bai.final <- NA
  subject.dfs.core[[i]]$bai.date.final <- NA
  subject.dfs.core[[i]]$date.diff.bai.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$bai.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$BAI)){
    if(is.na(subject.dfs.core[[i]][j,]$BAI) == 'FALSE'){
      subject.dfs.core[[i]]$bai.final <- rep(subject.dfs.core[[i]][j,]$BAI, length(subject.dfs.core[[i]]$bai.final))
      subject.dfs.core[[i]]$bai.date.final <- rep(subject.dfs.core[[i]][j,]$bai.date, length(subject.dfs.core[[i]]$bai.date.final))
      subject.dfs.core[[i]]$date.diff.bai.final <- rep(subject.dfs.core[[i]][j,]$date.diff.bai, length(subject.dfs.core[[i]]$date.diff.bai.final))
    } else {
      subject.dfs.core[[i]]$bai.final <- NA
      subject.dfs.core[[i]]$bai.date.final <- NA
      subject.dfs.core[[i]]$date.diff.bai.final <- NA
    }
    j <- j + 1
  }
}

#MMPI Depression 

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.mmpid), ]
  subject.dfs.core[[i]]$mmpid.final <- NA
  subject.dfs.core[[i]]$mmpid.date.final <- NA
  subject.dfs.core[[i]]$date.diff.mmpid.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$mmpid.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$MMPI.D)){
    if(is.na(subject.dfs.core[[i]][j,]$MMPI.D) == 'FALSE'){
      subject.dfs.core[[i]]$mmpid.final <- rep(subject.dfs.core[[i]][j,]$MMPI.D, length(subject.dfs.core[[i]]$mmpid.final))
      subject.dfs.core[[i]]$mmpid.date.final <- rep(subject.dfs.core[[i]][j,]$mmpid.date, length(subject.dfs.core[[i]]$mmpid.date.final))
      subject.dfs.core[[i]]$date.diff.mmpid.final <- rep(subject.dfs.core[[i]][j,]$date.diff.mmpid, length(subject.dfs.core[[i]]$date.diff.mmpid.final))
    } else {
      subject.dfs.core[[i]]$mmpid.final <- NA
      subject.dfs.core[[i]]$mmpid.date.final <- NA
      subject.dfs.core[[i]]$date.diff.mmpid.final <- NA
    }
    j <- j + 1
  }
}

#MMPI Hs

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.mmpihs), ]
  subject.dfs.core[[i]]$mmpihs.final <- NA
  subject.dfs.core[[i]]$mmpihs.date.final <- NA
  subject.dfs.core[[i]]$date.diff.mmpihs.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$mmpihs.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$MMPI.HS)){
    if(is.na(subject.dfs.core[[i]][j,]$MMPI.HS) == 'FALSE'){
      subject.dfs.core[[i]]$mmpihs.final <- rep(subject.dfs.core[[i]][j,]$MMPI.HS, length(subject.dfs.core[[i]]$mmpihs.final))
      subject.dfs.core[[i]]$mmpihs.date.final <- rep(subject.dfs.core[[i]][j,]$mmpihs.date, length(subject.dfs.core[[i]]$mmpihs.date.final))
      subject.dfs.core[[i]]$date.diff.mmpihs.final <- rep(subject.dfs.core[[i]][j,]$date.diff.mmpihs, length(subject.dfs.core[[i]]$date.diff.mmpihs.final))
    } else {
      subject.dfs.core[[i]]$mmpihs.final <- NA
      subject.dfs.core[[i]]$mmpihs.date.final <- NA
      subject.dfs.core[[i]]$date.diff.mmpihs.final <- NA
    }
    j <- j + 1
  }
}

#MMPI Hy

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.mmpihy), ]
  subject.dfs.core[[i]]$mmpihy.final <- NA
  subject.dfs.core[[i]]$mmpihy.date.final <- NA
  subject.dfs.core[[i]]$date.diff.mmpihy.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$mmpihy.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$MMPI.HY)){
    if(is.na(subject.dfs.core[[i]][j,]$MMPI.HY) == 'FALSE'){
      subject.dfs.core[[i]]$mmpihy.final <- rep(subject.dfs.core[[i]][j,]$MMPI.HY, length(subject.dfs.core[[i]]$mmpihy.final))
      subject.dfs.core[[i]]$mmpihy.date.final <- rep(subject.dfs.core[[i]][j,]$mmpihy.date, length(subject.dfs.core[[i]]$mmpihy.date.final))
      subject.dfs.core[[i]]$date.diff.mmpihy.final <- rep(subject.dfs.core[[i]][j,]$date.diff.mmpihy, length(subject.dfs.core[[i]]$date.diff.mmpihy.final))
    } else {
      subject.dfs.core[[i]]$mmpihy.final <- NA
      subject.dfs.core[[i]]$mmpihy.date.final <- NA
      subject.dfs.core[[i]]$date.diff.mmpihy.final <- NA
    }
    j <- j + 1
  }
}

#MMPI Pd

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.mmpipd), ]
  subject.dfs.core[[i]]$mmpipd.final <- NA
  subject.dfs.core[[i]]$mmpipd.date.final <- NA
  subject.dfs.core[[i]]$date.diff.mmpipd.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$mmpipd.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$MMPI.PD)){
    if(is.na(subject.dfs.core[[i]][j,]$MMPI.PD) == 'FALSE'){
      subject.dfs.core[[i]]$mmpipd.final <- rep(subject.dfs.core[[i]][j,]$MMPI.PD, length(subject.dfs.core[[i]]$mmpipd.final))
      subject.dfs.core[[i]]$mmpipd.date.final <- rep(subject.dfs.core[[i]][j,]$mmpipd.date, length(subject.dfs.core[[i]]$mmpipd.date.final))
      subject.dfs.core[[i]]$date.diff.mmpipd.final <- rep(subject.dfs.core[[i]][j,]$date.diff.mmpipd, length(subject.dfs.core[[i]]$date.diff.mmpipd.final))
    } else {
      subject.dfs.core[[i]]$mmpipd.final <- NA
      subject.dfs.core[[i]]$mmpipd.date.final <- NA
      subject.dfs.core[[i]]$date.diff.mmpipd.final <- NA
    }
    j <- j + 1
  }
}

#MMPI Pa

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.mmpipa), ]
  subject.dfs.core[[i]]$mmpipa.final <- NA
  subject.dfs.core[[i]]$mmpipa.date.final <- NA
  subject.dfs.core[[i]]$date.diff.mmpipa.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$mmpipa.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$MMPI.PA)){
    if(is.na(subject.dfs.core[[i]][j,]$MMPI.PA) == 'FALSE'){
      subject.dfs.core[[i]]$mmpipa.final <- rep(subject.dfs.core[[i]][j,]$MMPI.PA, length(subject.dfs.core[[i]]$mmpipa.final))
      subject.dfs.core[[i]]$mmpipa.date.final <- rep(subject.dfs.core[[i]][j,]$mmpipa.date, length(subject.dfs.core[[i]]$mmpipa.date.final))
      subject.dfs.core[[i]]$date.diff.mmpipa.final <- rep(subject.dfs.core[[i]][j,]$date.diff.mmpipa, length(subject.dfs.core[[i]]$date.diff.mmpipa.final))
    } else {
      subject.dfs.core[[i]]$mmpipa.final <- NA
      subject.dfs.core[[i]]$mmpipa.date.final <- NA
      subject.dfs.core[[i]]$date.diff.mmpipa.final <- NA
    }
    j <- j + 1
  }
}

#MMPI Pt

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.mmpipt), ]
  subject.dfs.core[[i]]$mmpipt.final <- NA
  subject.dfs.core[[i]]$mmpipt.date.final <- NA
  subject.dfs.core[[i]]$date.diff.mmpipt.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$mmpipt.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$MMPI.PT)){
    if(is.na(subject.dfs.core[[i]][j,]$MMPI.PT) == 'FALSE'){
      subject.dfs.core[[i]]$mmpipt.final <- rep(subject.dfs.core[[i]][j,]$MMPI.PT, length(subject.dfs.core[[i]]$mmpipt.final))
      subject.dfs.core[[i]]$mmpipt.date.final <- rep(subject.dfs.core[[i]][j,]$mmpipt.date, length(subject.dfs.core[[i]]$mmpipt.date.final))
      subject.dfs.core[[i]]$date.diff.mmpipt.final <- rep(subject.dfs.core[[i]][j,]$date.diff.mmpipt, length(subject.dfs.core[[i]]$date.diff.mmpipt.final))
    } else {
      subject.dfs.core[[i]]$mmpipt.final <- NA
      subject.dfs.core[[i]]$mmpipt.date.final <- NA
      subject.dfs.core[[i]]$date.diff.mmpipt.final <- NA
    }
    j <- j + 1
  }
}

#MMPI Sc

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.mmpisc), ]
  subject.dfs.core[[i]]$mmpisc.final <- NA
  subject.dfs.core[[i]]$mmpisc.date.final <- NA
  subject.dfs.core[[i]]$date.diff.mmpisc.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$mmpisc.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$MMPI.SC)){
    if(is.na(subject.dfs.core[[i]][j,]$MMPI.SC) == 'FALSE'){
      subject.dfs.core[[i]]$mmpisc.final <- rep(subject.dfs.core[[i]][j,]$MMPI.SC, length(subject.dfs.core[[i]]$mmpisc.final))
      subject.dfs.core[[i]]$mmpisc.date.final <- rep(subject.dfs.core[[i]][j,]$mmpisc.date, length(subject.dfs.core[[i]]$mmpisc.date.final))
      subject.dfs.core[[i]]$date.diff.mmpisc.final <- rep(subject.dfs.core[[i]][j,]$date.diff.mmpisc, length(subject.dfs.core[[i]]$date.diff.mmpisc.final))
    } else {
      subject.dfs.core[[i]]$mmpisc.final <- NA
      subject.dfs.core[[i]]$mmpisc.date.final <- NA
      subject.dfs.core[[i]]$date.diff.mmpisc.final <- NA
    }
    j <- j + 1
  }
}

#MMPI Ma

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.mmpima), ]
  subject.dfs.core[[i]]$mmpima.final <- NA
  subject.dfs.core[[i]]$mmpima.date.final <- NA
  subject.dfs.core[[i]]$date.diff.mmpima.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$mmpima.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$MMPI.MA)){
    if(is.na(subject.dfs.core[[i]][j,]$MMPI.MA) == 'FALSE'){
      subject.dfs.core[[i]]$mmpima.final <- rep(subject.dfs.core[[i]][j,]$MMPI.MA, length(subject.dfs.core[[i]]$mmpima.final))
      subject.dfs.core[[i]]$mmpima.date.final <- rep(subject.dfs.core[[i]][j,]$mmpima.date, length(subject.dfs.core[[i]]$mmpima.date.final))
      subject.dfs.core[[i]]$date.diff.mmpima.final <- rep(subject.dfs.core[[i]][j,]$date.diff.mmpima, length(subject.dfs.core[[i]]$date.diff.mmpima.final))
    } else {
      subject.dfs.core[[i]]$mmpima.final <- NA
      subject.dfs.core[[i]]$mmpima.date.final <- NA
      subject.dfs.core[[i]]$date.diff.mmpima.final <- NA
    }
    j <- j + 1
  }
}

#MMPI Si

for(i in id.list.core){
  subject.dfs.core[[i]] <- subject.dfs.core[[i]][order(subject.dfs.core[[i]]$date.diff.mmpisi), ]
  subject.dfs.core[[i]]$mmpisi.final <- NA
  subject.dfs.core[[i]]$mmpisi.date.final <- NA
  subject.dfs.core[[i]]$date.diff.mmpisi.final <- NA
  j <- 1
  while(is.na(subject.dfs.core[[i]][1,]$mmpisi.final) == 'TRUE' & j <= length(subject.dfs.core[[i]]$MMPI.SI)){
    if(is.na(subject.dfs.core[[i]][j,]$MMPI.SI) == 'FALSE'){
      subject.dfs.core[[i]]$mmpisi.final <- rep(subject.dfs.core[[i]][j,]$MMPI.SI, length(subject.dfs.core[[i]]$mmpisi.final))
      subject.dfs.core[[i]]$mmpisi.date.final <- rep(subject.dfs.core[[i]][j,]$mmpisi.date, length(subject.dfs.core[[i]]$mmpisi.date.final))
      subject.dfs.core[[i]]$date.diff.mmpisi.final <- rep(subject.dfs.core[[i]][j,]$date.diff.mmpisi, length(subject.dfs.core[[i]]$date.diff.mmpisi.final))
    } else {
      subject.dfs.core[[i]]$mmpisi.final <- NA
      subject.dfs.core[[i]]$mmpisi.date.final <- NA
      subject.dfs.core[[i]]$date.diff.mmpisi.final <- NA
    }
    j <- j + 1
  }
}


#Take all columns (or maybe the first value in that column because they're all the same) that end in .final (e.g., tmta.final, tmta.date.final, and date.diff.tmta.final) and place them in the df.core dataframe

df.core.final <- data.frame()
df.core.final.1 <- data.frame()

for(i in id.list.core){
  df.core.final.1 <- subject.dfs.core[[i]][1,]
  df.core.final <- rbind(df.core.final, df.core.final.1)
}

#Create funciton to print the signed version (i.e., maintain + or -) of the smallest date diff

abs.min <- function(x) { x[which.min( abs(x) )]}

#Create a vector ids that made it to this point

id.list.final <- df.core.final$RedID

#Collapse across WAIS versions on same subtest AFTER making a single dataframe of .final data

#Similarities

#First create a dataframe with IDs and all relevant subtest data, dates, and date diffs; order it by ID

df.core.final.sim <- subset(df.core.final, select = c('RedID', 'wais.r.sim.final', 'wais.r.sim.date.final', 'date.diff.wais.r.sim.final', 'wais.3.sim.final', 'wais.3.sim.date.final', 'date.diff.wais.3.sim.final', 'wais.iv.sim.final', 'wais.iv.sim.date.final', 'date.diff.wais.iv.sim.final'))

df.core.final.sim <- df.core.final.sim[order(df.core.final.sim$RedID), ]

#Create variables to fill in later

df.core.final.sim$sim <- NA

df.core.final.sim$sim.date <- NA

df.core.final.sim$date.diff.sim <- NA

#Create a vector of IDs that have at least one non-NA value for one of wais r/3/iv

df.core.final.sim.noNA <- df.core.final.sim[is.na(df.core.final.sim$wais.r.sim.final) == 'FALSE' | is.na(df.core.final.sim$wais.3.sim.final) == 'FALSE' | is.na(df.core.final.sim$wais.iv.sim.final) == 'FALSE', ]

id.list.final.sim <- df.core.final.sim.noNA$RedID

for(i in id.list.final.sim){
  target.date.diff <- abs.min(c(df.core.final.sim[df.core.final.sim$RedID == i, ]$date.diff.wais.r.sim.final, df.core.final.sim[df.core.final.sim$RedID == i,]$date.diff.wais.3.sim.final, df.core.final.sim[df.core.final.sim$RedID == i,]$date.diff.wais.iv.sim.final))
  subject.vector <- df.core.final.sim[df.core.final.sim$RedID == i,]
  target.date.diff.position <- match(target.date.diff, subject.vector)
  target.value <- df.core.final.sim[df.core.final.sim$RedID == i, (target.date.diff.position - 2)]
  target.date <- df.core.final.sim[df.core.final.sim$RedID == i, (target.date.diff.position - 1)]
  if(is.na(df.core.final.sim[df.core.final.sim$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.sim[df.core.final.sim$RedID == i,11] <- target.value
  } else {
    df.core.final.sim[df.core.final.sim$RedID == i,11] <- NA
  }
  if(is.na(df.core.final.sim[df.core.final.sim$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.sim[df.core.final.sim$RedID == i,12] <- target.date
  } else {
    df.core.final.sim[df.core.final.sim$RedID == i,12] <- NA
  }
  if(is.na(df.core.final.sim[df.core.final.sim$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.sim[df.core.final.sim$RedID == i,13] <- target.date.diff
  } else {
    df.core.final.sim[df.core.final.sim$RedID == i,13] <- NA
  }
}

#Information

df.core.final.inf <- subset(df.core.final, select = c('RedID', 'wais.r.inf.final', 'wais.r.inf.date.final', 'date.diff.wais.r.inf.final', 'wais.3.inf.final', 'wais.3.inf.date.final', 'date.diff.wais.3.inf.final', 'wais.iv.inf.final', 'wais.iv.inf.date.final', 'date.diff.wais.iv.inf.final')) 

df.core.final.inf <- df.core.final.inf[order(df.core.final.inf$RedID), ]

#Create variables to fill in later

df.core.final.inf$inf <- NA

df.core.final.inf$inf.date <- NA

df.core.final.inf$date.diff.inf <- NA

#Create a vector of IDs that have at least one non-NA value for one of wais r/3/iv

df.core.final.inf.noNA <- df.core.final.inf[is.na(df.core.final.inf$wais.r.inf.final) == 'FALSE' | is.na(df.core.final.inf$wais.3.inf.final) == 'FALSE' | is.na(df.core.final.inf$wais.iv.inf.final) == 'FALSE', ]

id.list.final.inf <- df.core.final.inf.noNA$RedID

for(i in id.list.final.inf){
  target.date.diff <- abs.min(c(df.core.final.inf[df.core.final.inf$RedID == i, ]$date.diff.wais.r.inf.final, df.core.final.inf[df.core.final.inf$RedID == i,]$date.diff.wais.3.inf.final, df.core.final.inf[df.core.final.inf$RedID == i,]$date.diff.wais.iv.inf.final))
  subject.vector <- df.core.final.inf[df.core.final.inf$RedID == i,]
  target.date.diff.position <- match(target.date.diff, subject.vector)
  target.value <- df.core.final.inf[df.core.final.inf$RedID == i, (target.date.diff.position - 2)]
  target.date <- df.core.final.inf[df.core.final.inf$RedID == i, (target.date.diff.position - 1)]
  if(is.na(df.core.final.inf[df.core.final.inf$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.inf[df.core.final.inf$RedID == i,11] <- target.value
  } else {
    df.core.final.inf[df.core.final.inf$RedID == i,11] <- NA
  }
  if(is.na(df.core.final.inf[df.core.final.inf$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.inf[df.core.final.inf$RedID == i,12] <- target.date
  } else {
    df.core.final.inf[df.core.final.inf$RedID == i,12] <- NA
  }
  if(is.na(df.core.final.inf[df.core.final.inf$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.inf[df.core.final.inf$RedID == i,13] <- target.date.diff
  } else {
    df.core.final.inf[df.core.final.inf$RedID == i,13] <- NA
  }
}

#Coding

df.core.final.cod <- subset(df.core.final, select = c('RedID', 'wais.r.cod.final', 'wais.r.cod.date.final', 'date.diff.wais.r.cod.final', 'wais.3.cod.final', 'wais.3.cod.date.final', 'date.diff.wais.3.cod.final', 'wais.iv.cod.final', 'wais.iv.cod.date.final', 'date.diff.wais.iv.cod.final'))

df.core.final.cod <- df.core.final.cod[order(df.core.final.cod$RedID), ]

#Create variables to fill in later

df.core.final.cod$cod <- NA

df.core.final.cod$cod.date <- NA

df.core.final.cod$date.diff.cod <- NA

#Create a vector of IDs that have at least one non-NA value for one of wais r/3/iv

df.core.final.cod.noNA <- df.core.final.cod[is.na(df.core.final.cod$wais.r.cod.final) == 'FALSE' | is.na(df.core.final.cod$wais.3.cod.final) == 'FALSE' | is.na(df.core.final.cod$wais.iv.cod.final) == 'FALSE', ]

id.list.final.cod <- df.core.final.cod.noNA$RedID

for(i in id.list.final.cod){
  target.date.diff <- abs.min(c(df.core.final.cod[df.core.final.cod$RedID == i, ]$date.diff.wais.r.cod.final, df.core.final.cod[df.core.final.cod$RedID == i,]$date.diff.wais.3.cod.final, df.core.final.cod[df.core.final.cod$RedID == i,]$date.diff.wais.iv.cod.final))
  subject.vector <- df.core.final.cod[df.core.final.cod$RedID == i,]
  target.date.diff.position <- match(target.date.diff, subject.vector)
  target.value <- df.core.final.cod[df.core.final.cod$RedID == i, (target.date.diff.position - 2)]
  target.date <- df.core.final.cod[df.core.final.cod$RedID == i, (target.date.diff.position - 1)]
  if(is.na(df.core.final.cod[df.core.final.cod$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.cod[df.core.final.cod$RedID == i,11] <- target.value
  } else {
    df.core.final.cod[df.core.final.cod$RedID == i,11] <- NA
  }
  if(is.na(df.core.final.cod[df.core.final.cod$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.cod[df.core.final.cod$RedID == i,12] <- target.date
  } else {
    df.core.final.cod[df.core.final.cod$RedID == i,12] <- NA
  }
  if(is.na(df.core.final.cod[df.core.final.cod$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.cod[df.core.final.cod$RedID == i,13] <- target.date.diff
  } else {
    df.core.final.cod[df.core.final.cod$RedID == i,13] <- NA
  }
}

#Block Design

df.core.final.bd <- subset(df.core.final, select = c('RedID', 'wais.r.bd.final', 'wais.r.bd.date.final', 'date.diff.wais.r.bd.final', 'wais.3.bd.final', 'wais.3.bd.date.final', 'date.diff.wais.3.bd.final', 'wais.iv.bd.final', 'wais.iv.bd.date.final', 'date.diff.wais.iv.bd.final'))

df.core.final.bd <- df.core.final.bd[order(df.core.final.bd$RedID), ]

#Create variables to fill in later

df.core.final.bd$bd <- NA

df.core.final.bd$bd.date <- NA

df.core.final.bd$date.diff.bd <- NA

#Create a vector of IDs that have at least one non-NA value for one of wais r/3/iv

df.core.final.bd.noNA <- df.core.final.bd[is.na(df.core.final.bd$wais.r.bd.final) == 'FALSE' | is.na(df.core.final.bd$wais.3.bd.final) == 'FALSE' | is.na(df.core.final.bd$wais.iv.bd.final) == 'FALSE', ]

id.list.final.bd <- df.core.final.bd.noNA$RedID

for(i in id.list.final.bd){
  target.date.diff <- abs.min(c(df.core.final.bd[df.core.final.bd$RedID == i, ]$date.diff.wais.r.bd.final, df.core.final.bd[df.core.final.bd$RedID == i,]$date.diff.wais.3.bd.final, df.core.final.bd[df.core.final.bd$RedID == i,]$date.diff.wais.iv.bd.final))
  subject.vector <- df.core.final.bd[df.core.final.bd$RedID == i,]
  target.date.diff.position <- match(target.date.diff, subject.vector)
  target.value <- df.core.final.bd[df.core.final.bd$RedID == i, (target.date.diff.position - 2)]
  target.date <- df.core.final.bd[df.core.final.bd$RedID == i, (target.date.diff.position - 1)]
  if(is.na(df.core.final.bd[df.core.final.bd$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.bd[df.core.final.bd$RedID == i,11] <- target.value
  } else {
    df.core.final.bd[df.core.final.bd$RedID == i,11] <- NA
  }
  if(is.na(df.core.final.bd[df.core.final.bd$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.bd[df.core.final.bd$RedID == i,12] <- target.date
  } else {
    df.core.final.bd[df.core.final.bd$RedID == i,12] <- NA
  }
  if(is.na(df.core.final.bd[df.core.final.bd$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.bd[df.core.final.bd$RedID == i,13] <- target.date.diff
  } else {
    df.core.final.bd[df.core.final.bd$RedID == i,13] <- NA
  }
}

#Digit Span

df.core.final.dig <- subset(df.core.final, select = c('RedID', 'wais.r.dig.final', 'wais.r.dig.date.final', 'date.diff.wais.r.dig.final', 'wais.3.dig.final', 'wais.3.dig.date.final', 'date.diff.wais.3.dig.final', 'wais.iv.dig.final', 'wais.iv.dig.date.final', 'date.diff.wais.iv.dig.final'))

df.core.final.dig <- df.core.final.dig[order(df.core.final.dig$RedID), ]

#Create variables to fill in later

df.core.final.dig$dig <- NA

df.core.final.dig$dig.date <- NA

df.core.final.dig$date.diff.dig <- NA

#Create a vector of IDs that have at least one non-NA value for one of wais r/3/iv

df.core.final.dig.noNA <- df.core.final.dig[is.na(df.core.final.dig$wais.r.dig.final) == 'FALSE' | is.na(df.core.final.dig$wais.3.dig.final) == 'FALSE' | is.na(df.core.final.dig$wais.iv.dig.final) == 'FALSE', ]

id.list.final.dig <- df.core.final.dig.noNA$RedID

for(i in id.list.final.dig){
  target.date.diff <- abs.min(c(df.core.final.dig[df.core.final.dig$RedID == i, ]$date.diff.wais.r.dig.final, df.core.final.dig[df.core.final.dig$RedID == i,]$date.diff.wais.3.dig.final, df.core.final.dig[df.core.final.dig$RedID == i,]$date.diff.wais.iv.dig.final))
  subject.vector <- df.core.final.dig[df.core.final.dig$RedID == i,]
  target.date.diff.position <- match(target.date.diff, subject.vector)
  target.value <- df.core.final.dig[df.core.final.dig$RedID == i, (target.date.diff.position - 2)]
  target.date <- df.core.final.dig[df.core.final.dig$RedID == i, (target.date.diff.position - 1)]
  if(is.na(df.core.final.dig[df.core.final.dig$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.dig[df.core.final.dig$RedID == i,11] <- target.value
  } else {
    df.core.final.dig[df.core.final.dig$RedID == i,11] <- NA
  }
  if(is.na(df.core.final.dig[df.core.final.dig$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.dig[df.core.final.dig$RedID == i,12] <- target.date
  } else {
    df.core.final.dig[df.core.final.dig$RedID == i,12] <- NA
  }
  if(is.na(df.core.final.dig[df.core.final.dig$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.dig[df.core.final.dig$RedID == i,13] <- target.date.diff
  } else {
    df.core.final.dig[df.core.final.dig$RedID == i,13] <- NA
  }
}

#Arithmetic

df.core.final.ari <- subset(df.core.final, select = c('RedID', 'wais.r.ari.final', 'wais.r.ari.date.final', 'date.diff.wais.r.ari.final', 'wais.3.ari.final', 'wais.3.ari.date.final', 'date.diff.wais.3.ari.final', 'wais.iv.ari.final', 'wais.iv.ari.date.final', 'date.diff.wais.iv.ari.final'))

df.core.final.ari <- df.core.final.ari[order(df.core.final.ari$RedID), ]

#Create variables to fill in later

df.core.final.ari$ari <- NA

df.core.final.ari$ari.date <- NA

df.core.final.ari$date.diff.ari <- NA

#Create a vector of IDs that have at least one non-NA value for one of wais r/3/iv

df.core.final.ari.noNA <- df.core.final.ari[is.na(df.core.final.ari$wais.r.ari.final) == 'FALSE' | is.na(df.core.final.ari$wais.3.ari.final) == 'FALSE' | is.na(df.core.final.ari$wais.iv.ari.final) == 'FALSE', ]

id.list.final.ari <- df.core.final.ari.noNA$RedID

for(i in id.list.final.ari){
  target.date.diff <- abs.min(c(df.core.final.ari[df.core.final.ari$RedID == i, ]$date.diff.wais.r.ari.final, df.core.final.ari[df.core.final.ari$RedID == i,]$date.diff.wais.3.ari.final, df.core.final.ari[df.core.final.ari$RedID == i,]$date.diff.wais.iv.ari.final))
  subject.vector <- df.core.final.ari[df.core.final.ari$RedID == i,]
  target.date.diff.position <- match(target.date.diff, subject.vector)
  target.value <- df.core.final.ari[df.core.final.ari$RedID == i, (target.date.diff.position - 2)]
  target.date <- df.core.final.ari[df.core.final.ari$RedID == i, (target.date.diff.position - 1)]
  if(is.na(df.core.final.ari[df.core.final.ari$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.ari[df.core.final.ari$RedID == i,11] <- target.value
  } else {
    df.core.final.ari[df.core.final.ari$RedID == i,11] <- NA
  }
  if(is.na(df.core.final.ari[df.core.final.ari$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.ari[df.core.final.ari$RedID == i,12] <- target.date
  } else {
    df.core.final.ari[df.core.final.ari$RedID == i,12] <- NA
  }
  if(is.na(df.core.final.ari[df.core.final.ari$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.ari[df.core.final.ari$RedID == i,13] <- target.date.diff
  } else {
    df.core.final.ari[df.core.final.ari$RedID == i,13] <- NA
  }
}

#WRAT Word Reading 

df.core.final.wrat <- subset(df.core.final, select = c('RedID', 'wrat.r.final', 'wrat.r.date.final', 'date.diff.wrat.r.final', 'wrat.iv.final', 'wrat.iv.date.final', 'date.diff.wrat.iv.final'))

df.core.final.wrat <- df.core.final.wrat[order(df.core.final.wrat$RedID), ]

#Create variables to fill in later

df.core.final.wrat$wrat <- NA

df.core.final.wrat$wrat.date <- NA

df.core.final.wrat$date.diff.wrat <- NA

#Create a vector of IDs that have at least one non-NA value for one of wais r/3/iv

df.core.final.wrat.noNA <- df.core.final.wrat[is.na(df.core.final.wrat$wrat.r.final) == 'FALSE' | is.na(df.core.final.wrat$wrat.iv.final) == 'FALSE', ]

id.list.final.wrat <- df.core.final.wrat.noNA$RedID

for(i in id.list.final.wrat){
  target.date.diff <- abs.min(c(df.core.final.wrat[df.core.final.wrat$RedID == i, ]$date.diff.wrat.r.final, df.core.final.wrat[df.core.final.wrat$RedID == i,]$date.diff.wrat.iv.final))
  subject.vector <- df.core.final.wrat[df.core.final.wrat$RedID == i,]
  target.date.diff.position <- match(target.date.diff, subject.vector)
  target.value <- df.core.final.wrat[df.core.final.wrat$RedID == i, (target.date.diff.position - 2)]
  target.date <- df.core.final.wrat[df.core.final.wrat$RedID == i, (target.date.diff.position - 1)]
  if(is.na(df.core.final.wrat[df.core.final.wrat$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.wrat[df.core.final.wrat$RedID == i,8] <- target.value
  } else {
    df.core.final.wrat[df.core.final.wrat$RedID == i,8] <- NA
  }
  if(is.na(df.core.final.wrat[df.core.final.wrat$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.wrat[df.core.final.wrat$RedID == i,9] <- target.date
  } else {
    df.core.final.wrat[df.core.final.wrat$RedID == i,9] <- NA
  }
  if(is.na(df.core.final.wrat[df.core.final.wrat$RedID == i, (target.date.diff.position - 2)]) == 'FALSE'){
    df.core.final.wrat[df.core.final.wrat$RedID == i,10] <- target.date.diff
  } else {
    df.core.final.wrat[df.core.final.wrat$RedID == i,10] <- NA
  }
}

#Take final value, date, and date diff for each subtest and put it back in df.core.final

df.core.final$sim <- df.core.final.sim$sim

df.core.final$sim.date <- df.core.final.sim$sim.date

df.core.final$date.diff.sim <- df.core.final.sim$date.diff.sim

df.core.final$inf <- df.core.final.inf$inf

df.core.final$inf.date <- df.core.final.inf$inf.date

df.core.final$date.diff.inf <- df.core.final.inf$date.diff.inf

df.core.final$cod <- df.core.final.cod$cod

df.core.final$cod.date <- df.core.final.cod$cod.date

df.core.final$date.diff.cod <- df.core.final.cod$date.diff.cod

df.core.final$bd <- df.core.final.bd$bd

df.core.final$bd.date <- df.core.final.bd$bd.date

df.core.final$date.diff.bd <- df.core.final.bd$date.diff.bd

df.core.final$dig <- df.core.final.dig$dig

df.core.final$dig.date <- df.core.final.dig$dig.date

df.core.final$date.diff.dig <- df.core.final.dig$date.diff.dig

df.core.final$ari <- df.core.final.ari$ari

df.core.final$ari.date <- df.core.final.ari$ari.date

df.core.final$date.diff.ari <- df.core.final.ari$date.diff.ari

df.core.final$wrat <- df.core.final.wrat$wrat

df.core.final$wrat.date <- df.core.final.wrat$wrat.date

df.core.final$date.diff.wrat <- df.core.final.wrat$date.diff.wrat

#Reorder variables in df.core.final

df.core.final <- subset(df.core.final, select = c('RedID', 'DOB', 'SEX', 'HAND', 'EDUC', 'RACE', 'HISPANIC', 'ONSET', 'Lesions', 'DX', 'ETIOLOGY', 'SUBETIOLOGY', 'ETIOLOGY2', 'SUBETIOLOGY2', 'GROUP', 'scan.date', 'Scan._', 'Type', 'sim', 'sim.date', 'date.diff.sim', 'inf', 'inf.date', 'date.diff.inf', 'wrat', 'wrat.date', 'date.diff.wrat', 'bnt.final', 'bnt.date.final', 'date.diff.bnt.final', 'bd', 'bd.date', 'date.diff.bd', 'jlo.final', 'jlo.date.final', 'date.diff.jlo.final', 'vrtc.final', 'vrtc.date.final', 'date.diff.vrtc.final', 'cftc.final', 'cftc.date.final', 'date.diff.cftc.final', 'cftr.final', 'cftr.date.final', 'date.diff.cftr.final', 'rey5.final', 'rey5.date.final', 'date.diff.rey5.final', 'reyr.final', 'reyr.date.final', 'date.diff.reyr.final', 'reyh.final', 'reyh.date.final', 'date.diff.reyh.final', 'cod', 'cod.date', 'date.diff.cod', 'tmta.final', 'tmta.date.final', 'date.diff.tmta.final', 'tmtb.final', 'tmtb.date.final', 'date.diff.tmtb.final', 'dig', 'dig.date', 'date.diff.dig', 'ari', 'ari.date', 'date.diff.ari'))

colnames(df.core.final) <- c('id', 'dob', 'sex', 'hand', 'edu', 'race', 'hispanic', 'onset', 'n.foci', 'dx', 'etiology', 'subetiology', 'etiology2', 'subetiology2', 'group', 'scan.date', 'n.scans', 'scan.type', 'sim', 'sim.date', 'date.diff.sim', 'inf', 'inf.date', 'date.diff.inf', 'wrat', 'wrat.date', 'date.diff.wrat', 'bnt', 'bnt.date', 'date.diff.bnt', 'bd', 'bd.date', 'date.diff.bd', 'jlo', 'jlo.date', 'date.diff.jlo', 'vrtc', 'vrtc.date', 'date.diff.vrtc', 'cftc', 'cftc.date', 'date.diff.cftc', 'cftr', 'cftr.date', 'date.diff.cftr', 'rey5', 'rey5.date', 'date.diff.rey5', 'reyr', 'reyr.date', 'date.diff.reyr', 'reyh', 'reyh.date', 'date.diff.reyh', 'cod', 'cod.date', 'date.diff.cod', 'tmta', 'tmta.date', 'date.diff.tmta', 'tmtb', 'tmtb.date', 'date.diff.tmtb', 'dig', 'dig.date', 'date.diff.dig', 'ari', 'ari.date', 'date.diff.ari')

#Recode values of "I" into appropriate missing values for tests that have them: tmta, tmtb, jlo, 

df.core.final.tests <- subset(df.core.final, select = c('sim', 'inf', 'wrat', 'bnt', 'bd', 'jlo', 'cftc', 'cftr', 'rey5', 'reyr', 'reyh', 'cod', 'tmta', 'tmtb', 'dig', 'ari'))

apply(df.core.final.tests, 2, table)

df.core.final$jlo[df.core.final$jlo == 'I'] <- '0'

df.core.final$tmta[df.core.final$tmta == 'I'] <- '115'

df.core.final$tmtb[df.core.final$tmtb == 'I'] <- '315'

df.core.final$tmtb[df.core.final$tmtb == 'DC'] <- '315'

df.core.final$cftc[df.core.final$cftc == 'I'] <- '0'

df.core.final$rey5[df.core.final$rey5 == 'I'] <- '0'

df.core.final$reyr[df.core.final$reyr == 'I'] <- '0'

df.core.final$bnt[df.core.final$bnt == 'I'] <- '0'

df.core.final$vrtc[df.core.final$vrtc == 'I'] <- '0'

df.core.final$vrte[df.core.final$vrte == 'I'] <- '23'

df.core.final$face[df.core.final$face == 'I'] <- '25'

#Someone erroneously entered 92 for a bd score instead of 9 (to confirm, look at their wais.r and wais.3 bd scores). Change to 9.

df.core.final$bd[df.core.final$bd == '92'] <- '9'

#Recode all tests into integers since the transformations somehow made them characters

df.core.final$sim <- as.integer(df.core.final$sim)

df.core.final$inf <- as.integer(df.core.final$inf)

df.core.final$cod <- as.integer(df.core.final$cod)

df.core.final$bd <- as.integer(df.core.final$bd)

df.core.final$wrat <- as.integer(df.core.final$wrat)

df.core.final$bnt <- as.integer(df.core.final$bnt)

df.core.final$rey5 <- as.integer(df.core.final$rey5)

df.core.final$reyr <- as.integer(df.core.final$reyr)

df.core.final$reyh <- as.integer(df.core.final$reyh)

df.core.final$cftc <- as.integer(df.core.final$cftc)

df.core.final$cftr <- as.integer(df.core.final$cftr)

df.core.final$jlo <- as.integer(df.core.final$jlo)

df.core.final$vrtc <- as.integer(df.core.final$vrtc)

df.core.final$vrte <- as.integer(df.core.final$vrte)

df.core.final$tmta <- as.integer(df.core.final$tmta)

df.core.final$tmtb <- as.integer(df.core.final$tmtb)

df.core.final$dig <- as.integer(df.core.final$dig)

df.core.final$ari <- as.integer(df.core.final$ari)

#Save dataframe before subsetting based on imputation

write.csv(df.core.final, file = '/Users/markbowren/Documents/uiowa/core/data/core_master.csv')

#Select only subjects missing <= 25% of tests

missing <- apply(df.core.final.tests, 1, percent.missing)

df.core.final <- subset(df.core.final, missing < 25)

nrow(df.core.final)

#Remove developmental cases (i.e., only subjects with age at lesion onset > 18)

df.core <- df.core.final

df.core$age.at.lesion.onset <- as.numeric(as.Date(df.core$onset) - as.Date(df.core$dob))/365

df.core <- subset(df.core, age.at.lesion.onset > 18)

nrow(df.core)

#Add in nifty file location as a column called nii.path

id.list <- df.core$id

nii.path <- vector()

j <- 1

for(i in id.list){
  nii.path[j] <- as.vector(paste0('/Users/markbowren/Documents/uiowa/master/nii/', i, '.nii.gz'))
  j <- j + 1
}

master.nii <- Sys.glob(file.path('/Users/markbowren/Documents/uiowa/master/nii', '*.nii.gz'))

for(i in 1:length(nii.path)){
  if(nii.path[i] %in% master.nii == 'TRUE'){
    nii.path[i] <- nii.path[i]
  } else {
    nii.path[i] <- NA
  }
}

df.core$nii.path <- nii.path

#How many subjects with lesion masks?

nrow(df.core) - ((percent.missing(df.core$nii.path)/100) * nrow(df.core))

#Age at scan

df.core$age.at.scan <- as.numeric(as.Date(df.core$scan.date) - as.Date(df.core$dob))/365

mean(df.core$age.at.scan)

sd(df.core$age.at.scan)

range(df.core$age.at.scan)

#Sex

table(df.core$sex)

#Race (Note: Add two to uknown (coded as 7) for missing demographic data)

df.core$race[df.core$race == 1] <- 'american indian'

df.core$race[df.core$race == 2] <- 'asian'

df.core$race[df.core$race == 3] <- 'african american'

df.core$race[df.core$race == 4] <- 'no label'

df.core$race[df.core$race == 5] <- 'caucasian'

df.core$race[df.core$race == 6] <- 'other'

df.core$race[df.core$race == 7] <- 'uknown'

df.core$race[df.core$race == 8] <- 'native pacific islander'

table(df.core$race)

#Hispanic

table(df.core$hispanic)

#Hand

mean(df.core$hand, na.rm = T)

sd(df.core$hand, na.rm = T)

#Education

mean(df.core$edu, na.rm = T)

sd(df.core$edu, na.rm = T)

#Lesion etiology

df.core$etiology <- as.character(df.core$etiology)

df.core$etiology[df.core$etiology == 'STROKE'] <- 'stroke'

df.core$etiology[df.core$etiology == 'RESECTION'] <- 'resection'

df.core$etiology[df.core$etiology == 'RESECTION '] <- 'resection'

df.core$etiology[df.core$etiology == 'ENCEPHALITIS'] <- 'encephalitis'

df.core$subetiology <- as.character(df.core$subetiology)

df.core$subetiology[df.core$subetiology == 'TUMOR'] <- 'tumor'

df.core$subetiology[df.core$subetiology == 'HEMORRHAGE'] <- 'hemorrhagic'

df.core$subetiology[df.core$subetiology == 'HEMORRHAGIC'] <- 'hemorrhagic'

df.core$subetiology[df.core$subetiology == 'ISCHEMIC'] <- 'ischemic'

df.core$subetiology[df.core$subetiology == 'HSE'] <- 'hse'

df.core$etiology2 <- as.character(df.core$etiology2)

df.core$etiology2[df.core$etiology2 == 'OTHER'] <- 'other'

df.core$etiology2[df.core$etiology2 == 'STROKE'] <- 'stroke'

df.core$etiology2[df.core$etiology2 == 'RESECTION'] <- 'resection'

df.core$subetiology2 <- as.character(df.core$subetiology2)

df.core$subetiology2[df.core$subetiology2 == 'Other (see Dx)'] <- 'other (see dx)'

df.core$subetiology2[df.core$subetiology2 == 'HEMORRHAGIC'] <- 'hemorrhagic'

df.core$subetiology2[df.core$subetiology2 == 'ISCHEMIC'] <- 'ischemic'

df.core$subetiology2[df.core$subetiology2 == 'TUMOR'] <- 'tumor'

table(df.core$etiology)

table(df.core$subetiology)

table(df.core$etiology2)

table(df.core$subetiology2)

#Lesion Chronicity

df.core$lesion.chronicity.months <- as.numeric(as.Date(df.core$scan.date) - as.Date(df.core$onset))/30

mean(df.core$lesion.chronicity.months)

sd(df.core$lesion.chronicity.months)

range(df.core$lesion.chronicity.months)

df.core <- subset(df.core, lesion.chronicity.months >= 3)

nrow(df.core)

#Recode scan type

df.core$scan.type <- as.character(df.core$scan.type)

df.core$scan.type[df.core$scan.type == 'V'] <- 'mri'

df.core$scan.type[df.core$scan.type == 'v'] <- 'mri'

df.core$scan.type[df.core$scan.type == 'M'] <- 'mri'

df.core$scan.type[df.core$scan.type == 'C'] <- 'ct'

df.core$scan.type[df.core$scan.type == 'T'] <- 't'

table(df.core$scan.type)

#Clean up environment

rm(list = ls()[grep('^df.master', ls())])

rm(list = ls()[grep('^df.core.final', ls())])

rm(list = ls()[grep('^df.core.scan', ls())])

rm(list = ls()[grep('^subject.dfs', ls())])

rm(list = ls()[grep('^subject.vector', ls())])

#Subset for only people with lesion masks

df.core <- subset(df.core, is.na(nii.path) == 'FALSE')

nrow(df.core)

#Print id list to directory for FSL processing in bash to get volumes

cat(na.omit(df.core$nii.path), file = '/Users/markbowren/Documents/uiowa/core/lsm/lesymap/running/sem_z/nii/nii_path.txt', sep = '\n')

#The following code is commented out to avoid significantly high run times; after being calculated the first time, just put the lv values back in using the saved dataframe
#Read in lesion mask data

library(ANTsRCore)

library(ANTsR)

filenames.core <- c(as.character('/Users/markbowren/googleDrive/uiowa/researchResources/structuralMR/mniTemplate/MNI152_T1_1mm_brain_mask.nii.gz'), na.omit(df.core$nii.path))

image.list.core <- imageFileNames2ImageList(filenames.core)

nii.matrix.core <- imageListToMatrix(image.list.core) #A matrix with rows as subjects and columns as voxels; the first row is MNI T1 1mm brain mask which is needed as reference for the remaining images

nii.matrix.core <- nii.matrix.core[,-1]

#Make a column in df.core for lesion volume

j <- 1

k <- 2

lv <- vector()

while(j <= length(df.core$id)){
  if(is.na(df.core[j,]$nii.path) == 'FALSE'){
    lv[j] <- sum(nii.matrix.core[k,])
    k <- k + 1
  } else {
    lv[j] <- NA
  }
  j <- j + 1
}

df.core$lv <- lv

rm(nii.matrix.core)

rm(list = ls()[grep('^filename', ls())])

#Calculate left/right/bilateral lesion volumes

filenames.core.l <- c(as.character('/Users/markbowren/googleDrive/uiowa/researchResources/structuralMR/harvardOxford/thr50_left_hemisphere.nii.gz'), na.omit(df.core$nii.path))

image.list.core.l <- imageFileNames2ImageList(filenames.core.l)

nii.matrix.core.l <- imageListToMatrix(image.list.core.l) #A matrix with rows as subjects and columns as voxels; the first row is MNI T1 1mm brain mask which is needed as reference for the remaining images

j <- 1

k <- 2

lv.l <- vector()

while(j <= length(df.core$id)){
  if(is.na(df.core[j,]$nii.path) == 'FALSE'){
    lv.l[j] <- sum(nii.matrix.core.l[k,])
    k <- k + 1
  } else {
    lv.l[j] <- NA
  }
  j <- j + 1
}

df.core$lv.l <- lv.l

rm(nii.matrix.core.l)

filenames.core.r <- c(as.character('/Users/markbowren/googleDrive/uiowa/researchResources/structuralMR/harvardOxford/thr50_right_hemisphere.nii.gz'), na.omit(df.core$nii.path))

image.list.core.r <- imageFileNames2ImageList(filenames.core.r)

nii.matrix.core.r <- imageListToMatrix(image.list.core.r) #A matrix with rows as subjects and columns as voxels; the first row is MNI T1 1mm brain mask which is needed as reference for the remaining images

j <- 1

k <- 2

lv.r <- vector()

while(j <= length(df.core$id)){
  if(is.na(df.core[j,]$nii.path) == 'FALSE'){
    lv.r[j] <- sum(nii.matrix.core.r[k,])
    k <- k + 1
  } else {
    lv.r[j] <- NA
  }
  j <- j + 1
}

df.core$lv.r <- lv.r

rm(nii.matrix.core.r)

#Write to file

df.lv <- data.frame(df.core$id, lv, df.core$lv.l, df.core$lv.r)

write.csv(df.lv, file = '/Users/markbowren/Documents/uiowa/core/data/lv.csv')

#Age at lesion onset

range(df.core$age.at.lesion.onset)

mean(df.core$age.at.lesion.onset)

sd(df.core$age.at.lesion.onset)

#Recode group

df.core$group <- as.character(df.core$group)

df.core$group[df.core$group == 'GROUP'] <- 'group'

df.core$group[df.core$group == 'CASE STUDY'] <- 'case study'

table(df.core$group)

#Check ranges on each test

df.core.tests <- subset(df.core, select = c('sim', 'inf', 'wrat', 'bnt', 'bd', 'jlo', 'cftc', 'cftr', 'reyr', 'rey5', 'reyh', 'tmta', 'tmtb', 'cod', 'dig', 'ari'))

apply(na.omit(df.core.tests), 2, range)

#Create age columns, commented out because age is already calculated in the spreadsheet; this code is just if needed for future analyses

df.core$sim.age <- as.numeric(as.Date(df.core$sim.date) - as.Date(df.core$dob))/365

df.core$inf.age <- as.numeric(as.Date(df.core$inf.date) - as.Date(df.core$dob))/365

df.core$wrat.age <- as.numeric(as.Date(df.core$wrat.date) - as.Date(df.core$dob))/365

df.core$bnt.age <- as.numeric(as.Date(df.core$bnt.date) - as.Date(df.core$dob))/365

df.core$bd.age <- as.numeric(as.Date(df.core$bd.date) - as.Date(df.core$dob))/365

df.core$jlo.age <- as.numeric(as.Date(df.core$jlo.date) - as.Date(df.core$dob))/365

df.core$cftc.age <- as.numeric(as.Date(df.core$cftc.date) - as.Date(df.core$dob))/365

df.core$cftr.age <- as.numeric(as.Date(df.core$cftr.date) - as.Date(df.core$dob))/365

df.core$vrtc.age <- as.numeric(as.Date(df.core$vrtc.date) - as.Date(df.core$dob))/365

df.core$reyr.age <- as.numeric(as.Date(df.core$reyr.date) - as.Date(df.core$dob))/365

df.core$rey5.age <- as.numeric(as.Date(df.core$rey5.date) - as.Date(df.core$dob))/365

df.core$reyh.age <- as.numeric(as.Date(df.core$reyh.date) - as.Date(df.core$dob))/365

df.core$tmta.age <- as.numeric(as.Date(df.core$tmta.date) - as.Date(df.core$dob))/365

df.core$tmtb.age <- as.numeric(as.Date(df.core$tmtb.date) - as.Date(df.core$dob))/365

df.core$cod.age <- as.numeric(as.Date(df.core$cod.date) - as.Date(df.core$dob))/365

#Select only rows with a nifty file

df.core <- subset(df.core, is.na(df.core$nii.path) == 'FALSE')

nrow(df.core)

#Determine lesion side

l.lesions <- nrow(subset(df.core, lv.l >= 50 & lv.r <= 50))

l.lesions

r.lesions <- nrow(subset(df.core, lv.r >= 50 & lv.l <= 50))

r.lesions

bi.lesions <- nrow(subset(df.core, lv.r >= 50 & lv.l >= 50))

bi.lesions

r.lesions + l.lesions + bi.lesions

#Enter lesion laterality variable

df.core.l <- subset(df.core, lv.l >= 50 & lv.r <= 50)

df.core.r <- subset(df.core, lv.r >= 50 & lv.l <= 50)

df.core.bi <- subset(df.core, lv.r >= 50 & lv.l >= 50)

df.core.l$lesion.lat <- as.character('left')

df.core.r$lesion.lat <- as.character('right')

df.core.bi$lesion.lat <- as.character('bi')

df.core <- rbind(df.core.l, df.core.r, df.core.bi)

df.core <- df.core[order(df.core$id), ]

#Make final dataframe putting variables in neat order

df.core <- subset(df.core, select = c('id', 'dob', 'sex', 'race', 'hispanic', 'hand', 'edu', 'onset', 'age.at.lesion.onset', 'dx', 'lesion.chronicity.months', 'lv', 'lv.l', 'lv.r', 'etiology', 'subetiology', 'etiology2', 'subetiology2', 'group', 'scan.date', 'age.at.scan', 'n.scans', 'scan.type', 'nii.path', 'sim', 'sim.date', 'date.diff.sim', 'sim.age', 'inf', 'inf.date', 'date.diff.inf', 'inf.age', 'wrat', 'wrat.age', 'wrat.date', 'date.diff.wrat', 'bnt', 'bnt.date', 'date.diff.bnt', 'bd', 'bd.date', 'date.diff.bd', 'bd.age', 'jlo', 'jlo.date', 'date.diff.jlo', 'jlo.age', 'vrtc', 'vrtc.date', 'date.diff.vrtc', 'vrtc.age', 'cftc', 'cftc.date', 'date.diff.cftc', 'cftc.age', 'cftr', 'cftr.date', 'date.diff.cftr', 'cftr.age', 'rey5', 'rey5.date', 'date.diff.rey5', 'rey5.age', 'reyr', 'reyr.date', 'date.diff.reyr', 'reyr.age', 'reyh', 'reyh.date', 'date.diff.reyh', 'reyh.age', 'cod', 'cod.date', 'date.diff.cod', 'cod.age', 'tmta', 'tmta.date', 'date.diff.tmta', 'tmta.age', 'tmtb', 'tmtb.date', 'date.diff.tmtb', 'tmtb.age', 'dig', 'dig.date', 'date.diff.dig', 'ari', 'ari.date', 'date.diff.ari'))

#Save to hard file

write.csv(df.core, file = '/Users/markbowren/Documents/uiowa/core/data/core_master_402.csv')

