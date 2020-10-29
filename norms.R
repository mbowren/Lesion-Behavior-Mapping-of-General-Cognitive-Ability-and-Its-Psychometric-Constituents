#Read in raw data

df.core.master <- read.csv('/Users/markbowren/drive/uiowa/core/data/old/402/core_master_402.csv')

df.core <- df.core.master

#Set up missing function

percent.missing <- function (x){
  sum(is.na(x)/length(x) * 100)
}

#Applying Normative Corrections

#Transformed to standard units based on Mitrushina (2005) Handbook of Normative Data

#Boston Naming Test - not included in dataset but here if I ever need to look it up

#df.core$bnt.pred <- 47.36842 + 0.4489501*df.core$bnt.age - 0.0052924*(df.core$bnt.age^2)

#df.core$bnt.pred.sd <- 4.542304 - 0.0992503*df.core$bnt.age + 0.0016771*(df.core$bnt.age^2)

#df.core$bnt.norm <- (df.core$bnt - df.core$bnt.pred)/(df.core$bnt.pred.sd)

#Benton Visual Retention Test (not used in g/Gwm study)

df.core$vrtc.norm <- NA

df.core.vrtc.edu.14.age.39 <- subset(df.core, edu <= 14 & vrtc.age < 40)
df.core.vrtc.edu.14.age.40.49 <- subset(df.core, edu <= 14 & vrtc.age >= 40 & vrtc.age < 50)
df.core.vrtc.edu.14.age.50.59 <- subset(df.core, edu <= 14 & vrtc.age >= 50 & vrtc.age < 60)
df.core.vrtc.edu.14.age.60.69 <- subset(df.core, edu <= 14 & vrtc.age >= 60 & vrtc.age < 70)
df.core.vrtc.edu.14.age.70 <- subset(df.core, edu <= 14 & vrtc.age > 70)
df.core.vrtc.edu.14.age.na <- subset(df.core, edu <= 14 & is.na(vrtc.age) == 'TRUE')

df.core.vrtc.edu.14.age.39$vrtc.norm <- (df.core.vrtc.edu.14.age.39$vrtc - 7.59)/1.52
df.core.vrtc.edu.14.age.40.49$vrtc.norm <- (df.core.vrtc.edu.14.age.40.49$vrtc - 7.11)/1.53
df.core.vrtc.edu.14.age.50.59$vrtc.norm <- (df.core.vrtc.edu.14.age.50.59$vrtc - 6.66)/1.47
df.core.vrtc.edu.14.age.60.69$vrtc.norm <- (df.core.vrtc.edu.14.age.60.69$vrtc - 6.18)/1.67
df.core.vrtc.edu.14.age.70$vrtc.norm <- (df.core.vrtc.edu.14.age.70$vrtc - 5.62)/1.73

df.core.vrtc.edu.15.17.age.39 <- subset(df.core, edu > 14 & edu <= 17 & vrtc.age < 40)
df.core.vrtc.edu.15.17.age.40.49 <- subset(df.core, edu > 14 & edu <= 17 & vrtc.age >= 40 & vrtc.age < 50)
df.core.vrtc.edu.15.17.age.50.59 <- subset(df.core, edu > 14 & edu <= 17 & vrtc.age >= 50 & vrtc.age < 60)
df.core.vrtc.edu.15.17.age.60.69 <- subset(df.core, edu > 14 & edu <= 17 & vrtc.age >= 60 & vrtc.age < 70)
df.core.vrtc.edu.15.17.age.70 <- subset(df.core, edu > 14 & edu <= 17 & vrtc.age > 70)
df.core.vrtc.edu.15.17.age.na <- subset(df.core, edu > 14 & edu <= 17 & is.na(vrtc.age) == 'TRUE')

df.core.vrtc.edu.15.17.age.39$vrtc.norm <- (df.core.vrtc.edu.15.17.age.39$vrtc - 8.04)/1.19
df.core.vrtc.edu.15.17.age.40.49$vrtc.norm <- (df.core.vrtc.edu.15.17.age.40.49$vrtc - 7.78)/1.54
df.core.vrtc.edu.15.17.age.50.59$vrtc.norm <- (df.core.vrtc.edu.15.17.age.50.59$vrtc - 7.08)/1.70
df.core.vrtc.edu.15.17.age.60.69$vrtc.norm <- (df.core.vrtc.edu.15.17.age.60.69$vrtc - 6.70)/1.47
df.core.vrtc.edu.15.17.age.70$vrtc.norm <- (df.core.vrtc.edu.15.17.age.70$vrtc - 6.06)/1.84

df.core.vrtc.edu.18.age.39 <- subset(df.core, edu > 17 & vrtc.age < 40)
df.core.vrtc.edu.18.age.40.49 <- subset(df.core, edu > 17 & vrtc.age >= 40 & vrtc.age < 50)
df.core.vrtc.edu.18.age.50.59 <- subset(df.core, edu > 17 & vrtc.age >= 50 & vrtc.age < 60)
df.core.vrtc.edu.18.age.60.69 <- subset(df.core, edu > 17 & vrtc.age >= 60 & vrtc.age < 70)
df.core.vrtc.edu.18.age.70 <- subset(df.core, edu > 17 & vrtc.age > 70)
df.core.vrtc.edu.18.age.na <- subset(df.core, edu > 17 & is.na(vrtc.age) == 'TRUE')

df.core.vrtc.edu.18.age.39$vrtc.norm <- (df.core.vrtc.edu.18.age.39$vrtc - 8.11)/1.28
df.core.vrtc.edu.18.age.40.49$vrtc.norm <- (df.core.vrtc.edu.18.age.40.49$vrtc - 7.42)/1.22
df.core.vrtc.edu.18.age.50.59$vrtc.norm <- (df.core.vrtc.edu.18.age.50.59$vrtc - 7.55)/1.53
df.core.vrtc.edu.18.age.60.69$vrtc.norm <- (df.core.vrtc.edu.18.age.60.69$vrtc - 6.80)/1.55
df.core.vrtc.edu.18.age.70$vrtc.norm <- (df.core.vrtc.edu.18.age.70$vrtc - 6.22)/1.57

df.core <- rbind(df.core.vrtc.edu.14.age.39, df.core.vrtc.edu.14.age.40.49, df.core.vrtc.edu.14.age.50.59, df.core.vrtc.edu.14.age.60.69, df.core.vrtc.edu.14.age.70, df.core.vrtc.edu.14.age.na,
                 df.core.vrtc.edu.15.17.age.39, df.core.vrtc.edu.15.17.age.40.49, df.core.vrtc.edu.15.17.age.50.59, df.core.vrtc.edu.15.17.age.60.69, df.core.vrtc.edu.15.17.age.70, df.core.vrtc.edu.15.17.age.na,
                 df.core.vrtc.edu.18.age.39, df.core.vrtc.edu.18.age.40.49, df.core.vrtc.edu.18.age.50.59, df.core.vrtc.edu.18.age.60.69, df.core.vrtc.edu.18.age.70, df.core.vrtc.edu.18.age.na)

nrow(df.core)

#df.core <- df.core[order(df.core$id), ]

#Rey AVLT

#Note: The Rey AVLT norms are taken from Ken and Nick's excel sheet. 
#The meta-norms were not a good fit for this test due to administration differences across normative reports. 
#E.g., the recognition trial came before recall in some studies. 
#As a result, the meta-norms are only provided for some measures. 
#To be consistent across this test, I use the Schmidt (1996) norms.

#Rey AVLT Trial 5

df.core.rey5.16.19 <- subset(df.core, rey5.age >= 16 & rey5.age < 20)
df.core.rey5.20.29 <- subset(df.core, rey5.age >= 20 & rey5.age < 30)
df.core.rey5.30.39 <- subset(df.core, rey5.age >= 30 & rey5.age < 40)
df.core.rey5.40.49 <- subset(df.core, rey5.age >= 40 & rey5.age < 50)
df.core.rey5.50.59 <- subset(df.core, rey5.age >= 50 & rey5.age < 60)
df.core.rey5.60.69 <- subset(df.core, rey5.age >= 60 & rey5.age < 70)
df.core.rey5.70.79 <- subset(df.core, rey5.age >= 70 & rey5.age < 80)
df.core.rey5.80.89 <- subset(df.core, rey5.age >= 80)
df.core.rey5.na <- subset(df.core, is.na(df.core$rey5.age))

df.core.rey5.16.19$rey5.norm <- (df.core.rey5.16.19$rey5 - 12.8)/1.4
df.core.rey5.20.29$rey5.norm <- (df.core.rey5.20.29$rey5 - 12.9)/1.8
df.core.rey5.30.39$rey5.norm <- (df.core.rey5.30.39$rey5 - 12.7)/1.9
df.core.rey5.40.49$rey5.norm <- (df.core.rey5.40.49$rey5 - 12.3)/1.9
df.core.rey5.50.59$rey5.norm <- (df.core.rey5.50.59$rey5 - 12.1)/2.1
df.core.rey5.60.69$rey5.norm <- (df.core.rey5.60.69$rey5 - 11.3)/2.3
df.core.rey5.70.79$rey5.norm <- (df.core.rey5.70.79$rey5 - 10.3)/2.4
df.core.rey5.80.89$rey5.norm <- (df.core.rey5.80.89$rey5 - 10)/2.3
df.core.rey5.na$rey5.norm <- NA

df.core <- rbind(df.core.rey5.16.19, df.core.rey5.20.29, df.core.rey5.30.39, df.core.rey5.40.49, df.core.rey5.50.59, df.core.rey5.60.69, df.core.rey5.70.79, df.core.rey5.80.89, df.core.rey5.na)

nrow(df.core)

#Rey AVLT Delayed Recall

df.core.reyr.16.19 <- subset(df.core, reyr.age >= 16 & reyr.age < 20)
df.core.reyr.20.29 <- subset(df.core, reyr.age >= 20 & reyr.age < 30)
df.core.reyr.30.39 <- subset(df.core, reyr.age >= 30 & reyr.age < 40)
df.core.reyr.40.49 <- subset(df.core, reyr.age >= 40 & reyr.age < 50)
df.core.reyr.50.59 <- subset(df.core, reyr.age >= 50 & reyr.age < 60)
df.core.reyr.60.69 <- subset(df.core, reyr.age >= 60 & reyr.age < 70)
df.core.reyr.70.79 <- subset(df.core, reyr.age >= 70 & reyr.age < 80)
df.core.reyr.80.89 <- subset(df.core, reyr.age >= 80)
df.core.reyr.na <- subset(df.core, is.na(df.core$reyr.age))

df.core.reyr.16.19$reyr.norm <- (df.core.reyr.16.19$reyr - 11.7)/2.2
df.core.reyr.20.29$reyr.norm <- (df.core.reyr.20.29$reyr - 11.3)/2.5
df.core.reyr.30.39$reyr.norm <- (df.core.reyr.30.39$reyr - 11.1)/2.8
df.core.reyr.40.49$reyr.norm <- (df.core.reyr.40.49$reyr - 10.2)/2.8
df.core.reyr.50.59$reyr.norm <- (df.core.reyr.50.59$reyr - 9.9)/3.2
df.core.reyr.60.69$reyr.norm <- (df.core.reyr.60.69$reyr - 8.8)/3
df.core.reyr.70.79$reyr.norm <- (df.core.reyr.70.79$reyr - 7)/2.4
df.core.reyr.80.89$reyr.norm <- (df.core.reyr.80.89$reyr - 7)/2.4
df.core.reyr.na$reyr.norm <- NA

df.core <- rbind(df.core.reyr.16.19, df.core.reyr.20.29, df.core.reyr.30.39, df.core.reyr.40.49, df.core.reyr.50.59, df.core.reyr.60.69, df.core.reyr.70.79, df.core.reyr.80.89, df.core.reyr.na)

nrow(df.core)

#Rey AVLT Recognition Hits

df.core.reyh.16.19 <- subset(df.core, reyh.age >= 16 & reyh.age < 20)
df.core.reyh.20.29 <- subset(df.core, reyh.age >= 20 & reyh.age < 30)
df.core.reyh.30.39 <- subset(df.core, reyh.age >= 30 & reyh.age < 40)
df.core.reyh.40.49 <- subset(df.core, reyh.age >= 40 & reyh.age < 50)
df.core.reyh.50.59 <- subset(df.core, reyh.age >= 50 & reyh.age < 60)
df.core.reyh.60.69 <- subset(df.core, reyh.age >= 60 & reyh.age < 70)
df.core.reyh.70.79 <- subset(df.core, reyh.age >= 70 & reyh.age < 80)
df.core.reyh.80.89 <- subset(df.core, reyh.age >= 80)
df.core.reyh.na <- subset(df.core, is.na(df.core$reyh.age))

df.core.reyh.16.19$reyh.norm <- (df.core.reyh.16.19$reyh - 14.2)/1.2
df.core.reyh.20.29$reyh.norm <- (df.core.reyh.20.29$reyh - 14.3)/1.1
df.core.reyh.30.39$reyh.norm <- (df.core.reyh.30.39$reyh - 14.2)/1.2
df.core.reyh.40.49$reyh.norm <- (df.core.reyh.40.49$reyh - 14)/1.4
df.core.reyh.50.59$reyh.norm <- (df.core.reyh.50.59$reyh - 13.9)/1.4
df.core.reyh.60.69$reyh.norm <- (df.core.reyh.60.69$reyh - 13.5)/1.3
df.core.reyh.70.79$reyh.norm <- (df.core.reyh.70.79$reyh - 13.3)/1.5
df.core.reyh.80.89$reyh.norm <- (df.core.reyh.80.89$reyh - 13)/2.3
df.core.reyh.na$reyh.norm <- NA

df.core <- rbind(df.core.reyh.16.19, df.core.reyh.20.29, df.core.reyh.30.39, df.core.reyh.40.49, df.core.reyh.50.59, df.core.reyh.60.69, df.core.reyh.70.79, df.core.reyh.80.89, df.core.reyh.na)

nrow(df.core)

df.core <- df.core[order(df.core$id), ]

#Rey-O CFT Copy

df.core$cftc.pred <- 34.40434 + 0.0595862*df.core$cftc.age - 0.0013855*(df.core$cftc.age^2)

df.core$cftc.pred.sd <- 0.333026 + 0.0625042*df.core$cftc.age

df.core$cftc.norm <- (df.core$cftc - df.core$cftc.pred)/(df.core$cftc.pred.sd)

#Rey-O CFT Recall

df.core$cftr.pred <- 25.39903 + 0.0416485*df.core$cftr.age - 0.0022144*(df.core$cftr.age^2)

df.core$cftr.pred.sd <- 6.67

df.core$cftr.norm <- (df.core$cftr - df.core$cftr.pred)/(df.core$cftr.pred.sd)

#Trails A

df.core$tmta.pred <- 26.50094 - 0.2665049*df.core$tmta.age + 0.0069935*(df.core$tmta.age^2)

df.core$tmta.pred.sd <- 8.760348 - 0.1138093*df.core$tmta.age + 0.0028324*(df.core$tmta.age^2)

df.core$tmta.norm <- (df.core$tmta - df.core$tmta.pred)/(df.core$tmta.pred.sd)

#Trails B

df.core$tmtb.pred <- 64.07469 - 0.9881013*df.core$tmtb.age + 0.0235581*(df.core$tmtb.age^2)

df.core$tmtb.pred.sd <- 29.8444 - 0.8080508*df.core$tmtb.age + 0.0148732*(df.core$tmtb.age^2)

df.core$tmtb.norm <- (df.core$tmtb - df.core$tmtb.pred)/(df.core$tmtb.pred.sd)

#JLO

df.core$jlo[df.core$jlo == 32 | df.core$jlo == 31 | df.core$jlo == 30 | df.core$jlo == 29 | df.core$jlo == 28 | df.core$jlo == 27] <- 0.64

df.core$jlo[df.core$jlo == 26 | df.core$jlo == 25] <- 0.18

df.core$jlo[df.core$jlo == 24 | df.core$jlo == 23] <- -0.25

df.core$jlo[df.core$jlo == 22 | df.core$jlo == 21] <- -0.77

df.core$jlo[df.core$jlo == 20 | df.core$jlo == 19] <- -1.23

df.core$jlo[df.core$jlo == 18 | df.core$jlo == 17] <- -1.64

df.core$jlo[df.core$jlo == 16 | df.core$jlo == 15] <- -1.88

df.core$jlo[df.core$jlo == 14 | df.core$jlo == 13 | df.core$jlo == 12 | df.core$jlo == 11 | df.core$jlo == 10 | df.core$jlo == 9 | df.core$jlo == 8 | df.core$jlo == 7 | df.core$jlo == 6 | df.core$jlo == 5 | df.core$jlo == 4 | df.core$jlo == 3 | df.core$jlo == 2 | df.core$jlo == 1] <- -2.46

#Similarities 

df.core$sim[df.core$sim == 18] <- 2.67

df.core$sim[df.core$sim == 17] <- 2.33

df.core$sim[df.core$sim == 15] <- 1.67

df.core$sim[df.core$sim == 14] <- 1.33

df.core$sim[df.core$sim == 12] <- 0.67

df.core$sim[df.core$sim == 11] <- 0.33

df.core$sim[df.core$sim == 10] <- 0.00

df.core$sim[df.core$sim == 9] <- -0.33

df.core$sim[df.core$sim == 8] <--0.67

df.core$sim[df.core$sim == 7] <- -1.00

df.core$sim[df.core$sim == 6] <- -1.33

df.core$sim[df.core$sim == 5] <- -1.67

df.core$sim[df.core$sim == 4] <- -2.00

df.core$sim[df.core$sim == 3] <- -2.33

df.core$sim[df.core$sim == 2] <- -2.67

df.core$sim[df.core$sim == 1] <- -3.00

df.core$sim[df.core$sim == 19] <- 3.00

df.core$sim[df.core$sim == 16] <- 2.00

df.core$sim[df.core$sim == 13] <- 1.00

#Information

df.core$inf[df.core$inf == 18] <- 2.67

df.core$inf[df.core$inf == 17] <- 2.33

df.core$inf[df.core$inf == 15] <- 1.67

df.core$inf[df.core$inf == 14] <- 1.33

df.core$inf[df.core$inf == 12] <- 0.67

df.core$inf[df.core$inf == 11] <- 0.33

df.core$inf[df.core$inf == 10] <- 0.00

df.core$inf[df.core$inf == 9] <- -0.33

df.core$inf[df.core$inf == 8] <--0.67

df.core$inf[df.core$inf == 7] <- -1.00

df.core$inf[df.core$inf == 6] <- -1.33

df.core$inf[df.core$inf == 5] <- -1.67

df.core$inf[df.core$inf == 4] <- -2.00

df.core$inf[df.core$inf == 3] <- -2.33

df.core$inf[df.core$inf == 2] <- -2.67

df.core$inf[df.core$inf == 1] <- -3.00

df.core$inf[df.core$inf == 19] <- 3.00

df.core$inf[df.core$inf == 16] <- 2.00

df.core$inf[df.core$inf == 13] <- 1.00

#Block Design

df.core$bd[df.core$bd == 18] <- 2.67

df.core$bd[df.core$bd == 17] <- 2.33

df.core$bd[df.core$bd == 15] <- 1.67

df.core$bd[df.core$bd == 14] <- 1.33

df.core$bd[df.core$bd == 12] <- 0.67

df.core$bd[df.core$bd == 11] <- 0.33

df.core$bd[df.core$bd == 10] <- 0.00

df.core$bd[df.core$bd == 9] <- -0.33

df.core$bd[df.core$bd == 8] <--0.67

df.core$bd[df.core$bd == 7] <- -1.00

df.core$bd[df.core$bd == 6] <- -1.33

df.core$bd[df.core$bd == 5] <- -1.67

df.core$bd[df.core$bd == 4] <- -2.00

df.core$bd[df.core$bd == 3] <- -2.33

df.core$bd[df.core$bd == 2] <- -2.67

df.core$bd[df.core$bd == 1] <- -3.00

df.core$bd[df.core$bd == 19] <- 3.00

df.core$bd[df.core$bd == 16] <- 2.00

df.core$bd[df.core$bd == 13] <- 1.00

#Coding

df.core$cod[df.core$cod == 18] <- 2.67

df.core$cod[df.core$cod == 17] <- 2.33

df.core$cod[df.core$cod == 15] <- 1.67

df.core$cod[df.core$cod == 14] <- 1.33

df.core$cod[df.core$cod == 12] <- 0.67

df.core$cod[df.core$cod == 11] <- 0.33

df.core$cod[df.core$cod == 10] <- 0.00

df.core$cod[df.core$cod == 9] <- -0.33

df.core$cod[df.core$cod == 8] <- -0.67

df.core$cod[df.core$cod == 7] <- -1.00

df.core$cod[df.core$cod == 6] <- -1.33

df.core$cod[df.core$cod == 5] <- -1.67

df.core$cod[df.core$cod == 4] <- -2.00

df.core$cod[df.core$cod == 3] <- -2.33

df.core$cod[df.core$cod == 2] <- -2.67

df.core$cod[df.core$cod == 1] <- -3.00

df.core$cod[df.core$cod == 19] <- 3.00

df.core$cod[df.core$cod == 16] <- 2.00

df.core$cod[df.core$cod == 13] <- 1.00

#Digit Span

df.core$dig[df.core$dig == 18] <- 2.67

df.core$dig[df.core$dig == 17] <- 2.33

df.core$dig[df.core$dig == 15] <- 1.67

df.core$dig[df.core$dig == 14] <- 1.33

df.core$dig[df.core$dig == 12] <- 0.67

df.core$dig[df.core$dig == 11] <- 0.33

df.core$dig[df.core$dig == 10] <- 0.00

df.core$dig[df.core$dig == 9] <- -0.33

df.core$dig[df.core$dig == 8] <- -0.67

df.core$dig[df.core$dig == 7] <- -1.00

df.core$dig[df.core$dig == 6] <- -1.33

df.core$dig[df.core$dig == 5] <- -1.67

df.core$dig[df.core$dig == 4] <- -2.00

df.core$dig[df.core$dig == 3] <- -2.33

df.core$dig[df.core$dig == 2] <- -2.67

df.core$dig[df.core$dig == 1] <- -3.00

df.core$dig[df.core$dig == 19] <- 3.00

df.core$dig[df.core$dig == 16] <- 2.00

df.core$dig[df.core$dig == 13] <- 1.00

#Arithmetic

df.core$ari[df.core$ari == 18] <- 2.67

df.core$ari[df.core$ari == 17] <- 2.33

df.core$ari[df.core$ari == 15] <- 1.67

df.core$ari[df.core$ari == 14] <- 1.33

df.core$ari[df.core$ari == 12] <- 0.67

df.core$ari[df.core$ari == 11] <- 0.33

df.core$ari[df.core$ari == 10] <- 0.00

df.core$ari[df.core$ari == 9] <- -0.33

df.core$ari[df.core$ari == 8] <- -0.67

df.core$ari[df.core$ari == 7] <- -1.00

df.core$ari[df.core$ari == 6] <- -1.33

df.core$ari[df.core$ari == 5] <- -1.67

df.core$ari[df.core$ari == 4] <- -2.00

df.core$ari[df.core$ari == 3] <- -2.33

df.core$ari[df.core$ari == 2] <- -2.67

df.core$ari[df.core$ari == 1] <- -3.00

df.core$ari[df.core$ari == 19] <- 3.00

df.core$ari[df.core$ari == 16] <- 2.00

df.core$ari[df.core$ari == 13] <- 1.00

#WRAT Word Reading

df.core$wrat.z <- (df.core$wrat - 100)/15

#New dataframe with norm corrected values

df.core.norm <- subset(df.core, select = c('sim', 'inf', 'wrat.z', 'bd', 'jlo', 'cftc.norm', 'cftr.norm', 'reyr.norm', 'rey5.norm', 'reyh.norm', 'tmta.norm', 'tmtb.norm', 'cod', 'dig', 'ari', 'vrtc.norm'))

colnames(df.core.norm) <- c('sim', 'inf', 'wrat', 'bd', 'jlo', 'cftc', 'cftr', 'reyr', 'rey5', 'reyh', 'tmta', 'tmtb', 'cod', 'dig', 'ari', 'vrtc')

#Make numeric

df.core.norm$sim <- as.numeric(df.core.norm$sim)
df.core.norm$inf <- as.numeric(df.core.norm$inf)
df.core.norm$wrat <- as.numeric(df.core.norm$wrat)
df.core.norm$bd <- as.numeric(df.core.norm$bd)
df.core.norm$jlo <- as.numeric(df.core.norm$jlo)
df.core.norm$cftc <- as.numeric(df.core.norm$cftc)
df.core.norm$cftr <- as.numeric(df.core.norm$cftr)
df.core.norm$reyr <- as.numeric(df.core.norm$reyr)
df.core.norm$rey5 <- as.numeric(df.core.norm$rey5)
df.core.norm$reyh <- as.numeric(df.core.norm$reyh)
df.core.norm$tmta <- as.numeric(df.core.norm$tmta)
df.core.norm$tmtb <- as.numeric(df.core.norm$tmtb)
df.core.norm$cod <- as.numeric(df.core.norm$cod)
df.core.norm$dig <- as.numeric(df.core.norm$dig)
df.core.norm$ari <- as.numeric(df.core.norm$ari)
df.core.norm$vrtc <- as.numeric(df.core.norm$vrtc)

#Recode Direction of Trails A and B

df.core.norm$tmta <- df.core.norm$tmta*(-1)

df.core.norm$tmtb <- df.core.norm$tmtb*(-1)

#Save non-imputed data

df.core.norm$id <- df.core.master$id

df.core.norm$nii.path <- df.core.master$nii.path

write.csv(df.core.norm, file = '/Users/markbowren/drive/uiowa/core/data/df_core_norm_no_imputation.csv')

#Impute data

library(mice)

df.core.norm.tests <- subset(df.core.norm, select = c('sim', 'inf', 'wrat', 'bd', 'jlo', 'cftc', 'cftr', 'reyr', 'rey5', 'reyh', 'tmta', 'tmtb', 'cod', 'dig', 'ari', 'vrtc'))

df.core.mice <- mice(df.core.norm.tests, maxit = 5, m = 5)

df.core.complete <- complete(df.core.mice, 1)

apply(na.omit(df.core.complete), 2, range)

df.core.complete$id <- df.core$id

df.core.complete$nii.path <- df.core$nii.path

df.core.norm <- df.core.complete

#Save dataframe

write.csv(df.core.norm, file = '/Users/markbowren/drive/uiowa/core/data/old/402/df_core_norm.csv')

#Percent missing data in entire dataframe

df.core.ni <- read.csv('/Users/markbowren/drive/uiowa/core/data/old/402/df_core_norm_no_imputation.csv')

df.core.ni <- df.core.ni[,-1]

df.core.ni <- df.core.ni[,-ncol(df.core.ni)]

df.core.ni <- df.core.ni[,-ncol(df.core.ni)]

df.core.ni <- df.core.ni[,-ncol(df.core.ni)]

sum(is.na(df.core.ni))/prod(dim(df.core.ni))*100

#Percent missing by test

apply(df.core.ni, 2, percent.missing)
