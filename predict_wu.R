#Evaluate Predictions of Corbetta Factor Scores

#Read in data

df.corb <- read.csv('/Users/mbowren/drive/datasets/wu/data/FCS_Demographics_and_Behavior_MDF.csv')
df.corb <- subset(df.corb, select = c("id", "nii_path", "age", "lesion_side", "ethnicity", "race", "gender", "education", "handed", "date_stroke", "acute_beh", "acute_scan", "redcap_event_name", "word_scaled", "commands_scaled", "complex_scaled", "boston_scaled", "hvlt_imt", "hvlt_delayt", "hvlt_discrimt", "ss_forsc", "ss_backsc", "bvmt_imt", 'bvmt_delayt', "fim_total", "fim_motor", "fim_cognitive", "lv"))
colnames(df.corb) <- c("id", "nii.path", "age", "lesion.side", "eth", "race", "sex", "edu", "hand", "lesion.onset.date", "acute.beh.date", "acute.scan.date", "epoch", "word", "comm", "comp", "bnt", "him", "hd", "hdis", "ssf", "ssb", "bvmi", 'bvmd', "fimt", "fimm", "fimc", "lv")

#Subset for only acute epoch data and people with lesion masks

df.corb.acute <- subset(df.corb, epoch == "acute_arm_1" & nii.path != "")
nrow(df.corb.acute)

#Subset for only those with >75% target tests (target tests = df.corb.acute.data)

df.corb.acute.data <- subset(df.corb.acute, select = c("word", "comp", "bnt", "him", "hd", "hdis", "ssf", "ssb"))

percent.missing <- function (x){
  sum(is.na(x)/length(x) * 100)
}

corb.missing <- apply(df.corb.acute.data, 1, percent.missing)
df.corb.acute <- subset(df.corb.acute, corb.missing < 25)
df.corb.acute.data <- subset(df.corb.acute.data, corb.missing < 25)
nrow(df.corb.acute)
nrow(df.corb.acute.data)

#Get demographics

df.corb.dem <- read.csv('/Users/mbowren/drive/datasets/wu/data/FCS_Demographics_and_Behavior_MDF.csv')
df.corb.dem <- subset(df.corb.dem, redcap_event_name == 'basic_subject_info_arm_1')
df.corb.dem$nii_path[df.corb.dem$nii_path == ''] <- NA
dem.list <- df.corb.acute$id
df.dem <- data.frame()
df.dem.2 <- data.frame()

for(i in dem.list){
  df.dem <- df.corb.dem[df.corb.dem$id == i, ]
  df.dem.2 <- rbind(df.dem.2, df.dem)
}

df.corb.dem <- df.dem.2
nrow(df.corb.dem)
mean(df.corb.dem$age)
sd(df.corb.dem$age)
table(df.corb.dem$gender)
mean(df.corb.dem$education)
sd(df.corb.dem$education)
table(df.corb.dem$handed)
table(df.corb.dem$lesion_side)
df.corb.dem$chronicity <- (as.Date(df.corb.dem$acute_scan) - as.Date(df.corb.dem$date_stroke))/30
mean(df.corb.dem$chronicity)
sd(df.corb.dem$chronicity)
df.corb.acute$lesion.onset.date <- df.corb.dem$lesion.onset.date
df.corb.acute$acute.beh.date <- df.corb.dem$acute.beh.date
df.corb.acute$acute.scan.date <- df.corb.dem$acute.scan.date
df.corb.acute$epoch <- df.corb.dem$epoch

#Convert data to z-scores

#Words

df.corb.acute.data$word[df.corb.acute.data$word == 20] <- -0.84
df.corb.acute.data$word[df.corb.acute.data$word == 30] <- -0.52
df.corb.acute.data$word[df.corb.acute.data$word == 40] <- -0.25
df.corb.acute.data$word[df.corb.acute.data$word == 45] <- -0.13
df.corb.acute.data$word[df.corb.acute.data$word == 50] <- 0.00
df.corb.acute.data$word[df.corb.acute.data$word == 60] <- 0.25
df.corb.acute.data$word[df.corb.acute.data$word == 70] <- 0.52
df.corb.acute.data$word[df.corb.acute.data$word == 100] <- 3.00

#Complex Ideation

df.corb.acute.data$comp[df.corb.acute.data$comp == 0] <- -3.00
df.corb.acute.data$comp[df.corb.acute.data$comp == 15] <- -1.04
df.corb.acute.data$comp[df.corb.acute.data$comp == 20] <- -0.84
df.corb.acute.data$comp[df.corb.acute.data$comp == 30] <- -0.52
df.corb.acute.data$comp[df.corb.acute.data$comp == 40] <- -0.25
df.corb.acute.data$comp[df.corb.acute.data$comp == 50] <- 0.00
df.corb.acute.data$comp[df.corb.acute.data$comp == 60] <- 0.25
df.corb.acute.data$comp[df.corb.acute.data$comp == 80] <- 0.84
df.corb.acute.data$comp[df.corb.acute.data$comp == 90] <- 1.28
df.corb.acute.data$comp[df.corb.acute.data$comp == 100] <- 3.00

#BNT

df.corb.acute.data$bnt[df.corb.acute.data$bnt == 26] <- -0.64
df.corb.acute.data$bnt[df.corb.acute.data$bnt == 45] <- -0.13
df.corb.acute.data$bnt[df.corb.acute.data$bnt == 55] <- 0.13
df.corb.acute.data$bnt[df.corb.acute.data$bnt == 60] <- 0.25
df.corb.acute.data$bnt[df.corb.acute.data$bnt == 70] <- 0.52
df.corb.acute.data$bnt[df.corb.acute.data$bnt == 75] <- 0.67
df.corb.acute.data$bnt[df.corb.acute.data$bnt == 80] <- 0.84
df.corb.acute.data$bnt[df.corb.acute.data$bnt == 85] <- 1.04
df.corb.acute.data$bnt[df.corb.acute.data$bnt == 90] <- 1.28
df.corb.acute.data$bnt[df.corb.acute.data$bnt == 100] <- 3.00

#HVLT Immediate

df.corb.acute.data$him[df.corb.acute.data$him == 0] <- -3.00
df.corb.acute.data$him[df.corb.acute.data$him == 21] <- -0.81
df.corb.acute.data$him[df.corb.acute.data$him == 22] <- -0.77
df.corb.acute.data$him[df.corb.acute.data$him == 24] <- -0.64
df.corb.acute.data$him[df.corb.acute.data$him == 25] <- -0.67
df.corb.acute.data$him[df.corb.acute.data$him == 26] <- -0.64
df.corb.acute.data$him[df.corb.acute.data$him == 27] <- -0.61
df.corb.acute.data$him[df.corb.acute.data$him == 30] <- -0.52
df.corb.acute.data$him[df.corb.acute.data$him == 31] <- -0.50
df.corb.acute.data$him[df.corb.acute.data$him == 32] <- -0.47
df.corb.acute.data$him[df.corb.acute.data$him == 33] <- -0.44
df.corb.acute.data$him[df.corb.acute.data$him == 34] <- -0.41
df.corb.acute.data$him[df.corb.acute.data$him == 35] <- -0.39
df.corb.acute.data$him[df.corb.acute.data$him == 36] <- -0.36
df.corb.acute.data$him[df.corb.acute.data$him == 37] <- -0.33
df.corb.acute.data$him[df.corb.acute.data$him == 39] <- -0.28
df.corb.acute.data$him[df.corb.acute.data$him == 41] <- -0.23
df.corb.acute.data$him[df.corb.acute.data$him == 42] <- -0.20
df.corb.acute.data$him[df.corb.acute.data$him == 43] <- -0.18
df.corb.acute.data$him[df.corb.acute.data$him == 44] <- -0.15
df.corb.acute.data$him[df.corb.acute.data$him == 46] <- -0.10
df.corb.acute.data$him[df.corb.acute.data$him == 49] <- -0.03
df.corb.acute.data$him[df.corb.acute.data$him == 50] <- 0.00
df.corb.acute.data$him[df.corb.acute.data$him == 51] <- 0.03
df.corb.acute.data$him[df.corb.acute.data$him == 52] <- 0.05
df.corb.acute.data$him[df.corb.acute.data$him == 54] <- 0.10
df.corb.acute.data$him[df.corb.acute.data$him == 55] <- 0.13
df.corb.acute.data$him[df.corb.acute.data$him == 56] <- 0.15
df.corb.acute.data$him[df.corb.acute.data$him == 58] <- 0.20
df.corb.acute.data$him[df.corb.acute.data$him == 60] <- 0.25

#HVLT Delay

df.corb.acute.data$hd[df.corb.acute.data$hd == 0] <- -3.00
df.corb.acute.data$hd[df.corb.acute.data$hd == 21] <- -0.81
df.corb.acute.data$hd[df.corb.acute.data$hd == 23] <- -0.74
df.corb.acute.data$hd[df.corb.acute.data$hd == 25] <- -0.67
df.corb.acute.data$hd[df.corb.acute.data$hd == 26] <- -0.64
df.corb.acute.data$hd[df.corb.acute.data$hd == 28] <- -0.58
df.corb.acute.data$hd[df.corb.acute.data$hd == 31] <- -0.50
df.corb.acute.data$hd[df.corb.acute.data$hd == 32] <- -0.47
df.corb.acute.data$hd[df.corb.acute.data$hd == 33] <- -0.44
df.corb.acute.data$hd[df.corb.acute.data$hd == 37] <- -0.33
df.corb.acute.data$hd[df.corb.acute.data$hd == 39] <- -0.28
df.corb.acute.data$hd[df.corb.acute.data$hd == 41] <- -0.23
df.corb.acute.data$hd[df.corb.acute.data$hd == 44] <- -0.15
df.corb.acute.data$hd[df.corb.acute.data$hd == 45] <- -0.13
df.corb.acute.data$hd[df.corb.acute.data$hd == 47] <- -0.08
df.corb.acute.data$hd[df.corb.acute.data$hd == 49] <- -0.03
df.corb.acute.data$hd[df.corb.acute.data$hd == 50] <- 0.00
df.corb.acute.data$hd[df.corb.acute.data$hd == 53] <- 0.05
df.corb.acute.data$hd[df.corb.acute.data$hd == 55] <- 0.13
df.corb.acute.data$hd[df.corb.acute.data$hd == 61] <- 0.28

#HVLT Discrimination Index

df.corb.acute.data$hdis[df.corb.acute.data$hdis == 0] <- -3.00
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 21] <- -0.81
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 23] <- -0.74
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 24] <- -0.64
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 25] <- -0.67
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 26] <- -0.64
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 30] <- -0.52
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 31] <- -0.50
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 33] <- -0.44
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 34] <- -0.41
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 37] <- -0.33
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 38] <- -0.31
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 41] <- -0.23
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 42] <- -0.20
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 43] <- -0.18
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 44] <- -0.15
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 45] <- -0.13
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 46] <- -0.10
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 48] <- -0.05
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 49] <- -0.03
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 50] <- 0.00
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 51] <- 0.03
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 52] <- 0.05
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 57] <- 0.15
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 58] <- 0.20
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 59] <- 0.23
df.corb.acute.data$hdis[df.corb.acute.data$hdis == 60] <- 0.25

#Spatial Span Forward

df.corb.acute.data$ssf[df.corb.acute.data$ssf == 1] <- -3.00
df.corb.acute.data$ssf[df.corb.acute.data$ssf == 2] <- -2.67
df.corb.acute.data$ssf[df.corb.acute.data$ssf == 3] <- -2.33
df.corb.acute.data$ssf[df.corb.acute.data$ssf == 4] <- -2.00
df.corb.acute.data$ssf[df.corb.acute.data$ssf == 5] <- -1.67
df.corb.acute.data$ssf[df.corb.acute.data$ssf == 6] <- -1.33
df.corb.acute.data$ssf[df.corb.acute.data$ssf == 7] <- 1.00
df.corb.acute.data$ssf[df.corb.acute.data$ssf == 8] <- -0.67
df.corb.acute.data$ssf[df.corb.acute.data$ssf == 9] <- -0.33
df.corb.acute.data$ssf[df.corb.acute.data$ssf == 10] <- 0.00
df.corb.acute.data$ssf[df.corb.acute.data$ssf == 12] <- 0.67

#Spatial Span Background

df.corb.acute.data$ssb[df.corb.acute.data$ssb == 1] <- -3.00
df.corb.acute.data$ssb[df.corb.acute.data$ssb == 2] <- -2.67
df.corb.acute.data$ssb[df.corb.acute.data$ssb == 3] <- -2.33
df.corb.acute.data$ssb[df.corb.acute.data$ssb == 5] <- -1.67
df.corb.acute.data$ssb[df.corb.acute.data$ssb == 6] <- -1.33
df.corb.acute.data$ssb[df.corb.acute.data$ssb == 7] <- 1.00
df.corb.acute.data$ssb[df.corb.acute.data$ssb == 8] <- -0.67
df.corb.acute.data$ssb[df.corb.acute.data$ssb == 9] <- -0.33
df.corb.acute.data$ssb[df.corb.acute.data$ssb == 10] <- 0.00
df.corb.acute.data$ssb[df.corb.acute.data$ssb == 12] <- 0.67
df.corb.acute.data$ssb[df.corb.acute.data$ssb == 13] <- 1.00
df.corb.acute.data$ssb[df.corb.acute.data$ssb == 15] <- 1.67

#Check missing data

sum(is.na(df.corb.acute.data))/(nrow(df.corb.acute.data)*ncol(df.corb.acute.data))

#Write dataframes to files

#write.csv(df.corb.acute, file = '/Users/mbowren/drive/uiowa/core/corbetta/data/df_corb_acute_noBVMT.csv')

#EFA

library(psych)

df.corb.acute.tests <- df.corb.acute.data

fa.1 <- fa(df.corb.acute.tests, nfactors = 1, fm = 'ml', rotate = 'oblimin', scores = T)
fa.1

fa.2 <- fa(df.corb.acute.tests, nfactors = 2, fm = 'ml', rotate = 'oblimin', scores = T)
fa.2

fa.3 <- fa(df.corb.acute.tests, nfactors = 3, fm = 'ml', rotate = 'oblimin', scores = T)
fa.3

fa.4 <- fa(df.corb.acute.tests, nfactors = 4, fm = 'ml', rotate = 'oblimin', scores = T)
fa.4

#Compare fits

anova(fa.1, fa.2)
anova(fa.2, fa.3)
anova(fa.3, fa.4)

#CFI

cfi.fa.1 <- ((fa.1$null.chisq-fa.1$null.dof)-(fa.1$STATISTIC-fa.1$dof))/(fa.1$null.chisq-fa.1$null.dof)
cfi.fa.2 <- ((fa.2$null.chisq-fa.1$null.dof)-(fa.2$STATISTIC-fa.2$dof))/(fa.2$null.chisq-fa.2$null.dof)
cfi.fa.3 <- ((fa.3$null.chisq-fa.3$null.dof)-(fa.3$STATISTIC-fa.3$dof))/(fa.3$null.chisq-fa.3$null.dof)
cfi.fa.1
cfi.fa.2
cfi.fa.3

#RMSEA

fa.1$RMSEA
fa.2$RMSEA
fa.3$RMSEA

#RMS

fa.1$rms
fa.2$rms
fa.3$rms

#Diagrams

fa.diagram(fa.1)
fa.diagram(fa.2)
fa.diagram(fa.3)
fa.diagram(fa.4)

#Keep scores from best model

fa.3.scores <- fa.3$scores

#Derive g 

fa.g <- fa(fa.3.scores, nfactors = 1, fm = 'ml', rotate = 'oblimin', scores = T)
fa.g
fa.diagram(fa.g)

df.corb.acute$g <- fa.g$scores

#Use antsr tools

library(ANTsR)
library(ANTsRCore)

#Add predicted g to dataframe

#Lesion matrix of new sample

cat(as.character(df.corb.acute$nii.path), file = '/Users/mbowren/drive/uiowa/core/corbetta/nii_acute/nii_path_acute_noBVMT.txt', sep = '\n')

corb.filenames <- c('/Users/mbowren/drive/datasets/mni/MNI152_T1_1mm_brain_mask.nii.gz', Sys.glob(file.path('/Users/mbowren/drive/uiowa/core/corbetta/nii_acute', '*.nii.gz')))
corb.image.list <- imageFileNames2ImageList(corb.filenames)
corb.matrix <- imageListToMatrix(corb.image.list)
ncol(corb.matrix)
nrow(corb.matrix)
corb.matrix <- corb.matrix[-1,]
nrow(corb.matrix)

#Vector of raw weights

#g hi no Gwm

raw.weights.filenames.g.hi <- c('/Users/mbowren/drive/datasets/mni/MNI152_T1_1mm_brain_mask.nii.gz', '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/g/hi/noGwm/sccan/overlap_3/rawWeights_img.nii.gz')
raw.weights.image.list.g.hi <- imageFileNames2ImageList(raw.weights.filenames.g.hi)
raw.weights.g.hi <- imageListToMatrix(raw.weights.image.list.g.hi)
raw.weights.g.hi <- as.vector(raw.weights.g.hi[2,])

#Gc hi non-sig map, here for legacy

#raw.weights.filenames.Gc.cf <- c('/Users/mbowren/drive/datasets/mni/MNI152_T1_1mm_brain_mask.nii.gz', '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gc/hi/sccan/overlap_3/rawWeights_img.nii.gz')
#raw.weights.image.list.Gc.cf <- imageFileNames2ImageList(raw.weights.filenames.Gc.cf)
#raw.weights.Gc.cf <- imageListToMatrix(raw.weights.image.list.Gc.cf)
#raw.weights.Gc.cf <- as.vector(raw.weights.Gc.cf[2,])

#Gc bifactor

raw.weights.filenames.Gc.bi <- c('/Users/mbowren/drive/datasets/mni/MNI152_T1_1mm_brain_mask.nii.gz', '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gc/bi/sccan/overlap_3/rawWeights_img.nii.gz')
raw.weights.image.list.Gc.bi <- imageFileNames2ImageList(raw.weights.filenames.Gc.bi)
raw.weights.Gc.bi <- imageListToMatrix(raw.weights.image.list.Gc.bi)
raw.weights.Gc.bi <- as.vector(raw.weights.Gc.bi[2,])

#Gv hi

raw.weights.filenames.Gv.cf <- c('/Users/mbowren/drive/datasets/mni/MNI152_T1_1mm_brain_mask.nii.gz', '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gv/hi/sccan/overlap_3/rawWeights_img.nii.gz')
raw.weights.image.list.Gv.cf <- imageFileNames2ImageList(raw.weights.filenames.Gv.cf)
raw.weights.Gv.cf <- imageListToMatrix(raw.weights.image.list.Gv.cf)
raw.weights.Gv.cf <- as.vector(raw.weights.Gv.cf[2,])

#Gv bifactor

raw.weights.filenames.Gv.bi <- c('/Users/mbowren/drive/datasets/mni/MNI152_T1_1mm_brain_mask.nii.gz', '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gv/bi/sccan/overlap_3/rawWeights_img.nii.gz')
raw.weights.image.list.Gv.bi <- imageFileNames2ImageList(raw.weights.filenames.Gv.bi)
raw.weights.Gv.bi <- imageListToMatrix(raw.weights.image.list.Gv.bi)
raw.weights.Gv.bi <- as.vector(raw.weights.Gv.bi[2,])

#Gl hi

raw.weights.filenames.Gl.cf <- c('/Users/mbowren/drive/datasets/mni/MNI152_T1_1mm_brain_mask.nii.gz', '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gl/hi/sccan/overlap_3/rawWeights_img.nii.gz')
raw.weights.image.list.Gl.cf <- imageFileNames2ImageList(raw.weights.filenames.Gl.cf)
raw.weights.Gl.cf <- imageListToMatrix(raw.weights.image.list.Gl.cf)
raw.weights.Gl.cf <- as.vector(raw.weights.Gl.cf[2,])

#Gl bifactor

raw.weights.filenames.Gl.bi <- c('/Users/mbowren/drive/datasets/mni/MNI152_T1_1mm_brain_mask.nii.gz', '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gl/bi/sccan/overlap_3/rawWeights_img.nii.gz')
raw.weights.image.list.Gl.bi <- imageFileNames2ImageList(raw.weights.filenames.Gl.bi)
raw.weights.Gl.bi <- imageListToMatrix(raw.weights.image.list.Gl.bi)
raw.weights.Gl.bi <- as.vector(raw.weights.Gl.bi[2,])

#Gs hi

raw.weights.filenames.Gs.cf <- c('/Users/mbowren/drive/datasets/mni/MNI152_T1_1mm_brain_mask.nii.gz', '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gs/hi/sccan/overlap_3/rawWeights_img.nii.gz')
raw.weights.image.list.Gs.cf <- imageFileNames2ImageList(raw.weights.filenames.Gs.cf)
raw.weights.Gs.cf <- imageListToMatrix(raw.weights.image.list.Gs.cf)
raw.weights.Gs.cf <- as.vector(raw.weights.Gs.cf[2,])

#Gs bifactor

raw.weights.filenames.Gs.bi <- c('/Users/mbowren/drive/datasets/mni/MNI152_T1_1mm_brain_mask.nii.gz', '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gs/bi/sccan/overlap_3/rawWeights_img.nii.gz')
raw.weights.image.list.Gs.bi <- imageFileNames2ImageList(raw.weights.filenames.Gs.bi)
raw.weights.Gs.bi <- imageListToMatrix(raw.weights.image.list.Gs.bi)
raw.weights.Gs.bi <- as.vector(raw.weights.Gs.bi[2,])

#Gwm cf

raw.weights.filenames.Gwm <- c('/Users/mbowren/drive/datasets/mni/MNI152_T1_1mm_brain_mask.nii.gz', '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gwm/hi/sccan/overlap_3/rawWeights_img.nii.gz')
raw.weights.image.list.Gwm <- imageFileNames2ImageList(raw.weights.filenames.Gwm)
raw.weights.Gwm <- imageListToMatrix(raw.weights.image.list.Gwm)
raw.weights.Gwm <- as.vector(raw.weights.Gwm[2,])

#Eig2 value from lesymap output 

options(scipen = 999, digits = 16) #Ensure that it reads all of the decimal places in eig2

#g hierarchical

lsm.g.hi.output <- readLines(con = '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3/Info.txt')
line.eig2.g.hi <- lsm.g.hi.output[23]
line.eig2.g.hi <- as.vector(strsplit(line.eig2.g.hi, ' '))
eig2.g.hi <- as.double(line.eig2.g.hi[[1]][2])

#Gc hi

#lsm.Gc.output.cf <- readLines(con = '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gc/hi/sccan/overlap_3/Info.txt')
#line.eig2.Gc.cf <- lsm.Gc.output.cf[23]
#line.eig2.Gc.cf <- as.vector(strsplit(line.eig2.Gc.cf, ' '))
#eig2.Gc.cf <- as.double(line.eig2.Gc.cf[[1]][2])

#Gc bifactor

lsm.Gc.output.bi <- readLines(con = '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gc/bi/sccan/overlap_3/Info.txt')
line.eig2.Gc.bi <- lsm.Gc.output.bi[23]
line.eig2.Gc.bi <- as.vector(strsplit(line.eig2.Gc.bi, ' '))
eig2.Gc.bi <- as.double(line.eig2.Gc.bi[[1]][2])

#Gv cf

lsm.Gv.output.cf <- readLines(con = '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gv/hi/sccan/overlap_3/Info.txt')
line.eig2.Gv.cf <- lsm.Gv.output.cf[23]
line.eig2.Gv.cf <- as.vector(strsplit(line.eig2.Gv.cf, ' '))
eig2.Gv.cf <- as.double(line.eig2.Gv.cf[[1]][2])

#Gv bifactor

lsm.Gv.output.bi <- readLines(con = '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gv/bi/sccan/overlap_3/Info.txt')
line.eig2.Gv.bi <- lsm.Gv.output.bi[23]
line.eig2.Gv.bi <- as.vector(strsplit(line.eig2.Gv.bi, ' '))
eig2.Gv.bi <- as.double(line.eig2.Gv.bi[[1]][2])

#Gl bifactor

lsm.Gl.output.cf <- readLines(con = '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gl/hi/sccan/overlap_3/Info.txt')
line.eig2.Gl.cf <- lsm.Gl.output.cf[23]
line.eig2.Gl.cf <- as.vector(strsplit(line.eig2.Gl.cf, ' '))
eig2.Gl.cf <- as.double(line.eig2.Gl.cf[[1]][2])

#Gl bifactor

lsm.Gl.output.bi <- readLines(con = '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gl/bi/sccan/overlap_3/Info.txt')
line.eig2.Gl.bi <- lsm.Gl.output.bi[23]
line.eig2.Gl.bi <- as.vector(strsplit(line.eig2.Gl.bi, ' '))
eig2.Gl.bi <- as.double(line.eig2.Gl.bi[[1]][2])

#Gs cf

lsm.Gs.output.cf <- readLines(con = '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gs/hi/sccan/overlap_3/Info.txt')
line.eig2.Gs.cf <- lsm.Gs.output.cf[23]
line.eig2.Gs.cf <- as.vector(strsplit(line.eig2.Gs.cf, ' '))
eig2.Gs.cf <- as.double(line.eig2.Gs.cf[[1]][2])

#Gs bifactor

lsm.Gs.output.bi <- readLines(con = '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gs/bi/sccan/overlap_3/Info.txt')
line.eig2.Gs.bi <- lsm.Gs.output.bi[23]
line.eig2.Gs.bi <- as.vector(strsplit(line.eig2.Gs.bi, ' '))
eig2.Gs.bi <- as.double(line.eig2.Gs.bi[[1]][2])

#Gwm

lsm.Gwm.output <- readLines(con = '/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gwm/hi/sccan/overlap_3/Info.txt')
line.eig2.Gwm <- lsm.Gwm.output[23]
line.eig2.Gwm <- as.vector(strsplit(line.eig2.Gwm, ' '))
eig2.Gwm <- as.double(line.eig2.Gwm[[1]][2])

#Predict scores

pred.g.hi <- corb.matrix %*% raw.weights.g.hi %*% eig2.g.hi
pred.Gc.bi <- corb.matrix %*% raw.weights.Gc.bi %*% eig2.Gc.bi
pred.Gv.cf <- corb.matrix %*% raw.weights.Gv.cf %*% eig2.Gv.cf
pred.Gv.bi <- corb.matrix %*% raw.weights.Gv.bi %*% eig2.Gv.bi
pred.Gl.cf <- corb.matrix %*% raw.weights.Gl.cf %*% eig2.Gl.cf
pred.Gl.bi <- corb.matrix %*% raw.weights.Gl.bi %*% eig2.Gl.bi
pred.Gs.cf <- corb.matrix %*% raw.weights.Gs.cf %*% eig2.Gs.cf
pred.Gs.bi <- corb.matrix %*% raw.weights.Gs.bi %*% eig2.Gs.bi
pred.Gwm <- corb.matrix %*% raw.weights.Gwm %*% eig2.Gwm

#Add predicted scores to dataframe

df.corb.acute$pred.g.hi <- pred.g.hi
df.corb.acute$pred.Gc.bi <- pred.Gc.bi
df.corb.acute$pred.Gv.cf <- pred.Gv.cf
df.corb.acute$pred.Gv.bi <- pred.Gv.bi
df.corb.acute$pred.Gl.cf <- pred.Gl.cf
df.corb.acute$pred.Gl.bi <- pred.Gl.bi
df.corb.acute$pred.Gs.cf <- pred.Gs.cf
df.corb.acute$pred.Gs.bi <- pred.Gs.bi
df.corb.acute$pred.Gwm <- pred.Gwm

#Zero-order correlations

r.g.hi <- cor(df.corb.acute$g, df.corb.acute$pred.g.hi)
r.Gv <- cor(df.corb.acute$g, df.corb.acute$pred.Gv.cf)
r.Gl <- cor(df.corb.acute$g, df.corb.acute$pred.Gl.cf)
r.Gs <- cor(df.corb.acute$g, df.corb.acute$pred.Gs.cf)
r.Gwm <- cor(df.corb.acute$g, df.corb.acute$pred.Gwm)
r.Gc.bi <- cor(df.corb.acute$g, df.corb.acute$pred.Gc.bi)
r.Gv.bi <- cor(df.corb.acute$g, df.corb.acute$pred.Gv.bi)
r.Gl.bi <- cor(df.corb.acute$g, df.corb.acute$pred.Gl.bi)
r.Gs.bi <- cor(df.corb.acute$g, df.corb.acute$pred.Gs.bi)

r <- c(r.g.hi,
       r.Gv,
       r.Gl,
       r.Gs,
       r.Gwm,
       r.Gc.bi,
       r.Gv.bi,
       r.Gl.bi,
       r.Gs.bi)

#Permutation Tests for Zero-Order Correlations

set.seed(113862)

df.perm <- df.corb.acute

perm.r.g.hi <- vector()
perm.r.Gv <- vector()
perm.r.Gl <- vector()
perm.r.Gs <- vector()
perm.r.Gwm <- vector()
perm.r.Gc.bi <- vector()
perm.r.Gv.bi <- vector()
perm.r.Gl.bi <- vector()
perm.r.Gs.bi <- vector()

i <- 1

while(i <= 10000){
  df.perm <- transform(df.perm, g = sample(g))
  perm.r.g.hi[i] <- cor(df.perm$g, df.perm$pred.g.hi)
  perm.r.Gv[i] <- cor(df.perm$g, df.perm$pred.Gv.cf)
  perm.r.Gl[i] <- cor(df.perm$g, df.perm$pred.Gl.cf)
  perm.r.Gs[i] <- cor(df.perm$g, df.perm$pred.Gs.cf)
  perm.r.Gwm[i] <- cor(df.perm$g, df.perm$pred.Gwm)
  perm.r.Gc.bi[i] <- cor(df.perm$g, df.perm$pred.Gc.bi)
  perm.r.Gv.bi[i] <- cor(df.perm$g, df.perm$pred.Gv.bi)
  perm.r.Gl.bi[i] <- cor(df.perm$g, df.perm$pred.Gl.bi)
  perm.r.Gs.bi[i] <- cor(df.perm$g, df.perm$pred.Gs.bi)
  i <- i + 1
}

perm.r.pval.g.hi <- sum(abs(perm.r.g.hi) > abs(as.numeric(r.g.hi)))/length(perm.r.g.hi)
perm.r.pval.Gv <- sum(abs(perm.r.Gv) > abs(as.numeric(r.Gv)))/length(perm.r.Gv)
perm.r.pval.Gl <- sum(abs(perm.r.Gl) > abs(as.numeric(r.Gl)))/length(perm.r.Gl)
perm.r.pval.Gs <- sum(abs(perm.r.Gs) > abs(as.numeric(r.Gs)))/length(perm.r.Gs)
perm.r.pval.Gwm <- sum(abs(perm.r.Gwm) > abs(as.numeric(r.Gwm)))/length(perm.r.Gwm)
perm.r.pval.Gc.bi <- sum(abs(perm.r.Gc.bi) > abs(as.numeric(r.Gc.bi)))/length(perm.r.Gc.bi)
perm.r.pval.Gv.bi <- sum(abs(perm.r.Gv.bi) > abs(as.numeric(r.Gv.bi)))/length(perm.r.Gv.bi)
perm.r.pval.Gl.bi <- sum(abs(perm.r.Gl.bi) > abs(as.numeric(r.Gl.bi)))/length(perm.r.Gl.bi)
perm.r.pval.Gs.bi <- sum(abs(perm.r.Gs.bi) > abs(as.numeric(r.Gs.bi)))/length(perm.r.Gs.bi)

perm.r.pval <- c(perm.r.pval.g.hi,
                 perm.r.pval.Gv,
                 perm.r.pval.Gl,
                 perm.r.pval.Gs,
                 perm.r.pval.Gwm,
                 perm.r.pval.Gc.bi,
                 perm.r.pval.Gv.bi,
                 perm.r.pval.Gl.bi,
                 perm.r.pval.Gs.bi)

#Added value over lesion volume (log transformed), first using semi-partial correlation after controlling for log lesion volume

library(ppcor)

sr.g.hi <- spcor.test(df.corb.acute$g, df.corb.acute$pred.g.hi, log(df.corb.acute$lv))
sr.Gv.cf <- spcor.test(df.corb.acute$g, df.corb.acute$pred.Gv.cf, log(df.corb.acute$lv))
sr.Gl.cf <- spcor.test(df.corb.acute$g, df.corb.acute$pred.Gl.cf, log(df.corb.acute$lv))
sr.Gs.cf <- spcor.test(df.corb.acute$g, df.corb.acute$pred.Gs.cf, log(df.corb.acute$lv))
sr.Gwm <- spcor.test(df.corb.acute$g, df.corb.acute$pred.Gwm, log(df.corb.acute$lv))
sr.Gc.bi <- spcor.test(df.corb.acute$g, df.corb.acute$pred.Gc.bi, log(df.corb.acute$lv))
sr.Gv.bi <- spcor.test(df.corb.acute$g, df.corb.acute$pred.Gv.bi, log(df.corb.acute$lv))
sr.Gl.bi <- spcor.test(df.corb.acute$g, df.corb.acute$pred.Gl.bi, log(df.corb.acute$lv))
sr.Gs.bi <- spcor.test(df.corb.acute$g, df.corb.acute$pred.Gs.bi, log(df.corb.acute$lv))

sr <- c(sr.g.hi$estimate,
        sr.Gv.cf$estimate,
        sr.Gl.cf$estimate,
        sr.Gs.cf$estimate,
        sr.Gwm$estimate,
        sr.Gc.bi$estimate,
        sr.Gv.bi$estimate,
        sr.Gl.bi$estimate,
        sr.Gs.bi$estimate)

#Permutation Tests for Semi-Partials

df.perm <- df.corb.acute

perm.table.g.hi <- vector()
perm.table.Gv <- vector()
perm.table.Gl <- vector()
perm.table.Gs <- vector()
perm.table.Gwm <- vector()
perm.table.Gc.bi <- vector()
perm.table.Gv.bi <- vector()
perm.table.Gl.bi <- vector()
perm.table.Gs.bi <- vector()

i <- 1

while(i <= 10000){
  df.perm <- transform(df.perm, g = sample(g))
  perm.table.g.hi[i] <- spcor.test(df.perm$g, df.perm$pred.g.hi, log(df.perm$lv))$estimate
  perm.table.Gv[i] <- spcor.test(df.perm$g, df.perm$pred.Gv.cf, log(df.perm$lv))$estimate
  perm.table.Gl[i] <- spcor.test(df.perm$g, df.perm$pred.Gl.cf, log(df.perm$lv))$estimate
  perm.table.Gs[i] <- spcor.test(df.perm$g, df.perm$pred.Gs.cf, log(df.perm$lv))$estimate
  perm.table.Gwm[i] <- spcor.test(df.perm$g, df.perm$pred.Gwm, log(df.perm$lv))$estimate
  perm.table.Gc.bi[i] <- spcor.test(df.perm$g, df.perm$pred.Gc.bi, log(df.perm$lv))$estimate
  perm.table.Gv.bi[i] <- spcor.test(df.perm$g, df.perm$pred.Gv.bi, log(df.perm$lv))$estimate
  perm.table.Gl.bi[i] <- spcor.test(df.perm$g, df.perm$pred.Gl.bi, log(df.perm$lv))$estimate
  perm.table.Gs.bi[i] <- spcor.test(df.perm$g, df.perm$pred.Gs.bi, log(df.perm$lv))$estimate
  i <- i + 1
}

perm.sr.pval.g.hi <- sum(abs(perm.table.g.hi) > abs(sr.g.hi$estimate))/length(perm.table.g.hi)
perm.sr.pval.Gv <- sum(abs(perm.table.Gv) > abs(sr.Gv.cf$estimate))/length(perm.table.Gv)
perm.sr.pval.Gl <- sum(abs(perm.table.Gl) > abs(sr.Gl.cf$estimate))/length(perm.table.Gl)
perm.sr.pval.Gs <- sum(abs(perm.table.Gs) > abs(sr.Gs.cf$estimate))/length(perm.table.Gs)
perm.sr.pval.Gwm <- sum(abs(perm.table.Gwm) > abs(sr.Gwm$estimate))/length(perm.table.Gwm)
perm.sr.pval.Gc.bi <- sum(abs(perm.table.Gc.bi) > abs(sr.Gc.bi$estimate))/length(perm.table.Gc.bi)
perm.sr.pval.Gv.bi <- sum(abs(perm.table.Gv.bi) > abs(sr.Gv.bi$estimate))/length(perm.table.Gv.bi)
perm.sr.pval.Gl.bi <- sum(abs(perm.table.Gl.bi) > abs(sr.Gl.bi$estimate))/length(perm.table.Gl.bi)
perm.sr.pval.Gs.bi <- sum(abs(perm.table.Gs.bi) > abs(sr.Gs.bi$estimate))/length(perm.table.Gs.bi)

perm.sr.pval <- c(perm.sr.pval.g.hi,
                  perm.sr.pval.Gv,
                  perm.sr.pval.Gl,
                  perm.sr.pval.Gs,
                  perm.sr.pval.Gwm,
                  perm.sr.pval.Gc.bi,
                  perm.sr.pval.Gv.bi,
                  perm.sr.pval.Gl.bi,
                  perm.sr.pval.Gs.bi)

#Same correlations without people who did not hit the lsm (i.e., pred scores of 0)

#Create data frames for each map based on those with non-zero values

df.corb.acute.pred.g.hi <- subset(df.corb.acute, pred.g.hi != 0)
df.corb.acute.pred.Gv.cf <- subset(df.corb.acute, pred.Gv.cf != 0)
df.corb.acute.pred.Gl.cf <- subset(df.corb.acute, pred.Gl.cf != 0)
df.corb.acute.pred.Gs.cf <- subset(df.corb.acute, pred.Gs.cf != 0)
df.corb.acute.pred.Gwm <- subset(df.corb.acute, pred.Gwm != 0)
df.corb.acute.pred.Gc.bi <- subset(df.corb.acute, pred.Gc.bi != 0)
df.corb.acute.pred.Gv.bi <- subset(df.corb.acute, pred.Gv.bi != 0)
df.corb.acute.pred.Gl.bi <- subset(df.corb.acute, pred.Gl.bi != 0)
df.corb.acute.pred.Gs.bi <- subset(df.corb.acute, pred.Gs.bi != 0)

#Zero-order correlations

r.g.hi.2 <- cor(df.corb.acute.pred.g.hi$g, df.corb.acute.pred.g.hi$pred.g.hi)
r.Gv.2 <- cor(df.corb.acute.pred.Gv.cf$g, df.corb.acute.pred.Gv.cf$pred.Gv.cf)
r.Gl.2 <- cor(df.corb.acute.pred.Gl.cf$g, df.corb.acute.pred.Gl.cf$pred.Gl.cf)
r.Gs.2 <- cor(df.corb.acute.pred.Gs.cf$g, df.corb.acute.pred.Gs.cf$pred.Gs.cf)
r.Gwm.2 <- cor(df.corb.acute.pred.Gwm$g, df.corb.acute.pred.Gwm$pred.Gwm)
r.Gc.bi.2 <- cor(df.corb.acute.pred.Gc.bi$g, df.corb.acute.pred.Gc.bi$pred.Gc.bi)
r.Gv.bi.2 <- cor(df.corb.acute.pred.Gv.bi$g, df.corb.acute.pred.Gv.bi$pred.Gv.bi)
r.Gl.bi.2 <- cor(df.corb.acute.pred.Gl.bi$g, df.corb.acute.pred.Gl.bi$pred.Gl.bi)
r.Gs.bi.2 <- cor(df.corb.acute.pred.Gs.bi$g, df.corb.acute.pred.Gs.bi$pred.Gs.bi)

r.2 <- c(r.g.hi.2,
       r.Gv.2,
       r.Gl.2,
       r.Gs.2,
       r.Gwm.2,
       r.Gc.bi.2,
       r.Gv.bi.2,
       r.Gl.bi.2,
       r.Gs.bi.2)

#Permutation Tests for Zero-Order Correlations

df.perm <- df.corb.acute

perm.r.g.hi <- vector()
perm.r.Gv <- vector()
perm.r.Gl <- vector()
perm.r.Gs <- vector()
perm.r.Gwm <- vector()
perm.r.Gc.bi <- vector()
perm.r.Gv.bi <- vector()
perm.r.Gl.bi <- vector()
perm.r.Gs.bi <- vector()

i <- 1

while(i <= 10000){
  df.perm <- transform(df.perm, g = sample(g))
  perm.r.g.hi[i] <- cor(df.perm$g, df.perm$pred.g.hi)
  perm.r.Gv[i] <- cor(df.perm$g, df.perm$pred.Gv.cf)
  perm.r.Gl[i] <- cor(df.perm$g, df.perm$pred.Gl.cf)
  perm.r.Gs[i] <- cor(df.perm$g, df.perm$pred.Gs.cf)
  perm.r.Gwm[i] <- cor(df.perm$g, df.perm$pred.Gwm)
  perm.r.Gc.bi[i] <- cor(df.perm$g, df.perm$pred.Gc.bi)
  perm.r.Gv.bi[i] <- cor(df.perm$g, df.perm$pred.Gv.bi)
  perm.r.Gl.bi[i] <- cor(df.perm$g, df.perm$pred.Gl.bi)
  perm.r.Gs.bi[i] <- cor(df.perm$g, df.perm$pred.Gs.bi)
  i <- i + 1
}

perm.r.pval.g.hi.2 <- sum(abs(perm.r.g.hi) > abs(as.numeric(r.g.hi.2)))/length(perm.r.g.hi)
perm.r.pval.Gv.2 <- sum(abs(perm.r.Gv) > abs(as.numeric(r.Gv.2)))/length(perm.r.Gv)
perm.r.pval.Gl.2 <- sum(abs(perm.r.Gl) > abs(as.numeric(r.Gl.2)))/length(perm.r.Gl)
perm.r.pval.Gs.2 <- sum(abs(perm.r.Gs) > abs(as.numeric(r.Gs.2)))/length(perm.r.Gs)
perm.r.pval.Gwm.2 <- sum(abs(perm.r.Gwm) > abs(as.numeric(r.Gwm.2)))/length(perm.r.Gwm)
perm.r.pval.Gc.bi.2 <- sum(abs(perm.r.Gc.bi) > abs(as.numeric(r.Gc.bi.2)))/length(perm.r.Gc.bi)
perm.r.pval.Gv.bi.2 <- sum(abs(perm.r.Gv.bi) > abs(as.numeric(r.Gv.bi.2)))/length(perm.r.Gv.bi)
perm.r.pval.Gl.bi.2 <- sum(abs(perm.r.Gl.bi) > abs(as.numeric(r.Gl.bi.2)))/length(perm.r.Gl.bi)
perm.r.pval.Gs.bi.2 <- sum(abs(perm.r.Gs.bi) > abs(as.numeric(r.Gs.bi.2)))/length(perm.r.Gs.bi)

perm.r.pval.2 <- c(perm.r.pval.g.hi.2,
                 perm.r.pval.Gv.2,
                 perm.r.pval.Gl.2,
                 perm.r.pval.Gs.2,
                 perm.r.pval.Gwm.2,
                 perm.r.pval.Gc.bi.2,
                 perm.r.pval.Gv.bi.2,
                 perm.r.pval.Gl.bi.2,
                 perm.r.pval.Gs.bi.2)

#Added value over lesion volume (log transformed), first using semi-partial correlation after controlling for log lesion volume

sr.g.hi.2 <- spcor.test(df.corb.acute.pred.g.hi$g, df.corb.acute.pred.g.hi$pred.g.hi, log(df.corb.acute.pred.g.hi$lv))
sr.Gv.cf.2 <- spcor.test(df.corb.acute.pred.Gv.cf$g, df.corb.acute.pred.Gv.cf$pred.Gv.cf, log(df.corb.acute.pred.Gv.cf$lv))
sr.Gl.cf.2 <- spcor.test(df.corb.acute.pred.Gl.cf$g, df.corb.acute.pred.Gl.cf$pred.Gl.cf, log(df.corb.acute.pred.Gl.cf$lv))
sr.Gs.cf.2 <- spcor.test(df.corb.acute.pred.Gs.cf$g, df.corb.acute.pred.Gs.cf$pred.Gs.cf, log(df.corb.acute.pred.Gs.cf$lv))
sr.Gwm.2 <- spcor.test(df.corb.acute.pred.Gwm$g, df.corb.acute.pred.Gwm$pred.Gwm, log(df.corb.acute.pred.Gwm$lv))
sr.Gc.bi.2 <- spcor.test(df.corb.acute.pred.Gc.bi$g, df.corb.acute.pred.Gc.bi$pred.Gc.bi, log(df.corb.acute.pred.Gc.bi$lv))
sr.Gv.bi.2 <- spcor.test(df.corb.acute.pred.Gv.bi$g, df.corb.acute.pred.Gv.bi$pred.Gv.bi, log(df.corb.acute.pred.Gv.bi$lv))
sr.Gl.bi.2 <- spcor.test(df.corb.acute.pred.Gl.bi$g, df.corb.acute.pred.Gl.bi$pred.Gl.bi, log(df.corb.acute.pred.Gl.bi$lv))
sr.Gs.bi.2 <- spcor.test(df.corb.acute.pred.Gs.bi$g, df.corb.acute.pred.Gs.bi$pred.Gs.bi, log(df.corb.acute.pred.Gs.bi$lv))

sr.2 <- c(sr.g.hi.2$estimate,
        sr.Gv.cf.2$estimate,
        sr.Gl.cf.2$estimate,
        sr.Gs.cf.2$estimate,
        sr.Gwm.2$estimate,
        sr.Gc.bi.2$estimate,
        sr.Gv.bi.2$estimate,
        sr.Gl.bi.2$estimate,
        sr.Gs.bi.2$estimate)

#Permutation Tests for Semi-Partials

df.perm <- df.corb.acute

perm.table.g.hi <- vector()
perm.table.Gv <- vector()
perm.table.Gl <- vector()
perm.table.Gs <- vector()
perm.table.Gwm <- vector()
perm.table.Gc.bi <- vector()
perm.table.Gv.bi <- vector()
perm.table.Gl.bi <- vector()
perm.table.Gs.bi <- vector()

i <- 1

while(i <= 10000){
  df.perm <- transform(df.perm, g = sample(g))
  perm.table.g.hi[i] <- spcor.test(df.perm$g, df.perm$pred.g.hi, log(df.perm$lv))$estimate
  perm.table.Gv[i] <- spcor.test(df.perm$g, df.perm$pred.Gv.cf, log(df.perm$lv))$estimate
  perm.table.Gl[i] <- spcor.test(df.perm$g, df.perm$pred.Gl.cf, log(df.perm$lv))$estimate
  perm.table.Gs[i] <- spcor.test(df.perm$g, df.perm$pred.Gs.cf, log(df.perm$lv))$estimate
  perm.table.Gwm[i] <- spcor.test(df.perm$g, df.perm$pred.Gwm, log(df.perm$lv))$estimate
  perm.table.Gc.bi[i] <- spcor.test(df.perm$g, df.perm$pred.Gc.bi, log(df.perm$lv))$estimate
  perm.table.Gv.bi[i] <- spcor.test(df.perm$g, df.perm$pred.Gv.bi, log(df.perm$lv))$estimate
  perm.table.Gl.bi[i] <- spcor.test(df.perm$g, df.perm$pred.Gl.bi, log(df.perm$lv))$estimate
  perm.table.Gs.bi[i] <- spcor.test(df.perm$g, df.perm$pred.Gs.bi, log(df.perm$lv))$estimate
  i <- i + 1
}

perm.sr.pval.g.hi.2 <- sum(abs(perm.table.g.hi) > abs(sr.g.hi.2$estimate))/length(perm.table.g.hi)
perm.sr.pval.Gv.2 <- sum(abs(perm.table.Gv) > abs(sr.Gv.cf.2$estimate))/length(perm.table.Gv)
perm.sr.pval.Gl.2 <- sum(abs(perm.table.Gl) > abs(sr.Gl.cf.2$estimate))/length(perm.table.Gl)
perm.sr.pval.Gs.2 <- sum(abs(perm.table.Gs) > abs(sr.Gs.cf.2$estimate))/length(perm.table.Gs)
perm.sr.pval.Gwm.2 <- sum(abs(perm.table.Gwm) > abs(sr.Gwm.2$estimate))/length(perm.table.Gwm)
perm.sr.pval.Gc.bi.2 <- sum(abs(perm.table.Gc.bi) > abs(sr.Gc.bi.2$estimate))/length(perm.table.Gc.bi)
perm.sr.pval.Gv.bi.2 <- sum(abs(perm.table.Gv.bi) > abs(sr.Gv.bi.2$estimate))/length(perm.table.Gv.bi)
perm.sr.pval.Gl.bi.2 <- sum(abs(perm.table.Gl.bi) > abs(sr.Gl.bi.2$estimate))/length(perm.table.Gl.bi)
perm.sr.pval.Gs.bi.2 <- sum(abs(perm.table.Gs.bi) > abs(sr.Gs.bi.2$estimate))/length(perm.table.Gs.bi)

perm.sr.pval.2 <- c(perm.sr.pval.g.hi.2,
                  perm.sr.pval.Gv.2,
                  perm.sr.pval.Gl.2,
                  perm.sr.pval.Gs.2,
                  perm.sr.pval.Gwm.2,
                  perm.sr.pval.Gc.bi.2,
                  perm.sr.pval.Gv.bi.2,
                  perm.sr.pval.Gl.bi.2,
                  perm.sr.pval.Gs.bi.2)

#Table comparing all values

factor <- c('g', 'Gv', 'Gl', 'Gs', 'Gwm', 'Gc.bi', 'Gv.bi', 'Gl.bi', 'Gs.bi')

results.table <- data.frame(factor, r, perm.r.pval, r.2, perm.r.pval.2, sr, perm.sr.pval, sr.2, perm.sr.pval.2)

#Write results table to file

#write.csv(results.table, file = '/Users/mbowren/drive/uiowa/core/validation_analysis/validation_results_vrtc_noBVMT.csv')







#Lesion Network Mapping Lesion Load

#Gv bi

#Read in positively correlated LNM results for each seed, convert to arrays and vectors

Gv.lnm.seed06 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gv_bi/b98/seed06/groupZstat/Pos/logdir/thr8_Gv_seed06_pos.nii.gz')
Gv.lnm.seed07 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gv_bi/b98/seed07/groupZstat/Pos/logdir/thr8_Gv_seed07_pos.nii.gz')
Gv.lnm.seed08 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gv_bi/b98/seed08/groupZstat/Pos/logdir/thr8_Gv_seed08_pos.nii.gz')
Gv.lnm.seed09 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gv_bi/b98/seed09/groupZstat/Pos/logdir/thr8_Gv_seed09_pos.nii.gz')
Gv.lnm.seed10 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gv_bi/b98/seed10/groupZstat/Pos/logdir/thr8_Gv_seed10_pos.nii.gz')

Gv.lnm.seed06.arr <- as.array(Gv.lnm.seed06)
Gv.lnm.seed07.arr <- as.array(Gv.lnm.seed07)
Gv.lnm.seed08.arr <- as.array(Gv.lnm.seed08)
Gv.lnm.seed09.arr <- as.array(Gv.lnm.seed09)
Gv.lnm.seed10.arr <- as.array(Gv.lnm.seed10)

#Multiply each subject's mask by each seed and sum the result to get lesion load

corb.filenames.2mm <- Sys.glob(file.path('/Users/mbowren/drive/datasets/wu/nii/acute/2mm', '*.nii.gz'))
id.list <- df.corb.acute$id
df.corb.acute$Gv.seed06.ll <- NA
df.corb.acute$Gv.seed07.ll <- NA
df.corb.acute$Gv.seed08.ll <- NA
df.corb.acute$Gv.seed09.ll <- NA
df.corb.acute$Gv.seed10.ll <- NA
j <- 1

while(j <= length(id.list)){
  mask <- antsImageRead(corb.filenames.2mm[j])
  mask <- as.array(mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gv.seed06.ll <- sum(Gv.lnm.seed06.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gv.seed07.ll <- sum(Gv.lnm.seed07.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gv.seed08.ll <- sum(Gv.lnm.seed08.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gv.seed09.ll <- sum(Gv.lnm.seed09.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gv.seed10.ll <- sum(Gv.lnm.seed10.arr*mask)
  j <- j + 1
}

#Gl bi

#Read in positively correlated LNM results for each seed, convert to arrays and vectors

Gl.lnm.seed04 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gl_bi/b98/seed04/groupZstat/Pos/logdir/thr8_Gl_seed04_pos.nii.gz')
Gl.lnm.seed05 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gl_bi/b98/seed05/groupZstat/Pos/logdir/thr8_Gl_seed05_pos.nii.gz')
Gl.lnm.seed06 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gl_bi/b98/seed06/groupZstat/Pos/logdir/thr8_Gl_seed06_pos.nii.gz')

Gl.lnm.seed04.arr <- as.array(Gl.lnm.seed04)
Gl.lnm.seed05.arr <- as.array(Gl.lnm.seed05)
Gl.lnm.seed06.arr <- as.array(Gl.lnm.seed06)

#Multiply each subject's mask by each seed and sum the result to get lesion load

corb.filenames.2mm <- Sys.glob(file.path('/Users/mbowren/drive/datasets/wu/nii/acute/2mm', '*.nii.gz'))
id.list <- df.corb.acute$id
df.corb.acute$Gl.seed04.ll <- NA
df.corb.acute$Gl.seed05.ll <- NA
df.corb.acute$Gl.seed06.ll <- NA
j <- 1

while(j <= length(id.list)){
  mask <- antsImageRead(corb.filenames.2mm[j])
  mask <- as.array(mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gl.seed04.ll <- sum(Gl.lnm.seed04.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gl.seed05.ll <- sum(Gl.lnm.seed05.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gl.seed06.ll <- sum(Gl.lnm.seed06.arr*mask)
  j <- j + 1
}

#Gv bi

#Read in positively correlated LNM results for each seed, convert to arrays and vectors

Gs.lnm.seed03 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gs_bi/b98/seed03/groupZstat/Pos/logdir/thr8_Gs_seed03_pos.nii.gz')
Gs.lnm.seed04 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gs_bi/b98/seed04/groupZstat/Pos/logdir/thr8_Gs_seed04_pos.nii.gz')
Gs.lnm.seed05 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gs_bi/b98/seed05/groupZstat/Pos/logdir/thr8_Gs_seed05_pos.nii.gz')
Gs.lnm.seed06 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gs_bi/b98/seed06/groupZstat/Pos/logdir/thr8_Gs_seed06_pos.nii.gz')
Gs.lnm.seed07 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gs_bi/b98/seed07/groupZstat/Pos/logdir/thr8_Gs_seed07_pos.nii.gz')
Gs.lnm.seed08 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gs_bi/b98/seed08/groupZstat/Pos/logdir/thr8_Gs_seed08_pos.nii.gz')
Gs.lnm.seed09 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gs_bi/b98/seed09/groupZstat/Pos/logdir/thr8_Gs_seed09_pos.nii.gz')

Gs.lnm.seed03.arr <- as.array(Gs.lnm.seed03)
Gs.lnm.seed04.arr <- as.array(Gs.lnm.seed04)
Gs.lnm.seed05.arr <- as.array(Gs.lnm.seed05)
Gs.lnm.seed06.arr <- as.array(Gs.lnm.seed06)
Gs.lnm.seed07.arr <- as.array(Gs.lnm.seed07)
Gs.lnm.seed08.arr <- as.array(Gs.lnm.seed08)
Gs.lnm.seed09.arr <- as.array(Gs.lnm.seed09)

#Multiply each subject's mask by each seed and sum the result to get lesion load

corb.filenames.2mm <- Sys.glob(file.path('/Users/mbowren/drive/datasets/wu/nii/acute/2mm', '*.nii.gz'))
id.list <- df.corb.acute$id
df.corb.acute$Gs.seed03.ll <- NA
df.corb.acute$Gs.seed04.ll <- NA
df.corb.acute$Gs.seed05.ll <- NA
df.corb.acute$Gs.seed06.ll <- NA
df.corb.acute$Gs.seed07.ll <- NA
df.corb.acute$Gs.seed08.ll <- NA
df.corb.acute$Gs.seed09.ll <- NA
j <- 1

while(j <= length(id.list)){
  mask <- antsImageRead(corb.filenames.2mm[j])
  mask <- as.array(mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gs.seed03.ll <- sum(Gs.lnm.seed03.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gs.seed04.ll <- sum(Gs.lnm.seed04.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gs.seed05.ll <- sum(Gs.lnm.seed05.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gs.seed06.ll <- sum(Gs.lnm.seed06.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gs.seed07.ll <- sum(Gs.lnm.seed07.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gs.seed08.ll <- sum(Gs.lnm.seed08.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gs.seed09.ll <- sum(Gs.lnm.seed09.arr*mask)
  j <- j + 1
}

#Gwm cf

#Read in positively correlated LNM results for each seed, convert to arrays and vectors

Gwm.lnm.seed10 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gwm_cf/b98/seed10/groupZstat/Pos/logdir/thr8_Gwm_seed10_pos.nii.gz')
Gwm.lnm.seed11 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gwm_cf/b98/seed11/groupZstat/Pos/logdir/thr8_Gwm_seed11_pos.nii.gz')
Gwm.lnm.seed12 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gwm_cf/b98/seed12/groupZstat/Pos/logdir/thr8_Gwm_seed12_pos.nii.gz')
Gwm.lnm.seed13 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gwm_cf/b98/seed13/groupZstat/Pos/logdir/thr8_Gwm_seed13_pos.nii.gz')
Gwm.lnm.seed14 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gwm_cf/b98/seed14/groupZstat/Pos/logdir/thr8_Gwm_seed14_pos.nii.gz')
Gwm.lnm.seed15 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gwm_cf/b98/seed15/groupZstat/Pos/logdir/thr8_Gwm_seed15_pos.nii.gz')
Gwm.lnm.seed16 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gwm_cf/b98/seed16/groupZstat/Pos/logdir/thr8_Gwm_seed16_pos.nii.gz')
Gwm.lnm.seed17 <- antsImageRead('/Users/mbowren/drive/uiowa/core/lnm/results/Gwm_cf/b98/seed17/groupZstat/Pos/logdir/thr8_Gwm_seed17_pos.nii.gz')

Gwm.lnm.seed10.arr <- as.array(Gwm.lnm.seed10)
Gwm.lnm.seed11.arr <- as.array(Gwm.lnm.seed11)
Gwm.lnm.seed12.arr <- as.array(Gwm.lnm.seed12)
Gwm.lnm.seed13.arr <- as.array(Gwm.lnm.seed13)
Gwm.lnm.seed14.arr <- as.array(Gwm.lnm.seed14)
Gwm.lnm.seed15.arr <- as.array(Gwm.lnm.seed15)
Gwm.lnm.seed16.arr <- as.array(Gwm.lnm.seed16)
Gwm.lnm.seed17.arr <- as.array(Gwm.lnm.seed17)

#Multiply each subject's mask by each seed and sum the result to get lesion load

corb.filenames.2mm <- Sys.glob(file.path('/Users/mbowren/drive/datasets/wu/nii/acute/2mm', '*.nii.gz'))
id.list <- df.corb.acute$id
df.corb.acute$Gwm.seed10.ll <- NA
df.corb.acute$Gwm.seed11.ll <- NA
df.corb.acute$Gwm.seed12.ll <- NA
df.corb.acute$Gwm.seed13.ll <- NA
df.corb.acute$Gwm.seed14.ll <- NA
df.corb.acute$Gwm.seed15.ll <- NA
df.corb.acute$Gwm.seed16.ll <- NA
df.corb.acute$Gwm.seed17.ll <- NA
j <- 1

while(j <= length(id.list)){
  mask <- antsImageRead(corb.filenames.2mm[j])
  mask <- as.array(mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gwm.seed10.ll <- sum(Gwm.lnm.seed10.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gwm.seed11.ll <- sum(Gwm.lnm.seed11.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gwm.seed12.ll <- sum(Gwm.lnm.seed12.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gwm.seed13.ll <- sum(Gwm.lnm.seed13.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gwm.seed14.ll <- sum(Gwm.lnm.seed14.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gwm.seed15.ll <- sum(Gwm.lnm.seed15.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gwm.seed16.ll <- sum(Gwm.lnm.seed16.arr*mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gwm.seed17.ll <- sum(Gwm.lnm.seed17.arr*mask)
  j <- j + 1
}

df.corb.lnm <- subset(df.corb.acute, select = c('g', 'Gwm.seed10.ll', 'Gwm.seed11.ll', 'Gwm.seed12.ll', 'Gwm.seed13.ll', 'Gwm.seed14.ll', 'Gwm.seed15.ll', 'Gwm.seed16.ll', 'Gwm.seed17.ll', 'Gv.seed06.ll', 'Gv.seed07.ll', 'Gv.seed08.ll', 'Gv.seed09.ll', 'Gv.seed10.ll', 'Gl.seed04.ll', 'Gl.seed05.ll', 'Gl.seed06.ll', 'Gs.seed03.ll', 'Gs.seed04.ll', 'Gs.seed05.ll', 'Gs.seed06.ll', 'Gs.seed07.ll', 'Gs.seed08.ll', 'Gs.seed09.ll'))
cor(df.corb.lnm)[,1]


#Do network lesion loads add value above lesion location (Lesymap predicted score)?

spcor.test(df.corb.acute$g, df.corb.acute$Gwm.seed10.ll, df.corb.acute$pred.g)
spcor.test(df.corb.acute$g, df.corb.acute$Gwm.seed11.ll, df.corb.acute$pred.g)
spcor.test(df.corb.acute$g, df.corb.acute$Gwm.seed12.ll, df.corb.acute$pred.g)
spcor.test(df.corb.acute$g, df.corb.acute$Gwm.seed13.ll, df.corb.acute$pred.g)
spcor.test(df.corb.acute$g, df.corb.acute$Gwm.seed14.ll, df.corb.acute$pred.g)
spcor.test(df.corb.acute$g, df.corb.acute$Gwm.seed15.ll, df.corb.acute$pred.g)
spcor.test(df.corb.acute$g, df.corb.acute$Gwm.seed16.ll, df.corb.acute$pred.g)
spcor.test(df.corb.acute$g, df.corb.acute$Gwm.seed17.ll, df.corb.acute$pred.g)

#Does entering all seed lesion loads at one step of regression add value above lesion location (Lesymap predicted score)?

all.seeds.lm <- lm(scale(g) ~ scale(pred.g.hi), data = df.corb.acute)
summary(all.seeds.lm)
all.seeds.lm.2 <- lm(scale(g) ~ scale(pred.g.hi) + scale(Gwm.seed10.ll) + scale(Gwm.seed11.ll) + scale(Gwm.seed12.ll) + scale(Gwm.seed13.ll) + scale(Gwm.seed14.ll) + scale(Gwm.seed15.ll) + scale(Gwm.seed16.ll) + scale(Gwm.seed17.ll), data = df.corb.acute)
summary(all.seeds.lm.2)
lmSupport::modelCompare(all.seeds.lm, all.seeds.lm.2)

#Tractography lesion load

corb.filenames.1mm <- Sys.glob(file.path('/Users/mbowren/drive/datasets/wu/nii/acute', '*.nii.gz'))

#Gv

Gv.sumtracts <- antsImageRead('/Users/mbowren/drive/uiowa/core/tractography/lead_dbs/results/Gv_bi/sum_tracts/sum_tracts.nii.gz')
Gv.sumtracts.arr <- as.array(Gv.sumtracts)
df.corb.acute$Gv.sumtracts.ll <- NA
j <- 1

while(j <= length(id.list)){
  mask <- antsImageRead(corb.filenames.1mm[j])
  mask <- as.array(mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gv.sumtracts.ll <- sum(Gv.sumtracts.arr*mask)
  j <- j + 1
}

#Gl

Gl.sumtracts <- antsImageRead('/Users/mbowren/drive/uiowa/core/tractography/lead_dbs/results/Gl_bi/sum_tracts/sum_tracts.nii.gz')
Gl.sumtracts.arr <- as.array(Gl.sumtracts)
df.corb.acute$Gl.sumtracts.ll <- NA
j <- 1

while(j <= length(id.list)){
  mask <- antsImageRead(corb.filenames.1mm[j])
  mask <- as.array(mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gl.sumtracts.ll <- sum(Gl.sumtracts.arr*mask)
  j <- j + 1
}

#Gs

Gs.sumtracts <- antsImageRead('/Users/mbowren/drive/uiowa/core/tractography/lead_dbs/results/Gs_bi/sum_tracts/sum_tracts.nii.gz')
Gs.sumtracts.arr <- as.array(Gs.sumtracts)
df.corb.acute$Gs.sumtracts.ll <- NA
j <- 1

while(j <= length(id.list)){
  mask <- antsImageRead(corb.filenames.1mm[j])
  mask <- as.array(mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$Gs.sumtracts.ll <- sum(Gs.sumtracts.arr*mask)
  j <- j + 1
}

#g

g.sumtracts <- antsImageRead('/Users/mbowren/drive/uiowa/core/tractography/lead_dbs/results/g/sum_tracts/sum_tracts.nii.gz')
g.sumtracts.arr <- as.array(g.sumtracts)
df.corb.acute$g.sumtracts.ll <- NA
j <- 1

while(j <= length(id.list)){
  mask <- antsImageRead(corb.filenames.1mm[j])
  mask <- as.array(mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$g.sumtracts.ll <- sum(g.sumtracts.arr*mask)
  j <- j + 1
}

#Correlate

df.corb.sumtracts <- subset(df.corb.acute, select = c('g', 'g.sumtracts.ll', 'Gv.sumtracts.ll', 'Gl.sumtracts.ll', 'Gs.sumtracts.ll'))
cor(df.corb.sumtracts)

#Does tractography lesion load add value above lesion location (Lesymap predicted score)?

g.sumtracts.ll.lm <- lm(scale(g) ~ scale(pred.g.hi), data = df.corb.acute)
summary(g.sumtracts.ll.lm)
g.sumtracts.ll.lm.2 <- lm(scale(g) ~ scale(pred.g.hi) + scale(g.sumtracts.ll), data = df.corb.acute)
summary(g.sumtracts.ll.lm.2)
lmSupport::modelCompare(g.sumtracts.ll.lm, g.sumtracts.ll.lm.2)

spcor.test(df.corb.acute$g, df.corb.acute$g.sumtracts.ll, df.corb.acute$pred.g.hi)
spcor.test(df.corb.acute$g, df.corb.acute$Gv.sumtracts.ll, df.corb.acute$pred.g.hi)
spcor.test(df.corb.acute$g, df.corb.acute$Gl.sumtracts.ll, df.corb.acute$pred.g.hi)
spcor.test(df.corb.acute$g, df.corb.acute$Gs.sumtracts.ll, df.corb.acute$pred.g.hi)


#Test correlation strength of regular lsm ll

#g

g.lsm <- antsImageRead('/Users/mbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3/rawWeights_img.nii.gz')
g.lsm.arr <- as.array(g.lsm)
df.corb.acute$g.lsm.ll <- NA
j <- 1

while(j <= length(id.list)){
  mask <- antsImageRead(corb.filenames.1mm[j])
  mask <- as.array(mask)
  df.corb.acute[df.corb.acute$id == id.list[j], ]$g.lsm.ll <- sum(g.lsm.arr*mask)
  j <- j + 1
}

cor.test(df.corb.acute$g, df.corb.acute$g.lsm.ll)
cor.test(df.corb.acute$g, df.corb.acute$pred.g.hi)

g.lsm.ll.lm <- lm(scale(g) ~ scale(g.lsm.ll), data = df.corb.acute)
summary(g.lsm.ll.lm)
g.lsm.ll.lm.2 <- lm(scale(g) ~ scale(g.lsm.ll) + scale(g.sumtracts.ll), data = df.corb.acute)
summary(g.lsm.ll.lm.2)
lmSupport::modelCompare(g.lsm.ll.lm, g.lsm.ll.lm.2)

