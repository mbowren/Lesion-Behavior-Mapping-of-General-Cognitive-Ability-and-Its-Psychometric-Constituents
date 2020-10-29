#LESYMAP

#Read in data

df.core <- read.csv('/fmri/studies/Bowren/core/data/402/core_master_402.csv')
df.core <- df.core[,-1] #Remove first column because R saves a leading column with information we don't need. Do this for all csv files.
df.core <- subset(df.core, is.na(df.core$nii.path) == 'FALSE')

df.core.norm <- read.csv('/fmri/studies/Bowren/core/data/402/df_core_norm.csv')
df.core.norm <- df.core.norm[,-1]

df.core.factors.hi <- read.csv('/fmri/studies/Bowren/core/data/402/df_core_factors_hi_withAllResid.csv')
df.core.factors.hi <- df.core.factors.hi[,-1]

df.core.factors.bi <- read.csv('/fmri/studies/Bowren/core/data/402/df_core_factors_bi_withAllResid.csv')
df.core.factors.bi <- df.core.factors.bi[,-1]

#Add leading zeros to id numbers

library(stringr)
df.core$id <- str_pad(df.core$id, 4, '0', side = 'left')
df.core.factors.hi$id <- str_pad(df.core.factors.hi$id, 4, '0', side = 'left')
df.core.factors.bi$id <- str_pad(df.core.factors.bi$id, 4, '0', side = 'left')

#Set up nii filepath column, with values only for people with a mask in the master directory
#NA otherwise

id.list <- df.core$id #Create id list vector
nii.path <- vector() #Create empty vector to use later
j <- 1 #Assign starting value for j

#Loop through each id and create a filepath for each ID, storing it in the nii.path vector

for(i in id.list){
  nii.path[j] <- as.vector(paste0('/fmri/studies/Bowren/master/nii/', i, '.nii.gz'))
  j <- j + 1
}

#Create a string vector that contains all lesions in the master directory

master.nii <- Sys.glob(file.path('/fmri/studies/Bowren/master/nii', '*.nii.gz'))

#If an element in the vector nii.path exists in the vector master.nii, keep it, otherwise set that element to missing so we don't use it

for(i in 1:length(nii.path)){
  if(nii.path[i] %in% master.nii == 'TRUE'){
    nii.path[i] <- nii.path[i]
  } else {
    nii.path[i] <- NA
  }
}

#Add the nii.path vector the the dataframe df.core

df.core$nii.path <- nii.path

#Run LESYMAP's SCCAN on Factor Scores. Use contents of df.core to get list of filepaths for nifti files. Use other dataframes to get vectors of behavioral scores. Save results to directory.

lsm.g.hi <- lesymap(as.vector(df.core$nii.path), as.vector(df.core.factors.hi$g), method = 'sccan', minSubjectPerVoxel = '3', saveDir = '/fmri/studies/Bowren/core/lsm/lesymap/running/sem_z/g/hi/sccan/overlap_3', directionalSCCAN = T)
lsm.Gc.hi <- lesymap(as.vector(df.core$nii.path), as.vector(df.core.factors.hi$Gc), method = 'sccan', minSubjectPerVoxel = '3', saveDir = '/fmri/studies/Bowren/core/lsm/lesymap/running/sem_z/Gc/hi/sccan/overlap_3', directionalSCCAN = T)
lsm.Gv.hi <- lesymap(as.vector(df.core$nii.path), as.vector(df.core.factors.hi$Gv), method = 'sccan', minSubjectPerVoxel = '3', saveDir = '/fmri/studies/Bowren/core/lsm/lesymap/running/sem_z/Gv/hi/sccan/overlap_3', directionalSCCAN = T)
lsm.Gl.hi <- lesymap(as.vector(df.core$nii.path), as.vector(df.core.factors.hi$Gl), method = 'sccan', minSubjectPerVoxel = '3', saveDir = '/fmri/studies/Bowren/core/lsm/lesymap/running/sem_z/Gl/hi/sccan/overlap_3', directionalSCCAN = T)
lsm.Gs.hi <- lesymap(as.vector(df.core$nii.path), as.vector(df.core.factors.hi$Gs), method = 'sccan', minSubjectPerVoxel = '3', saveDir = '/fmri/studies/Bowren/core/lsm/lesymap/running/sem_z/Gs/hi/sccan/overlap_3', directionalSCCAN = T)
lsm.Gwm.hi <- lesymap(as.vector(df.core$nii.path), as.vector(df.core.factors.hi$Gwm), method = 'sccan', minSubjectPerVoxel = '3', saveDir = '/fmri/studies/Bowren/core/lsm/lesymap/running/sem_z/Gwm/hi/sccan/overlap_3', directionalSCCAN = T)

#Bifactor models. Same deal.

lsm.Gc.bi <- lesymap(as.vector(df.core$nii.path), as.vector(df.core.factors.bi$Gc), method = 'sccan', minSubjectPerVoxel = '3', saveDir = '/fmri/studies/Bowren/core/lsm/lesymap/running/sem_z/Gc/bi/sccan/overlap_3', directionalSCCAN = T)
lsm.Gv.bi <- lesymap(as.vector(df.core$nii.path), as.vector(df.core.factors.bi$Gv), method = 'sccan', minSubjectPerVoxel = '3', saveDir = '/fmri/studies/Bowren/core/lsm/lesymap/running/sem_z/Gv/bi//sccan/overlap_3', directionalSCCAN = T)
lsm.Gl.bi <- lesymap(as.vector(df.core$nii.path), as.vector(df.core.factors.bi$Gl), method = 'sccan', minSubjectPerVoxel = '3', saveDir = '/fmri/studies/Bowren/core/lsm/lesymap/running/sem_z/Gl/bi/sccan/overlap_3', directionalSCCAN = T)
lsm.Gs.bi <- lesymap(as.vector(df.core$nii.path), as.vector(df.core.factors.bi$Gs), method = 'sccan', minSubjectPerVoxel = '3', saveDir = '/fmri/studies/Bowren/core/lsm/lesymap/running/sem_z/Gs/bi/sccan/overlap_3', directionalSCCAN = T)
