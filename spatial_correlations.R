#Read in Iowa g

library(LESYMAP)

g.iowa.path <- '/Users/markbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3/rawWeights_img.nii.gz'
g.iowa <- antsImageRead(g.iowa.path)
g.wu.path <- '/Users/markbowren/drive/uiowa/core/lsm/lesymap/finished/corb/g/rawWeights_img.nii.gz'
g.wu <- antsImageRead(g.wu.path)
g.iowa.noGwm.path <- '/Users/markbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/g/hi/noGwm/sccan/overlap_3/rawWeights_img.nii.gz'
g.iowa.noGwm <- antsImageRead(g.iowa.noGwm.path)

Gv.iowa.path <- '/Users/markbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gv/hi/sccan/overlap_3/rawWeights_img.nii.gz'
Gv.iowa <- antsImageRead(Gv.iowa.path)
Gl.iowa.path <- '/Users/markbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gl/hi/sccan/overlap_3/rawWeights_img.nii.gz'
Gl.iowa <- antsImageRead(Gl.iowa.path)
Gs.iowa.path <- '/Users/markbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gs/hi/sccan/overlap_3/rawWeights_img.nii.gz'
Gs.iowa <- antsImageRead(Gs.iowa.path)
Gwm.iowa.path <- '/Users/markbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gwm/hi/sccan/overlap_3/rawWeights_img.nii.gz'
Gwm.iowa <- antsImageRead(Gwm.iowa.path)

Gc.bi.iowa.path <- '/Users/markbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gc/bi/sccan/overlap_3/rawWeights_img.nii.gz'
Gc.bi.iowa <- antsImageRead(Gc.bi.iowa.path)
Gv.bi.iowa.path <- '/Users/markbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gv/bi/sccan/overlap_3/rawWeights_img.nii.gz'
Gv.bi.iowa <- antsImageRead(Gv.bi.iowa.path)
Gl.bi.iowa.path <- '/Users/markbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gl/bi/sccan/overlap_3/rawWeights_img.nii.gz'
Gl.bi.iowa <- antsImageRead(Gl.bi.iowa.path)
Gs.bi.iowa.path <- '/Users/markbowren/drive/uiowa/core/lsm/lesymap/finished/iowa/sem_z/Gs/bi/sccan/overlap_3/rawWeights_img.nii.gz'
Gs.bi.iowa <- antsImageRead(Gs.bi.iowa.path)

#Convert to arrays

g.iowa.array <- as.array(g.iowa)
g.wu.array <- as.array(g.wu)
g.iowa.noGwm.array <- as.array(g.iowa.noGwm)

Gv.iowa.array <- as.array(Gv.iowa)
Gl.iowa.array <- as.array(Gl.iowa)
Gs.iowa.array <- as.array(Gs.iowa)
Gwm.iowa.array <- as.array(Gwm.iowa)

Gc.bi.iowa.array <- as.array(Gc.bi.iowa)
Gv.bi.iowa.array <- as.array(Gv.bi.iowa)
Gl.bi.iowa.array <- as.array(Gl.bi.iowa)
Gs.bi.iowa.array <- as.array(Gs.bi.iowa)

#Convert to vectors

g.iowa.vector <- as.vector(g.iowa.array)
g.wu.vector <- as.vector(g.wu.array)
g.iowa.noGwm.vector <- as.vector(g.iowa.noGwm.array)

Gv.iowa.vector <- as.vector(Gv.iowa.array)
Gl.iowa.vector <- as.vector(Gl.iowa.array)
Gs.iowa.vector <- as.vector(Gs.iowa.array)
Gwm.iowa.vector <- as.vector(Gwm.iowa.array)

Gc.bi.iowa.vector <- as.vector(Gc.bi.iowa.array)
Gv.bi.iowa.vector <- as.vector(Gv.bi.iowa.array)
Gl.bi.iowa.vector <- as.vector(Gl.bi.iowa.array)
Gs.bi.iowa.vector <- as.vector(Gs.bi.iowa.array)

#Place into dataframe

df <- data.frame(g.iowa.vector, g.wu.vector, g.iowa.noGwm.vector, Gv.iowa.vector, Gl.iowa.vector, Gs.iowa.vector, Gwm.iowa.vector, Gc.bi.iowa.vector, Gv.bi.iowa.vector, Gl.bi.iowa.vector, Gs.bi.iowa.vector)

#Correlate all vectors

cor(df)
