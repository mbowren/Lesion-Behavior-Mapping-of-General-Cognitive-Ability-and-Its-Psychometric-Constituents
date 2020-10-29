#Read in data

df.core.norm <- read.csv('/Users/mbowren/drive/uiowa/core/data/old/402/df_core_norm.csv')

df.core.norm <- df.core.norm[,-1]

#Load libraries

library(lavaan)
library(semPlot)

#Check multivariate normality

df.core.norm.tests <- df.core.norm[,1:(ncol(df.core.norm)-2)]
check.mvn <- MVN::mvn(df.core.norm.tests, univariateTest = 'SW', univariatePlot = 'qq', mvnTest = 'mardia')

#Due to lack of multivariate normality, it is better to proceed 
#with bootstrapped confidence intervals for parameter estimates

#Add sum of all variables to see if it is differentially correlated with neuroanatomy
#later in lesion-behavior mapping. 

df.core.norm$g.sum <- (df.core.norm$sim + df.core.norm$inf + df.core.norm$wrat + df.core.norm$bd + df.core.norm$jlo + df.core.norm$cftc + df.core.norm$cftr + df.core.norm$reyr + df.core.norm$reyr + df.core.norm$reyh + df.core.norm$tmta + df.core.norm$tmtb + df.core.norm$cod + df.core.norm$dig + df.core.norm$ari + df.core.norm$vrtc)/16

#Correlated Factors Model

mod.cf <- 'Gc =~ inf + sim + wrat
Gv =~ bd + jlo + cftc + cftr
Gl =~ rey5 + reyr + reyh
Gs =~ tmta + tmtb + cod
Gwm =~ dig + ari + vrtc
tmta ~~ tmtb
cftc ~~ cftr'

fit.cf <- sem(mod.cf,
              data = df.core.norm, 
              information = 'observed', 
              std.lv = TRUE, 
              se = 'bootstrap',
              test = 'bootstrap')

summary(fit.cf, 
        standardized = T, 
        fit.measures = T, 
        rsquare = T)

semPaths(fit.cf, 
         whatLabels = 'std', 
         layout = "tree2", 
         nDigits = 2, 
         residuals = F, 
         freeStyle = "black", 
         nCharNodes = 4, 
         reorder = F, 
         curve = 1.5, 
         rotation = 2)

#Hierarchical model

mod.hi <- 'Gc =~ inf + sim + wrat
Gv =~ bd + jlo + cftc + cftr
Gl =~ rey5 + reyr + reyh
Gs =~ tmta + tmtb + cod
Gwm =~ dig + ari + vrtc
g =~ Gc + Gv + Gl + Gs + Gwm
tmta ~~ tmtb
cftc ~~ cftr'

fit.hi <- sem(mod.hi, 
              data = df.core.norm, 
              information = 'observed', 
              std.lv = TRUE, 
              se = 'bootstrap',
              test = 'bootstrap')

summary(fit.hi, 
        standardized = T, 
        fit.measures = T, 
        rsquare = T)

semPlot::semPaths(fit.hi, 
         whatLabels = 'std', 
         layout = "tree2", 
         nDigits = 2, 
         residuals = F, 
         freeStyle = "black", 
         nCharNodes = 4, 
         reorder = F, 
         curve = 1.5, 
         rotation = 2)

mod.indices <- modificationindices(fit.hi, sort. = T)

mod.indices

#Hierarchical model didn't converge, so look at it without bootstrap to see parameter estimates

mod.hi <- 'Gc =~ inf + sim + wrat
Gv =~ bd + jlo + cftc + cftr
Gl =~ rey5 + reyr + reyh
Gs =~ tmta + tmtb + cod
Gwm =~ dig + ari + vrtc
g =~ Gc + Gv + Gl + Gs + Gwm
tmta ~~ tmtb
cftc ~~ cftr'

fit.hi <- sem(mod.hi, 
              data = df.core.norm, 
              information = 'observed', 
              std.lv = TRUE)

summary(fit.hi, 
        standardized = T, 
        fit.measures = T, 
        rsquare = T)

semPlot::semPaths(fit.hi, 
                  whatLabels = 'std', 
                  layout = "tree2", 
                  nDigits = 2, 
                  residuals = F, 
                  freeStyle = "black", 
                  nCharNodes = 4, 
                  reorder = F, 
                  curve = 1.5, 
                  rotation = 2)

#Hierarchical model with Gwm loading on g constrained to plausible value since it was 1 

mod.hi <- 'Gc =~ inf + sim + wrat
Gv =~ bd + jlo + cftc + cftr
Gl =~ rey5 + reyr + reyh + cftr
Gs =~ tmta + tmtb + cod
Gwm =~ dig + ari + vrtc
g =~ Gc + Gv + Gl + Gs + 200*Gwm
tmta ~~ tmtb
cftc ~~ cftr'

fit.hi <- sem(mod.hi, 
              data = df.core.norm, 
              information = 'observed', 
              std.lv = TRUE,
              se = 'bootstrap',
              test = 'bootstrap')

summary(fit.hi, 
        standardized = T, 
        fit.measures = T, 
        rsquare = T)

semPlot::semPaths(fit.hi, 
                  whatLabels = 'std', 
                  layout = "tree2", 
                  nDigits = 2, 
                  residuals = F, 
                  freeStyle = "black", 
                  nCharNodes = 4, 
                  reorder = F, 
                  curve = 1.5, 
                  rotation = 2)

mod.indices <- modificationindices(fit.hi, sort. = T)

mod.indices

#Response to Reviewer #3

mod.hi.2 <- 'Gc =~ inf + sim + wrat
Gv =~ bd + jlo + cftc + cftr
Gl =~ rey5 + reyr + reyh + cftr
Gs =~ tmta + tmtb + cod
Gwm =~ dig + ari + vrtc
g =~ Gc + Gv + Gl + Gs + dig + ari+ vrtc
tmta ~~ tmtb
cftc ~~ cftr'

fit.hi.2 <- sem(mod.hi.2, 
              data = df.core.norm, 
              information = 'observed', 
              std.lv = TRUE,
              se = 'bootstrap',
              test = 'bootstrap')

summary(fit.hi.2, 
        standardized = T, 
        fit.measures = T, 
        rsquare = T)

anova(fit.hi, fit.hi.2)

#Hierarchical model

mod.hi.noGvGs <- 'Gc =~ inf + sim + wrat
Gwm =~ dig + ari + vrtc
Gl =~ rey5 + reyr + reyh
g =~ Gc + Gwm + Gl'

fit.hi.noGvGs <- sem(mod.hi.noGvGs, 
                      data = df.core.norm, 
                      information = 'observed', 
                      std.lv = TRUE)

summary(fit.hi.noGvGs, 
        standardized = T, 
        fit.measures = T, 
        rsquare = T)

#Hierarchical model without Gwm

mod.hi.noGwm <- 'Gc =~ inf + sim + wrat
Gv =~ bd + jlo + cftc + cftr
Gl =~ rey5 + reyr + reyh + cftr
Gs =~ tmta + tmtb + cod
g =~ Gc + Gv + Gl + Gs
tmta ~~ tmtb
cftc ~~ cftr'

fit.hi.noGwm <- sem(mod.hi.noGwm, 
                    data = df.core.norm, 
                    information = 'observed', 
                    std.lv = TRUE,
                    se = 'bootstrap',
                    test = 'bootstrap')

summary(fit.hi.noGwm, 
        standardized = T, 
        fit.measures = T, 
        rsquare = T)

#Bifactor

mod.bi <- 'g =~ inf + sim + wrat + bd + jlo + cftc + cftr + rey5 + reyr + reyh + tmta + tmtb + cod + dig + ari + vrtc
Gc =~ inf + sim + wrat
Gv =~ bd + jlo + cftc + cftr
Gl =~ rey5 + reyr + reyh + cftr
Gs =~ tmta + tmtb + cod
Gwm =~ dig + ari + vrtc
cftc ~~ cftr
tmta ~~ tmtb'

fit.bi <- sem(mod.bi, 
              data = df.core.norm, 
              information = "observed", 
              std.lv = TRUE, 
              estimator = 'ML', 
              orthogonal = T,
              se = 'bootstrap',
              test = 'bootstrap')

summary(fit.bi, 
        standardized = T, 
        fit.measures = T, 
        rsquare = T)

semPaths(fit.bi, 
         whatLabels = 'std', 
         layout = "tree2", 
         nDigits = 2, 
         residuals = F, 
         freeStyle = "black", 
         exoCov = F, 
         nCharNodes = 4, 
         reorder = F, 
         curve = 1.5, 
         rotation = 2, 
         bifactor = 'g', 
         cardinal = 'man')

#Anomalous findings (non-sig or negative loadings) in Gwm factor consistent with its high correlation with g in hierarchical model
#This warrants a bifactor-S2 model where Gwm factor is removed entirely letting indiactors of Gwm load onto g

mod.bi.2 <- 'g =~ inf + sim + wrat + bd + jlo + cftc + cftr + rey5 + reyr + reyh + tmta + tmtb + cod + dig + ari + vrtc
Gc =~ inf + sim + wrat
Gv =~ bd + jlo + cftc + cftr
Gl =~ rey5 + reyr + reyh + cftr
Gs =~ tmta + tmtb + cod
cftc ~~ cftr
tmta ~~ tmtb'

fit.bi.2 <- sem(mod.bi.2, 
                data = df.core.norm, 
                information = "observed", 
                std.lv = TRUE, 
                estimator = 'ML', 
                orthogonal = T,
                se = 'bootstrap',
                test = 'bootstrap')

summary(fit.bi.2, 
        standardized = T, 
        fit.measures = T, 
        rsquare = T)

semPaths(fit.bi.2, whatLabels = 'std', layout = "tree2", nDigits = 2, residuals = F, freeStyle = "black", exoCov = F, nCharNodes = 4, reorder = F, curve = 1.5, rotation = 2, bifactor = 'g', cardinal = 'man')

#Is there a difference between the two bifactor models?

fit.bi <- fit.bi.2

#Because there is no significant difference between bifactor model 1 and bifactor model 2
#we should use bifactor model 2 because it is the more parsimonious model (more constraints = simpler)

reliability(fit.bi)

#Compare hi, sem.2, and bi models

anova(fit.hi, fit.bi)

#g without Gs and Gwm

#Put factor scores into dataframe

df.core.factors.cf <- as.data.frame(predict(fit.cf))
df.core.factors.cf$id <- df.core.norm$id

df.core.factors.hi.noGvGs <- as.data.frame(predict(fit.hi.noGvGs))
df.core.factors.hi.noGvGs$id <- df.core.norm$id

df.core.factors.hi.noGwm <- as.data.frame(predict(fit.hi.noGwm))
df.core.factors.hi.noGwm$id <- df.core.norm$id

df.core.factors.hi <- as.data.frame(predict(fit.hi))
df.core.factors.hi$id <- df.core.norm$id

df.core.factors.bi <- as.data.frame(predict(fit.bi))
df.core.factors.bi$id <- df.core.norm$id

#Read in from old export to csv because of new analyses - I don't want to overwrite old data files, but instead add to old and write new one

#Regress out confounds

#First factors back into original dataframe that has demographic data

df.core <- read.csv('/Users/mbowren/drive/uiowa/core/data/old/402/core_master_402.csv')
df.core <- df.core[,-1]
df.core$Gc <- df.core.factors.cf$Gc
df.core$Gv <- df.core.factors.cf$Gv
df.core$Gl <- df.core.factors.cf$Gl
df.core$Gs <- df.core.factors.cf$Gs
df.core$Gwm <- df.core.factors.cf$Gwm
df.core$g.hi <- df.core.factors.hi$g #g estimated from all factors including Gwm and Gs
df.core$g.hi.noGvGs <- df.core.factors.hi.noGvGs$g #g estimated from only Gc, Gv, and Gl
df.core$g.hi.noGwm <- df.core.factors.hi.noGwm$g #g estiamted without Gwm
df.core$Gc.hi <- df.core.factors.hi$Gc
df.core$Gv.hi <- df.core.factors.hi$Gv
df.core$Gl.hi <- df.core.factors.hi$Gl
df.core$Gs.hi <- df.core.factors.hi$Gs
df.core$Gwm.hi <- df.core.factors.hi$Gwm
df.core$g.bi <- df.core.factors.bi$g
df.core$Gc.bi <- df.core.factors.bi$Gc
df.core$Gv.bi <- df.core.factors.bi$Gv
df.core$Gl.bi <- df.core.factors.bi$Gl
df.core$Gs.bi <- df.core.factors.bi$Gs
df.core$Gwm.bi <- df.core.factors.bi$Gwm

#Check correlation between full g and g without Gwm

cor.test(df.core$g.hi, df.core$g.hi.noGwm)

#Look at which variables are correlated with g and Gs

df.core.reg <- subset(df.core, select = c('id', 'g.hi', 'g.hi.noGwm', 'Gc', 'Gv', 'Gl', 'Gs', 'Gwm', 'Gc.hi', 'Gv.hi', 'Gl.hi', 'Gs.hi', 'Gwm.hi', 'g.bi', 'Gc.bi', 'Gv.bi', 'Gl.bi', 'Gs.bi', 'age.at.scan', 'edu', 'lv', 'lesion.chronicity.months'))
df.core.reg <- as.data.frame(apply(df.core.reg, 2, as.numeric))
df.core.reg.1 <- df.core.reg
df.core.reg.1$sex <- as.factor(as.character(df.core$sex))
df.core.reg.1$scan.type <- as.factor(as.character(df.core$scan.type))
df.core.reg.1$edu <- as.numeric(df.core$edu)

#Is age at scan date correlated with g? Answer: Yes. So regress it out.

cor.test(df.core.reg.1$age.at.scan, df.core.reg.1$g.hi)

#Is sex correlated with g? Answer: No. So no need to regress it out.

cor.test(as.numeric(df.core.reg.1$sex), df.core.reg.1$g.hi)

#Is lesion chronicity correlated with g? Answer: No.

cor.test(df.core.reg.1$lesion.chronicity.months, df.core.reg.1$g.hi)

#Is education correlated with g? Answer: Yes. Regress it out.

cor.test(df.core.reg.1$edu, df.core.reg.1$g.hi)

#Now use regression models to residualize against lesion chronicity.

lm.g.hi <- lm(g.hi ~ age.at.scan + edu, data = df.core.reg.1)
lm.g.bi <- lm(g.bi ~ age.at.scan + edu, data = df.core.reg.1)
lm.Gc <- lm(Gc.hi ~ age.at.scan + edu, data = df.core.reg.1)
lm.Gv <- lm(Gv.hi ~ age.at.scan + edu, data = df.core.reg.1)
lm.Gl <- lm(Gl.hi ~ age.at.scan + edu, data = df.core.reg.1)
lm.Gs <- lm(Gs.hi ~ age.at.scan + edu, data = df.core.reg.1)
lm.Gwm <- lm(Gwm.hi ~ age.at.scan + edu, data = df.core.reg.1)

#Regress out Gc and lesion chronicity from other CF variables for network analysis

lm.g.hi.regGc <- lm(g.hi ~ Gc + lesion.chronicity.months + sex, data = df.core.reg.1)
lm.Gv.hi.regGc <- lm(Gv.hi ~ Gc + lesion.chronicity.months + sex, data = df.core.reg.1)
lm.Gl.hi.regGc <- lm(Gl.hi ~ Gc + lesion.chronicity.months + sex, data = df.core.reg.1)
lm.Gs.hi.regGc <- lm(Gs.hi ~ Gc + lesion.chronicity.months + sex, data = df.core.reg.1)
lm.Gwm.hi.regGc <- lm(Gwm.hi ~ Gc + lesion.chronicity.months + sex, data = df.core.reg.1)
df.core.reg.1$g.hi.regGc <- resid(lm.g.hi.regGc)
df.core.reg.1$Gv.hi.regGc <- resid(lm.Gv.hi.regGc)
df.core.reg.1$Gl.hi.regGc <- resid(lm.Gl.hi.regGc)
df.core.reg.1$Gs.hi.regGc <- resid(lm.Gs.hi.regGc)
df.core.reg.1$Gwm.hi.regGc <- resid(lm.Gwm.hi.regGc)

lm.Gv.cf.regGc <- lm(Gv ~ Gc + lesion.chronicity.months + sex, data = df.core.reg.1)
lm.Gl.cf.regGc <- lm(Gl ~ Gc + lesion.chronicity.months + sex, data = df.core.reg.1)
lm.Gs.cf.regGc <- lm(Gs ~ Gc + lesion.chronicity.months + sex, data = df.core.reg.1)
lm.Gwm.cf.regGc <- lm(Gwm ~ Gc + lesion.chronicity.months + sex, data = df.core.reg.1)
df.core.reg.1$Gv.cf.regGc <- resid(lm.Gv.cf.regGc)
df.core.reg.1$Gl.cf.regGc <- resid(lm.Gl.cf.regGc)
df.core.reg.1$Gs.cf.regGc <- resid(lm.Gs.cf.regGc)
df.core.reg.1$Gwm.cf.regGc <- resid(lm.Gwm.cf.regGc)

lm.Gv.bi.regGc <- lm(g.hi ~ Gc + lesion.chronicity.months + sex, data = df.core.reg.1)
lm.Gl.bi.regGc <- lm(g.hi ~ Gc + lesion.chronicity.months + sex, data = df.core.reg.1)
lm.Gs.bi.regGc <- lm(g.hi ~ Gc + lesion.chronicity.months + sex, data = df.core.reg.1)
df.core.reg.1$Gv.bi.regGc <- resid(lm.Gv.bi.regGc)
df.core.reg.1$Gl.bi.regGc <- resid(lm.Gl.bi.regGc)
df.core.reg.1$Gs.bi.regGc <- resid(lm.Gs.bi.regGc)

write.csv(df.core.reg.1, file = '/Users/mbowren/drive/uiowa/core/data/regGc.csv')

#Add to dataframes

df.core.reg.1$g.hi.resid <- resid(lm.g.hi)
df.core.factors.hi$g.hi.resid <- resid(lm.g.hi)

df.core.reg.1$g.bi.resid <- resid(lm.g.bi)
df.core.factors.bi$g.bi.resid <- resid(lm.g.bi)

df.core.reg.1$Gc.resid <- resid(lm.Gc)
df.core.factors.hi$Gc.resid <- resid(lm.Gc)

df.core.reg.1$Gv.resid <- resid(lm.Gv)
df.core.factors.hi$Gv.resid <- resid(lm.Gv)

df.core.reg.1$Gl.resid <- resid(lm.Gl)
df.core.factors.hi$Gl.resid <- resid(lm.Gl)

df.core.reg.1$Gs.resid <- resid(lm.Gs)
df.core.factors.hi$Gs.resid <- resid(lm.Gs)

df.core.reg.1$Gwm.resid <- resid(lm.Gwm)
df.core.factors.hi$Gwm.resid <- resid(lm.Gwm)

df.core.factors.hi$sex <- df.core.reg.1$sex
df.core.factors.bi$sex <- df.core.reg.1$sex
df.core.factors.hi$edu <- df.core.reg.1$edu
df.core.factors.bi$edu <- df.core.reg.1$edu
df.core.factors.hi$lesion.chronicity.months <- df.core.reg.1$lesion.chronicity.months
df.core.factors.bi$lesion.chronicity.months <- df.core.reg.1$lesion.chronicity.months

df.core.factors.hi <- subset(df.core.factors.hi, select = c('id', 'sex', 'edu', 'lesion.chronicity.months', 'g', 'Gc', 'Gv', 'Gl', 'Gs', 'Gwm', 'g.hi.resid', 'Gc.resid', 'Gv.resid', 'Gl.resid', 'Gs.resid', 'Gwm.resid'))

#Add raw sum score version of g to dataframe

df.core.factors.hi$g.sum <- df.core.norm$g.sum

#Write dataframes to files (don't over-write old)

write.csv(df.core.factors.cf, file = '/Users/mbowren/drive/uiowa/core/data/old/402/df_core_factors_cf.csv')
write.csv(df.core.factors.hi, file = '/Users/mbowren/drive/uiowa/core/data/old/402/df_core_factors_hi_withAllResid.csv')
write.csv(df.core.factors.bi, file = '/Users/mbowren/drive/uiowa/core/data/old/402/df_core_factors_bi_withAllResid.csv')
write.csv(df.core.factors.hi.noGwm, file = '/Users/mbowren/drive/uiowa/core/data/old/402/df_core_factors_hi_noGwm_withAllResid.csv')






#Compare to model without imputed data

df.core.norm.ni <- read.csv('/Users/mbowren/drive/uiowa/core/data/old/402/df_core_norm_no_imputation.csv')
df.core.norm.ni <- df.core.norm.ni[,-1]
df.core.norm.ni <- df.core.norm.ni[order(df.core.norm.ni$id), ]
df.core.norm.ni <- na.omit(df.core.norm.ni)
nrow(df.core.norm.ni)

#Hierarchical model

mod.hi.ni <- 'Gc =~ inf + sim + wrat
Gv =~ bd + jlo + cftc + cftr
Gl =~ rey5 + reyr + reyh + cftr
Gs =~ tmta + tmtb + cod
Gwm =~ dig + ari + vrtc
g =~ Gc + Gv + Gl + Gs + 100*Gwm
tmta ~~ tmtb
cftc ~~ cftr'

fit.hi.ni <- sem(mod.hi.ni, 
              data = df.core.norm.ni, 
              information = 'observed', 
              std.lv = TRUE,
              se = 'robust')

summary(fit.hi.ni, standardized = T, fit.measures = T, rsquare = T)

semPlot::semPaths(fit.hi, 
                  whatLabels = 'std', 
                  layout = "tree2", 
                  nDigits = 2, 
                  residuals = F, 
                  freeStyle = "black", 
                  nCharNodes = 4, 
                  reorder = F, 
                  curve = 1.5, 
                  rotation = 2)

#Bifactor model

mod.bi.ni <- mod.bi

fit.bi.ni <- sem(mod.bi.ni, 
                 data = df.core.norm.ni, 
                 information = "observed", 
                 std.lv = TRUE,
                 orthogonal = TRUE, 
                 se = 'robust')

summary(fit.bi.ni, standardized = T, fit.measures = T, rsquare = T)

semPaths(fit.bi, 
         whatLabels = 'std', 
         layout = "tree2", 
         nDigits = 2, 
         residuals = F, 
         freeStyle = "black", 
         exoCov = F, 
         nCharNodes = 4, 
         reorder = F, 
         curve = 1.5, 
         rotation = 2, 
         bifactor = 'g', 
         cardinal = 'man')

#Put factor scores into dataframe

df.core.factors.hi.ni <- as.data.frame(predict(fit.hi.ni))
df.core.factors.hi.ni$id <- df.core.norm.ni$id

df.core.factors.bi.ni <- as.data.frame(predict(fit.bi.ni))
df.core.factors.bi.ni$id <- df.core.norm.ni$id

#Write dataframes to files

#write.csv(df.core.factors.hi.ni, file = '/Users/mbowren/drive/uiowa/core/data/old/402/df_core_factors_hi_no_imputation.csv')
#write.csv(df.core.factors.bi.ni, file = '/Users/mbowren/drive/uiowa/core/data/old/402/df_core_factors_bi_no_imputation.csv')

#Correlate g without Gs and Gwm with the full g

df.core.factors.hi.noGsGwm$g.hi <- df.core.factors.hi$g
df.core.factors.hi.noGsGwm$g.bi <- df.core.factors.bi$g

cor.test(df.core.factors.hi.noGsGwm$g, df.core.factors.hi.noGsGwm$g.hi)
cor.test(df.core.factors.hi.noGsGwm$g, df.core.factors.hi.noGsGwm$g.bi)

#Correlate bifactor and hierarchical g scores

cor.test(df.core.factors.hi.noGsGwm$g.hi, df.core.factors.hi.noGsGwm$g.bi)
