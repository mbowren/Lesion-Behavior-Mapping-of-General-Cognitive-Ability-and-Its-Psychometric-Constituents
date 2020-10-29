#Psychometric analysis of g, Gs, and Gwm among 399 community-dwelling adults from wais et al. (2004) data.

#Load libraries

library(lavaan)
library(semTools)

#Copy correlation matrix from wais et al. table 2.

wais.cor.mat <- lav_matrix_lower2full(c(1,
                                        .49, 1,
                                        .45, .48, 1,
                                        .54, .51, .47, 1,
                                        .45, .74, .50, .51, 1,
                                        .50, .54, .60, .52, .57, 1,
                                        .41, .35, .40, .39, .34, .37, 1,
                                        .64, .44, .40, .53, .42, .48, .38, 1,
                                        .44, .64, .43, .49, .73, .57, .34, .43, 1,
                                        .40, .41, .45, .45, .41, .43, .65, .37, .34, 1))

#Create a vector names that match order of variables in correlation matrix

wais.cor.mat.names <- c('bd', 'sim', 'dig', 'mr', 'voc', 'ari', 'ss', 'vp', 'inf', 'cod')

#Create vector of SDs that order of variables in correlation matrix. Copied from table 1 (right hand side for normal, left side is a neurological sample)

wais.sd <- c(3.1, 2.9, 3.0, 3.1, 3.0, 3.0, 3.1, 3.1, 3.1, 3.0)

#Use the above objects to convert the correlation matrix to a covariance matrix for lavaan

wais.cov.mat <- cor2cov(wais.cor.mat, wais.sd, names = wais.cor.mat.names)

#Models

#Hierarchical

mod.wais.cf <- 'Gc =~ sim + inf + voc
Gv =~ bd + mr + vp
Gs =~ cod + ss
Gwm =~ dig + ari'

fit.wais.cf <- sem(mod.wais.cf, sample.cov = wais.cov.mat, information = "observed", std.lv = TRUE, sample.nobs = 2200) 

summary(fit.wais.cf, standardized = T, fit.measures = T, rsquare = T)

#Hierarchical

mod.wais.hi <- 'Gc =~ sim + inf + voc
Gv =~ bd + mr + vp
Gs =~ cod + ss
Gwm =~ dig + ari
g =~ Gc + Gv + Gs + Gwm'

fit.wais.hi <- sem(mod.wais.hi, sample.cov = wais.cov.mat, information = "observed", std.lv = TRUE, sample.nobs = 2200) 

summary(fit.wais.hi, standardized = T, fit.measures = T, rsquare = T)

reliability(fit.wais.hi)

#Bifactor; does not converge due to non-positive definite variance-covariance matrix

mod.wais.bi <- 'g =~ sim + inf + voc + bd + mr + vp + cod + ss + dig + ari
Gc =~ sim + inf + voc
Gv =~ bd + mr + vp
Gs =~ cod + ss
Gwm =~ dig + ari'

fit.wais.bi <- sem(mod.wais.bi, sample.cov = wais.cov.mat, information = "observed", std.lv = TRUE, sample.nobs = 2200, estimator = 'ML', orthogonal = T) 

summary(fit.wais.bi, standardized = T, fit.measures = T, rsquare = T)

reliability(fit.wais.bi)

#Bifactor S-1

mod.wais.bi <- 'g =~ sim + inf + voc + bd + mr + vp + cod + ss + dig + ari
Gc =~ sim + inf + voc
Gv =~ bd + mr + vp
Gs =~ cod + ss'

fit.wais.bi <- sem(mod.wais.bi, sample.cov = wais.cov.mat, information = "observed", std.lv = TRUE, sample.nobs = 2200, estimator = 'ML', orthogonal = T) 

summary(fit.wais.bi, standardized = T, fit.measures = T, rsquare = T)

reliability(fit.wais.bi)

anova(fit.wais.hi, fit.wais.bi)
