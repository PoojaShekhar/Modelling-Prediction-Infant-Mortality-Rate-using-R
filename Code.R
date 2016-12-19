//READ THE DATASET
infant.mortality <- read.csv("C:/Users/Desktop/ISL/Poster1/Child deaths.csv")
View(infant.mortality)

//SCATTER PLOT MATRIX
pairs(~imr+cdr+hdp+lbw+spw+tbm, data=infant.mortality,panel=panel.smooth,lwd=3,upper.panel=NULL,main="Scatterplot Matrix")

//MULTIPLE REGRESSION MODEL GENERATION
fit.lm=lm(imr~cdr+hdp+lbw+spw+tbm+cdr*hdp*spw*tbm,data=infant.mortality)
summary(fit.lm)

//NORMAL CONDITION CHECK - HISTOGRAM and NORMAL PROBABILITY PLOT
fit.residual=rstandard(fit.lm)
qqnorm(fit.residual,ylab="Standardized Residuals",xlab="Normal Scores",main="Normal Probability Plot")
hist(fit.residual,ylab="frequency",xlab="Residuals(Infant Mortality per 1,000 live births)",main="Nearly Normal Residual Histogram",col="grey")

//3-FOLD CROSS VALIDATION
library(DAAG)
cv.lm.fit=cv.lm(data=infant.mortality, fit.lm,m=3)