library(TSA); data(prescrip)

#(a)
plot(y=prescrip,x=as.vector(time(prescrip)),type='l',xlab='year',ylab='costs')
points(y=prescrip,x=as.vector(time(prescrip)),pch=as.vector(season(prescrip)))


#(b)
har.=harmonic(prescrip,m=1)
model.har.pre=lm(prescrip~har.+time(prescrip))
summary(model.har.pre)

plot(as.vector(time(prescrip)),fitted(model.har.pre),ylab='Costs',type='l',ylim=range(c(fitted(model.har.pre),prescrip)))
# the ylim option ensures that the y axis fits the raw data and the fitted values
points(prescrip,pch=as.vector(season(prescrip)))


#(c)
plot(rstudent(model.har.pre),type='o')


#(d)
runs(rstudent(model.har.pre))


#(e)
acf(rstudent(model.har.pre))


#(f)
hist(rstudent(model.har.pre))
qqnorm(rstudent(model.har.pre))
shapiro.test(rstudent(model.har.pre))


#(g)
detrend=lm(prescrip~time(prescrip))

month.=season(prescrip)

model2=lm(resid(detrend)~month.-1) # -1 removes the intercept term
summary(model2)

AIC(model.har.pre)
BIC(model.har.pre)

AIC(model2)
BIC(model2)
