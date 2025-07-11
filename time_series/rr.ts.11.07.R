rm(list=ls())
data1=ts(c(12,17,20,32,23,13,31),
   frequency = 12,
   start = c(2003,11),
   end = c(2004,5));data1
ts(c(12,17,20,32,23,13,31),
         frequency = 12,
         start = c(2003,11))
ts(c(12,17,20,32,23,13,31),
   frequency = 12,
   end = c(2004,5))
ts(1:15,frequency=4,start=c(2016,3))
help(ts)

mat= matrix(21:29,nrow=3);mat
ts(mat,start=2001,frequency = 1)


# excercise 1
rm(list=ls())
set.seed(42)
raw = rnorm(48, mean=2, sd=3)
ts1= ts(raw, deltat=1, start=1973);ts1
plot(ts1)
ts2= ts(raw, deltat=3/12, start=c(2002,4));ts2
plot(ts2)
ts3= ts(raw ,deltat=1/12, start=c(1984,11));ts3
#ts(, frequency=12, start=c(1984,11), end=c(1988,10))
plot(ts3)

# excercise 2
raw.mat= matrix(raw,ncol=6)
ts4= ts(raw.mat, frequency=4, start=c(2022,3),
        names=c("Delhi","Mumbai","Kolkata","Hyderabad","Agra","Chennai"));ts4
# plot separately
plot(ts4,plot.type="multiple",xy.labels="Time",
     ylab="Values", lwd=2)
# plot together
plot(ts4, plot.type = "single",xy.labels="Time",
     ylab="Values", col=1:6, lwd=2)
legend("topright", legend=colnames(ts4),
       col=1:6, lwd=2)