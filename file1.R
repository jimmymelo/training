###SACAR LIBRO
library(quantmod)
### IMPORTAR STOCKS
MSFT<-getSymbols("MSFT",from="2020-03-05",to="2021-03-05",src="yahoo",periodicity="daily",auto.assign=F)[,6]
####RENDIMIENTOS
RMSFT<-na.omit(ROC(MSFT,n=1,type="continuous",na.pad=T))
#### INFORMACI?N RESPECTO DEBT/EQUITY RATIO: 1.5=D/C
D<-MSFT[[length(MSFT)]]*5.5
t<-1
sdc<-sqrt(var(RMSFT)*252)
###  12-Month London Interbank Offered Rate (LIBOR), based on U.S. Dollar (USD12MD156N)
library(quantmod)
getSymbols("USD12MD156N",src="FRED",periodicity="daily",auto.assign=T)
r<-USD12MD156N[[9176]]/100
### vALOR DEL CAPITAL
C<-MSFT[[length(MSFT)]]
### starting values
A <-111
sd <- 0.1
####Iteracion
for(i in 1:1000) {
  A=(C+D*exp(-r*t)*pnorm((log(A/D)+(r+0.5*sd^2)*t)/(sd*sqrt(t))-sd*sqrt(t)))/pnorm((log(A/D)+(r+0.5*sd^2)*t)/(sd*sqrt(t)))
  sd=sdc*C/(pnorm((log(A/D)+(r+0.5*sd^2)*t)/(sd*sqrt(t)))*A)
}
#### formula simplificada de B-S
#### Delta: DEBT/EQUITY RATIO
d<-D*exp(-r*t)/A
h1<-(-(0.5*sd^2*t-log(d)))/sd*sqrt(t)
h2<-(-(0.5*sd^2*t+log(d)))/sd*sqrt(t)
VBBS<-D*exp(-r*t)*((1/d)*pnorm(h1)+pnorm(h2))
#### Premio por riesgo de contraparte
PRC<-100*(-1/t)*log(pnorm(h2)+(1/d)*pnorm(h1))
###Distance to Default
DD<-(D-A)/(sd*A)
### Expected Default Frequency
EDF<-pnorm(DD)