###########################################################
#
# RIPPLE EFFEKTER PÅ SVENSKA BOSTADSMARKNADEN - OTTO DAHLIN
#
###########################################################


install.packages("ggplot")
install.packages("lmtest")
install.packages("tseries")
install.packages("urca")
install.packages("dynlm")
install.packages("sandwich")
install.packages("readxl")
install.packages("forecast")
install.packages("xts")
install.packages("vars")
install.packages("zoo")
install.packages("quantmod")
install.packages("seasonal")
install.packages("CARS")
install.packages("car")
help.("causality")


library(ggplot2)
library(lmtest)
library(tseries)
library(urca)
library(dynlm)
library(sandwich)
library(readxl)
library(forecast)
library(xts)
library(vars)
library(zoo)
library(quantmod)
library(mFilter)
library(seasonal)
library(CARS)
library(car)

SCB <- read_excel("fastpi.xlsx")
View(SCB)
str(SCB)
summary(SCB)

#################            #################
#################   Steg 1   #################
#################            #################
#Skapar objekt för enskilda tidsserier (dvs alla kolumner från SCB FASTPI index)
#plocka hela Riket kolumn därav "Riket" efter kommatecken.

Riket_dataframe <- SCB[,"Riket"] 
class(Riket_dataframe)
Riket_serie<- ts(Riket_dataframe, start=c(1986,1), end = c(2018,4), frequency = 4)
Riket<- ts(Riket_dataframe, start=c(1986,1), end = c(2018,4), frequency = 4)
#Frågar efter klassificering av data. Vi har en tidsserie. TRUE
is.ts(Riket)
class(Riket)
ts.plot(Riket)

##Utför samma operation för samtliga tidsserier enl. data från SCB FASTPI index 

#StorStockholm
StorStockholm_dataframe <- SCB [, "StorStockholm"]
class(StorStockholm_dataframe)
StorStockholm_serie <- ts(StorStockholm_dataframe, start=c(1986,1), end=c(2018,4), frequency=4)
StorStockholm <- ts (StorStockholm_serie, start=c(1986,1), end=c(2018,4), frequency = 4)
#Frågar efter klassificering av data. VI har en tidsserie. TRUE
is.ts(StorStockholm)
class(StorStockholm)

#StorGöteborg
StorGoteborg_dataframe <- SCB [,"StorGoteborg"]
class(StorGoteborg_dataframe)
StorGoteborg_serie <- ts(StorGoteborg_dataframe, start=c(1986,1), end=c(2018,4), frequency=4)
StorGoteborg <- ts (StorGoteborg_serie, start=c(1986,1), end=c(2018,4), frequency = 4)
#Frågar efter klassificering av data. VI har en tidsserie. TRUE
is.ts(StorGoteborg)
class(StorGoteborg)
ts.plot(StorGoteborg)


#StorMalmö
StorMalmo_dataframe <- SCB [, "StorMalmo"]
class(StorMalmo_dataframe)
StorMalmo_serie <- ts(StorMalmo_dataframe, start=c(1986,1), end=c(2018,4), frequency=4)
StorMalmo <- ts (StorMalmo_serie, start=c(1986,1), end=c(2018,4), frequency = 4)
#Frågar efter klassificering av data. VI har en tidsserie. TRUE
is.ts(StorMalmo)
class(StorMalmo)

#StockholmsLän
StockholmsLan_dataframe <- SCB [, "StockholmsLan"]
class(StockholmsLan_dataframe)
StockholmsLan_serie <- ts(StockholmsLan_dataframe, start=c(1986,1), end=c(2018,4), frequency=4)
StockholmsLan <- ts (StockholmsLan_serie, start=c(1986,1), end=c(2018,4), frequency = 4)
#Frågar efter klassificering av data. VI har en tidsserie. TRUE
is.ts(StockholmsLan)
class(StockholmsLan)

#OstraMellanSverige
OstraMellanSverige_dataframe <- SCB [, "OstraMellanSverige"]
class(OstraMellanSverige_dataframe)
OstraMellanSverige_serie <- ts(OstraMellanSverige_dataframe, start=c(1986,1), end=c(2018,4), frequency=4)
OstraMellanSverige<- ts (OstraMellanSverige_serie, start=c(1986,1), end=c(2018,4), frequency = 4)
#Frågar efter klassificering av data. VI har en tidsserie. TRUE
is.ts(OstraMellanSverige)
class(OstraMellanSverige)

#SmålandMedOarna
SmalandMedOarna_dataframe <- SCB [, "SmalandMedOarna"]
class(SmalandMedOarna_dataframe)
SmalandMedOarna_serie <- ts(SmalandMedOarna_dataframe, start=c(1986,1), end=c(2018,4), frequency=4)
SmalandMedOarna<- ts (SmalandMedOarna_serie, start=c(1986,1), end=c(2018,4), frequency = 4)
#Frågar efter klassificering av data. VI har en tidsserie. TRUE
is.ts(SmalandMedOarna)
class(SmalandMedOarna)

#SydSverige
SydSverige_dataframe <- SCB [, "SydSverige"]
class(SydSverige_dataframe)
SydSverige_serie <- ts(SydSverige_dataframe, start=c(1986,1), end=c(2018,4), frequency=4)
SydSverige<- ts (SydSverige_serie, start=c(1986,1), end=c(2018,4), frequency = 4)
#Frågar efter klassificering av data. VI har en tidsserie. TRUE
is.ts(SydSverige)
class(SydSverige)

#VästSverige
VastSverige_dataframe <- SCB [, "VastSverige"]
class(VastSverige_dataframe)
VastSverige_serie <- ts(VastSverige_dataframe, start=c(1986,1), end=c(2018,4), frequency=4)
VastSverige<- ts (VastSverige_serie, start=c(1986,1), end=c(2018,4), frequency = 4)
#Frågar efter klassificering av data. VI har en tidsserie. TRUE
is.ts(VastSverige)
class(VastSverige)

#NorraMellanSverige
NorraMellanSverige_dataframe <- SCB [, "NorraMellanSverige"]
class(NorraMellanSverige_dataframe)
NorraMellanSverige_serie <- ts(NorraMellanSverige_dataframe, start=c(1986,1), end=c(2018,4), frequency=4)
NorraMellanSverige<- ts (NorraMellanSverige_serie, start=c(1986,1), end=c(2018,4), frequency = 4)
#Frågar efter klassificering av data. VI har en tidsserie. TRUE
is.ts(NorraMellanSverige)
class(NorraMellanSverige)


#MellerstaNorrland
MellerstaNorrland_dataframe <- SCB [, "MellerstaNorrland"]
class(MellerstaNorrland_dataframe)
MellerstaNorrland_serie <- ts(MellerstaNorrland_dataframe, start=c(1986,1), end=c(2018,4), frequency=4)
MellerstaNorrland<- ts (MellerstaNorrland_serie, start=c(1986,1), end=c(2018,4), frequency = 4)
#Frågar efter klassificering av data. VI har en tidsserie. TRUE
is.ts(MellerstaNorrland)
class(MellerstaNorrland)


#ÖvreNorrland
OvreNorrland_dataframe <- SCB [, "OvreNorrland"]
class(OvreNorrland_dataframe)
OvreNorrland_serie <- ts(OvreNorrland_dataframe, start=c(1986,1), end=c(2018,4), frequency=4)
OvreNorrland<- ts (OvreNorrland_serie, start=c(1986,1), end=c(2018,4), frequency = 4)
#Frågar efter klassificering av data. VI har en tidsserie. TRUE
is.ts(OvreNorrland)
class(OvreNorrland)
ts.plot(OvreNorrland)

#################            #################
#################   Steg 2   #################
#################            #################

#Samköra serien av FASTPI 

ts.plot(StorStockholm,StorGoteborg,StorMalmo,StockholmsLan,OstraMellanSverige,SmalandMedOarna,SydSverige,VastSverige,NorraMellanSverige,MellerstaNorrland,OvreNorrland, 
        main ="FASTPI; 1986:Q1 - 2018:Q4",
        gpars=list(xlab = "Year", col=c("blue","purple","red","green","pink","darkgreen","black","yellow","grey","orange","magenta")))

legend("topleft", legend = c("StorStockholm","StorGoteborg","StorMalmo","StockholmsLan","OstraMellanSverige",
                             "SmalandMedOarna","SydSverige","VastSverige","NorraMellanSverige","MellerstaNorrland","OvreNorrland"), lty = 1, cex=0.6)        


#################            #################
#################   Steg 3   #################
#################            #################

# Utför (ADF-TEST) & (KPSS-TEST) för samtliga tidsserier 

#ADF-test : För enhetsrot

ADF.test <- function(data){
  ADF <- function(type, data) {
    require(urca)
    result1 <- ur.df(data,
                     type = type,
                     lags = 3*frequency(data),
                     selectlags = "AIC")
    cbind(t(result1@teststat),result1@cval)
  }
  types <- c("trend", "drift", "none")
  result2 <- apply(t(types), 2, ADF, data)
  cat(rep("#", 17),'\n')
  cat("Augmented Dickey--Fuller test\n")
  cat(rep("#", 17),'\n')
  round(rbind(result2[[1]][c(1,3),],
              result2[[2]],
              result2[[3]]), 2)
}

#################             #################
#################   Steg 3:1  #################
#################             #################

#                  ADF-TEST

# Ho = hypotes om att det finns en enhetsrot
# Ha = Hypotes om att det inte finns en enhetsrot


#Riket 

# 3:1) NIVÅ
# ADF-test i nivå (utan några transformationer)
# Nivå: ursprungsserie
ADF.test(Riket) 
# Finns enhetsrot pga -1.67 < -3.43 givet 5pct (5%) nivå

# För att bli av med enhetsrötter och för att göra serien stationär behöver vi genomföra transformation.

# 3:2) FÖRSTA DIFF av Riket
# Skapar en serien med första differensen på Riket.
d.Riket <- diff(Riket)
ts.plot(d.Riket, main = "Första Differensen av Riket")
ADF.test(d.Riket) 
# Finns ingen enhetsrot 

# 3:3) LOG Riket
log.Riket <- log(Riket)
ts.plot(log.Riket, main = " LOG Riket Kvartal", col ="RED", ylab="Index" , xlab="Tid")
ADF.test(log.Riket) 

# 3:4) DIFF LOG Riket
d.log.Riket <- diff(log(Riket))
ts.plot(d.log.Riket, main ="DIFF LOG RIKET", col ="darkgreen")
ADF.test(d.log.Riket) 
# Finns ingen enhetsrot.

# Samköra serier/ Visa upp alla fyra serien i 1 figur.
par(mfrow=c(2,2))
ts.plot(Riket, main = "Riket")
ts.plot(d.log.Riket, main ="DIFF LOG RIKET", col ="darkgreen")
ts.plot(d.Riket, main = "Första Differensen av Riket")
ts.plot(log.Riket, main = " LOG Riket Kvartal", col ="RED", ylab="Index")


# Samma operationer som ovanstående utförs på samtliga tidsserier m.a.o samtliga regioner enl. FASTPI 1986:K1-2018:K4
#(Väljer 5pct nivå för resten av ADF-testen.)

####################################
#StorStockholm

#1. Nivå 
# ADF-test i nivå (utan några transformationer)
# Nivå: ursprungsserie
ADF.test(StorStockholm)
#Finns enhetsrot pga -3.13 < -3.43 givet 5pct nivå. 

# För att bli av enhetsrötter och för att göra serien stationär genomförs transformation från I(1) till I(0).

#2. Första differans StorStockholm
d.StorStockholm <-diff(StorStockholm)
ts.plot(d.StorStockholm, main = "Första differans av StorStockholm")
ADF.test(d.StorStockholm)
# Finns ingen enhetsrot pga -3.47 > -3.43 (givet 5pct nivå.)

#3. Log StorStockholm
log.StorStockholm <-log(StorStockholm)
ts.plot(log.StorStockholm, main ="Log StorStockholm", col= "black", ylab="Index", xlab="time")
ADF.test(log.StorStockholm)
# Finns ingen enhetsrot pga -3.48 > -3.43.

#4. Diff. Log. StorStockholm
d.log.StorStockholm <-diff(log(StorStockholm))
ADF.test(log.StorStockholm)
#Finns ingen enhetsrot pga  -3.48 > -3.43.

# Samköra serier/ Visa upp alla fyra serien i 1 figur. OBS! ändra namn på
par(mfrow=c(2,2))
ts.plot(StorStockholm, main = "StorStockholm ")
ts.plot(d.log.StorStockholm, main ="DIFF LOG StorStockholm", col ="darkgreen")
ts.plot(d.StorStockholm, main = "Första Differensen av StorStockholm")
ts.plot(log.StorStockholm, main = " LOG StorStockholm Kvartal", col ="RED", ylab="Index")        


################################## 
#StorGoteborg
#1. Nivå 
# ADF-test i nivå (utan några transformationer)
# Nivå: ursprungsserie
ADF.test(StorGoteborg)
#Finns enhetsrot pga -1.73 < -3.43

# För att bli av enhetsrötter och för att göra serien stationär genomförs transformation från I(1) till I(0).

#2. Första differans StorGöteborg
d.StorGoteborg<-diff(StorGoteborg)
ts.plot(d.StorGoteborg, main = "Första differans av StorGoteborg")
ADF.test(d.StorGoteborg)
# Finns ingen enhetsrot pga -6.41 > -3.43

#3. Log StorGöteborg
log.StorGoteborg <-log(StorGoteborg)
ts.plot(log.StorGoteborg, main ="Log StorGoteborg", col= "black", ylab="Index", xlab="time")
ADF.test(log.StorGoteborg)
# Finns enhetsrot pga -3.15 > 3.43

#4. Diff. Log. StorGöteborg
d.log.StorGoteborg <-diff(log(StorGoteborg))
ts.plot(d.log.StorGoteborg, main ="DIFF LOG StorGoteborg", col ="darkgreen")
ADF.test(d.log.StorGoteborg)
#Finns enhetsrot pga  -3.05 < -3.43.

# Samköra serier/ Visa upp alla fyra serien i 1 figur. 
par(mfrow=c(2,2))
ts.plot(StorGoteborg, main = "StorGoteborg")
ts.plot(d.log.StorGoteborg, main ="DIFF LOG StorGoteborg", col ="darkgreen")
ts.plot(d.StorGoteborg, main = "Första Differensen av StorGöteborg")
ts.plot(log.StorGoteborg, main = " LOG StorGoteborg Kvartal", col ="RED", ylab="Index")        

###################################
#StorMalmo
#1. Nivå 
# ADF-test i nivå (utan några transformationer)
# Nivå: ursprungsserie
ADF.test(StorMalmo)
#Finns enhetsrot pga -2.61 < -3.43

# För att bli av enhetsrötter och för att göra serien stationär genomförs transformation från I(1) till I(0).

#2. Första differans StorMalmö
d.StorMalmo<-diff(StorMalmo)
ts.plot(d.StorMalmo, main = "Första differans av StorMalmo")
ADF.test(d.StorMalmo)
# Finns ingen enhetsrot pga -3.63 > -3.43

#3. Log StorMalmö
log.StorMalmo <-log(StorMalmo)
ts.plot(log.StorMalmo, main ="Log StorMalmo", col= "black", ylab="Index", xlab="time")
ADF.test(log.StorMalmo)
# Finns ingen enhetsrot pga 2.15 > -1.95 
##################(OBS!!)##########
#tau3, phi3, tau2, phi1 < 5pct gränsvärde dock visar tau1 > 5pct gränsvärde därav anledning att det förekommer ingen 
#enhetsrot i detta fall. (Lundholm M.2018; Pfaff 2008, (från Lundholms kompendium))

#4. Diff. Log. StorMalmö
d.log.StorMalmo <-diff(log(StorMalmo))
ts.plot(d.log.StorMalmo, main ="DIFF LOG StorMalmo", col ="darkgreen")
ADF.test(d.log.StorMalmo)
#Finns ingen enhetsrot pga -3.95 > -3.43

# Samköra serier/ Visa upp alla fyra serien i 1 figur. 
par(mfrow=c(2,2))
ts.plot(StorMalmo, main = "StorMalmö")
ts.plot(d.log.StorMalmo, main ="DIFF LOG StorMalmp", col ="darkgreen")
ts.plot(d.StorMalmo, main = "Första Differensen av StorMalmo")
ts.plot(log.StorMalmo, main = " LOG StorMalmö Kvartal", col ="RED", ylab="Index") 

###################################
#StockholmsLan

#1. Nivå 
# ADF-test i nivå (utan några transformationer)
# Nivå: ursprungsserie
ADF.test(StockholmsLan)
#Finns enhetsrot pga -3.06 < -3.43

# För att bli av enhetsrötter och för att göra serien stationär genomförs transformation från I(1) till I(0).

#2. Första differans StockholmsLän
d.StockholmsLan<-diff(StockholmsLan)
ts.plot(d.StockholmsLan, main = "Första differans av StockholmsLan")
ADF.test(d.StockholmsLan)
# Finns ingen enhetsrot pga -3.47 > -3.43

#3. Log StockholmsLän
log.StockholmsLan <-log(StockholmsLan)
ts.plot(StockholmsLan, main ="Log StockholmsLan", col= "black", ylab="Index", xlab="time")
ADF.test(log.StockholmsLan)
# Finns ingen enhetsrot pga 1.80 > -1.95 

############(OBS!!)#####
#tau3, phi3, tau2, phi1 < 5pct gränsvärde dock visar tau1 > 5pct gränsvärde därav anledning att det förekommer ingen 
#enhetsrot i detta fall. (Lundholm M.2018; Pfaff 2008, (från Lundholms kompendium))

#4. Diff. Log. StockholmsLän
d.log.StockholmsLan <-diff(log(StockholmsLan))
ts.plot(d.log.StockholmsLan, main ="DIFF LOG StockholmsLan", col ="darkgreen")
ADF.test(d.log.StockholmsLan)
#Finns ingen enhetsrot pga -3.49 > -3.43

# Samköra serier/ Visa upp alla fyra serien i 1 figur. 
par(mfrow=c(2,2))
ts.plot(StockholmsLan, main = "StockholmsLan ")
ts.plot(d.log.StockholmsLan, main ="DIFF LOG StockholmsLan", col ="darkgreen")
ts.plot(d.StockholmsLan, main = "Första Differensen av StockholmsLan")
ts.plot(log.StockholmsLan, main = " LOG StockholmsLan Kvartal", col ="RED", ylab="Index") 

######################################
#OstraMellanSverige

#1. Nivå 
# ADF-test i nivå (utan några transformationer)
# Nivå: ursprungsserie
ADF.test(OstraMellanSverige)
#Finns enhetsrot -2.15 < -3.43

# För att bli av enhetsrötter och för att göra serien stationär genomförs transformation från I(1) till I(0).

#2. Första differans StockholmsLän
d.OstraMellanSverige<-diff(OstraMellanSverige)
ts.plot(d.OstraMellanSverige, main = "Första differans av ÖstraMellanSverige")
ADF.test(d.OstraMellanSverige)
# ????????????????? Hur löser man detta?  Samtliga statistic < 5pct gränsvärde ??????????????

#3. Log ÖstraMellanSverige
log.OstraMellanSverige <-log(OstraMellanSverige)
ts.plot(OstraMellanSverige, main ="Log OstraMellanSverige", col= "black", ylab="Index", xlab="time")
ADF.test(log.OstraMellanSverige)
# Finns ingen enhetsrot pga -3.57 > -3.43

#4. Diff. Log. ÖstraMellanSverige
d.log.OstraMellanSverige <-diff(log(OstraMellanSverige))
ts.plot(d.log.OstraMellanSverige, main ="DIFF LOG OstraMellanSverige", col ="darkgreen")
ADF.test(d.log.OstraMellanSverige)
#Finns ingen enhetsrot pga -3.57 > -3.43

# Samköra serier/ Visa upp alla fyra serien i 1 figur. 
par(mfrow=c(2,2))
ts.plot(OstraMellanSverige, main = "OstraMellanSverige")
ts.plot(d.log.OstraMellanSverige, main ="DIFF LOG OstraMellanSverige", col ="darkgreen")
ts.plot(d.OstraMellanSverige, main = "Första Differensen av OstraMellanSverige")
ts.plot(log.OstraMellanSverige, main = " LOG ÖstraMellanSverige Kvartal", col ="RED", ylab="Index")

############(OBS!!)############
"Saknas 1 graf vid namn DIFF LOG OstraMellanSverige pga oklarheter gällande FörstaDifferans StockholmsLän"

######################################
"SmalandMedOarna"

#1. Nivå 
# ADF-test i nivå (utan några transformationer)
# Nivå: ursprungsserie
ADF.test(SmalandMedOarna)
#Finns enhetsrot pga -2.20 < -3.43

# För att bli av enhetsrötter och för att göra serien stationär genomförs transformation från I(1) till I(0).

#2. Första differans SmålandMedÖarna
d.SmalandMedOarna<-diff(SmalandMedOarna)
ts.plot(d.SmalandMedOarna, main = "Första differans av SmalandMedOarna")
ADF.test(d.SmalandMedOarna)
# Finns inte enhetsrot tau3 = -3.61 > -3.43

#3. Log SmålandMedÖarna
log.SmalandMedOarna <-log(SmalandMedOarna)
ts.plot(SmalandMedOarna, main ="Log SmalandMedOarna", col= "black", ylab="Index", xlab="time")
ADF.test(log.SmalandMedOarna)
# Finns ingen enhetsrot (tau2>5pct Elimineringsordning m.h.a (Lundholm M.2018; Pfaff 2008, (från Lundholms kompendium)))

#4. Diff. Log. SmAlandMedSmaOarna
d.log.SmalandMedOarna <-diff(log(SmalandMedOarna))
ts.plot(d.log.SmalandMedOarna, main ="DIFF LOG SmalandMedOarna", col ="darkgreen")
ADF.test(d.log.SmalandMedOarna)
#Finns ingen enhetsrot (phi1>5pct Elimineringsordning m.h.a (Lundholm M.2018; Pfaff 2008, (från Lundholms kompendium)))

# Samköra serier/ Visa upp alla fyra serien i 1 figur. 
par(mfrow=c(2,2))
ts.plot(SmalandMedOarna, main = "SmalandMedOarna")
ts.plot(d.log.SmalandMedOarna, main ="DIFF LOG SmalandMedOarna", col ="darkgreen")
ts.plot(d.SmalandMedOarna, main = "Första Differensen av SmalandMedOarna")
ts.plot(log.SmalandMedOarna, main = " LOG SmalandMedOarna Kvartal", col ="RED", ylab="Index")

######################################
#SydSverige

#1. Nivå 
# ADF-test i nivå (utan några transformationer)
# Nivå: ursprungsserie
ADF.test(SydSverige)
#Finns enhetsrot pga -2.38 < -3.43

# För att bli av enhetsrötter och för att göra serien stationär genomförs transformation från I(1) till I(0).

#2. Första differans SydSverige
d.SydSverige<-diff(SydSverige)
ts.plot(d.SydSverige, main = "Första differans av SydSverige")
ADF.test(d.SydSverige)
# Finns enhetsrot -2.06 < -3.43

#3. Log SydSverige
log.SydSverige <-log(SydSverige)
ts.plot(SydSverige, main ="Log SydSverige", col= "black", ylab="Index", xlab="time")
ADF.test(log.SydSverige)
# Finns ingen enhetsrot (tau1 = 1.72 > -1.95 givet 5pct: Elimineringsordning m.h.a (Lundholm M.2018; Pfaff 2008, (från Lundholms kompendium)))

#4. Diff. Log. SydSverige
d.log.SydSverige <-diff(log(SydSverige))
ts.plot(d.log.SydSverige, main ="DIFF LOG SydSverige", col ="darkgreen")
ADF.test(d.log.SydSverige)
#Finns ingen enhetsrot (phi1 > 5pct Elimineringsordning m.h.a (Lundholm M.2018; Pfaff 2008, (från Lundholms kompendium)))

# Samköra serier/ Visa upp alla fyra serien i 1 figur. 
par(mfrow=c(2,2))
ts.plot(SydSverige, main = "SydSverige")
ts.plot(d.log.SydSverige, main ="DIFF LOG SydSverige", col ="darkgreen")
ts.plot(d.SydSverige, main = "Första Differensen av SydSverige")
ts.plot(log.SydSverige, main = " LOG SydSverige Kvartal", col ="RED", ylab="Index")


######################################
#VastSverige

#1. Nivå 
# ADF-test i nivå (utan några transformationer)
# Nivå: ursprungsserie
ADF.test(VastSverige)
#Finns enhetsrot pga -1.75 < -3.43

# För att bli av enhetsrötter och för att göra serien stationär genomförs transformation från I(1) till I(0).

#2. Första differans VästSverige
d.VastSverige<-diff(VastSverige)
ts.plot(d.VastSverige, main = "Första differans av VästSverige")
ADF.test(d.VastSverige)
# Finns inte enhetsrot -3.47 > -3.43

#3. Log VästSverige
log.VastSverige <-log(VastSverige)
ts.plot(VastSverige, main ="Log VastSverige", col= "black", ylab="Index", xlab="time")
ADF.test(log.VastSverige)
# Finns ingen enhetsrot (tau2 = 0.12 > -2.88 givet 5pct: 
#Elimineringsordning m.h.a (Lundholm M.2018; Pfaff 2008, (från Lundholms kompendium)))

#4. Diff. Log. VästSverige
d.log.VastSverige <-diff(log(VastSverige))
ts.plot(d.log.VastSverige, main ="DIFF LOG VastSverige", col ="darkgreen")
ADF.test(d.log.VastSverige)
#Finns ingen enhetsrot (phi1 > 5pct Elimineringsordning m.h.a (Lundholm M.2018; Pfaff 2008, (från Lundholms kompendium)))

# Samköra serier/ Visa upp alla fyra serien i 1 figur. 
par(mfrow=c(2,2))
ts.plot(VastSverige, main = "VästSverige")
ts.plot(d.log.VastSverige, main ="DIFF LOG VästSverige", col ="darkgreen")
ts.plot(d.VastSverige, main = "Första Differensen av VästSverige")
ts.plot(log.VastSverige, main = " LOG VästSverige Kvartal", col ="RED", ylab="Index")


######################################
#NorraMellanSverige
#1. Nivå 
# ADF-test i nivå (utan några transformationer)
# Nivå: ursprungsserie
ADF.test(NorraMellanSverige)
#Finns enhetsrot pga -1.36 < -3.43

# För att bli av enhetsrötter och för att göra serien stationär genomförs transformation från I(1) till I(0).

#2. Första differans NorraMellanSverige
d.NorraMellanSverige<-diff(NorraMellanSverige)
ts.plot(d.NorraMellanSverige, main = "Första differans av NorraMellanSverige")
ADF.test(d.NorraMellanSverige)
# Finns inte enhetsrot -3.47 > -3.43

#3. Log NorraMellanSverige
log.NorraMellanSverige <-log(NorraMellanSverige)
ts.plot(NorraMellanSverige, main ="Log NorraMellanSverige", col= "black", ylab="Index", xlab="time")
ADF.test(log.NorraMellanSverige)
# Finns ingen enhetsrot (tau2 = 0.45 > -2.88 givet 5pct: 
#Elimineringsordning m.h.a (Lundholm M.2018; Pfaff 2008, (från Lundholms kompendium)))

#4. Diff. Log. NorraMellanSverige
d.log.NorraMellanSverige <-diff(log(NorraMellanSverige))
ts.plot(d.log.NorraMellanSverige, main ="DIFF LOG NorraMellanSverige", col ="darkgreen")
ADF.test(d.log.NorraMellanSverige)
#Finns ingen enhetsrot (-3.69 > -3.43 vid 5pct 
#Elimineringsordning s.169 (Lundholm M.2018; Pfaff 2008, (från Lundholms kompendium)))

# Samköra serier/ Visa upp alla fyra serien i 1 figur. 
par(mfrow=c(2,2))
ts.plot(NorraMellanSverige, main = "NorraMellanSverige")
ts.plot(d.log.NorraMellanSverige, main ="DIFF LOG NorraMellanSverige", col ="darkgreen")
ts.plot(d.NorraMellanSverige, main = "Första Differensen av NorraMellanSverige")
ts.plot(log.NorraMellanSverige, main = " LOG NorraMellanSverige Kvartal", col ="RED", ylab="Index")

######################################
#MellerstaNorland

#1. Nivå 
# ADF-test i nivå (utan några transformationer)
# Nivå: ursprungsserie
ADF.test(MellerstaNorrland)
#Finns enhetsrot pga -1.35 < -3.43

# För att bli av enhetsrötter och för att göra serien stationär genomförs transformation från I(1) till I(0).

#2. Första differans MellerstaNorland
d.MellerstaNorrland <-diff(MellerstaNorrland)
ts.plot(d.MellerstaNorrland, main = "Första differans av MellerstaNorrland")
ADF.test(d.MellerstaNorrland)
# Finns enhetsrot pga -2.68 > -3.43

#3. Log MellerstaNorrland
log.MellerstaNorrland <-log(MellerstaNorrland)
ts.plot(MellerstaNorrland, main ="Log MellerstaNorrland", col= "black", ylab="Index", xlab="time")
ADF.test(log.MellerstaNorrland)
# Finns ingen enhetsrot (tau2 = 0.46 > -2.88 givet 5pct: 
#Elimineringsordning m.h.a (Lundholm M.2018; Pfaff 2008, (från Lundholms kompendium)))

#4. Diff. Log. MellerstaNorrland
d.log.MellerstaNorrland <-diff(log(MellerstaNorrland))
ts.plot(d.log.MellerstaNorrland, main ="DIFF LOG MellerstaNorrland", col ="darkgreen")
ADF.test(d.log.MellerstaNorrland)
#Finns ingen enhetsrot (-3.93 > -3.43 vid 5pct 
#Elimineringsordning s.169 (Lundholm M.2018; Pfaff 2008, (från Lundholms kompendium)))

# Samköra serier/ Visa upp alla fyra serien i 1 figur. 
par(mfrow=c(2,2))
ts.plot(MellerstaNorrland, main = "MellerstaNorrland")
ts.plot(d.log.MellerstaNorrland, main ="DIFF LOG MellerstaNorrland", col ="darkgreen")
ts.plot(d.MellerstaNorrland, main = "Första Differensen av MellerstaNorrland")
ts.plot(log.MellerstaNorrland, main = " LOG MellerstaNorrland Kvartal", col ="RED", ylab="Index")


######################################
#OvreNorrland

#1. Nivå 
# ADF-test i nivå (utan några transformationer)
# Nivå: ursprungsserie
ADF.test(OvreNorrland)
#Finns enhetsrot pga -0.94 < -3.43

# För att bli av enhetsrötter och för att göra serien stationär genomförs transformation från I(1) till I(0).

#2. Första differans ÖvreNorrland
d.OvreNorrland <-diff(OvreNorrland)
ts.plot(d.OvreNorrland, main = "Första differans av OvreNorrland")
ADF.test(d.OvreNorrland)
# Finns ingen enhetsrot pga phi3 = 18.26 > 6.49

#3. Log ÖvreNorrland
log.OvreNorrland <-log(OvreNorrland)
ts.plot(OvreNorrland, main ="Log OvreNorrland", col= "black", ylab="Index", xlab="time")
ADF.test(log.OvreNorrland)
# Finns ingen enhetsrot (tau2 = 0.18 > -2.88 givet 5pct: 
#Elimineringsordning m.h.a (Lundholm M.2018; Pfaff 2008, (från Lundholms kompendium)))

#4. Diff. Log. ÖvreNorrland
d.log.OvreNorrland <-diff(log(OvreNorrland))
ts.plot(d.log.OvreNorrland, main ="DIFF LOG OvreNorrland", col ="darkgreen")
ADF.test(d.log.OvreNorrland)
#Finns ingen enhetsrot (phi 3 9.56 > 6.49 vid 5pct 
#Elimineringsordning s.169 (Lundholm M.2018; Pfaff 2008, (från Lundholms kompendium)))

# Samköra serier/ Visa upp alla fyra serien i 1 figur. 
par(mfrow=c(2,2))
ts.plot(OvreNorrland, main = "OvreNorrland")
ts.plot(d.log.OvreNorrland, main ="DIFF LOG ÖvreNorrland", col ="darkgreen")
ts.plot(d.OvreNorrland, main = "Första Differensen av OvreNorrland")
ts.plot(log.OvreNorrland, main = " LOG ÖvreNorrland Kvartal", col ="RED", ylab="Index")


#################             #################
#################   Steg 3:2  #################
#################             #################

##                  KPSS-TEST 

##      (Kwiatkowski-Phillips-Schmidt-Shin Test)

#(Ytterligare statonäritetstest), använder 5% nivå.(5pct)

## H0: StationÃr
## HA: Icke-stationär 

## REGLER FÖR KPSS-TESTET:
## Teststatistikan är ett mått på variansen i testekvationen och om
## den är s.k. 'liten', är detta då en indikation på att vår serie
## är stationär. För att serien ska klassificeras som stationär krävs
## det att nollhypotesen i det genomförda KPSS-testet inte förkastas.

####################################
#Riket

# KPSS TEST PÅ Riket I NIVÅ (Ursprungsserie) !
ur.kpss(Riket, type = "tau")@teststat
#Teststatistika: 0.5427476
ur.kpss(Riket, type ="tau")@cval
#0.5427476 > 0.146 = EJ Stationärt!

# Eftersom att serien i nivå visar sig vara ICKE-STATIONÄRT i nivå,
# testar vi om DIFF LOG Riket är stationärt däremot. 
# Dvs ifall den transformerade/behandlade serien är stationärt. 
#Se bekräftelse nedan:

# KPSS TEST PÅ FÖRSTA DIFFERENSADE RIKET- serien
ur.kpss(d.Riket, type = "tau")@teststat
# Teststatistika: 0.06531914
ur.kpss(d.Riket, type ="tau")@cval
# 0.06531914 < 0.146 = STATIONÃRT!!!

#OBS! Samma operationer utförs på samtliga tidsserier/kolumner för att säkerställa stationaritet eller ej.

####################################
#StorStockholm

# KPSS TEST PÅ StorStockholm I NIVÅ (Ursprungsserie) !
ur.kpss(StorStockholm, type = "tau")@teststat
#Teststatistika: 0.5216141
ur.kpss(Riket, type ="tau")@cval
#0.5216141 > 0.146 = EJ Stationärt!

# KPSS TEST PÅ FÖRSTA DIFFERENSADE StorStockholm- serien
ur.kpss(d.StorStockholm, type = "tau")@teststat
# Teststatistika: 0.05898578
ur.kpss(d.StorStockholm, type ="tau")@cval
# 0.05898578 < 0.146 = STATIONÃRT!!!


####################################
#StorGöteborg

# KPSS TEST PÅ StorGöteborg I NIVÅ (Ursprungsserie) !
ur.kpss(StorGoteborg, type = "tau")@teststat
#Teststatistika: 0.571826
ur.kpss(StorGoteborg, type ="tau")@cval
#0.571826 > 0.146 = EJ Stationärt!

# KPSS TEST PÅ FÖRSTA DIFFERENSADE StorGöteborg- serien
ur.kpss(d.StorGoteborg, type = "tau")@teststat
# Teststatistika: 0.06808373
ur.kpss(d.StorGoteborg, type ="tau")@cval
# 0.06808373 < 0.146 = STATIONÃRT!!!

####################################
#StorMalmö

# KPSS TEST PÅ StorMalmö I NIVÅ (Ursprungsserie) 
ur.kpss(StorMalmo, type = "tau")@teststat
#Teststatistika: 0.4045587
ur.kpss(StorMalmo, type ="tau")@cval
#0.4045587 > 0.146 = EJ Stationärt!

# KPSS TEST PÅ FÖRSTA DIFFERENSADE StorMalmö- serien
ur.kpss(d.StorMalmo, type = "tau")@teststat
# Teststatistika: 0.06994547
ur.kpss(d.StorMalmo, type ="tau")@cval
# 0.06994547 < 0.146 = STATIONÃRT!!!

####################################
"StockholmsLän"

# KPSS TEST PÅ StockholmsLän I NIVÅ (Ursprungsserie) 
ur.kpss(StockholmsLan, type = "tau")@teststat
#Teststatistika: 0.525365
ur.kpss(StockholmsLan, type ="tau")@cval
#0.525365 > 0.146 = EJ Stationärt!

# KPSS TEST PÅ FÖRSTA DIFFERENSADE StockholmsLän- serien
ur.kpss(d.StockholmsLan, type = "tau")@teststat
# Teststatistika: 0.05935599
ur.kpss(d.StockholmsLan, type ="tau")@cval
# 0.05935599 < 0.146 = STATIONÃRT!!!

####################################
#ÖstraMellanSverige

# KPSS TEST PÅ ÖstraMellanSverige I NIVÅ (Ursprungsserie) 
ur.kpss(OstraMellanSverige, type = "tau")@teststat
#Teststatistika: 0.5347134
ur.kpss(OstraMellanSverige, type ="tau")@cval
#0.5347134 > 0.146 = EJ Stationärt!

# KPSS TEST PÅ FÖRSTA DIFFERENSADE ÖstraMellanSverige- serien
ur.kpss(d.OstraMellanSverige, type = "tau")@teststat
# Teststatistika: 0.08070094
ur.kpss(d.OstraMellanSverige, type ="tau")@cval
# 0.08070094 < 0.146 = STATIONÃRT!!!

####################################
#SmålandMedÖarna

# KPSS TEST PÅ SmålandMedÖarna I NIVÅ (Ursprungsserie) 
ur.kpss(SmalandMedOarna, type = "tau")@teststat
#Teststatistika: 0.5004575
ur.kpss(SmalandMedOarna, type ="tau")@cval
#0.5004575 > 0.146 = EJ Stationärt!

# KPSS TEST PÅ FÖRSTA DIFFERENSADE SmålandMedÖarna- serien
ur.kpss(d.SmalandMedOarna, type = "tau")@teststat
# Teststatistika: 0.07518791
ur.kpss(d.SmalandMedOarna, type ="tau")@cval
# 0.07518791 < 0.146 = STATIONÃRT!!!


####################################
#SydSverige

# KPSS TEST PÅ SydSverige I NIVÅ (Ursprungsserie) 
ur.kpss(SydSverige, type = "tau")@teststat
#Teststatistika: 0.415101
ur.kpss(SydSverige, type ="tau")@cval
#0.415101 > 0.146 = EJ Stationärt!

# KPSS TEST PÅ FÖRSTA DIFFERENSADE SydSverige- serien
ur.kpss(d.SydSverige, type = "tau")@teststat
# Teststatistika: 0.07790227
ur.kpss(d.SydSverige, type ="tau")@cval
# 0.07790227 < 0.146 = STATIONÃRT!!!

####################################
#VästSverige

# KPSS TEST PÅ VästSverige I NIVÅ (Ursprungsserie) 
ur.kpss(VastSverige, type = "tau")@teststat
#Teststatistika: 0.5676552
ur.kpss(VastSverige, type ="tau")@cval
#0.5676552 > 0.146 = EJ Stationärt!

# KPSS TEST PÅ FÖRSTA DIFFERENSADE VästSverige- serien
ur.kpss(d.VastSverige, type = "tau")@teststat
# Teststatistika: 0.07325507
ur.kpss(d.VastSverige, type ="tau")@cval
# 0.07325507 < 0.146 = STATIONÃRT!!!

####################################
#NorraMellanSverige

# KPSS TEST PÅ NorraMellanSverige I NIVÅ (Ursprungsserie) 
ur.kpss(NorraMellanSverige, type = "tau")@teststat
#Teststatistika: 0.5566522
ur.kpss(NorraMellanSverige, type ="tau")@cval
#0.5566522 > 0.146 = EJ Stationärt!

# KPSS TEST PÅ FÖRSTA DIFFERENSADE NorraMellanSverige- serien
ur.kpss(d.NorraMellanSverige, type = "tau")@teststat
# Teststatistika: 0.1113723
ur.kpss(d.NorraMellanSverige, type ="tau")@cval
# 0.1113723 < 0.146 = STATIONÃRT!!!

####################################
#MellerstaNorrland

# KPSS TEST PÅ MellerstaNorrland I NIVÅ (Ursprungsserie) 
ur.kpss(MellerstaNorrland, type = "tau")@teststat
#Teststatistika: 0.5764728
ur.kpss(MellerstaNorrland, type ="tau")@cval
#0.5764728 > 0.146 = EJ Stationärt!

# KPSS TEST PÅ FÖRSTA DIFFERENSADE MellerstaNorrland- serien
ur.kpss(d.MellerstaNorrland, type = "tau")@teststat
# Teststatistika: 0.09989885
ur.kpss(d.MellerstaNorrland, type ="tau")@cval
# 0.09989885 < 0.146 = STATIONÃRT!!!

####################################
#OvreNorrland

# KPSS TEST PÅ ÖvreNorrland I NIVÅ (Ursprungsserie) 
ur.kpss(OvreNorrland, type = "tau")@teststat
#Teststatistika: 0.5939076
ur.kpss(OvreNorrland, type ="tau")@cval
#0.5939076 > 0.146 = EJ Stationärt!

# KPSS TEST PÅ FÖRSTA DIFFERENSADE ÖvreNorrland- serien
ur.kpss(d.OvreNorrland, type = "tau")@teststat
# Teststatistika: 0.08364512
ur.kpss(d.OvreNorrland, type ="tau")@cval
# 0.08364512 < 0.146 = STATIONÃRT!!!

ur.kpss(d.log.OvreNorrland, type = "tau")@teststat
ur.kpss(d.log.OvreNorrland, type ="tau")@cval

ur.kpss(log.OvreNorrland, type = "tau")@teststat
ur.kpss(log.OvreNorrland, type ="tau")@cval

#################             #################
#################   Steg 4:1  #################
#################             #################

#               Granger Causality
#########################################################################
#
# KOINTEGRATION; PARVISA ENGLE-GRANGER TEST
#
##########################################################################

# THE ENGLE-GRANGER (EG-ADF) TEST OF COINTEGRATION 

# REGEL:

# 1) Vi genomför ett Engle-Granger test (Parvis) för kointegration genom att plocka fram residualerna från serierna
# och därmed undersöka för eventuella enhetsrötter samt ifall de är integrerade av samma ordning.
# Tidigare genomfördes ett traditionellt ADF-test på själva variablerna för att undersöka stationäritet, men
# när det kommer till genomförandet av "Engle-Granger 2 steps Cointegration procedure/test", så testar vi ifall
# residualerna som vi plockar fram är stationära: "In the case of Engle and Granger Cointegration you are testing 
# whether the residuals you obtain from the cointegrating relationship is stationary"

# OBS! För att kointegration ska vara möjligt, måste exempelvis serie "X" och serie "Y" ha samma integrationsordning
# dvs. "X" och "Y" ska båda antingen vara I(0) eller I(1). Om X är stationär men Y är icke-stationär då kan kointegration 
# ej finnas och sin tur ej godtas..
# M.a.o, nu när vi vet att alla våra variabler/tiddsserier behöver differensieras för att uppnå stationäritet, kan vi nu 
# konstatera att alla våra serier är I(1). Vi har dock "DIFF LOG serier" dvs tagit första differensen av logaritmerade serier.
# Som tolkning får vi: "tillväxttakter" mao "growth rates"= 0

# OBS! Vi genomför nu EG-ADF testet på transformerade serier dvs "difflog serier".

# Hypotes uppställning för EG-ADF testet:

# H0: "Non-stationary residual and NO cointegration between variables"
# HA: "Stationary residual and cointegration between variables".

# Visuellt kan man anta att StorStockholm och StorGöteborg delar ett långsiktigt samband.
# För att kontrollera detta se plot nedan.
ts.plot(StorStockholm, StorGoteborg, main=("StorStockholm vs StorGoteborg"))

# Residualer: StorStockhom vs. StorGöteborg (NIVÅ, men den ska BORT!, bara som extra har jag med denna)
resid.StorStockholm.StorGoteborg <- lm(StorStockholm ~ StorGoteborg)
coeftest(resid.StorStockholm.StorGoteborg)
#Testar nu residualerna för enhetsrot NIVÅ
ADF.test(residuals(resid.StorStockholm.StorGoteborg))
# Vi kan EJ förkasta nollhypotesen om enhetsrot. Vi behöver transformera den. Se nedan för "Diff.log" Format.

############################################################################################################################
# KÖR ALL NEDAN KOD: SAMTLIGA ÄR "DIFFLOG SERIER" VILKET ÄR ETT GRUNDKRAV DÅ ÅTERIGEN SAMMA INTEGRATIONSORDNING ÄR ETT KRAV! 
###########################################################################################################################

# Testar "DIFF LOG serier". VI SKA TESTA EG-ADF TEST D.V.S. PARVISA KOINTEGRATSTESTER PÅ SERIER
# SOM HAR SAMMA INTEGRATIONSORDNING. Vi genomför bara på "d.log.REGION" serier såsom nedan: 
# "d.log.StorStockholm" och "d.log.StorGöteborg".
resid.difflog.StorStockholm.StorGoteborg <- lm(d.log.StorStockholm ~ d.log.StorGoteborg)
coeftest(resid.difflog.StorStockholm.StorGoteborg)
#Testar nu residualerna för enhetsrot NIVÅ
ADF.test(residuals(resid.difflog.StorStockholm.StorGoteborg))

# Sammanfattning: StorStockholm vs StorGöteborg
# P.g.a (Tau3) -4.59 < -3.43 men även (Tau2) -4.53 < -2.88 , förkastar vi nollhypotesen om en enhetsrot i första differenserna
# när vi testar för residualerna i både StorStockholm och StorGöteborgs fall. De är alltså stationära.
# Och de två serierna är kointegrerade. Både "Diff.log.StorStockholm" och "Diff.log.StorGöteborg"
# är integrerade av samma ordning. Delar nog likartat stokastisk trend/drift
# De två verkar kointegrerade baserat på en OLS-estimation av residualerna.

###########################################################################
# StorStockholm vs StorMalmö

# Genomför EG-ADF test. Plockar fram residualerna. När vi regressar residualerna 
# vill vi förkasta nollhypotesen om enhetsrot. Detta bekräftar då att serierna är kointegrerade enligt EG-ADF testet.

# Testar Diff log serierna eftersom StorStockholm och StorMalmö är bägge I(1).
resid.difflog.StorStockholm.StorMalmo <- lm(d.log.StorStockholm ~ d.log.StorMalmo)
coeftest(resid.difflog.StorStockholm.StorMalmo)
#Testar nu residualerna för enhetsrot NIVÅ
ADF.test(residuals(resid.difflog.StorStockholm.StorMalmo))
# Kolla på Tau2 där vi har med DRIFT men ingen trend! Vi får ( TAU2)-3.23 < -2.88. 
# Vi förkastar nollhypotesen om en enhetsrot i residualerna.
# De är alltså stationära och seriena är kointegrerade. 

###########################################################################
# StorStockholm vs SthlmsLän

# Både StorSthlm och STHLMs län är I(1)
d.log.StockholmsLän

resid.difflog.StorStockholm.StocholmsLan <- lm(d.log.StorStockholm ~ d.log.StockholmsLan)
coeftest(resid.difflog.StorStockholm.StocholmsLan)
ADF.test(residuals(resid.difflog.StorStockholm.StocholmsLan))
# Vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder.

###########################################################################
# StorStockholm vs ÖstraMellanSverige

# Både StorSthlm och OstraMellanSverige  är I(1)
ADF.test(d.log.OstraMellanSverige) #bekräftar I(1)

resid.difflog.StorStockholm.OstraMellanSverige <- lm(d.log.StorStockholm ~ d.log.OstraMellanSverige)
coeftest(resid.difflog.StorStockholm.OstraMellanSverige)
ADF.test(residuals(resid.difflog.StorStockholm.OstraMellanSverige))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
#(tau3) -3.44 < -3.43
#(tau2) -3.34 < -2.88
###########################################################################
# StorStockholm vs SmålandÖarna

# Både StorSthlm och SmålandMedÖarna  är I(1)
ADF.test(d.log.SmalandMedOarna)

resid.difflog.StorStockholm.SmalandMedOarna <- lm(d.log.StorStockholm ~ d.log.SmalandMedOarna)
coeftest(resid.difflog.StorStockholm.SmalandMedOarna)
ADF.test(residuals(resid.difflog.StorStockholm.SmalandMedOarna))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder

###########################################################################
# StorStockholm vs SydSverige

# Både StorSthlm och SydSverige  är I(1)
resid.difflog.StorStockholm.SydSverige <- lm(d.log.StorStockholm ~ d.log.SydSverige)
coeftest(resid.difflog.StorStockholm.SydSverige)
ADF.test(residuals(resid.difflog.StorStockholm.SydSverige))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder


###########################################################################
# StorStockholm vs VästSverige

# Både StorSthlm och VastSverige  är I(1)
resid.difflog.StorStockholm.VastSverige <- lm(d.log.StorStockholm ~ d.log.VastSverige)
coeftest(resid.difflog.StorStockholm.VastSverige)
ADF.test(residuals(resid.difflog.StorStockholm.VastSverige))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
# StorStockholm vs MellerstaNorrland - Båda är I(1)

resid.difflog.StorStockholm.MellerstaNorrland <- lm(d.log.StorStockholm ~ d.log.MellerstaNorrland)
coeftest(resid.difflog.StorStockholm.MellerstaNorrland)
ADF.test(residuals(resid.difflog.StorStockholm.MellerstaNorrland))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
# StorStockholm vs OvreNorrland - Båda är I(1)

resid.difflog.StorStockholm.OvreNorrland <- lm(d.log.StorStockholm ~ d.log.OvreNorrland)
coeftest(resid.difflog.StorStockholm.OvreNorrland)
ADF.test(residuals(resid.difflog.StorStockholm.OvreNorrland))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder

############################################################################
# StorStockholm vs NorraMellanSverige - Båda är I(1)

resid.difflog.StorStockholm.NorraMellanSverige <- lm(d.log.StorStockholm ~ d.log.NorraMellanSverige)
coeftest(resid.difflog.StorStockholm.NorraMellanSverige)
ADF.test(residuals(resid.difflog.StorStockholm.NorraMellanSverige))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder


#                       KLAR StorStockholm och andra regioner
############################################################################
############################################################################
# ändrar riktning och ordning av regioner för att skapa en tabell med parvis tester av samtliga regioner med varandra i 
# båda riktningar för att säkerställa ifall kointegrationsriktningar finns eller ej. (En JA/NEJ tabell med alla regioner)
###########################################################################
# StorGöteborg vs StorStockholm - Båda är I(1)

resid.difflog.StorGoteborg.StorStockholm <- lm(d.log.StorGoteborg ~ d.log.StorStockholm)
coeftest(resid.difflog.StorGoteborg.StorStockholm)
ADF.test(residuals(resid.difflog.StorGoteborg.StorStockholm))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
# StorGöteborg vs StorMalmö - Båda är I(1)

resid.difflog.StorGoteborg.StorMalmo <- lm(d.log.StorGoteborg ~ d.log.StorMalmo)
coeftest(resid.difflog.StorGoteborg.StorMalmo)
ADF.test(residuals(resid.difflog.StorGoteborg.StorMalmo))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
# StorGoteborg vs StockholmsLan - Båda är I(1)

resid.difflog.StorGoteborg.StockholmsLan <- lm(d.log.StorGoteborg ~ d.log.StockholmsLan)
coeftest(resid.difflog.StorGoteborg.StockholmsLan)
ADF.test(residuals(resid.difflog.StorGoteborg.StockholmsLan))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
# StorGöteborg vs ÖstraMellanSverige - Båda är I(1)

resid.difflog.StorGoteborg.OstraMellanSverige <- lm(d.log.StorGoteborg ~ d.log.OstraMellanSverige)
coeftest(resid.difflog.StorGoteborg.OstraMellanSverige)
ADF.test(residuals(resid.difflog.StorGoteborg.OstraMellanSverige))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
# StorGöteborg vs SmålandMedÖarna - Båda är I(1)

resid.difflog.StorGoteborg.SmalandMedOarna <- lm(d.log.StorGoteborg ~ d.log.SmalandMedOarna)
coeftest(resid.difflog.StorGoteborg.SmalandMedOarna)
ADF.test(residuals(resid.difflog.StorGoteborg.SmalandMedOarna))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
# StorGöteborg vs SydSverige - Båda är I(1)

resid.difflog.StorGoteborg.SydSverige <- lm(d.log.StorGoteborg ~ d.log.SydSverige)
coeftest(resid.difflog.StorGoteborg.SydSverige)
ADF.test(residuals(resid.difflog.StorGoteborg.SydSverige))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
# StorGöteborg vs VästSverige - Båda är I(1)

resid.difflog.StorGoteborg.VastSverige <- lm(d.log.StorGoteborg ~ d.log.VastSverige)
coeftest(resid.difflog.StorGoteborg.VastSverige)
ADF.test(residuals(resid.difflog.StorGoteborg.VastSverige))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
#StorGöteborg vs NorraMellanSverige - Båda är I(1)

resid.difflog.StorGoteborg.NorraMellanSverige <- lm(d.log.StorGoteborg ~ d.log.NorraMellanSverige)
coeftest(resid.difflog.StorGoteborg.NorraMellanSverige)
ADF.test(residuals(resid.difflog.StorGoteborg.NorraMellanSverige))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
#StorGöteborg vs MellerstaNorrland - Båda är I(1)

resid.difflog.StorGoteborg.MellerstaNorrland <- lm(d.log.StorGoteborg ~ d.log.MellerstaNorrland)
coeftest(resid.difflog.StorGoteborg.MellerstaNorrland)
ADF.test(residuals(resid.difflog.StorGoteborg.MellerstaNorrland))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
#(tau3) -3.05 > -3.43
#(tau2) -3.02 < -2.88
###########################################################################
#StorGöteborg vs ÖvreNorrland - Båda är I(1)

resid.difflog.StorGoteborg.OvreNorrland <- lm(d.log.StorGoteborg ~ d.log.OvreNorrland)
coeftest(resid.difflog.StorGoteborg.OvreNorrland)
ADF.test(residuals(resid.difflog.StorGoteborg.OvreNorrland))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
#                       KLAR StorGöteborg och andra regioner
############################################################################
############################################################################

#StorMalmö vs StorStockholm - Båda är I(1)

resid.difflog.StorMalmo.StorStockholm <- lm(d.log.StorMalmo ~ d.log.StorStockholm)
coeftest(resid.difflog.StorMalmo.StorStockholm)
ADF.test(residuals(resid.difflog.StorMalmo.StorStockholm))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
#StorMalmö vs StorGöteborg - Båda är I(1)

resid.difflog.StorMalmo.StorGoteborg <- lm(d.log.StorMalmo ~ d.log.StorGoteborg)
coeftest(resid.difflog.StorMalmo.StorGoteborg)
ADF.test(residuals(resid.difflog.StorMalmo.StorGoteborg))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
#StorMalmö vs StockholmsLän - Båda är I(1)

resid.difflog.StorMalmo.StockholmsLan <- lm(d.log.StorMalmo ~ d.log.StockholmsLan)
coeftest(resid.difflog.StorMalmo.StockholmsLan)
ADF.test(residuals(resid.difflog.StorMalmo.StockholmsLan))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
#StorMalmö vs ÖstraMellanSverige - Båda är I(1)

resid.difflog.StorMalmo.OstraMellanSverige <- lm(d.log.StorMalmo ~ d.log.OstraMellanSverige)
coeftest(resid.difflog.StorMalmo.OstraMellanSverige)
ADF.test(residuals(resid.difflog.StorMalmo.OstraMellanSverige))
# Återigen, vi förkastar nollhypotesen. För teori och genomgång av EG-ADF testet se ovan.
# och vilka tillämpningar som råder
###########################################################################
#StorMalmö vs SmålandMedÖarna - Båda är I(1)

resid.difflog.StorMalmo.SmalandMedOarna <- lm(d.log.StorMalmo ~ d.log.SmalandMedOarna)
coeftest(resid.difflog.StorMalmo.SmalandMedOarna)
ADF.test(residuals(resid.difflog.StorMalmo.SmalandMedOarna))
###########################################################################
#StorMalmö vs SydSverige - Båda är I(1)

resid.difflog.StorMalmo.SydSverige  <- lm(d.log.StorMalmo ~ d.log.SydSverige )
coeftest(resid.difflog.StorMalmo.SydSverige )
ADF.test(residuals(resid.difflog.StorMalmo.SydSverige ))
###########################################################################
#StorMalmö vs VästSverige - Båda är I(1)

resid.difflog.StorMalmo.VastSverige  <- lm(d.log.StorMalmo ~ d.log.VastSverige )
coeftest(resid.difflog.StorMalmo.VastSverige )
ADF.test(residuals(resid.difflog.StorMalmo.VastSverige ))
###########################################################################
#StorMalmö vs NorraMellanSverige - Båda är I(1)

resid.difflog.StorMalmo.NorraMellanSverige  <- lm(d.log.StorMalmo ~ d.log.NorraMellanSverige )
coeftest(resid.difflog.StorMalmo.NorraMellanSverige )
ADF.test(residuals(resid.difflog.StorMalmo.NorraMellanSverige ))
###########################################################################
#StorMalmö vs MellerstaNorrland - Båda är I(1)

resid.difflog.StorMalmo.MellerstaNorrland <- lm(d.log.StorMalmo ~ d.log.MellerstaNorrland )
coeftest(resid.difflog.StorMalmo.MellerstaNorrland )
ADF.test(residuals(resid.difflog.StorMalmo.MellerstaNorrland ))
###########################################################################
#StorMalmö vs ÖvreNorrland - Båda är I(1)

resid.difflog.StorMalmo.OvreNorrland <- lm(d.log.StorMalmo  ~ d.log.OvreNorrland)
coeftest(resid.difflog.StorMalmo.OvreNorrland )
ADF.test(residuals(resid.difflog.StorMalmo.OvreNorrland ))
###########################################################################
#                       KLAR StorMalmö och andra regioner
############################################################################
############################################################################

#StockholmsLän vs StorStockholm - Båda är I(1)

resid.difflog.StockholmsLan.StorStockholm <- lm(d.log.StockholmsLan  ~ d.log.StorStockholm)
coeftest(resid.difflog.StockholmsLan.StorStockholm )
ADF.test(residuals(resid.difflog.StockholmsLan.StorStockholm ))
###########################################################################
#StockholmsLän vs StorGöteborg - Båda är I(1)

resid.difflog.StockholmsLan.StorGoteborg  <- lm(d.log.StockholmsLan  ~ d.log.StorGoteborg )
coeftest(resid.difflog.StockholmsLan.StorGoteborg  )
ADF.test(residuals(resid.difflog.StockholmsLan.StorGoteborg  ))
###########################################################################
#StockholmsLän vs StorMalmö - Båda är I(1)

resid.difflog.StockholmsLan.StorMalmo  <- lm(d.log.StockholmsLan  ~ d.log.StorMalmo )
coeftest(resid.difflog.StockholmsLan.StorMalmo )
ADF.test(residuals(resid.difflog.StockholmsLan.StorMalmo))
###########################################################################
#StockholmsLän vs ÖstraMellanSverige - Båda är I(1)

resid.difflog.StockholmsLan.StorMalmo  <- lm(d.log.StockholmsLan  ~ d.log.StorMalmo )
coeftest(resid.difflog.StockholmsLan.StorMalmo )
ADF.test(residuals(resid.difflog.StockholmsLan.StorMalmo))
###########################################################################
#StockholmsLän vs SmålandMedÖarna - Båda är I(1)

resid.difflog.StockholmsLan.SmalandMedOarna  <- lm(d.log.StockholmsLan  ~ d.log.SmalandMedOarna )
coeftest(resid.difflog.StockholmsLan.SmalandMedOarna)
ADF.test(residuals(resid.difflog.StockholmsLan.SmalandMedOarna))
###########################################################################
#StockholmsLän vs SydSverige - Båda är I(1)

resid.difflog.StockholmsLan.SydSverige  <- lm(d.log.StockholmsLan  ~ d.log.SydSverige )
coeftest(resid.difflog.StockholmsLan.SydSverige)
ADF.test(residuals(resid.difflog.StockholmsLan.SydSverige))
###########################################################################
#StockholmsLän vs VästSverige - Båda är I(1)

resid.difflog.StockholmsLan.VastSverige  <- lm(d.log.StockholmsLan  ~ d.log.VastSverige )
coeftest(resid.difflog.StockholmsLan.VastSverige)
ADF.test(residuals(resid.difflog.StockholmsLan.VastSverige))
###########################################################################
#StockholmsLän vs VästSverige - Båda är I(1)

resid.difflog.StockholmsLan.NorraMellanSverige  <- lm(d.log.StockholmsLan  ~ d.log.NorraMellanSverige )
coeftest(resid.difflog.StockholmsLan.NorraMellanSverige)
ADF.test(residuals(resid.difflog.StockholmsLan.NorraMellanSverige))
###########################################################################
#StockholmsLän vs MellerstaNorrland - Båda är I(1)

resid.difflog.StockholmsLan.MellerstaNorrland  <- lm(d.log.StockholmsLan  ~ d.log.MellerstaNorrland )
coeftest(resid.difflog.StockholmsLan.MellerstaNorrland)
ADF.test(residuals(resid.difflog.StockholmsLan.MellerstaNorrland))
###########################################################################
#StockholmsLän vs ÖvreNorrland - Båda är I(1)

resid.difflog.StockholmsLan.OvreNorrland   <- lm(d.log.StockholmsLan  ~ d.log.OvreNorrland)
coeftest(resid.difflog.StockholmsLan.OvreNorrland )
ADF.test(residuals(resid.difflog.StockholmsLan.OvreNorrland ))
###########################################################################
#                       KLAR StockholmsLän och andra regioner
############################################################################
############################################################################
#ÖstraMellanSverige vs StorStockholm - Båda är I(1)

resid.difflog.OstraMellanSverige.StorStockholm   <- lm(d.log.OstraMellanSverige  ~ d.log.StorStockholm)
coeftest(resid.difflog.OstraMellanSverige.StorStockholm)
ADF.test(residuals(resid.difflog.OstraMellanSverige.StorStockholm))
###########################################################################
#ÖstraMellanSverige vs StorGöteborg - Båda är I(1)

resid.difflog.OstraMellanSverige.StorGoteborg   <- lm(d.log.OstraMellanSverige  ~ d.log.StorGoteborg)
coeftest(resid.difflog.OstraMellanSverige.StorGoteborg)
ADF.test(residuals(resid.difflog.OstraMellanSverige.StorGoteborg))
###########################################################################
#ÖstraMellanSverige vs StorMalmö - Båda är I(1)

resid.difflog.OstraMellanSverige.StorMalmo   <- lm(d.log.OstraMellanSverige  ~ d.log.StorMalmo)
coeftest(resid.difflog.OstraMellanSverige.StorMalmo)
ADF.test(residuals(resid.difflog.OstraMellanSverige.StorMalmo))
###########################################################################
#ÖstraMellanSverige vs StockholmsLän - Båda är I(1)

resid.difflog.OstraMellanSverige.StockholmsLan  <- lm(d.log.OstraMellanSverige  ~ d.log.StockholmsLan)
coeftest(resid.difflog.OstraMellanSverige.StockholmsLan)
ADF.test(residuals(resid.difflog.OstraMellanSverige.StockholmsLan))
###########################################################################
#ÖstraMellanSverige vs SmålandMedÖarna - Båda är I(1)

resid.difflog.OstraMellanSverige.SmalandMedOarna  <- lm(d.log.OstraMellanSverige  ~ d.log.SmalandMedOarna)
coeftest(resid.difflog.OstraMellanSverige.SmalandMedOarna)
ADF.test(residuals(resid.difflog.OstraMellanSverige.SmalandMedOarna))
###########################################################################
#ÖstraMellanSverige vs SydSverige - Båda är I(1)

resid.difflog.OstraMellanSverige.SydSverige  <- lm(d.log.OstraMellanSverige  ~ d.log.SydSverige)
coeftest(resid.difflog.OstraMellanSverige.SydSverige)
ADF.test(residuals(resid.difflog.OstraMellanSverige.SydSverige))
###########################################################################
#ÖstraMellanSverige vs VästSverige - Båda är I(1)

resid.difflog.OstraMellanSverige.VastSverige  <- lm(d.log.OstraMellanSverige  ~ d.log.VastSverige)
coeftest(resid.difflog.OstraMellanSverige.VastSverige)
ADF.test(residuals(resid.difflog.OstraMellanSverige.VastSverige))
###########################################################################
#ÖstraMellanSverige vs NorraMellanSverige - Båda är I(1)

resid.difflog.OstraMellanSverige.NorraMellanSverige  <- lm(d.log.OstraMellanSverige  ~ d.log.NorraMellanSverige)
coeftest(resid.difflog.OstraMellanSverige.NorraMellanSverige)
ADF.test(residuals(resid.difflog.OstraMellanSverige.NorraMellanSverige))
###########################################################################
#ÖstraMellanSverige vs MellerstaNorrland - Båda är I(1)

resid.difflog.OstraMellanSverige.MellerstaNorrland <- lm(d.log.OstraMellanSverige  ~ d.log.MellerstaNorrland)
coeftest(resid.difflog.OstraMellanSverige.MellerstaNorrland)
ADF.test(residuals(resid.difflog.OstraMellanSverige.MellerstaNorrland))
###########################################################################
#ÖstraMellanSverige vs ÖvreNorrland - Båda är I(1)

resid.difflog.OstraMellanSverige.OvreNorrland <- lm(d.log.OstraMellanSverige  ~ d.log.OvreNorrland)
coeftest(resid.difflog.OstraMellanSverige.OvreNorrland)
ADF.test(residuals(resid.difflog.OstraMellanSverige.OvreNorrland))
###########################################################################

#                       KLAR ÖstraMellanSverige och andra regioner

############################################################################
############################################################################
#SmålandMedÖarna vs StorStockholm - Båda är I(1)
resid.difflog.SmalandMedOarna.StorStockholm <- lm(d.log.SmalandMedOarna  ~ d.log.StorStockholm)
coeftest(resid.difflog.SmalandMedOarna.StorStockholm)
ADF.test(residuals(resid.difflog.SmalandMedOarna.StorStockholm))
###########################################################################
#SmålandMedÖarna vs StorGöteborg - Båda är I(1)
resid.difflog.SmalandMedOarna.StorGoteborg <- lm(d.log.SmalandMedOarna  ~ d.log.StorGoteborg)
coeftest(resid.difflog.SmalandMedOarna.StorGoteborg)
ADF.test(residuals(resid.difflog.SmalandMedOarna.StorGoteborg))
###########################################################################

#SmålandMedÖarna vs StorMalmö - Båda är I(1)
resid.difflog.SmalandMedOarna.StorMalmo <- lm(d.log.SmalandMedOarna  ~ d.log.StorMalmo)
coeftest(resid.difflog.SmalandMedOarna.StorMalmo)
ADF.test(residuals(resid.difflog.SmalandMedOarna.StorMalmo))
###########################################################################

#SmålandMedÖarna vs StockholmsLän - Båda är I(1)
resid.difflog.SmalandMedOarna.StockholmsLan  <- lm(d.log.SmalandMedOarna  ~ d.log.StockholmsLan )
coeftest(resid.difflog.SmalandMedOarna.StockholmsLan )
ADF.test(residuals(resid.difflog.SmalandMedOarna.StockholmsLan ))
###########################################################################
#SmålandMedÖarna vs ÖstraMellanSverige - Båda är I(1)
resid.difflog.SmalandMedOarna.OstraMellanSverige  <- lm(d.log.SmalandMedOarna  ~ d.log.OstraMellanSverige )
coeftest(resid.difflog.SmalandMedOarna.OstraMellanSverige )
ADF.test(residuals(resid.difflog.SmalandMedOarna.OstraMellanSverige ))
###########################################################################
#SmålandMedÖarna vs ÖstraMellanSverige - Båda är I(1)
resid.difflog.SmalandMedOarna.OstraMellanSverige  <- lm(d.log.SmalandMedOarna  ~ d.log.OstraMellanSverige )
coeftest(resid.difflog.SmalandMedOarna.OstraMellanSverige  )
ADF.test(residuals(resid.difflog.SmalandMedOarna.OstraMellanSverige  ))
###########################################################################
#SmålandMedÖarna vs SydSverige - Båda är I(1)
resid.difflog.SmalandMedOarna.SydSverige   <- lm(d.log.SmalandMedOarna  ~ d.log.SydSverige)
coeftest(resid.difflog.SmalandMedOarna.SydSverige)
ADF.test(residuals(resid.difflog.SmalandMedOarna.SydSverige))
###########################################################################
#SmålandMedÖarna vs SydSverige - Båda är I(1)
resid.difflog.SmalandMedarna.VastSverige   <- lm(d.log.SmalandMedOarna  ~ d.log.VastSverige)
coeftest(resid.difflog.SmalandMedarna.VastSverige)
ADF.test(residuals(resid.difflog.SmalandMedarna.VastSverige))
###########################################################################
#SmålandMedÖarna vs NorraMellanSverige - Båda är I(1)
resid.difflog.SmalandMedOarna.NorraMellanSverige    <- lm(d.log.SmalandMedOarna  ~ d.log.NorraMellanSverige )
coeftest(resid.difflog.SmalandMedOarna.NorraMellanSverige )
ADF.test(residuals(resid.difflog.SmalandMedOarna.NorraMellanSverige ))
###########################################################################
#SmålandMedÖarna vs MellerstaNorrland - Båda är I(1)
resid.difflog.SmalandMedOarna.MellerstaNorrland     <- lm(d.log.SmalandMedOarna  ~ d.log.MellerstaNorrland)
coeftest(resid.difflog.SmalandMedOarna.MellerstaNorrland  )
ADF.test(residuals(resid.difflog.SmalandMedOarna.MellerstaNorrland ))
###########################################################################
resid.difflog.SmalandMedOarna.OvreNorrland <- lm(d.log.SmalandMedOarna  ~ d.log.OvreNorrland )
coeftest(resid.difflog.SmalandMedOarna.OvreNorrland)
ADF.test(residuals(resid.difflog.SmalandMedOarna.OvreNorrland))
############################################################################

#                       KLAR ÖstraMellanSverige och andra regioner

############################################################################
############################################################################
#SydSverige vs StorStockholm - Båda är I(1)
resid.difflog.SydSverige.StorStockholm <- lm(d.log.SydSverige  ~ d.log.StorStockholm)
coeftest(resid.difflog.SydSverige.StorStockholm)
ADF.test(residuals(resid.difflog.SydSverige.StorStockholm))
###########################################################################
#SydSverige vs StorGoteborg - Båda är I(1)
resid.difflog.SydSverige.StorGoteborg <- lm(d.log.SydSverige  ~ d.log.StorGoteborg)
coeftest(resid.difflog.SydSverige.StorGoteborg )
ADF.test(residuals(resid.difflog.SydSverige.StorGoteborg))
###########################################################################
#SydSverige vs StorMalmö - Båda är I(1)
resid.difflog.SydSverige.StorMalmo <- lm(d.log.SydSverige  ~ d.log.StorMalmo)
coeftest(resid.difflog.SydSverige.StorMalmo)
ADF.test(residuals(resid.difflog.SydSverige.StorMalmo))
###########################################################################
#SydSverige vs StockholmsLän - Båda är I(1)
resid.difflog.SydSverige.StockholmsLan <- lm(d.log.SydSverige  ~ d.log.StockholmsLan)
coeftest(resid.difflog.SydSverige.StockholmsLan)
ADF.test(residuals(resid.difflog.SydSverige.StockholmsLan))
###########################################################################
#SydSverige vs ÖstraMellanSverige - Båda är I(1)
resid.difflog.SydSverige.OstraMellanSverige <- lm(d.log.SydSverige  ~ d.log.OstraMellanSverige)
coeftest(resid.difflog.SydSverige.OstraMellanSverige)
ADF.test(residuals(resid.difflog.SydSverige.OstraMellanSverige))
###########################################################################
#SydSverige vs SmålandMedÖarna - Båda är I(1)
resid.difflog.SydSverige.SmalandMedOarna <- lm(d.log.SydSverige  ~ d.log.SmalandMedOarna)
coeftest(resid.difflog.SydSverige.SmalandMedOarna)
ADF.test(residuals(resid.difflog.SydSverige.SmalandMedOarna))
###########################################################################
#SydSverige vs VästSverige - Båda är I(1)
resid.difflog.SydSverige.VastSverige <- lm(d.log.SydSverige  ~ d.log.VastSverige)
coeftest(resid.difflog.SydSverige.VastSverige)
ADF.test(residuals(resid.difflog.SydSverige.VastSverige))
###########################################################################
#SydSverige vs NorraMellanSverige - Båda är I(1)
resid.difflog.SydSverige.NorraMellanSverige <- lm(d.log.SydSverige  ~ d.log.NorraMellanSverige)
coeftest(resid.difflog.SydSverige.NorraMellanSverige)
ADF.test(residuals(resid.difflog.SydSverige.NorraMellanSverige ))
###########################################################################
#SydSverige vs MellerstaNorrland - Båda är I(1)
resid.difflog.SydSverige.MellerstaNorrland <- lm(d.log.SydSverige  ~ d.log.MellerstaNorrland )
coeftest(resid.difflog.SydSverige.MellerstaNorrland )
ADF.test(residuals(resid.difflog.SydSverige.MellerstaNorrland))
###########################################################################
#SydSverige vs ÖvreNorrland - Båda är I(1)
resid.difflog.SydSverige.OvreNorrland <- lm(d.log.SydSverige  ~ d.log.OvreNorrland)
coeftest(resid.difflog.SydSverige.OvreNorrland)
ADF.test(residuals(resid.difflog.SydSverige.OvreNorrland))
###########################################################################

#                       KLAR SydSverige och andra regioner

############################################################################
############################################################################
#VästSverige vs StorStockholm - Båda är I(1)
resid.difflog.VastSverige.StorStockholm <- lm(d.log.VastSverige  ~ d.log.StorStockholm)
coeftest(resid.difflog.VastSverige.StorStockholm)
ADF.test(residuals(resid.difflog.VastSverige.StorStockholm))
###########################################################################
#VästSverige vs StorGöteborg - Båda är I(1)
resid.difflog.VastSverige.StorGoteborg <- lm(d.log.VastSverige  ~ d.log.StorGoteborg)
coeftest(resid.difflog.VastSverige.StorGoteborg)
ADF.test(residuals(resid.difflog.VastSverige.StorGoteborg))
###########################################################################
#VästSverige vs StorMalmö - Båda är I(1)
resid.difflog.VastSverige.StorMalmo  <- lm(d.log.VastSverige  ~ d.log.StorMalmo)
coeftest(resid.difflog.VastSverige.StorMalmo )
ADF.test(residuals(resid.difflog.VastSverige.StorMalmo))
###########################################################################
#VästSverige vs StorMalmö - Båda är I(1)
resid.difflog.VastSverige.StockholmsLan  <- lm(d.log.VastSverige  ~ d.log.StockholmsLan)
coeftest(resid.difflog.VastSverige.StockholmsLan)
ADF.test(residuals(resid.difflog.VastSverige.StockholmsLan))
###########################################################################
#VästSverige vs ÖstraMellanSverige - Båda är I(1)
resid.difflog.VastSverige.OstraMellanSverige  <- lm(d.log.VastSverige  ~ d.log.OstraMellanSverige)
coeftest(resid.difflog.VastSverige.OstraMellanSverige)
ADF.test(residuals(resid.difflog.VastSverige.OstraMellanSverige))
###########################################################################
#VästSverige vs SmålandMedÖarna - Båda är I(1)
resid.difflog.VastSverige.SmalandMedOarna  <- lm(d.log.VastSverige  ~ d.log.SmalandMedOarna)
coeftest(resid.difflog.VastSverige.SmalandMedOarna)
ADF.test(residuals(resid.difflog.VastSverige.SmalandMedOarna))
###########################################################################
#VästSverige vs SmålandMedÖarna - Båda är I(1)
resid.difflog.VastSverige.SydSverige  <- lm(d.log.VastSverige  ~ d.log.SydSverige)
coeftest(resid.difflog.VastSverige.SydSverige)
ADF.test(residuals(resid.difflog.VastSverige.SydSverige))
###########################################################################
#VästSverige vs NorraMellanSverige - Båda är I(1)
resid.difflog.VastSverige.NorraMellanSverige <- lm(d.log.VastSverige  ~ d.log.NorraMellanSverige)
coeftest(resid.difflog.VastSverige.NorraMellanSverige)
ADF.test(residuals(resid.difflog.VastSverige.NorraMellanSverige))
###########################################################################
#VästSverige vs MellerstaNorrland - Båda är I(1)
resid.difflog.VastSverige.MellerstaNorrland <- lm(d.log.VastSverige  ~ d.log.MellerstaNorrland)
coeftest(resid.difflog.VastSverige.MellerstaNorrland)
ADF.test(residuals(resid.difflog.VastSverige.MellerstaNorrland))
###########################################################################
#VästSverige vs ÖvreNorrland - Båda är I(1)
resid.difflog.VastSverige.OvreNorrland <- lm(d.log.VastSverige  ~ d.log.OvreNorrland)
coeftest(resid.difflog.VastSverige.OvreNorrland)
ADF.test(residuals(resid.difflog.VastSverige.OvreNorrland))
###########################################################################

#                       KLAR VästSverige och andra regioner

############################################################################
############################################################################
#NorraMellanSverige vs StorStockholm - Båda är I(1)
resid.difflog.NorraMellanSverige.StorStockholm <- lm(d.log.NorraMellanSverige  ~ d.log.StorStockholm)
coeftest(resid.difflog.NorraMellanSverige.StorStockholm)
ADF.test(residuals(resid.difflog.NorraMellanSverige.StorStockholm))
###########################################################################
#NorraMellanSverige vs StorGöteborg - Båda är I(1)
resid.difflog.NorraMellanSverige.StorGoteborg<- lm(d.log.NorraMellanSverige  ~ d.log.StorGoteborg)
coeftest(resid.difflog.NorraMellanSverige.StorGoteborg)
ADF.test(residuals(resid.difflog.NorraMellanSverige.StorGoteborg))
###########################################################################
#NorraMellanSverige vs StorMalmö - Båda är I(1)
resid.difflog.NorraMellanSverige.StorMalmo <- lm(d.log.NorraMellanSverige  ~ d.log.StorMalmo)
coeftest(resid.difflog.NorraMellanSverige.StorMalmo)
ADF.test(residuals(resid.difflog.NorraMellanSverige.StorMalmo))
###########################################################################
#NorraMellanSverige vs StockholmsLän - Båda är I(1)
resid.difflog.NorraMellanSverige.StockholmsLan <- lm(d.log.NorraMellanSverige  ~ d.log.StockholmsLan)
coeftest(resid.difflog.NorraMellanSverige.StockholmsLan)
ADF.test(residuals(resid.difflog.NorraMellanSverige.StockholmsLan))
###########################################################################
#NorraMellanSverige vs ÖstraMellanSverige - Båda är I(1)
resid.difflog.NorraMellanSverige.OstraMellanSverige <- lm(d.log.NorraMellanSverige  ~ d.log.OstraMellanSverige)
coeftest(resid.difflog.NorraMellanSverige.OstraMellanSverige)
ADF.test(residuals(resid.difflog.NorraMellanSverige.OstraMellanSverige))
###########################################################################
#NorraMellanSverige vs SmålandMedÖarna - Båda är I(1)
resid.difflog.NorraMellanSverige.SmalandMedOarna <- lm(d.log.NorraMellanSverige  ~ d.log.SmalandMedOarna)
coeftest(resid.difflog.NorraMellanSverige.SmalandMedOarna)
ADF.test(residuals(resid.difflog.NorraMellanSverige.SmalandMedOarna))
###########################################################################
#NorraMellanSverige vs VästSverige - Båda är I(1)
resid.difflog.NorraMellanSverige.VastSverige <- lm(d.log.NorraMellanSverige  ~ d.log.VastSverige)
coeftest(resid.difflog.NorraMellanSverige.VastSverige)
ADF.test(residuals(resid.difflog.NorraMellanSverige.VastSverige))
###########################################################################
#NorraMellanSverige vs MellerstaNorrland - Båda är I(1)
resid.difflog.NorraMellanSverige.MellerstaNorrland <- lm(d.log.NorraMellanSverige  ~ d.log.MellerstaNorrland)
coeftest(resid.difflog.NorraMellanSverige.MellerstaNorrland)
ADF.test(residuals(resid.difflog.NorraMellanSverige.MellerstaNorrland))
###########################################################################
#NorraMellanSverige vs ÖvreNorrland - Båda är I(1)
resid.difflog.NorraMellanSverige.OvreNorrland <- lm(d.log.NorraMellanSverige  ~ d.log.OvreNorrland)
coeftest(resid.difflog.NorraMellanSverige.OvreNorrland)
ADF.test(residuals(resid.difflog.NorraMellanSverige.OvreNorrland))
###########################################################################

#                       KLAR NorraMellanSverige med alla andra regioner

############################################################################
############################################################################
#MellerstaNorrland vs StorStockholm - Båda är I(1)
resid.difflog.MellerstaNorrland.StorStockholm <- lm(d.log.MellerstaNorrland  ~ d.log.StorStockholm)
coeftest(resid.difflog.MellerstaNorrland.StorStockholm)
ADF.test(residuals(resid.difflog.MellerstaNorrland.StorStockholm))
###########################################################################
#MellerstaNorrland vs StorGöteborg - Båda är I(1)
resid.difflog.MellerstaNorrland.StorGoteborg  <- lm(d.log.MellerstaNorrland  ~ d.log.StorGoteborg )
coeftest(resid.difflog.MellerstaNorrland.StorGoteborg )
ADF.test(residuals(resid.difflog.MellerstaNorrland.StorGoteborg ))
###########################################################################
#MellerstaNorrland vs StorMalmö - Båda är I(1)
resid.difflog.MellerstaNorrland.StorMalmo <- lm(d.log.MellerstaNorrland  ~ d.log.StorMalmo )
coeftest(resid.difflog.MellerstaNorrland.StorNalmo)
ADF.test(residuals(resid.difflog.MellerstaNorrland.StorMalmo))
###########################################################################
#MellerstaNorrland vs StockholmsLän - Båda är I(1)
resid.difflog.MellerstaNorrland.StockholmsLan <- lm(d.log.MellerstaNorrland  ~ d.log.StockholmsLan)
coeftest(resid.difflog.MellerstaNorrland.StockholmsLan)
ADF.test(residuals(resid.difflog.MellerstaNorrland.StockholmsLan))
###########################################################################
#MellerstaNorrland vs ÖstraMellanSverige - Båda är I(1)
resid.difflog.MellerstaNorrland.OstraMellanSverige <- lm(d.log.MellerstaNorrland  ~ d.log.OstraMellanSverige)
coeftest(resid.difflog.MellerstaNorrland.OstraMellanSverige)
ADF.test(residuals(resid.difflog.MellerstaNorrland.OstraMellanSverige))
###########################################################################
#MellerstaNorrland vs SmålandMedÖarna - Båda är I(1)
resid.difflog.MellerstaNorrland.SmalandMedOarna <- lm(d.log.MellerstaNorrland  ~ d.log.SmalandMedOarna)
coeftest(resid.difflog.MellerstaNorrland.SmalandMedOarna)
ADF.test(residuals(resid.difflog.MellerstaNorrland.SmalandMedOarna))
###########################################################################
#MellerstaNorrland vs SydSverige - Båda är I(1)
resid.difflog.MellerstaNorrland.SydSverige <- lm(d.log.MellerstaNorrland  ~ d.log.SydSverige)
coeftest(resid.difflog.MellerstaNorrland.SydSverige)
ADF.test(residuals(resid.difflog.MellerstaNorrland.SydSverige))
###########################################################################
#MellerstaNorrland vs VästSverige - Båda är I(1)
resid.difflog.MellerstaNorrland.VastSverige <- lm(d.log.MellerstaNorrland  ~ d.log.VastSverige)
coeftest(resid.difflog.MellerstaNorrland.VastSverige)
ADF.test(residuals(resid.difflog.MellerstaNorrland.VastSverige))
###########################################################################
#MellerstaNorrland vs NorraMellanSverige - Båda är I(1)
resid.difflog.MellerstaNorrland.NorraMellanSverige <- lm(d.log.MellerstaNorrland  ~ d.log.NorraMellanSverige)
coeftest(resid.difflog.MellerstaNorrland.NorraMellanSverige)
ADF.test(residuals(resid.difflog.MellerstaNorrland.NorraMellanSverige))
###########################################################################
#MellerstaNorrland vs ÖvreNorrland - Båda är I(1)
resid.difflog.MellerstaNorrland.OvreNorrland <- lm(d.log.MellerstaNorrland  ~ d.log.OvreNorrland)
coeftest(resid.difflog.MellerstaNorrland.OvreNorrland)
ADF.test(residuals(resid.difflog.MellerstaNorrland.OvreNorrland))
###########################################################################

#                       KLAR MellerstaNorrland med alla andra regioner

############################################################################
############################################################################
#ÖvreNorrland vs StorStockholm - Båda är I(1)
resid.difflog.OvreNorrland.StorStockholm  <- lm(d.log.OvreNorrland  ~ d.log.StorStockholm )
coeftest(resid.difflog.OvreNorrland.StorStockholm )
ADF.test(residuals(resid.difflog.OvreNorrland.StorStockholm ))
###########################################################################
#ÖvreNorrland vs StorGöteborg - Båda är I(1)
resid.difflog.OvreNorrland.StorGoteborg  <- lm(d.log.OvreNorrland  ~ d.log.StorGoteborg )
coeftest(resid.difflog.OvreNorrland.StorGoteborg )
ADF.test(residuals(resid.difflog.OvreNorrland.StorGoteborg ))
###########################################################################
#ÖvreNorrland vs StorMalmö - Båda är I(1)
resid.difflog.OvreNorrland.StorMalmo  <- lm(d.log.OvreNorrland  ~ d.log.StorMalmo)
coeftest(resid.difflog.OvreNorrland.StorMalmo)
ADF.test(residuals(resid.difflog.OvreNorrland.StorMalmo))
###########################################################################
#ÖvreNorrland vs StockholmsLän - Båda är I(1)
resid.difflog.OvreNorrland.StockholmsLan  <- lm(d.log.OvreNorrland  ~ d.log.StockholmsLan)
coeftest(resid.difflog.OvreNorrland.StockholmsLan)
ADF.test(residuals(resid.difflog.OvreNorrland.StockholmsLan))
###########################################################################
#ÖvreNorrland vs ÖstraMellanSverige - Båda är I(1)
resid.difflog.OvreNorrland.OstraMellanSverige  <- lm(d.log.OvreNorrland  ~ d.log.OstraMellanSverige)
coeftest(resid.difflog.OvreNorrland.OstraMellanSverige)
ADF.test(residuals(resid.difflog.OvreNorrland.OstraMellanSverige))
###########################################################################
#ÖvreNorrland vs SmålandMedÖarna - Båda är I(1)
resid.difflog.OvreNorrland.SmalandMedOarna  <- lm(d.log.OvreNorrland  ~ d.log.SmalandMedOarna)
coeftest(resid.difflog.OvreNorrland.SmalandMedOarna)
ADF.test(residuals(resid.difflog.OvreNorrland.SmalandMedOarna))
###########################################################################
#ÖvreNorrland vs SydSverige - Båda är I(1)
resid.difflog.OvreNorrland.SydSverige   <- lm(d.log.OvreNorrland  ~ d.log.SydSverige)
coeftest(resid.difflog.OvreNorrland.SydSverige)
ADF.test(residuals(resid.difflog.OvreNorrland.SydSverige))
###########################################################################
#ÖvreNorrland vs VästSverige - Båda är I(1)
resid.difflog.OvreNorrland.VastSverige <- lm(d.log.OvreNorrland  ~ d.log.VastSverige)
coeftest(resid.difflog.OvreNorrland.VastSverige)
ADF.test(residuals(resid.difflog.OvreNorrland.VastSverige))
###########################################################################
#ÖvreNorrland vs NorraMellanSverige - Båda är I(1)
resid.difflog.OvreNorrland.NorraMellanSverige <- lm(d.log.OvreNorrland  ~ d.log.NorraMellanSverige)
coeftest(resid.difflog.OvreNorrland.NorraMellanSverige)
ADF.test(residuals(resid.difflog.OvreNorrland.NorraMellanSverige))
###########################################################################
#ÖvreNorrland vs NorraMellanSverige - Båda är I(1)
resid.difflog.OvreNorrland.NorraMellanSverige <- lm(d.log.OvreNorrland  ~ d.log.NorraMellanSverige)
coeftest(resid.difflog.OvreNorrland.NorraMellanSverige)
ADF.test(residuals(resid.difflog.OvreNorrland.NorraMellanSverige))
###########################################################################
#ÖvreNorrland vs MellerstaNorrland - Båda är I(1)
resid.difflog.OvreNorrland.MellerstaNorrland <- lm(d.log.OvreNorrland  ~ d.log.MellerstaNorrland)
coeftest(resid.difflog.OvreNorrland.MellerstaNorrland)
ADF.test(residuals(resid.difflog.OvreNorrland.MellerstaNorrland))
###########################################################################

#                       KLAR MellerstaNorrland med alla andra regioner

############################################################################
############################################################################


#################             #################
#################   Steg 4:2  #################
#################             #################

# GRANGER-KAUSALITET OCH VAR



############################################################################
# # Bygger bivariat VAR; StorStockholm vs StorGöteborg och dess GC-test
############################################################################


var.data.StorStockholm.StorGoteborg <- cbind(window(d.log.StorStockholm, start = c(1986,2)),
                                             window(d.log.StorGoteborg, start = c(1986,2)))

dimnames(var.data.StorStockholm.StorGoteborg)[[2]] <- c("StorStockholm","StorGoteborg")
var.data.StorStockholm.StorGoteborg

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorStockholm.StorGoteborg, type ="const") # SC=1 men kommer behövas justeras upp beroende på diagnostiken av residualerna.

var.result.StorStockholm.StorGoteborg <- VAR(var.data.StorStockholm.StorGoteborg, p = 4, type = "const") # där p=2 är lags
var.result.StorStockholm.StorGoteborg # SÄGER INGET!


# Nollhyptes: autokorrelation är noll
# Alternativ autokorrelation är inte noll

# Autokorrelationstest av residualerna
# När p=2 så hade vi autokorrelation bland residualerna. 
# För att bli av med autokorrelation, höjde vi därav lags till p=3 och till p=4. Både 3 och 4 funkar bra. Ger bra diagnostik.

serial.test(var.result.StorStockholm.StorGoteborg) 
# eftersom att P=0.401 när p=4 så kan vi ej förkasta Nollhypotesen om att autokorrelationen är lika med noll.
# Då 0.401 > 0.05 kan vi ej förkasta H0.

# Nollhyptes: Residualerna är normala
# Alternativ Hypotes: Residualerna är ej normala.

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorStockholm.StorGoteborg)$jb.mul$JB

# Ifall P-värde överstiger 0.05 så residualerna normalfördelade.
# 0.9801 > 0.05. Vi kan ej förkasta H0. Så residualer är normala.

roots(var.result.StorStockholm.StorGoteborg) # -1 TILL 1 intervall.

# Summa: Vi har väluppfostrade residualer.

# "var.result.StorStockholm.StorGöteborg" som objekt/modell består av StorSthlm och StorGöteborg.
# GC-test mellan StorGBG och StorSthlm

# Granger causality 
# H0: StorStockholm do not Granger-cause StorGöteborg
# HA: StorStockholm does Granger-cause StorGöteborg

causality(var.result.StorStockholm.StorGoteborg, cause = "StorStockholm")$Granger
causality(var.result.StorStockholm.StorGoteborg, cause = "StorGoteborg")$Granger

# REGEL: När P-värde understiger H0 kan vi förkasta H0.
# När P-värde överstiger H0 kan vi EJ förkasta H0!

# Slutsats: StorStockholm GC-orsakar StorGBG MEN inte tvärtom dvs StorGBG GC-orsakar inte StorSthlm.

############################################################################
# # Bygger bivariat VAR; StorStockholm vs StorMalmo
############################################################################

var.data.StorStockholm.StorMalmo <- cbind(window(d.log.StorStockholm, start = c(1986,2)),
                                          window(d.log.StorMalmo, start = c(1986,2)))

dimnames(var.data.StorStockholm.StorMalmo )[[2]] <- c("StorStockholm","StorMalmo")
var.data.StorStockholm.StorMalmo  

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorStockholm.StorMalmo , type ="const") # SC=1 säger fortsatt 1 men AIC säger 5. Vi följer SC=1

var.result.StorStockholm.StorMalmo  <- VAR(var.data.StorStockholm.StorMalmo , p = 4, type = "const") # där p=2 är lags
var.result.StorStockholm.StorMalmo  


# Nollhyptes: autokorrelation är noll
# Alternativ autokorrelation är inte noll

# Autokorrelationstest av residualerna
# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3.
# Även p=4 funkar bra.

serial.test(var.result.StorStockholm.StorMalmo ) 
# eftersom att P=0.3644 när p=4 så kan vi ej förkasta Nollhypotesen om att autokorrelationen är lika med noll.


# Nollhyptes: Residualerna är normala
# Alternativ Hypotes: Residualerna är ej normala.

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorStockholm.StorMalmo )$jb.mul$JB

# Ifall P-värde överstiger 0.05 så residualerna normalfördelade.
# 0.7901 > 0.05. Vi kan ej förkasta H0. Så residualer är normala.

roots(var.result.StorStockholm.StorMalmo) # -1 TILL 1 intervall. STABIL

# Summa: Vi har väluppfostrade residualer.

# "var.result.StorStockholm.StorMalmö" som objekt/modell består av StorSthlm och StorMalmö.
# GC-test mellan StorMalmö och StorSthlm

# Granger causality 
# H0: StorStockholm do not Granger-cause StorMalmö
# HA: StorStockholm does Granger-cause StorMalmö

causality(var.result.StorStockholm.StorMalmo, cause = "StorStockholm")$Granger # LÅGT p-värde; 1.529e-06
causality(var.result.StorStockholm.StorMalmo, cause = "StorMalmo")$Granger # Högt p-värde; 0.5322

# Slutsats: StorStockholm GC-orsakar StorMalmö MEN inte tvärtom dvs StorMalmö GC-orsakar inte StorSthlm.

############################################################################
# # Bygger bivariat VAR; StorStockholm vs StockholmsLän
############################################################################

var.data.StorStockholm.StockholmsLan <- cbind(window(d.log.StorStockholm, start = c(1986,2)),
                                              window(d.log.StockholmsLan, start = c(1986,2)))

dimnames(var.data.StorStockholm.StockholmsLan )[[2]] <- c("StorStockholm","StockholmsLan")
var.data.StorStockholm.StockholmsLan 

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorStockholm.StockholmsLan , type ="const") # SC=1 säger fortsatt 1 men AIC säger 5. Vi följer SC=1

var.result.StorStockholm.StockholmsLan <- VAR(var.data.StorStockholm.StockholmsLan, p = 4, type = "const") # där p=4 är lags
var.result.StorStockholm.StockholmsLan 


# Nollhyptes: autokorrelation är noll
# Alternativ autokorrelation är inte noll

# Autokorrelationstest av residualerna
# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3, funkar ej!
# BAra p=4 funkar bra.

serial.test(var.result.StorStockholm.StockholmsLan) 
# eftersom att 0.1026 när p=4 så kan vi ej förkasta Nollhypotesen om att autokorrelationen är lika med noll.


# Nollhyptes: Residualerna är normala
# Alternativ Hypotes: Residualerna är ej normala.

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorStockholm.StockholmsLan)$jb.mul$JB

# Ifall P-värde överstiger 0.05 så residualerna normalfördelade.
# 2.2e-16 < 0.05. Vi kan förkasta H0. Så residualer är ICKE normala.
#  OBS OBS! Här har vi problem!

roots(var.result.StorStockholm.StockholmsLan) # -1 TILL 1 intervall. STABIL

# Summa: Vi har väluppfostrade residualer.

# "var.result.StorStockholm.StockholmsLän" som objekt/modell består av StorSthlm och StockholmsLän.
# GC-test mellan StockholmsLän och StorSthlm

# Granger causality 
# H0: StorStockholm do not Granger-cause StockholmsLän
# HA: StorStockholm does Granger-cause StockholmsLän

causality(var.result.StorStockholm.StockholmsLan, cause = "StockholmsLan")$Granger # LÅGT p-värde; 3.14e-05
causality(var.result.StorStockholm.StockholmsLan, cause = "StockholmsLan")$Granger # Högt p-värde;  0.1126

# Slutsats: StorStocholm GC orsakar StockholmsLän men ej tvärtom.(StockholmsLän orsakar ej StorStockholm)

############################################################################
# # Bygger bivariat VAR; StorStockholm vs ÖstraMellanSverige
############################################################################

var.data.StorStockholm.OstraMellanSverige <- cbind(window(d.log.StorStockholm, start = c(1986,2)),
                                                   window(d.log.OstraMellanSverige, start = c(1986,2)))

dimnames(var.data.StorStockholm.OstraMellanSverige )[[2]] <- c("StorStockholm","OstraMellanSverige")
var.data.StorStockholm.OstraMellanSverige

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorStockholm.OstraMellanSverige , type ="const") # SC=1 säger fortsatt 1 men AIC säger 5. Vi följer SC=1

var.result.StorStockholm.OstraMellanSverige <- VAR(var.data.StorStockholm.OstraMellanSverige, p = 4, type = "const") # där p=4 är lags
var.result.StorStockholm.OstraMellanSverige

# Nollhyptes: autokorrelation är noll
# Alternativ autokorrelation är inte noll

# Autokorrelationstest av residualerna
# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3, funkar ej!
# BAra p=4 funkar bra.

serial.test(var.result.StorStockholm.OstraMellanSverige) 
# eftersom att 0.4436 när p=4 så kan vi ej förkasta Nollhypotesen om att autokorrelationen är lika med noll.
# Vid p=3 så är värdet 0.01365 < 0.05 provar med 4lags.
# Vid 4 lags 0.4436 > 0.05. OK!

# Nollhyptes: Residualerna är normala
# Alternativ Hypotes: Residualerna är ej normala.

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorStockholm.OstraMellanSverige)$jb.mul$JB

# Ifall P-värde överstiger 0.05 så residualerna normalfördelade.
# 0.7656 > 0.05. Vi kan ej förkasta H0. Så residualer är normala.

roots(var.result.StorStockholm.OstraMellanSverige) # -1 TILL 1 intervall. STABIL

# Summa: Vi har väluppfostrade residualer.

# "var.result.StorStockholm.ÖstraMellanSverige" som objekt/modell består av StorSthlm och ÖstraMellanSverige.
# GC-test mellan ÖstraMellanSverige och StorSthlm

# Granger causality 
# H0: StorStockholm do not Granger-cause ÖstraMellanSverige
# HA: StorStockholm does Granger-cause ÖstraMellanSverige

causality(var.result.StorStockholm.OstraMellanSverige, cause = "StorStockholm")$Granger       # LÅGT p-värde; 0.000271
causality(var.result.StorStockholm.OstraMellanSverige, cause = "OstraMellanSverige")$Granger  # Högt p-värde;  0.4496

# Slutsats: StorStockholm GC orsakar ÖstraMellanSverige dock ej tvärtom.

############################################################################
# # Bygger bivariat VAR; StorStockholm vs SmålandMedÖarna
############################################################################

var.data.StorStockholm.SmalandMedOarna <- cbind(window(d.log.StorStockholm, start = c(1986,2)),
                                                window(d.log.SmalandMedOarna, start = c(1986,2)))

dimnames(var.data.StorStockholm.SmalandMedOarna )[[2]] <- c("StorStockholm","SmalandMedOarna")
var.data.StorStockholm.SmalandMedOarna

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorStockholm.SmalandMedOarna , type ="const") # SC=1 säger fortsatt 1 men AIC säger 8. Vi följer SC=1

var.result.StorStockholm.SmalandMedOarna<- VAR(var.data.StorStockholm.SmalandMedOarna, p = 4, type = "const") # där p=4 är lags
var.result.StorStockholm.SmalandMedOarna

# Nollhyptes: autokorrelation är noll
# Alternativ autokorrelation är inte noll

# Autokorrelationstest av residualerna
# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3, funkar ej!
# BAra p=4 funkar bra.

serial.test(var.result.StorStockholm.SmalandMedOarna) 
# eftersom att 0.2571 när p=4 så kan vi ej förkasta Nollhypotesen om att autokorrelationen är lika med noll.

# Nollhyptes: Residualerna är normala
# Alternativ Hypotes: Residualerna är ej normala.

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorStockholm.SmalandMedOarna)$jb.mul$JB

# Ifall P-värde överstiger 0.05 så residualerna normalfördelade.
# 0.7059 > 0.05. Vi kan ej förkasta H0. Så residualer är normala.

roots(var.result.StorStockholm.SmalandMedOarna) # -1 TILL 1 intervall. STABIL

# Summa: Vi har väluppfostrade residualer.

# "var.result.StorStockholm.SmålandMedÖarna" som objekt/modell består av StorSthlm och SmålandMedÖarna.
# GC-test mellan StorGBG och SmålandMedÖarna

# Granger causality 
# H0: StorStockholm do not Granger-cause SmålandMedÖarna
# HA: StorStockholm does Granger-cause SmålandMedÖarna

causality(var.result.StorStockholm.SmalandMedOarna, cause = "StorStockholm")$Granger    # LÅGT p-värde; 4.839e-06 
causality(var.result.StorStockholm.SmalandMedOarna, cause = "SmalandMedOarna")$Granger  # Högt p-värde; 0.04313 


# Slutsats: StorStockholm GC orsakar SmålandMedÖarna dock ej tvärtom.

############################################################################
# # Bygger bivariat VAR; StorStockholm vs SydSverige
############################################################################

var.data.StorStockholm.SydSverige <- cbind(window(d.log.StorStockholm, start = c(1986,2)),
                                           window(d.log.SydSverige, start = c(1986,2)))

dimnames(var.data.StorStockholm.SydSverige )[[2]] <- c("StorStockholm","SydSverige")
var.data.StorStockholm.SydSverige

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorStockholm.SydSverige , type ="const") # SC=1 säger fortsatt 1 men AIC säger 5. Vi följer SC=1

var.result.StorStockholm.SydSverige <- VAR(var.data.StorStockholm.SydSverige, p = 4, type = "const") # där p=4 är lags
var.result.StorStockholm.SydSverige

# Nollhyptes: autokorrelation är noll
# Alternativ autokorrelation är inte noll

# Autokorrelationstest av residualerna
# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3, 
# funkar dock väljer ej detta även om 0.06217 > 0.05!
# BAra p=4 (0.545) funkar bra.

serial.test(var.result.StorStockholm.SydSverige) 
# eftersom att 0.545 när p=4 så kan vi ej förkasta Nollhypotesen om att autokorrelationen är lika med noll.

# Nollhyptes: Residualerna är normala
# Alternativ Hypotes: Residualerna är ej normala.

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorStockholm.SydSverige)$jb.mul$JB

# Ifall P-värde överstiger 0.05 så residualerna normalfördelade.
# 0.05416 > 0.05. Vi kan ej förkasta H0. Så residualer är normala.

roots(var.result.StorStockholm.SydSverige) # -1 TILL 1 intervall. STABIL

# Summa: Vi har väluppfostrade residualer.

# "var.result.StorStockholm.SydSverige" som objekt/modell består av StorSthlm och SydSverige.
# GC-test mellan StorStockholm och SydSverige

# Granger causality 
# H0: StorStockholm do not Granger-cause SydSverige
# HA: StorStockholm does Granger-cause SydSverige

causality(var.result.StorStockholm.SydSverige, cause = "StorStockholm")$Granger    # LÅGT p-värde; 9.881e-05
causality(var.result.StorStockholm.SydSverige, cause = "SydSverige")$Granger  # Högt p-värde; 0.4195 

# Slutsats: StorStockholm GC orsakar SydSverige dock ej tvärtom.

############################################################################
# # Bygger bivariat VAR; StorStockholm vs VästSverige
############################################################################

var.data.StorStockholm.VastSverige <- cbind(window(d.log.StorStockholm, start = c(1986,2)),
                                            window(d.log.VastSverige, start = c(1986,2)))

dimnames(var.data.StorStockholm.VastSverige )[[2]] <- c("StorStockholm","VastSverige")
var.data.StorStockholm.VastSverige

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorStockholm.VastSverige , type ="const") # SC=1 säger fortsatt 1 men AIC säger 5. Vi följer SC=1

var.result.StorStockholm.VastSverige <- VAR(var.data.StorStockholm.VastSverige, p = 6, type = "const") # där p=4 är lags
var.result.StorStockholm.VastSverige

# Nollhyptes: autokorrelation är noll
# Alternativ autokorrelation är inte noll

# Autokorrelationstest av residualerna
# När p=2 och p=3 funkar ej. så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3, 
# funkar dock väljer ej detta även om 0.6139 > 0.05!
# Bara p=6 (0.6139) funkar bra.
# OBS OBS Detta kan diskuteras vidare.

serial.test(var.result.StorStockholm.VastSverige) 
# eftersom att 0.6139 när p=6 så kan vi ej förkasta Nollhypotesen om att autokorrelationen är lika med noll.

# Nollhyptes: Residualerna är normala
# Alternativ Hypotes: Residualerna är ej normala.

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorStockholm.VastSverige)$jb.mul$JB

# Ifall P-värde överstiger 0.05 så residualerna normalfördelade.
# 0.01519 < 0.05. Vi kan förkasta H0. Så residualer är ej normala.

roots(var.result.StorStockholm.VastSverige) # -1 TILL 1 intervall. STABIL

# Summa: Vi har väluppfostrade residualer.

# "var.result.StorStockholm.VästSverige" som objekt/modell består av StorSthlm och VästSverige.
# GC-test mellan StorStockholm och VästSverige

# Granger causality 
# H0: StorStockholm do not Granger-cause VästSverige
# HA: StorStockholm does Granger-cause VästSverige

causality(var.result.StorStockholm.VastSverige, cause = "StorStockholm")$Granger    # LÅGT p-värde;     1.543e-05
causality(var.result.StorStockholm.VastSverige, cause = "VastSverige")$Granger      # Högt p-värde;     0.8116 

# Slutsats: StorStockholm GC orsakar VästSverige dock ej tvärtom.

############################################################################
# # Bygger bivariat VAR; StorStockholm vs NorraMellanSverige
############################################################################

var.data.StorStockholm.NorraMellanSverige <- cbind(window(d.log.StorStockholm, start = c(1986,2)),
                                                   window(d.log.NorraMellanSverige, start = c(1986,2)))

dimnames(var.data.StorStockholm.NorraMellanSverige )[[2]] <- c("StorStockholm","NorraMellanSverige")
var.data.StorStockholm.NorraMellanSverige

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorStockholm.NorraMellanSverige , type ="const") # SC=1 säger fortsatt 1 men AIC säger 5. Vi följer SC=1

var.result.StorStockholm.NorraMellanSverige <- VAR(var.data.StorStockholm.NorraMellanSverige, p = 4
                                                   , type = "const") # där p=4 är lagsvar.result.StorStockholm.NorraMellanSverige

# Nollhyptes: autokorrelation är noll
# Alternativ autokorrelation är inte noll

# Autokorrelationstest av residualerna
# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3, 
# funkar dock väljer ej detta även om > 0.05!
# BAra p=4 (0.09344) funkar bra.

serial.test(var.result.StorStockholm.NorraMellanSverige) 
# eftersom att 0.09344 när p=4 så kan vi ej förkasta Nollhypotesen om att autokorrelationen är lika med noll.

# Nollhyptes: Residualerna är normala
# Alternativ Hypotes: Residualerna är ej normala.

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorStockholm.NorraMellanSverige)$jb.mul$JB

# Ifall P-värde överstiger 0.05 så residualerna normalfördelade.
# 0.09629 > 0.05. Vi kan ej förkasta H0. Så residualer är normala.

roots(var.result.StorStockholm.NorraMellanSverige) # -1 TILL 1 intervall. STABIL

# Summa: Vi har väluppfostrade residualer.

# "var.result.StorStockholm.NorraMellanSverige" som objekt/modell består av StorSthlm och NorraMellanSverige.
# GC-test mellan StorStockholm och NorraMellanSverige

# Granger causality 
# H0: StorStockholm do not Granger-cause NorraMellanSverige
# HA: StorStockholm does Granger-cause NorraMellanSverige

causality(var.result.StorStockholm.NorraMellanSverige, cause = "StorStockholm")$Granger       # LÅGT p-värde; 5.721e-08
causality(var.result.StorStockholm.NorraMellanSverige, cause = "NorraMellanSverige")$Granger  # Högt p-värde; 0.3193

# Slutsats: StorStockholm GC orsakar NorraMellanSverige dock ej tvärtom.

############################################################################
# # Bygger bivariat VAR; StorStockholm vs MellerstaNorrland
############################################################################

var.data.StorStockholm.MellerstaNorrland <- cbind(window(d.log.StorStockholm, start = c(1986,2)),
                                                  window(d.log.MellerstaNorrland, start = c(1986,2)))

dimnames(var.data.StorStockholm.MellerstaNorrland)[[2]] <- c("StorStockholm","MellerstaNorrland")
var.data.StorStockholm.MellerstaNorrland

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorStockholm.MellerstaNorrland , type ="const") # SC=1 säger fortsatt 1 men AIC säger 5. Vi följer SC=1

var.result.StorStockholm.MellerstaNorrland <- VAR(var.data.StorStockholm.MellerstaNorrland, p = 4
                                                  , type = "const") # där p=4 är lagsvar.result.StorStockholm.MellerstaNorrland

# Nollhyptes: autokorrelation är noll
# Alternativ autokorrelation är inte noll

# Autokorrelationstest av residualerna
# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3, 
# funkar dock väljer ej detta även om 0.2632 > 0.05!
# BAra p=4 (0.6482) funkar bra.

serial.test(var.result.StorStockholm.MellerstaNorrland) 
# eftersom att 0.6482 när p=4 så kan vi ej förkasta Nollhypotesen om att autokorrelationen är lika med noll.

# Nollhyptes: Residualerna är normala
# Alternativ Hypotes: Residualerna är ej normala.

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorStockholm.MellerstaNorrland)$jb.mul$JB

# Ifall P-värde överstiger 0.05 så residualerna normalfördelade.
# 0.8769 > 0.05. Vi kan ej förkasta H0. Så residualer är normala.

roots(var.result.StorStockholm.MellerstaNorrland) # -1 TILL 1 intervall. STABIL

# Summa: Vi har väluppfostrade residualer.

# "var.result.StorStockholm.MellerstaNorrland" som objekt/modell består av StorSthlm och MellerstaNorrland.
# GC-test mellan StorStockholm och MellerstaNorrland

# Granger causality 
# H0: StorStockholm do not Granger-cause MellerstaNorrland
# HA: StorStockholm does Granger-cause MellerstaNorrland

causality(var.result.StorStockholm.MellerstaNorrland, cause = "StorStockholm")$Granger      # LÅGT p-värde; 9.023e-06
causality(var.result.StorStockholm.MellerstaNorrland, cause = "MellerstaNorrland")$Granger  # Högt p-värde;  0.2255

# Slutsats: StorStockholm GC orsakar MellerstaNorrland dock ej tvärtom.

############################################################################
# # Bygger bivariat VAR; StorStockholm vs ÖvreNorrland
############################################################################

var.data.StorStockholm.OvreNorrland <- cbind(window(d.log.StorStockholm, start = c(1986,2)),
                                             window(d.log.OvreNorrland, start = c(1986,2)))

dimnames(var.data.StorStockholm.OvreNorrland)[[2]] <- c("StorStockholm","OvreNorrland")
var.data.StorStockholm.OvreNorrland

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorStockholm.OvreNorrland , type ="const") # SC=1 säger fortsatt 1 men AIC säger 5. Vi följer SC=1

var.result.StorStockholm.OvreNorrland <- VAR(var.data.StorStockholm.OvreNorrland, p = 4
                                             , type = "const") # där p=4 är lagsvar.result.StorStockholm.ÖvreNorrland
# Nollhyptes: autokorrelation är noll
# Alternativ autokorrelation är inte noll

# Autokorrelationstest av residualerna
# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3, 
# funkar dock väljer ej detta även om 0.1622 > 0.05!
# Bara p=4 (0.4505) funkar bra.

serial.test(var.result.StorStockholm.OvreNorrland) 
# eftersom att 0.6482 när p=4 så kan vi ej förkasta Nollhypotesen om att autokorrelationen är lika med noll.

# Nollhyptes:         Residualerna är normala
# Alternativ Hypotes: Residualerna är ej normala.

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorStockholm.OvreNorrland)$jb.mul$JB

# Ifall P-värde överstiger 0.05 så residualerna normalfördelade.
# 0.03498 < 0.05. Vi kan förkasta H0. Så residualer är ICKE normala.
## OBS OBS!

roots(var.result.StorStockholm.OvreNorrland) # -1 TILL 1 intervall. STABIL

# Summa: Vi har väluppfostrade residualer.

# "var.result.StorStockholm.ÖvreNorrland" som objekt/modell består av StorSthlm och ÖvreNorrland.
# GC-test mellan StorStockholm och ÖvreNorrland

# Granger causality 
# H0: StorStockholm do not Granger-cause ÖvreNorrland
# HA: StorStockholm does Granger-cause ÖvreNorrland
causality(var.result.StorStockholm.OvreNorrland, cause = "StorStockholm")$Granger   # LÅGT p-värde; 5.092e-06
causality(var.result.StorStockholm.OvreNorrland, cause = "OvreNorrland")$Granger    # Högt p-värde; 0.3743

# Slutsats: StorStockholm GC orsakar ÖvreNorrland dock ej tvärtom.

############################################################################################

#               KLAR: StorStockholm med alla regioner

############################################################################################

############################################################################
# # Bygger bivariat VAR; StorGöteborg vs StorMalmö
############################################################################

var.data.StorGoteborg.StorMalmo <- cbind(window(d.log.StorGoteborg, start = c(1986,2)),
                                         window(d.log.StorMalmo, start = c(1986,2)))

dimnames(var.data.StorGoteborg.StorMalmo)[[2]] <- c("StorGoteborg","StorMalmo")
var.data.StorGoteborg.StorMalmo

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorGoteborg.StorMalmo , type ="const") # SC=1 säger fortsatt 1 men AIC säger 2. Vi följer SC=1

var.result.StorGoteborg.StorMalmo <- VAR(var.data.StorGoteborg.StorMalmo, p = 4
                                         , type = "const") # där p=4 är lagsvar.result.StorGöteborg.StorMalmö
# Nollhyptes: autokorrelation är noll
# Alternativ autokorrelation är inte noll

# Autokorrelationstest av residualerna
# När p=2 och p=3 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=4, 
# Bara p=4 (0.1466) funkar bra.

serial.test(var.result.StorGoteborg.StorMalmo) 
# eftersom att 0.1466 när p=4 så kan vi ej förkasta Nollhypotesen om att autokorrelationen är lika med noll.

# Nollhyptes:         Residualerna är normala
# Alternativ Hypotes: Residualerna är ej normala.

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorGoteborg.StorMalmo)$jb.mul$JB

# Ifall P-värde överstiger 0.05 så residualerna normalfördelade.
# 1.523e-07 < 0.05. Vi kan förkasta H0. Så residualer är ICKE normala.
## OBS OBS!

roots(var.result.StorGoteborg.StorMalmo) # -1 TILL 1 intervall. STABIL

# Summa: Vi har väluppfostrade residualer.

# "var.result.StorGöteborg.StorMalmö" som objekt/modell består av StorSthlm och StorMalmö.
# GC-test mellan StorGöteborg och StorMalmö

# Granger causality 
# H0: StorGöteborg do not Granger-cause StorMalmö
# HA: StorGöteborg does Granger-cause StorMalmö
causality(var.result.StorGoteborg.StorMalmo, cause = "StorGoteborg")$Granger    # LÅGT p-värde; 0.00162
causality(var.result.StorGoteborg.StorMalmo, cause = "StorMalmo")$Granger       # Högt p-värde; 0.8307

# Slutsats: StorGöteborg GC orsakar StorMalmö dock ej tvärtom.

############################################################################
# # Bygger bivariat VAR; StorGöteborg vs StockholmsLän
############################################################################
var.data.StorGoteborg.StockholmsLan <- cbind(window(d.log.StorGoteborg, start = c(1986,2)),
                                             window(d.log.StockholmsLan, start = c(1986,2)))

dimnames(var.data.StorGoteborg.StockholmsLan)[[2]] <- c("StorGoteborg","StockholmsLan")
var.data.StorGoteborg.StockholmsLan

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorGoteborg.StockholmsLan, type ="const") # SC=1 säger fortsatt 1 men AIC säger 2. Vi följer SC=1

var.result.StorGoteborg.StockholmsLan <- VAR(var.data.StorGoteborg.StockholmsLan, p = 4
                                             , type = "const") # där p=4 är lagsvar.result.StorGöteborg.StorMalmö

serial.test(var.result.StorGoteborg.StockholmsLan) 

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorGoteborg.StockholmsLan)$jb.mul$JB
roots(var.result.StorGoteborg.StockholmsLan) # -1 TILL 1 intervall. STABIL

# Granger causality 
# H0: StorGöteborg do not Granger-cause StockholmsLän
# HA: StorGöteborg does Granger-cause StockholmsLän

causality(var.result.StorGoteborg.StockholmsLan, cause = "StorGoteborg")$Granger        # LÅGT p-värde; 0.9777
causality(var.result.StorGoteborg.StockholmsLan, cause = "StockholmsLan")$Granger       # Högt p-värde; 3.011e-06

# Slutsats: StorGöteborg GC orsakar StockholmsLän dock ej tvärtom.

############################################################################
# # Bygger bivariat VAR; StorGöteborg vs ÖstraMellanSverige
############################################################################
var.data.StorGoteborg.OstraMellanSverige <- cbind(window(d.log.StorGoteborg, start = c(1986,2)),
                                                  window(d.log.OstraMellanSverige, start = c(1986,2)))

dimnames(var.data.StorGoteborg.OstraMellanSverige)[[2]] <- c("StorGoteborg","OstraMellanSverige")
var.data.StorGoteborg.OstraMellanSverige

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorGoteborg.OstraMellanSverige, type ="const") # SC=1 säger fortsatt 1 men AIC säger 2. Vi följer SC=1

var.result.StorGoteborg.OstraMellanSverige <- VAR(var.data.StorGoteborg.OstraMellanSverige, p = 4
                                                  , type = "const") # där p=4 är lagsvar.result.StorGöteborg.StorMalmö

serial.test(var.result.StorGoteborg.OstraMellanSverige) 

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorGoteborg.OstraMellanSverige)$jb.mul$JB
roots(var.result.StorGoteborg.OstraMellanSverige) # -1 TILL 1 intervall. STABIL

# Granger causality 
# H0: StorGöteborg do not Granger-cause ÖstraMellanSverige
# HA: StorGöteborg does Granger-cause ÖstraMellanSverige

causality(var.result.StorGoteborg.OstraMellanSverige, cause = "StorGoteborg")$Granger             # LÅGT p-värde; 0.02644
causality(var.result.StorGoteborg.OstraMellanSverige, cause = "OstraMellanSverige")$Granger       # Högt p-värde; 0.1873
# Slutsats: StorGöteborg GC orsakar ÖstraMellanSverige dock ej tvärtom.

############################################################################
# # Bygger bivariat VAR; StorGöteborg vs SmålandMedÖarna
############################################################################
var.data.StorGoteborg.SmalandMedOarna <- cbind(window(d.log.StorGoteborg, start = c(1986,2)),
                                               window(d.log.SmalandMedOarna, start = c(1986,2)))

dimnames(var.data.StorGoteborg.SmalandMedOarna)[[2]] <- c("StorGoteborg","SmalandMedOarna")
var.data.StorGoteborg.SmalandMedOarna

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorGoteborg.SmalandMedOarna, type ="const") # SC=1 säger fortsatt 1 men AIC säger 2. Vi följer SC=1

var.result.StorGoteborg.SmalandMedOarna <- VAR(var.data.StorGoteborg.SmalandMedOarna, p = 4
                                               , type = "const") # där p=4 är lagsvar.result.StorGöteborg.StorMalmö

serial.test(var.result.StorGoteborg.SmalandMedOarna) 

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorGoteborg.SmalandMedOarna)$jb.mul$JB
roots(var.result.StorGoteborg.SmalandMedOarna) # -1 TILL 1 intervall. STABIL

# Granger causality 
# H0: StorGöteborg do not Granger-cause SmålandMedÖarna
# HA: StorGöteborg does Granger-cause SmålandMedÖarna

causality(var.result.StorGoteborg.SmalandMedOarna, cause = "StorGoteborg")$Granger             # LÅGT p-värde; 6.804e-06
causality(var.result.StorGoteborg.SmalandMedOarna, cause = "SmalandMedOarna")$Granger          # Högt p-värde; 0.2658
# Slutsats: StorGöteborg GC orsakar SmålandMedÖarna dock ej tvärtom.

############################################################################
# # Bygger bivariat VAR; StorGöteborg vs SydSverige 
############################################################################
var.data.StorGoteborg.SydSverige <- cbind(window(d.log.StorGoteborg, start = c(1986,2)),
                                          window(d.log.SydSverige, start = c(1986,2)))

dimnames(var.data.StorGoteborg.SydSverige)[[2]] <- c("StorGoteborg","SydSverige")
var.data.StorGoteborg.SydSverige

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorGoteborg.SydSverige, type ="const") # SC=1 säger fortsatt 1 men AIC säger 2. Vi följer SC=1

var.result.StorGoteborg.SydSverige <- VAR(var.data.StorGoteborg.SydSverige, p = 4
                                          , type = "const") # där p=4 är lagsvar.result.StorGöteborg.StorMalmö

serial.test(var.result.StorGoteborg.SydSverige) 

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorGoteborg.SydSverige)$jb.mul$JB
roots(var.result.StorGoteborg.SydSverige) # -1 TILL 1 intervall. STABIL

# Granger causality 
# H0: StorGöteborg do not Granger-cause SmålandMedÖarna
# HA: StorGöteborg does Granger-cause SmålandMedÖarna

causality(var.result.StorGoteborg.SydSverige, cause = "StorGoteborg")$Granger             # LÅGT p-värde; 0.03403
causality(var.result.StorGoteborg.SydSverige, cause = "SydSverige")$Granger               # Högt p-värde; 0.6825
# Slutsats: StorGöteborg GC orsakar SydSverige dock ej tvärtom.

############################################################################
# # Bygger bivariat VAR; StorGöteborg vs VästSverige 
############################################################################
var.data.StorGoteborg.VastSverige <- cbind(window(d.log.StorGoteborg, start = c(1986,2)),
                                           window(d.log.VastSverige, start = c(1986,2)))

dimnames(var.data.StorGoteborg.VastSverige)[[2]] <- c("StorGoteborg","VastSverige")
var.data.StorGoteborg.VastSverige

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorGoteborg.VastSverige, type ="const") # SC=1 säger fortsatt 1 men AIC säger 2. Vi följer SC=1

var.result.StorGoteborg.VastSverige <- VAR(var.data.StorGoteborg.VastSverige, p = 4
                                           , type = "const") # där p=4 är lagsvar.result.StorGöteborg.StorMalmö

serial.test(var.result.StorGoteborg.VastSverige) 

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorGoteborg.VastSverige)$jb.mul$JB
roots(var.result.StorGoteborg.VastSverige) # -1 TILL 1 intervall. STABIL

# Granger causality 
# H0: StorGöteborg do not Granger-cause VästSverige
# HA: StorGöteborg does Granger-cause VästSverige

causality(var.result.StorGoteborg.VastSverige, cause = "StorGoteborg")$Granger             # LÅGT p-värde; 0.05395
causality(var.result.StorGoteborg.VastSverige, cause = "VastSverige")$Granger              # Högt p-värde; 0.1312
# Slutsats: StorGöteborg GC orsakar VästSverige dock ej tvärtom.

############################################################################
# # Bygger bivariat VAR; StorGöteborg vs NorraMellanSverige 
############################################################################
var.data.StorGoteborg.NorraMellanSverige  <- cbind(window(d.log.StorGoteborg, start = c(1986,2)),
                                                   window(d.log.NorraMellanSverige , start = c(1986,2)))

dimnames(var.data.StorGoteborg.NorraMellanSverige )[[2]] <- c("StorGoteborg","NorraMellanSverige")
var.data.StorGoteborg.NorraMellanSverige 

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorGoteborg.NorraMellanSverige , type ="const") # SC=1 säger fortsatt 1 men AIC säger 2. Vi följer SC=1

var.result.StorGoteborg.NorraMellanSverige  <- VAR(var.data.StorGoteborg.NorraMellanSverige , p = 4
                                                   , type = "const") # där p=4 är lagsvar.result.StorGöteborg.StorMalmö

serial.test(var.result.StorGoteborg.NorraMellanSverige ) 

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorGoteborg.NorraMellanSverige )$jb.mul$JB
roots(var.result.StorGoteborg.NorraMellanSverige ) # -1 TILL 1 intervall. STABIL

# Granger causality 
# H0: StorGöteborg do not Granger-cause NorraMellanSverige 
# HA: StorGöteborg does Granger-cause NorraMellanSverige 

causality(var.result.StorGoteborg.NorraMellanSverige,cause = "StorGoteborg")$Granger                   # LÅGT p-värde; 0.0001644
causality(var.result.StorGoteborg.NorraMellanSverige,cause = "NorraMellanSverige")$Granger             # Högt p-värde:0.6011 

# Slutsats: StorGöteborg GC orsakar NorraMellanSverige  dock ej tvärtom.



############################################################################
# # Bygger bivariat VAR; StorGöteborg vs MellerstaNorrland
############################################################################
var.data.StorGoteborg.MellerstaNorrland  <- cbind(window(d.log.StorGoteborg, start = c(1986,2)),
                                                  window(d.log.MellerstaNorrland , start = c(1986,2)))

dimnames(var.data.StorGoteborg.MellerstaNorrland )[[2]] <- c("StorGoteborg","MellerstaNorrland")
var.data.StorGoteborg.MellerstaNorrland 

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorGoteborg.MellerstaNorrland , type ="const") # SC=1 säger fortsatt 1 men AIC säger 2. Vi följer SC=1

var.result.StorGoteborg.MellerstaNorrland  <- VAR(var.data.StorGoteborg.MellerstaNorrland , p = 4
                                                  , type = "const") # där p=4 är lagsvar.result.StorGöteborg.StorMalmö

serial.test(var.result.StorGoteborg.MellerstaNorrland ) 

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorGoteborg.MellerstaNorrland )$jb.mul$JB
roots(var.result.StorGoteborg.MellerstaNorrland ) # -1 TILL 1 intervall. STABIL

# Granger causality 
# H0: StorGöteborg do not Granger-cause MellerstaNorrland 
# HA: StorGöteborg does Granger-cause MellerstaNorrland

causality(var.result.StorGoteborg.MellerstaNorrland,cause = "StorGoteborg")$Granger                 # LÅGT p-värde; 4.019e-05
causality(var.result.StorGoteborg.MellerstaNorrland,cause = "MellerstaNorrland")$Granger           # Högt p-värde: 0.105

# Slutsats: StorGöteborg GC orsakar MellerstaNorrland dock ej tvärtom.

############################################################################
# # Bygger bivariat VAR; StorGöteborg vs ÖvreNorrland
############################################################################
var.data.StorGoteborg.OvreNorrland  <- cbind(window(d.log.StorGoteborg, start = c(1986,2)),
                                             window(d.log.OvreNorrland,start = c(1986,2)))

dimnames(var.data.StorGoteborg.OvreNorrland )[[2]] <- c("StorGoteborg","OvreNorrland ")
var.data.StorGoteborg.OvreNorrland 

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorGoteborg.OvreNorrland, type ="const") # SC=1 säger fortsatt 1 men AIC säger 2. Vi följer SC=1

var.result.StorGoteborg.OvreNorrland  <- VAR(var.data.StorGoteborg.OvreNorrland , p = 4, type = "const") # där p=4 är lagsvar.result.StorGöteborg.StorMalmö

serial.test(var.result.StorGoteborg.OvreNorrland) 

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorGoteborg.OvreNorrland)$jb.mul$JB
roots(var.result.StorGoteborg.OvreNorrland) # -1 TILL 1 intervall. STABIL

# Granger causality 
# H0: StorGöteborg do not Granger-cause ÖvreNorrland 
# HA: StorGöteborg does Granger-cause ÖvreNorrland

causality(var.result.StorGoteborg.OvreNorrland,cause = "StorGoteborg")$Granger                 # LÅGT p-värde; 5.199e-06
causality(var.result.StorGoteborg.OvreNorrland,cause = "OvreNorrland")$Granger           # Högt p-värde: 

# Slutsats: StorGöteborg GC orsakar ÖvreNorrland dock ej tvärtom.

############################################################################
# # Bygger bivariat VAR; StorMalmö vs Sthlmslän
############################################################################

var.data.StorMalmo.StockholmsLan  <- cbind(window(d.log.StorMalmo, start = c(1986,2)),
                                           window(d.log.StockholmsLan,start = c(1986,2)))

dimnames(var.data.StorMalmo.StockholmsLan)[[2]] <- c("StorMalmo","StockholmsLan")
var.data.StorMalmo.StockholmsLan

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorMalmo.StockholmsLan, type ="const") # SC=1 säger fortsatt 1 men AIC säger 2. Vi följer SC=1

var.result.StorMalmo.StockholmsLan  <- VAR(var.data.StorMalmo.StockholmsLan , p = 4, type = "const") # där p=4 är lagsvar.result.StorGöteborg.StorMalmö

serial.test(var.result.StorMalmo.StockholmsLan) # ingen autokorr 

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorMalmo.StockholmsLan)$jb.mul$JB # ej normal
roots(var.result.StorMalmo.StockholmsLan) # -1 TILL 1 intervall. STABIL

# Granger causality 
# H0: StorGöteborg do not Granger-cause ÖvreNorrland 
# HA: StorGöteborg does Granger-cause ÖvreNorrland

causality(var.result.StorMalmo.StockholmsLan,cause = "StorMalmo")$Granger                 # LÅGT p-värde; 0.6413
causality(var.result.StorMalmo.StockholmsLan,cause = "StockholmsLan")$Granger           # Högt p-värde: 2.916e-06

# # Slutsats: StorMalmo GC orsakar EJ Sthlmslän. Stockhlms län gör det dock!

############################################################################
# # Bygger bivariat VAR; StorMalmö vs ostramellansverige
############################################################################

var.data.StorMalmo.OstraMellanSverige  <- cbind(window(d.log.StorMalmo, start = c(1986,2)),
                                                window(d.log.OstraMellanSverige,start = c(1986,2)))

dimnames(var.data.StorMalmo.OstraMellanSverige)[[2]] <- c("StorMalmo","OstraMellanSverige")
var.data.StorMalmo.OstraMellanSverige

# När p=2 så hade vi autokorrelation bland residualerna. För att bli av med autokorrelation, höjde vi därav lags till p=3
VARselect(var.data.StorMalmo.OstraMellanSverige, type ="const") # SC=1 säger fortsatt 1 men AIC säger 2. Vi följer SC=1

var.result.StorMalmo.OstraMellanSverige  <- VAR(var.data.StorMalmo.OstraMellanSverige , p = 4, type = "const") # där p=4 är lagsvar.result.StorGöteborg.StorMalmö

serial.test(var.result.StorMalmo.OstraMellanSverige) # ingen autokorr 

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorMalmo.OstraMellanSverige)$jb.mul$JB # Normal
roots(var.result.StorMalmo.OstraMellanSverige) # -1 TILL 1 intervall. STABIL

# Granger causality 
# H0: StorGöteborg do not Granger-cause ÖvreNorrland 
# HA: StorGöteborg does Granger-cause ÖvreNorrland

causality(var.result.StorMalmo.OstraMellanSverige,cause = "StorMalmo")$Granger                 # LÅGT p-värde;0.3644
causality(var.result.StorMalmo.OstraMellanSverige,cause = "OstraMellanSverige")$Granger           # Högt p-värde: 0.01123

# # Slutsats: Ostramellansverige GC orsakar Malmö men inte tvärtom.

############################################################################
# # Bygger bivariat VAR; StorMalmö vs SmalandMedOarna
############################################################################

var.data.StorMalmo.SmalandMedOarna  <- cbind(window(d.log.StorMalmo, start = c(1986,2)),
                                             window(d.log.SmalandMedOarna,start = c(1986,2)))

dimnames(var.data.StorMalmo.SmalandMedOarna)[[2]] <- c("StorMalmo","SmalandMedOarna")
var.data.StorMalmo.SmalandMedOarna


VARselect(var.data.StorMalmo.SmalandMedOarna, type ="const") # säger 2 lags och ej 1 som alltid förr. 

var.result.StorMalmo.SmalandMedOarna  <- VAR(var.data.StorMalmo.SmalandMedOarna , p = 4, type = "const") # p=4

serial.test(var.result.StorMalmo.SmalandMedOarna) # ingen autokorr när p=4. ingen autokorr

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorMalmo.SmalandMedOarna)$jb.mul$JB # ej Normal
roots(var.result.StorMalmo.SmalandMedOarna) # -1 TILL 1 intervall. STABIL

# Granger causality 
# H0: StorGöteborg do not Granger-cause ÖvreNorrland 
# HA: StorGöteborg does Granger-cause ÖvreNorrland

causality(var.result.StorMalmo.SmalandMedOarna,cause = "StorMalmo")$Granger                 # LÅGT p-värde;0.002363
causality(var.result.StorMalmo.SmalandMedOarna,cause = "SmalandMedOarna")$Granger           # Högt p-värde: 0.1826

# # Slutsats: Stormalmö gc orsakar smålandmedöarna men EJ tvärtom!

############################################################################
# # Bygger bivariat VAR; StorMalmö vs SydSverige
############################################################################

var.data.StorMalmo.SydSverige  <- cbind(window(d.log.StorMalmo, start = c(1986,2)),
                                        window(d.log.SydSverige,start = c(1986,2)))

dimnames(var.data.StorMalmo.SydSverige)[[2]] <- c("StorMalmo","SydSverige")
var.data.StorMalmo.SydSverige


VARselect(var.data.StorMalmo.SydSverige, type ="const") # säger 2 lags och ej 1 som alltid förr. 

var.result.StorMalmo.SydSverige  <- VAR(var.data.StorMalmo.SydSverige , p = 4, type = "const") # p=4 men säger p=1

serial.test(var.result.StorMalmo.SydSverige) # ingen autokorr när p=4. ingen autokorr

# Normalitet (normala residualer- normalfördelade residualer)
normality.test(var.result.StorMalmo.SydSverige)$jb.mul$JB # ej Normal
roots(var.result.StorMalmo.SydSverige) # -1 TILL 1 intervall. STABIL
