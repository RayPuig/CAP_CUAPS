#[16.07.2019]
#[15.07.2019] 
#--------------------------------------------------------------------------------#
#[11.07.2019]
#--------------------------------------------------------------------------------#
#[04.07.2019]
#--------------------------------------------------------------------------------#
#[26.03.2019]
#--------------------------------------------------------------------------------#
#										dia 07/03/2018
#--------------------------------------------------------------------------------#
library("Hmisc")
library("gdata")
library("xtable")
library("SNPassoc")
library("survival")
library("epitools")
library("tools")
library("HardyWeinberg")
library("rmarkdown")
library("knitr")
library("psych")
library("qgraph")
library("MASS")
#--------------------------------------------------------------------------------#
library(DiagrammeR)
library(DiagrammeRsvg)
library(magrittr)
library(svglite)
library(rsvg)
library(jpeg)
library(plotrix)
#--------------------------------------------------------------------------------#
library(rasterImage) 
library(png)
library(grid)
library(Rcpp)
library(htmlTable)
library(Gmisc)
library(ggplot2)
library(stats)
library(graphics)
library(plotly)
#--------------------------------------------------------------------------------#
library("Hmisc")
library("gdata")
library("xtable")
library("SNPassoc")
library("survival")
library("epitools")
library("tools")
library("HardyWeinberg")
library("rmarkdown")
library("knitr")
library("psych")
library("qgraph")
library("MASS")
library("labelled")
library("haplo.stats")
#--------------------------------------------------------------------------------#
library("compareGroups")
library("naniar")
library("glue")
library("svglite")
#--------------------------------------------------------------------------------#
etiquetar<-function(d=dadestotal,taulavariables="variables_R.xls") {
  
  ####  Llegir etiquetes i variables a analitzar ####
  variables <- readxl::read_excel(taulavariables)
  variables[is.na(variables)]<- 0
  #
  #
  ###################################   etiquetar variables         
  seleccio<-variables
  camp<- as.vector(seleccio$camp) #
  descripcio<- as.vector(seleccio$descripcio) #
  ### etiquetar variables seleccionades     #
  for (i in 2:length(descripcio)){if (any(colnames(d) == camp[i])) {Hmisc::label(d[[camp[i]]]) <- descripcio[i]}}
  d
}

#------------------------------------------------------------------------------------------#
#                        2. CONVERTIR DATES![de num?ric a codo Data!]
#------------------------------------------------------------------------------------------#




#------------------------------------------------------------------#
convertir_dates<-function(d=dadestotal,taulavariables="variables_R.xls")
  
{
  ####  Llegir etiquetes i variables a analitzar ####
  variables <- readxl::read_excel(taulavariables)
  variables[is.na(variables)]<- 0
  #
  #
  ###################################   etiquetar variables         
  seleccio<-variables
  camp<- as.vector(seleccio$camp) #
  # dates<-as.vector(seleccio$dates)
  # dates<-seleccio[[campdata]]
  ### etiquetar variables seleccionades     #
  
  for (i in 1:length(camp)){if (seleccio$dates[i]==1) { 
    
    pepito<-paste0("as.Date(d[[camp[",i,"]]], '%Y%d%m')")
    
    d[[camp[i]]]<-eval(parse(text=pepito))
    
  } }
  
  d
  
}
#------------------------------------------------------------------#




#------------------------------------------------------------------#
LAB_ETIQ_v2<-function(dt=dades,variables_factors=conductor_variables,fulla="etiquetes",idioma="etiqueta1"){
  
  dt=dades
  variables_factors=conductor_variables
  
  
  #------------------------------------------------------------------#
  variables_factors<-readxl::read_excel(variables_factors,sheet=fulla)
  #------------------------------------------------------------------#
  
  
  
  if (idioma=="etiqueta1") {
    
    k<-variables_factors%>%select(camp, valor,etiqueta1)
    
  } else if (idioma=="etiqueta2") {
    
    k<-variables_factors%>%select(camp, valor,etiqueta2)
    k<-k%>%mutate(etiqueta1=etiqueta2)
    k<-k%>%select(camp, valor,etiqueta1)
    
  }
  
  
  #------------------------------------------------------------------#
  pepe<-k %>% split(list(.$camp))
  #------------------------------------------------------------------#
  #
  noms_variables<-names(pepe)
  num_vars<-length(noms_variables)
  
  for (i in 1:num_vars) {
    
    dt[noms_variables[i]]<-lapply(dt[noms_variables[i]],function(y) factor(y,levels=pepe[[i]]$valor,labels=pepe[[i]]$etiqueta1))
    
  }
  
  dt}
#------------------------------------------------------------------#


#-----------------------------------------------------------------------------------------------------------------------------------#.
library("compareGroups")
#-----------------------------------------------------------------------------------------------------------------------------------#.
#-----------------------------------------------------------------------------------------------------------------------------------#.
#										dia 01/03/2018
#-----------------------------------------------------------------------------------------------------------------------------------#.
#										dia 06/03/2018
#-----------------------------------------------------------------------------------------------------------------------------------#.
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------.
#Adjunto codi exemple del Comparegrups
#Si em truques t'ho explico
#res1 <- compareGroups(Retinopathy ~ .-Codigo, data=dades,include.miss = F,na.exclude=F,include.label=T)
#restab2<-createTable(res1, show.ratio = F, hide.no = c('NA','No'), show.p.overall=T,show.n=T,show.all=F)
#restab2
#------------------------------------------------------------------------------------------------------------------------------------------------------------------.
#------------------------------------------------------------------------------------------------------------------------------------------------------------------.
###########################exemple II
#------------------------------------------------------------------------------------------------------------------------------------------------------------------.
#require(compareGroups)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------.
# load REGICOR data
#data(regicor)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------.
# compute a time-to-cardiovascular event variable
#regicor$tcv <- with(regicor,Surv(tocv, as.integer(cv=='Yes')))
#label(regicor$tcv)<-"Cardiovascular incidence"
#------------------------------------------------------------------------------------------------------------------------------------------------------------------.
# remove variables used to create time-to variables
#regicor<-remove.vars(regicor,c("todeath","death","tocv","cv"))
#------------------------------------------------------------------------------------------------------------------------------------------------------------------.
# descriptives by time-to-cardiovascular event, taking 'no' category as 
# the reference in computing HRs.
#res <- compareGroups(tcv ~ age + sex + smoker + sbp + histhtn + 
#         chol + txchol + bmi + phyact + pcs + tcv, regicor, ref.no='no')
#------------------------------------------------------------------------------------------------------------------------------------------------------------------.
# build table showing HR and hiding the 'no' category
#restab <- createTable(res, show.ratio = TRUE, hide.no = 'no')
#restab
#------------------------------------------------------------------------------------------------------------------------------------------------------------------.

#15.7.2019



#[Horta].
#------------------------------------------------------------------------------------------------------------------------------------------------------------------.
#setwd("C:/Users/Suta/Desktop/hospital2")
#"C:/Users/Suta/Desktop/Hospital_Final"%>% file.path("FUNCIONS_PROPIES3.R") %>% source()
#------------------------------------------------------------------------------------------------------------------------------------------------------------------.
#C:\Users\38122893W\Desktop\hospital_terrassa





#[Gr?cia].
setwd("C:/Users/38122893W/Desktop/hospital_terrassa")
"C:/Users/38122893W/Desktop/hospital_terrassa"%>% file.path("FUNCIONS_PROPIES3.R") %>% source()
#----------------------------------------------------

#setwd("C:/Users/38122893W/Desktop/hospital2")




#-----------------------------------------------------------------------------------------------------------------------------------#.
#UCI<- 	read.csv("UCI.csv", header=TRUE,sep=";" , dec=",")
#-------------------------------------------------------------------------w----------------------------------------------------------#.
UCI<-	read.csv("UCI.csv", header=TRUE,sep=";" , dec=",")
#-----------------------------------------------------------------------------------------------------------------------------------#.
#--------------------------------------------------------------------------------#
-----------------------------------------------------------------#
#                        1. ETIQUETAR VARIABLES! []
#------------------------------------------------------------------------------------------#
###

  
UCI2<-UCI


#-----------------------------------------------------------------------------------------------------------------------------------#.
UCI2$hosp[UCI2$tipcenter=="CUAP"		   | 	UCI2$tipcenter=="PAC"]  								<- "EXTRA"
UCI2$hosp[UCI2$tipcenter=="Alta resoluci?n"  | 	UCI2$tipcenter=="Referencia" | 	UCI2$tipcenter=="Comarcal"]  		<- "INTRA"
#-----------------------------------------------------------------------------------------------------------------------------------#.
UCI2
#-----------------------------------------------------------------------------------------------------------------------------------#.
UCI2$iru			[UCI2$iru==888.0000 			| 	UCI2$iru==999.0000  			| 	UCI2$iru==888 			| 	UCI2$iru==999]			<- NA
UCI2$ncamas			[UCI2$ncamas==888.0000 			| 	UCI2$ncamas==999.0000 	 		| 	UCI2$ncamas==888 			| 	UCI2$ncamas==999]			<- NA
UCI2$dependeucia		[UCI2$dependeucia==888.0000 		| 	UCI2$dependeucia==999.0000 	 	| 	UCI2$dependeucia==888 		| 	UCI2$dependeucia==999]		<- NA
UCI2$soportecap		[UCI2$soportecap==888.0000 		| 	UCI2$soportecap==999.0000  		| 	UCI2$soportecap==888 		| 	UCI2$soportecap==999]		<- NA
UCI2$analisec		[UCI2$analisec==888.0000 		| 	UCI2$analisec==999.0000  		| 	UCI2$analisec==888 		| 	UCI2$analisec==999]		<- NA
UCI2$rx			[UCI2$rx==888.0000 			| 	UCI2$rx==999.0000  			| 	UCI2$rx==888 			| 	UCI2$rx==999]			<- NA
UCI2$ambulancia		[UCI2$ambulancia==888.0000 		| 	UCI2$ambulancia==999.0000  		| 	UCI2$ambulancia==888 		| 	UCI2$ambulancia==999]		<- NA
UCI2$percentambula	[UCI2$percentambula==888.0000 	| 	UCI2$percentambula==999.0000  	| 	UCI2$percentambula==888 	| 	UCI2$percentambula==999]	<- NA
UCI2$famy			[UCI2$famy==888.0000 			| 	UCI2$famy==999.0000  			| 	UCI2$famy==888 			| 	UCI2$famy==999]			<- NA
UCI2$medint			[UCI2$medint==888.0000 			| 	UCI2$medint==999.0000 	 		| 	UCI2$medint==888 			| 	UCI2$medint==999]			<- NA
UCI2$anestesia		[UCI2$anestesia==888.0000 		| 	UCI2$anestesia==999.0000  		| 	UCI2$anestesia==888 		| 	UCI2$anestesia==999]		<- NA
UCI2$quirurgi		[UCI2$quirurgi==888.0000 		| 	UCI2$quirurgi==999.0000  		| 	UCI2$quirurgi==888 		| 	UCI2$quirurgi==999]		<- NA
UCI2$noespecia		[UCI2$noespecia==888.0000 		| 	UCI2$noespecia==999.0000  		| 	UCI2$noespecia==888 		| 	UCI2$noespecia==999]	 	<- NA
UCI2$tasaexitus		[UCI2$tasaexitus ==888.0000 		| 	UCI2$tasaexitus ==999.0000 		| 	UCI2$tasaexitus ==888 		| 	UCI2$tasaexitus ==999]		<- NA
UCI2$minespera1		[UCI2$minespera1==888.0000 		| 	UCI2$minespera1==999.0000  		| 	UCI2$minespera1==888 		| 	UCI2$minespera1==999]		<- NA
UCI2$minespera2		[UCI2$minespera2==888.0000 		| 	UCI2$minespera2==999.0000  		| 	UCI2$minespera2==888 		| 	UCI2$minespera2==999]		<- NA
UCI2$minespera3		[UCI2$minespera3==888.0000 		| 	UCI2$minespera3==999.0000  		| 	UCI2$minespera3==888 		| 	UCI2$minespera3==999]		<- NA
UCI2$minespera4		[UCI2$minespera4==888.0000 		| 	UCI2$minespera4==999.0000  		| 	UCI2$minespera4==888 		| 	UCI2$minespera4==999]		<- NA
UCI2$minespera5		[UCI2$minespera5==888.0000 		| 	UCI2$minespera5==999.0000  		| 	UCI2$minespera5==888 		| 	UCI2$minespera5==999]		<- NA
UCI2$personpropi		[UCI2$personpropio ==888.0000 	| 	UCI2$personpropio ==999.0000  	| 	UCI2$personpropio==888 		| 	UCI2$personpropio==999]		<- NA
UCI2$percentambula	[UCI2$percentambula ==888.0000 	| 	UCI2$percentambula ==999.0000  	| 	UCI2$percentambula==888 	| 	UCI2$percentambula==999]	<- NA
UCI2$experienmin		[UCI2$experienmin==888.0000 		| 	UCI2$experienmin==999.0000  		| 	UCI2$experienmin==888 		| 	UCI2$experienmin==999]		<- NA
UCI2$uciasprev		[UCI2$uciasprev==888.0000 		| 	UCI2$uciasprev==999.0000  		| 	UCI2$uciasprev==888 		| 	UCI2$uciasprev==999]		<- NA
UCI2$nivelantes		[UCI2$nivelantes==888.0000 		| 	UCI2$ nivelantes==999.0000  		| 	UCI2$nivelantes==888 		| 	UCI2$ nivelantes==999]		<- NA
UCI2$niveldespues		[UCI2$niveldespues==888.0000 		| 	UCI2$niveldespues==999.0000		| 	UCI2$niveldespues==888 		| 	UCI2$niveldespues==999]		<- NA
UCI2$existprotocol	[UCI2$existprotocol==888.0000 	| 	UCI2$existprotocol==999.0000		| 	UCI2$existprotocol==888 	| 	UCI2$existprotocol==999]	<- NA
UCI2$percderiv		[UCI2$percderiv ==888.0000 		| 	UCI2$ percderiv ==999.0000  		| 	UCI2$percderiv ==888 		| 	UCI2$percderiv ==999]		<- NA
UCI2$existconti		[UCI2$existconti==888.0000 		| 	UCI2$existconti==999.0000  		| 	UCI2$existconti==888 		| 	UCI2$existconti==999]		<- NA
UCI2$percreder		[UCI2$percreder==888.0000 		| 	UCI2$percreder==999.0000  		| 	UCI2$percreder==888 		| 	UCI2$percreder==999]	 	<- NA
UCI2$impacvolum		[UCI2$impacvolum==888.0000 		| 	UCI2$impacvolum==999.0000  		| 	UCI2$impacvolum==888 		| 	UCI2$impacvolum==999]		<- NA
UCI2$derinv			[UCI2$derinv==888.0000 			| 	UCI2$derinv==999.0000 	  	 	| 	UCI2$derinv==888 			| 	UCI2$derinv==999]			<- NA
UCI2$qnderiv		[UCI2$qnderiv==888.0000 		| 	UCI2$qnderiv==999.0000  		| 	UCI2$qnderiv==888 		| 	UCI2$qnderiv==999]	 	<- NA
UCI2$qnmesaterri		[UCI2$qnmesaterri==888.0000 		| 	UCI2$qnmesaterri==999.0000  		| 	UCI2$qnmesaterri==888 		| 	UCI2$qnmesaterri==999]		<- NA
UCI2$systriaje		[UCI2$systriaje==888.0000 		| 	UCI2$systriaje==999.0000  		| 	UCI2$systriaje==888 		| 	UCI2$systriaje==999]		<- NA
UCI2$persoucia		[UCI2$persoucia==888.0000 		| 	UCI2$persoucia==999.0000  		| 	UCI2$persoucia==888 		| 	UCI2$persoucia==999]		<- NA
UCI2$internsiva		[UCI2$internsiva==888.0000 		| 	UCI2$internsiva==999.0000  		| 	UCI2$internsiva==888 		| 	UCI2$internsiva==999]		<- NA
UCI2$iru			[UCI2$iru==888.0000 			| 	UCI2$iru==999.0000  			| 	UCI2$iru==888 			| 	UCI2$iru==999]			<- NA
#-----------------------------------------------------------------------------------------------------------------------------------#.


dades<-UCI2
#------------------------------------------------------------------#
conductor_variables<-"taulavariables_v2_UCI.xls"
#------------------------------------------------------------------#

#taulavariables_v2_UCI.xls


LAB_ETIQ_UCI<-LAB_ETIQ_v2(dt=dades,variables_factors=conductor_variables,fulla="etiquetes",idioma="etiqueta1")
LAB_ETIQ_UCI<-convertir_dates(d=LAB_ETIQ_UCI,taulavariables="taulavariables_v2_UCI.xls")
LAB_ETIQ_UCI<-etiquetar(d=LAB_ETIQ_UCI,taulavariables="taulavariables_v2_UCI.xls")



#TAULES!


#names(LAB_ETIQ_PEU_CAT)
#***********************************************************************#
#taula00 Criteris Inclusi?
formula_taula00<-formula_compare("taula00",y="",taulavariables = conductor_variables)
T00<-descrTable(formula_taula00,data=LAB_ETIQ_UCI)
#***********************************************************************#
formula_taula0<-formula_compare("taula0",y="",taulavariables = conductor_variables)
T0<-descrTable(formula_taula0,data=LAB_ETIQ_UCI)
#***********************************************************************#
formula_taula1<-formula_compare("taula1",y="",taulavariables = conductor_variables)
T1<-descrTable(formula_taula1,data=LAB_ETIQ_UCI)
#***********************************************************************#
formula_taula2<-formula_compare("taula2",y="",taulavariables = conductor_variables)
T2<-descrTable(formula_taula2,data=LAB_ETIQ_UCI)
#***********************************************************************#

T00
T0
T1
T2




save(T00,T0,T1,T2,file="UCI.Rdata")




# 2 PART!!

#setwd("C:/Users/38122893W/Desktop/hospital_MAR?")
#"C:/Users/38122893W/Desktop/hospital_MAR?"%>% file.path("FUNCIONS_PROPIES3.R") %>% source()
#----------------------------------------------------



#UCI<- 	read.csv("UCI.csv", header=TRUE,sep=";" , dec=",")






#-------------------------------------------------------------#
#19.07.2019
#-----------------------------------------------#
library("ggplot2")
library("ggpubr")
library("nlme")
#-------------------------------------------------------------#
UCI_FINAL<-	read.csv("ucies_FINAL.csv", header=TRUE,sep=";" , dec=",")
UCI_FINAL
#-------------------------------------------------------------#
#UCI_FINAL<-UCI_FINAL%>%mutate(DIFF1=NUM_PAC_cuap_menys_3km_N4-NUM_PAC_cuap_mes_3km_N4,DIFF2=NUM_PAC_cuap_menys_3km_N5-NUM_PAC_cuap_mes_3km_N5)
#-------------------------------------------------------------#
#> UCI_FINAL
#HOSPITAL ZONA NUM_PAC_cuap_menys_3km_N4 NUM_PAC_cuap_mes_3km_N4 NUM_PAC_cuap_menys_3km_N5 NUM_PAC_cuap_mes_3km_N5
#1      clinic    0                     43722                   28381                      6542                    4884
#2     canruti    0                     30504                   17298                     16728                   10090
#3       tauli    1                     37241                   23309                      2224                    6146
#4      trueta    1                     37960                   21207                     29200                   16743
#5 esperitsant    2                     27144                   13651                     11569                    4331
#6      Altaia    2                     17545                   10266                     10846                    6436
#-------------------------------------------------------------#
print(shapiro.test(UCI_FINAL$NUM_PAC_cuap_menys_3km_N4))
# Shapiro-Wilk normality test
# data:  UCI_FINAL$NUM_PAC_cuap_menys_3km_N4
# W = 0.96379, p-value = 0.8484

print(shapiro.test(UCI_FINAL$NUM_PAC_cuap_mes_3km_N4))
# Shapiro-Wilk normality test
# data:  UCI_FINAL$NUM_PAC_cuap_mes_3km_N4
# W = 0.98615, p-value = 0.9777

print(shapiro.test(UCI_FINAL$NUM_PAC_cuap_menys_3km_N5))
# Shapiro-Wilk normality test
# data:  UCI_FINAL$NUM_PAC_cuap_menys_3km_N5
# W = 0.93208, p-value = 0.5963

print(shapiro.test(UCI_FINAL$NUM_PAC_cuap_mes_3km_N5))
# Shapiro-Wilk normality test
# data:  UCI_FINAL$NUM_PAC_cuap_mes_3km_N5
# W = 0.81637, p-value = 0.08207


#-------------------------------------------------------------#
#acceptem Normalitat, per tant fem test aparellat T-TEST! per PAC/CUAP!
#-------------------------------------------------------------#
P_NUM_PAC_N4<-t.test(x = UCI_FINAL$NUM_PAC_cuap_menys_3km_N4, y = UCI_FINAL$NUM_PAC_cuap_mes_3km_N4, alternative = "two.sided",
       mu = 0, paired = TRUE, conf.level = 0.95)
#-------------------------------------------------------------#
P_NUM_PAC_N5<-t.test(x = UCI_FINAL$NUM_PAC_cuap_menys_3km_N5, y = UCI_FINAL$NUM_PAC_cuap_mes_3km_N5, alternative = "two.sided",
                     mu = 0, paired = TRUE, conf.level = 0.95)
#-------------------------------------------------------------#
P_NUM_PAC_N4
#Paired t-test
#data:  UCI_FINAL$NUM_PAC_cuap_menys_3km_N4 and UCI_FINAL$NUM_PAC_cuap_mes_3km_N4
#t = 10.053, df = 5, p-value = 0.0001667
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  9924.387 16743.613
#sample estimates:
#  mean of the differences 
#13334 

#acceptem difre?ncies!!!

P_NUM_PAC_N5
#Paired t-test
#data:  UCI_FINAL$NUM_PAC_cuap_menys_3km_N5 and UCI_FINAL$NUM_PAC_cuap_mes_3km_N5
#t = 2.0947, df = 5, p-value = 0.09036
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1078.239 10571.239
#sample estimates:
#  mean of the differences 
#4746.5 
#-------------------------------------------------------------#

#no hi ha evidencia que hi hagi difer?ncies , per? la p ?s baixa --> 0.0904!!!!







UCI_FINAL2<-	read.csv("ucies_FINAL2.csv", header=TRUE,sep=";" , dec=",")
#-------------------------------------------------------------#
#HOSPITAL ZONA TEMPS_ESP_PAC_cuap_menys_3km_N4 TEMPS_ESP_NUM_PAC_cuap_mes_3km_N4
#1      clinic    0                              50                                55
#2     canruti    0                               5                                 5
#3       tauli    1                              50                                50
#4      trueta    1                              10                                15
#5 esperitsant    2                               5                                10
#6      Altaia    2                              10                                15
#TEMPS_ESP_NUM_PAC_cuap_menys_3km_N5 TEMPS_ESP_NUM_PAC_cuap_mes_3km_N5
#1                                  60                                65
#2                                  15                                15
#3                                  18                                25
#4                                  15                                15
#5                                  15                                15
#6                                  10                                15
#-------------------------------------------------------------#

#UCI_FINAL2
#UCI_FINAL2<-UCI_FINAL2%>%mutate(DIFF1=TEMPS_ESP_PAC_cuap_menys_3km_N4-TEMPS_ESP_NUM_PAC_cuap_mes_3km_N4,DIFF2=TEMPS_ESP_NUM_PAC_cuap_menys_3km_N5-TEMPS_ESP_NUM_PAC_cuap_mes_3km_N5)
#-------------------------------------------------------------#
print(shapiro.test(UCI_FINAL2$TEMPS_ESP_PAC_cuap_menys_3km_N4))
# Shapiro-Wilk normality test
# data:  UCI_FINAL2$TEMPS_ESP_PAC_cuap_menys_3km_N4
# W = 0.71002, p-value = 0.007869
#-------------------------------------------------------------#
print(shapiro.test(UCI_FINAL2$TEMPS_ESP_NUM_PAC_cuap_mes_3km_N4))
# Shapiro-Wilk normality test
# data:  UCI_FINAL2$TEMPS_ESP_NUM_PAC_cuap_mes_3km_N4
# W = 0.80067, p-value = 0.05959
#-------------------------------------------------------------#
print(shapiro.test(UCI_FINAL2$TEMPS_ESP_NUM_PAC_cuap_menys_3km_N5))
# Shapiro-Wilk normality test
# data:  UCI_FINAL2$TEMPS_ESP_NUM_PAC_cuap_menys_3km_N5
# W = 0.62164, p-value = 0.00083
#-------------------------------------------------------------#
print(shapiro.test(UCI_FINAL2$TEMPS_ESP_NUM_PAC_cuap_mes_3km_N5))
# Shapiro-Wilk normality test
# data:  UCI_FINAL2$TEMPS_ESP_NUM_PAC_cuap_mes_3km_N5
# W = 0.61095, p-value = 0.0006206


#[no accptem NORMALITAT!!!]
#wilcox.test(x = antes, y = despues, alternative = "two.sided", mu = 0, paired = TRUE)

wilcox.test(x = UCI_FINAL2$TEMPS_ESP_PAC_cuap_menys_3km_N4, y = UCI_FINAL2$TEMPS_ESP_NUM_PAC_cuap_mes_3km_N4, alternative = "two.sided", mu = 0,
            paired = TRUE, conf.int = 0.95)
#-------------------------------------------------------------#
#Wilcoxon signed rank test with continuity correction
#data:  UCI_FINAL2$TEMPS_ESP_PAC_cuap_menys_3km_N4 and UCI_FINAL2$TEMPS_ESP_NUM_PAC_cuap_mes_3km_N4
#V = 0, p-value = 0.07186
#-------------------------------------------------------------#

wilcox.test(x = UCI_FINAL2$TEMPS_ESP_NUM_PAC_cuap_menys_3km_N5, y = UCI_FINAL2$TEMPS_ESP_NUM_PAC_cuap_mes_3km_N5, alternative = "two.sided", mu = 0,
            paired = TRUE, conf.int = 0.95)
#-------------------------------------------------------------#
#Wilcoxon signed rank test with continuity correction
#data:  UCI_FINAL2$TEMPS_ESP_NUM_PAC_cuap_menys_3km_N5 and UCI_FINAL2$TEMPS_ESP_NUM_PAC_cuap_mes_3km_N5
#V = 0, p-value = 0.1736
#-------------------------------------------------------------#





UCI_FINAL3<-	read.csv("ucies_FINAL3.csv", header=TRUE,sep=";" , dec=",")
#UCI_FINAL3
#     HOSPITAL ZONA fuguesmenys3 fuguesmes3
#1      clinic    0         0.01        1.0
#2     canruti    0         1.00        1.0
#3       tauli    1         1.80        2.3
#4      trueta    1         1.00        1.3
#5 esperitsant    2         1.00        2.0
#6      Altaia    2         1.00        1.2
#-------------------------------------------------------------#
print(shapiro.test(UCI_FINAL3$fuguesmenys3))
#Shapiro-Wilk normality test
#data:  UCI_FINAL3$fuguesmenys3
#W = 0.82066, p-value = 0.08942
#-------------------------------------------------------------#
print(shapiro.test(UCI_FINAL3$ fuguesmes3))
#Shapiro-Wilk normality test
#data:  UCI_FINAL3$fuguesmes3
#W = 0.83691, p-value = 0.1229
#-------------------------------------------------------------#
#-------------------------------------------------------------#
t.test(x = UCI_FINAL3$fuguesmenys3, y = UCI_FINAL3$fuguesmes3, alternative = "two.sided",
                     mu = 0, paired = TRUE, conf.level = 0.95)

#Paired t-test

#data:  UCI_FINAL3$fuguesmenys3 and UCI_FINAL3$fuguesmes3
#t = -2.9262, df = 5, p-value = 0.03278
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.93610861 -0.06055806
#sample estimates:
#  mean of the differences 
#-0.4983333 




























UCI_4PART<-	read.csv("ucies4.csv", header=TRUE,sep=";" , dec=",")
UCI_4PART
#-------------------------------------------------------------#
#> UCI_4PART
#-------------------------------------------------------------#
#         HOSPITAL      ZONA NIVELL_DIST NUM_PAC_CUAP
#1          clinic      0      menys3        43722
#2          canruti     0      menys3        30504
#3          tauli       1      menys3        37241
#4          trueta      1      menys3        37960
#5          esperitsant 2      menys3        27144
#6          Altaia      2      menys3        17545
#7          clinic      0        mes3        28381
#8          canruti     0        mes3        17298
#9          tauli       1        mes3        23309
#10         trueta      1        mes3        21207
#11         esperitsant 2        mes3        13651
#12         Altaia      2        mes3        10266
#-------------------------------------------------------------#

t.test(x = datos$antes, y = datos$despues, alternative = "two.sided",
       mu = 0, paired = TRUE, conf.level = 0.95)







UCI_5PART<-	read.csv("ucies5.csv", header=TRUE,sep=";" , dec=",")
#-------------------------------------------------------------#
UCI_5PART
#-------------------------------------------------------------#
#           HOSPITAL ZONA NIVELL_DIST NUM_PAC_CUAP
#1          clinic      0      menys3         6542
#2          canruti     0      menys3        16728
#3          tauli       1      menys3         2224
#4          trueta      1      menys3        29200
#5          esperitsant 2      menys3        11569
#6          Altaia      2      menys3        10846
#7          clinic      0        mes3         4884
#8          canruti     0        mes3        10090
#9          tauli       1        mes3         6146
#10         trueta      1        mes3        16743
#11         esperitsant 2        mes3         4331
#12         Altaia      2        mes3         6436
#-------------------------------------------------------------#




#[ucies4.csv][csvucies5.csv]

#x=NIVELL_DIST,y=NUM_PAC_CUAP
#----------------------------------------------------------------------
UCI_4PART<-	read.csv("ucies4.csv", header=TRUE,sep=";" , dec=",")
#----------------------------------------------------------------------
UCI_4PART_MENYS3<-UCI_4PART%>%filter(NIVELL_DIST=="menys3")
print(shapiro.test(UCI_4PART_MENYS3$NUM_PAC_CUAP))
UCI_4PART_MES3<-UCI_4PART%>%filter(NIVELL_DIST=="mes3")
print(shapiro.test(UCI_4PART_MES3$NUM_PAC_CUAP))
#----------------------------------------------------------------------
KK2<-oneway.test(NUM_PAC_CUAP~ NIVELL_DIST,       data = UCI_4PART)
#comp<-compareGroups ( NIVELL_DIST~NUM_PAC_CUAP,method = c(NUM_PAC_CUAP= 2),data=UCI_4PART ,simplify=FALSE)
comp<-compareGroups ( NIVELL_DIST~NUM_PAC_CUAP,method = c(NUM_PAC_CUAP= 1),data=UCI_4PART ,simplify=FALSE)
#----------------------------------------------------------------------
comp2	<-createTable(comp,show.all=TRUE)
comp2b<-update(comp2)
p4 <- ggplot(UCI_4PART, aes(x=NIVELL_DIST,y=NUM_PAC_CUAP)) + geom_boxplot()
p4b<-summary(NUM_PAC_CUAP~NIVELL_DIST, data = UCI_4PART) 
#----------------------------------------------------------------------
UCI_4PART
KK2
comp2b
p4
p4b
#----------------------------------------------------------------------
res4 <- lme(NUM_PAC_CUAP~ NIVELL_DIST, data = UCI_4PART, random = ~ 1|ZONA)
summary(res4 )
#----------------------------------------------------------------------
#
#
#
#----------------------------------------------------------------------
UCI_5PART<-	read.csv("ucies5.csv", header=TRUE,sep=";" , dec=",")
#----------------------------------------------------------------------
UCI_5PART_MENYS3<-UCI_5PART%>%filter(NIVELL_DIST=="menys3")
print(shapiro.test(UCI_5PART_MENYS3$NUM_PAC_CUAP))
UCI_5PART_MES3<-UCI_5PART%>%filter(NIVELL_DIST=="mes3")
print(shapiro.test(UCI_5PART_MES3$NUM_PAC_CUAP))
#----------------------------------------------------------------------
KK2<-oneway.test(NUM_PAC_CUAP~ NIVELL_DIST,       data = UCI_5PART)
#comp<-compareGroups ( NIVELL_DIST~NUM_PAC_CUAP,method = c(NUM_PAC_CUAP= 2),data=UCI_5PART ,simplify=FALSE)
comp<-compareGroups ( NIVELL_DIST~NUM_PAC_CUAP,data=UCI_5PART ,simplify=FALSE)
comp2	<-createTable(comp,show.all=TRUE)
comp2b<-update(comp2)
p5 <- ggplot(UCI_5PART, aes(x=NIVELL_DIST,y=NUM_PAC_CUAP)) + geom_boxplot()
p5b<-summary(NUM_PAC_CUAP~NIVELL_DIST, data = UCI_5PART) 
#----------------------------------------------------------------------
UCI_5PART
KK2
comp2b
p5
p5b
#----------------------------------------------------------------------
res5 <- lme(NUM_PAC_CUAP~ NIVELL_DIST, data = UCI_5PART, random = ~ 1|ZONA)
summary(res5 )
#----------------------------------------------------------------------






#[ucies4A.csv][csvucies5A.csv]

#x=NIVELL_DIST,y=TEMPS_ESPERA_PAC_CUAP
#----------------------------------------------------------------------
UCI_4PART_A<-	read.csv("ucies4A.csv", header=TRUE,sep=";" , dec=",")
#----------------------------------------------------------------------
UCI_4PART_MENYS3_A<-UCI_4PART_A%>%filter(NIVELL_DIST=="menys3")
print(shapiro.test(UCI_4PART_MENYS3_A$TEMPS_ESPERA_PAC_CUAP))
UCI_4PART_MES3_A<-UCI_4PART_A%>%filter(NIVELL_DIST=="mes3")
print(shapiro.test(UCI_4PART_MES3_A$TEMPS_ESPERA_PAC_CUAP))
#----------------------------------------------------------------------
KK2<-oneway.test(TEMPS_ESPERA_PAC_CUAP~ NIVELL_DIST,       data = UCI_4PART_A)
comp<-compareGroups ( NIVELL_DIST~TEMPS_ESPERA_PAC_CUAP,method = c(TEMPS_ESPERA_PAC_CUAP= 2),data=UCI_4PART_A ,simplify=FALSE)
#comp<-compareGroups ( NIVELL_DIST~TEMPS_ESPERA_PAC_CUAP,method = c(TEMPS_ESPERA_PAC_CUAP= 1),data=UCI_4PART_A ,simplify=FALSE)
#----------------------------------------------------------------------
comp2	<-createTable(comp,show.all=TRUE)
comp2b<-update(comp2)
p4 <- ggplot(UCI_4PART_A, aes(x=NIVELL_DIST,y=TEMPS_ESPERA_PAC_CUAP)) + geom_boxplot()
p4b<-summary(TEMPS_ESPERA_PAC_CUAP~NIVELL_DIST, data = UCI_4PART_A) 
#----------------------------------------------------------------------
UCI_4PART_A
KK2
comp2b
p4
p4b
#----------------------------------------------------------------------
res4 <- lme(TEMPS_ESPERA_PAC_CUAP~ NIVELL_DIST, data = UCI_4PART_A, random = ~ 1|ZONA)
summary(res4 )
#----------------------------------------------------------------------
#
#
#
#----------------------------------------------------------------------
UCI_5PART_A<-	read.csv("ucies5A.csv", header=TRUE,sep=";" , dec=",")
#----------------------------------------------------------------------
UCI_5PART_MENYS3_A<-UCI_5PART_A%>%filter(NIVELL_DIST=="menys3")
print(shapiro.test(UCI_5PART_MENYS3_A$TEMPS_ESPERA_PAC_CUAP))
UCI_5PART_MES3_A<-UCI_5PART_A%>%filter(NIVELL_DIST=="mes3")
print(shapiro.test(UCI_5PART_MES3_A$TEMPS_ESPERA_PAC_CUAP))
#----------------------------------------------------------------------
KK2<-oneway.test(TEMPS_ESPERA_PAC_CUAP~ NIVELL_DIST,       data = UCI_5PART_A)
comp<-compareGroups ( NIVELL_DIST~TEMPS_ESPERA_PAC_CUAP,method = c(TEMPS_ESPERA_PAC_CUAP= 2),data=UCI_5PART_A ,simplify=FALSE)
#comp<-compareGroups ( NIVELL_DIST~TEMPS_ESPERA_PAC_CUAP,method = c(TEMPS_ESPERA_PAC_CUAP= 1),data=UCI_5PART_A ,simplify=FALSE)
#----------------------------------------------------------------------
comp2	<-createTable(comp,show.all=TRUE)
comp2b<-update(comp2)
p5 <- ggplot(UCI_5PART_A, aes(x=NIVELL_DIST,y=TEMPS_ESPERA_PAC_CUAP)) + geom_boxplot()
p5b<-summary(TEMPS_ESPERA_PAC_CUAP~NIVELL_DIST, data = UCI_5PART_A) 
#----------------------------------------------------------------------
UCI_5PART_A
KK2
comp2b
p5
p5b
res5 <- lme(TEMPS_ESPERA_PAC_CUAP~ NIVELL_DIST, data = UCI_5PART_A, random = ~ 1|ZONA)
summary(res5 )
#----------------------------------------------------------------------





#["ucies_C.csv"]#

#FUGUES%_PAC_CUAP
#----------------------------------------------------------------------
UCI_4PART_B<-	read.csv("ucies_C.csv", header=TRUE,sep=";" , dec=",")
#----------------------------------------------------------------------
UCI_4PART_B
UCI_4PART_MENYS3_B<-UCI_4PART_B%>%filter(NIVELL_DIST=="menys3")
print(shapiro.test(UCI_4PART_MENYS3_B$FUGUES_PAC_CUAP))
UCI_4PART_MES3_B<-UCI_4PART_B%>%filter(NIVELL_DIST=="mes3")
print(shapiro.test(UCI_4PART_MES3_B$FUGUES_PAC_CUAP))
#----------------------------------------------------------------------
KK2<-oneway.test(FUGUES_PAC_CUAP~ NIVELL_DIST,       data = UCI_4PART_B)
#comp<-compareGroups ( NIVELL_DIST~TEMPS_ESPERA_PAC_CUAP,method = c(TEMPS_ESPERA_PAC_CUAP= 2),data=UCI_4PART_A ,simplify=FALSE)
comp<-compareGroups ( NIVELL_DIST~FUGUES_PAC_CUAP,method = c(FUGUES_PAC_CUAP= 1),data=UCI_4PART_B ,simplify=FALSE)
#----------------------------------------------------------------------
comp2	<-createTable(comp,show.all=TRUE)
comp2b<-update(comp2)
p4 <- ggplot(UCI_4PART_B, aes(x=NIVELL_DIST,y=FUGUES_PAC_CUAP)) + geom_boxplot()
p4b<-summary(FUGUES_PAC_CUAP~NIVELL_DIST, data = UCI_4PART_B) 
#----------------------------------------------------------------------
UCI_4PART_B
KK2
comp2b
p4
p4b
#----------------------------------------------------------------------
res4 <- lme(FUGUES_PAC_CUAP~ NIVELL_DIST, data = UCI_4PART_B, random = ~ 1|ZONA)
summary(res4 )
#----------------------------------------------------------------------
































#	colnames(UCI2) <- c(	
#	"	id	"	,
#	"	centernamecat"	,
#	"	centername	"	,
#	"	localidad	"	,
#	"	localidadcat"	,
#	"	comarca	"	,
#	"	comarcacat	"	,
#	"	tipcenter	"	,
#	"	titucenter	"	,
#	"	activsu	"	,
#	"	superfic	"	,
#	"	difgrave	"	,
#	"	uniobserv	"	,
#	"	unicorta	"	,
#	"	ncamas	"	,
#	"	dependeucia	"	,
#	"	soportecap	"	,
#	"	analisec	"	,
#	"	rx	"	,
#	"	ambulancia	"	,
#	"	percentambula"	,
#	"	uciaslab	"	,
#	"	uciasfinde	"	,
#	"	uciasmorning"	,
#	"	uciasafternoon"	,
#	"	uciasnight	"	,
#	"	turismo	"	,
#	"	incrcapacidad"	,
#	"	incrpersonal"	,
#	"	triaje	"	,
#	"	triaje24	"	,
#	"	systriaje	"	,
#	"	quientria	"	,
#	"	persoucia	"	,
#	"	famy	"	,
#	"	medint	"	,
#	"	anestesia	"	,
#	"	internsiva	"	,
#	"	quirurgi	"	,
#	"	noespecia	"	,
#	"	percentria	"	,
#	"	tasaexitus	"	,
#	"	tasafuga	"	,
#	"	tasaretorno	"	,
#	"	iru	"	,
#	"	nivelprior1	"	,
#	"	nivelprior2	"	,
#	"	nivelprior3	"	,
#	"	nivelprior4	"	,
#	"	nivelprior5	"	,
#	"	aumenpuntos	"	,
#	"	minespera1	"	,
#	"	minespera2	"	,
#	"	minespera3	"	,
#	"	minespera4	"	,
#	"	minespera5	"	,
#	"	personpropio"	,
#	"	experienmin	"	,
#	"	uciasprev	"	,
#	"	nivelantes	"	,
#	"	niveldespues"	,
#	"	existprotocol"	,
#	"	percderiv	"	,
#	"	existconti	"	,
#	"	existreder	"	,
#	"	percreder	"	,
#	"	impacvolum	"	,
#	"	derinv	"	,
#	"	qnderiv	"	,
#	"	mesaterri	"	,
#	"	qnmesaterri	"	,
#	"	sonadeq	"	,
#	"	medsufi	"	,
#	"	mejoramed	"	,
#	"	mejorainf	"	,
#	"	mejorabox	"	,
#	"	mejorarx	"	,
#	"	mejoralab	"	,
#	"	mejoraotr	"	,
#	"	buenuso	"	,
#	"	noxdescono	"	,
#	"	noxnopodra	"	,
#	"	noxcomodo	"	,
#	"	funcmejora	"	,
#	"	sicuap	"	,
#	"	debepoten	"	,
#	"	desconges	"	,
#	"	comentfi	"	,
#	"	tasaingr	"	)
#-----------------------------------------------------------------------------------------------------------------------------------#.
#-----------------------------------------------------------------------------------------------------------------------------------#.
#comprovacio:[]!!!
#-----------------------------------------------------------------------------------------------------------------------------------#.
############
#titucenter#
############
#-----------------------------------------------------------------------------------------------------------------------------------#.


res0<-compareGroups (hosp~
superfic+
difgrave+
uniobserv+
unicorta+
ncamas+
dependeucia+
soportecap+
analisec+
rx+
ambulancia+
percentambula+
uciaslab+
uciasfinde+
uciasmorning+
uciasafternoon+
uciasnight+
turismo+
incrcapacidad+
incrpersonal+
triaje+
triaje24+
systriaje+
quientria+
persoucia+
famy+
medint+
anestesia+
internsiva+
quirurgi+
noespecia+
percentria+
tasaexitus+
tasafuga+
tasaretorno+
iru+
nivelprior1+
nivelprior2+
nivelprior3+
nivelprior4+
nivelprior5+
aumenpuntos+
minespera1+
minespera2+
minespera3+
minespera4+
minespera5+
personpropio+
experienmin+
uciasprev+
nivelantes+
niveldespues+
existprotocol+
percderiv+
existconti+
existreder+
percreder+
impacvolum+
derinv+
qnderiv+
mesaterri+
qnmesaterri+
sonadeq+
medsufi+
mejoramed+
mejorainf+
mejorabox+
mejorarx+
mejoralab+
mejoraotr+
buenuso+
noxdescono+
noxnopodra+
noxcomodo+
funcmejora+
sicuap+
debepoten+
desconges+
comentfi+
tasaingr,
method=c(superfic		=NA,	
difgrave		=NA,		
uniobserv		=NA,	
unicorta		=NA,
ncamas		=NA,
dependeucia 	=NA,
soportecap		=NA,
analisec		=NA,
rx			=NA,
ambulancia		=NA,
percentambula	=NA,
uciaslab		=NA,
uciasfinde		=NA,
uciasmorning	=NA,
uciasafternoon	=NA,
uciasnight		=NA,
turismo		=NA,
incrcapacidad	=NA,
incrpersonal	=NA,
triaje		=NA,
triaje24		=NA,
systriaje		=NA,
quientria		=NA,
persoucia		=NA,
famy			=2,
medint		=2,
anestesia		=2,
internsiva		=2,
quirurgi		=2,
noespecia		=2,
percentria		=2,
tasaexitus		=NA,
tasafuga		=NA,
tasaretorno		=NA,
iru			=NA,
nivelprior1		=NA,
nivelprior2		=NA,
nivelprior3		=NA,
nivelprior4		=NA,
nivelprior5		=NA,
aumenpuntos		=NA,
minespera1		=NA,
minespera2		=NA,
minespera3		=NA,
minespera4		=NA,
minespera5		=NA,
personpropio	=NA,
experienmin		=NA,
uciasprev		=NA,
nivelantes		=2,
niveldespues	=2,
existprotocol	=NA,
percderiv		=2,
existconti		=NA,
existreder		=NA,
percreder		=2,
impacvolum		=2,
derinv		=NA,
qnderiv		=NA,
mesaterri		=NA,
qnmesaterri		=NA,
sonadeq		=NA,
medsufi		=NA,
mejoramed		=NA,
mejorainf		=NA,
mejorabox		=NA,
mejorarx		=NA,
mejoralab		=NA,
mejoraotr		=NA,
buenuso		=NA,
noxdescono		=NA,
noxnopodra		=NA,
noxcomodo		=NA,
funcmejora		=NA,
sicuap		=NA,	
debepoten		=NA,
desconges		=NA,
comentfi		=NA), 
data=UCI2 ,simplify=FALSE)
#-----------------------------------------------------------------------------------------------------------------------------------#.
#res0
#summary(res0)
#-----------------------------------------------------------------------------------------------------------------------------------#.
kk0		<-createTable(res0,show.all=TRUE)
restab0	<-update(kk0,hide="888")
#-----------------------------------------------------------------------------------------------------------------------------------#.
restab0
#-----------------------------------------------------------------------------------------------------------------------------------#.
print(restab0,which.table="avail")
#-----------------------------------------------------------------------------------------------------------------------------------#.












#-----------------------------------------------------------------------------------------------------------------------------------#.
############
##activsu  #
############
#-----------------------------------------------------------------------------------------------------------------------------------#.
#-----------------------------------------------------------------------------------------------------------------------------------#.
res3<-compareGroups (activsu~
superfic+
difgrave+
uniobserv+
unicorta+
ncamas+
dependeucia+
soportecap+
analisec+
rx+
ambulancia+
percentambula+
uciaslab+
uciasfinde+
uciasmorning+
uciasafternoon+
uciasnight+
turismo+
incrcapacidad+
incrpersonal+
triaje+
triaje24+
systriaje+
quientria+
persoucia+
famy+
medint+
anestesia+
internsiva+
quirurgi+
noespecia+
percentria+
tasaexitus+
tasafuga+
tasaretorno+
iru+
nivelprior1+
nivelprior2+
nivelprior3+
nivelprior4+
nivelprior5+
aumenpuntos+
minespera1+
minespera2+
minespera3+
minespera4+
minespera5+
personpropio+
experienmin+
uciasprev+
nivelantes+
niveldespues+
existprotocol+
percderiv+
existconti+
existreder+
percreder+
impacvolum+
derinv+
qnderiv+
mesaterri+
qnmesaterri+
sonadeq+
medsufi+
mejoramed+
mejorainf+
mejorabox+
mejorarx+
mejoralab+
mejoraotr+
buenuso+
noxdescono+
noxnopodra+
noxcomodo+
funcmejora+
sicuap+
debepoten+
desconges+
comentfi+
tasaingr,
method=c(superfic		=NA,	
difgrave		=NA,		
uniobserv		=NA,	
unicorta		=NA,
ncamas		=NA,
dependeucia 	=NA,
soportecap		=NA,
analisec		=NA,
rx			=NA,
ambulancia		=NA,
percentambula	=NA,
uciaslab		=NA,
uciasfinde		=NA,
uciasmorning	=NA,
uciasafternoon	=NA,
uciasnight		=NA,
turismo		=NA,
incrcapacidad	=NA,
incrpersonal	=NA,
triaje		=NA,
triaje24		=NA,
systriaje		=NA,
quientria		=NA,
persoucia		=NA,
famy			=2,
medint		=2,
anestesia		=2,
internsiva		=2,
quirurgi		=2,
noespecia		=2,
percentria		=2,
tasaexitus		=NA,
tasafuga		=NA,
tasaretorno		=NA,
iru			=NA,
nivelprior1		=NA,
nivelprior2		=NA,
nivelprior3		=NA,
nivelprior4		=NA,
nivelprior5		=NA,
aumenpuntos		=NA,
minespera1		=NA,
minespera2		=NA,
minespera3		=NA,
minespera4		=NA,
minespera5		=NA,
personpropio	=NA,
experienmin		=NA,
uciasprev		=NA,
nivelantes		=2,
niveldespues	=2,
existprotocol	=NA,
percderiv		=2,
existconti		=NA,
existreder		=NA,
percreder		=2,
impacvolum		=2,
derinv		=NA,
qnderiv		=NA,
mesaterri		=NA,
qnmesaterri		=NA,
sonadeq		=NA,
medsufi		=NA,
mejoramed		=NA,
mejorainf		=NA,
mejorabox		=NA,
mejorarx		=NA,
mejoralab		=NA,
mejoraotr		=NA,
buenuso		=NA,
noxdescono		=NA,
noxnopodra		=NA,
noxcomodo		=NA,
funcmejora		=NA,
sicuap		=NA,	
debepoten		=NA,
desconges		=NA,
comentfi		=NA), 
data=UCI2 ,simplify=FALSE)
#-----------------------------------------------------------------------------------------------------------------------------------#.
#res3
#summary(res3)
#-----------------------------------------------------------------------------------------------------------------------------------#.
kk3		<-createTable(res3,show.all=TRUE)
restab3	<-update(kk3,hide="888")
#-----------------------------------------------------------------------------------------------------------------------------------#.
restab3
#-----------------------------------------------------------------------------------------------------------------------------------#.
print(restab3,which.table="avail")
#-----------------------------------------------------------------------------------------------------------------------------------#.




#-----------------------------------------------------------------------------------------------------------------------------------#.
############
#titucenter#
############
#-----------------------------------------------------------------------------------------------------------------------------------#.
res4<-compareGroups (titucenter~
superfic+
difgrave+
uniobserv+
unicorta+
ncamas+
dependeucia+
soportecap+
analisec+
rx+
ambulancia+
percentambula+
uciaslab+
uciasfinde+
uciasmorning+
uciasafternoon+
uciasnight+
turismo+
incrcapacidad+
incrpersonal+
triaje+
triaje24+
systriaje+
quientria+
persoucia+
famy+
medint+
anestesia+
internsiva+
quirurgi+
noespecia+
percentria+
tasaexitus+
tasafuga+
tasaretorno+
iru+
nivelprior1+
nivelprior2+
nivelprior3+
nivelprior4+
nivelprior5+
aumenpuntos+
minespera1+
minespera2+
minespera3+
minespera4+
minespera5+
personpropio+
experienmin+
uciasprev+
nivelantes+
niveldespues+
existprotocol+
percderiv+
existconti+
existreder+
percreder+
impacvolum+
derinv+
qnderiv+
mesaterri+
qnmesaterri+
sonadeq+
medsufi+
mejoramed+
mejorainf+
mejorabox+
mejorarx+
mejoralab+
mejoraotr+
buenuso+
noxdescono+
noxnopodra+
noxcomodo+
funcmejora+
sicuap+
debepoten+
desconges+
comentfi+
tasaingr,
method=c(superfic		=NA,	
difgrave		=NA,		
uniobserv		=NA,	
unicorta		=NA,
ncamas		=NA,
dependeucia 	=NA,
soportecap		=NA,
analisec		=NA,
rx			=NA,
ambulancia		=NA,
percentambula	=NA,
uciaslab		=NA,
uciasfinde		=NA,
uciasmorning	=NA,
uciasafternoon	=NA,
uciasnight		=NA,
turismo		=NA,
incrcapacidad	=NA,
incrpersonal	=NA,
triaje		=NA,
triaje24		=NA,
systriaje		=NA,
quientria		=NA,
persoucia		=NA,
famy			=2,
medint		=2,
anestesia		=2,
internsiva		=2,
quirurgi		=2,
noespecia		=2,
percentria		=2,
tasaexitus		=NA,
tasafuga		=NA,
tasaretorno		=NA,
iru			=NA,
nivelprior1		=NA,
nivelprior2		=NA,
nivelprior3		=NA,
nivelprior4		=NA,
nivelprior5		=NA,
aumenpuntos		=NA,
minespera1		=NA,
minespera2		=NA,
minespera3		=NA,
minespera4		=NA,
minespera5		=NA,
personpropio	=NA,
experienmin		=NA,
uciasprev		=NA,
nivelantes		=2,
niveldespues	=2,
existprotocol	=NA,
percderiv		=2,
existconti		=NA,
existreder		=NA,
percreder		=2,
impacvolum		=2,
derinv		=NA,
qnderiv		=NA,
mesaterri		=NA,
qnmesaterri		=NA,
sonadeq		=NA,
medsufi		=NA,
mejoramed		=NA,
mejorainf		=NA,
mejorabox		=NA,
mejorarx		=NA,
mejoralab		=NA,
mejoraotr		=NA,
buenuso		=NA,
noxdescono		=NA,
noxnopodra		=NA,
noxcomodo		=NA,
funcmejora		=NA,
sicuap		=NA,	
debepoten		=NA,
desconges		=NA,
comentfi		=NA), 
data=UCI2 ,simplify=FALSE)
#-----------------------------------------------------------------------------------------------------------------------------------#.
#res4
#summary(res4)
#-----------------------------------------------------------------------------------------------------------------------------------#.
kk4		<-createTable(res4,show.all=TRUE)
restab4	<-update(kk4,hide="888")
#-----------------------------------------------------------------------------------------------------------------------------------#.
restab4
#-----------------------------------------------------------------------------------------------------------------------------------#.
#-----------------------------------------------------------------------------------------------------------------------------------#.
print(restab4,which.table="avail")
#-----------------------------------------------------------------------------------------------------------------------------------#.

# no l'he posat qnderiv+


#-----------------------------------------------------------------------------------------------------------------------------------#.
############
#tipcenter#
############
#-----------------------------------------------------------------------------------------------------------------------------------#.
res5<-compareGroups (tipcenter~
superfic+
difgrave+
uniobserv+
unicorta+
ncamas+
dependeucia+
soportecap+
analisec+
rx+
ambulancia+
percentambula+
uciaslab+
uciasfinde+
uciasmorning+
uciasafternoon+
uciasnight+
turismo+
incrcapacidad+
incrpersonal+
triaje+
triaje24+
systriaje+
quientria+
persoucia+
famy+
medint+
anestesia+
internsiva+
quirurgi+
noespecia+
percentria+
tasaexitus+
tasafuga+
tasaretorno+
iru+
nivelprior1+
nivelprior2+
nivelprior3+
nivelprior4+
nivelprior5+
aumenpuntos+
minespera1+
minespera2+
minespera3+
minespera4+
minespera5+
personpropio+
experienmin+
uciasprev+
nivelantes+
niveldespues+
existprotocol+
percderiv+
existconti+
existreder+
percreder+
impacvolum+
derinv+
mesaterri+
qnmesaterri+
sonadeq+
medsufi+
mejoramed+
mejorainf+
mejorabox+
mejorarx+
mejoralab+
mejoraotr+
buenuso+
noxdescono+
noxnopodra+
noxcomodo+
funcmejora+
sicuap+
debepoten+
desconges+
comentfi+
tasaingr,
method=c(superfic		=NA,	
difgrave		=NA,		
uniobserv		=NA,	
unicorta		=NA,
ncamas		=NA,
dependeucia 	=NA,
soportecap		=NA,
analisec		=NA,
rx			=NA,
ambulancia		=NA,
percentambula	=NA,
uciaslab		=NA,
uciasfinde		=NA,
uciasmorning	=NA,
uciasafternoon	=NA,
uciasnight		=NA,
turismo		=NA,
incrcapacidad	=NA,
incrpersonal	=NA,
triaje		=NA,
triaje24		=NA,
systriaje		=NA,
quientria		=NA,
persoucia		=NA,
famy			=2,
medint		=2,
anestesia		=2,
internsiva		=2,
quirurgi		=2,
noespecia		=2,
percentria		=2,
tasaexitus		=NA,
tasafuga		=NA,
tasaretorno		=NA,
iru			=NA,
nivelprior1		=NA,
nivelprior2		=NA,
nivelprior3		=NA,
nivelprior4		=NA,
nivelprior5		=NA,
aumenpuntos		=NA,
minespera1		=NA,
minespera2		=NA,
minespera3		=NA,
minespera4		=NA,
minespera5		=NA,
personpropio	=NA,
experienmin		=NA,
uciasprev		=NA,
nivelantes		=2,
niveldespues	=2,
existprotocol	=NA,
percderiv		=2,
existconti		=NA,
existreder		=NA,
percreder		=2,
impacvolum		=2,
derinv		=NA,
mesaterri		=NA,
qnmesaterri		=NA,
sonadeq		=NA,
medsufi		=NA,
mejoramed		=NA,
mejorainf		=NA,
mejorabox		=NA,
mejorarx		=NA,
mejoralab		=NA,
mejoraotr		=NA,
buenuso		=NA,
noxdescono		=NA,
noxnopodra		=NA,
noxcomodo		=NA,
funcmejora		=NA,
sicuap		=NA,	
debepoten		=NA,
desconges		=NA,
comentfi		=NA), 
data=UCI2 ,simplify=FALSE)
#-----------------------------------------------------------------------------------------------------------------------------------#.
#res5
#summary(res5)
#-----------------------------------------------------------------------------------------------------------------------------------#.
kk5		<-createTable(res5,show.all=TRUE)
restab5	<-update(kk5,hide="888")
#-----------------------------------------------------------------------------------------------------------------------------------#.
restab5
#-----------------------------------------------------------------------------------------------------------------------------------#.
#-----------------------------------------------------------------------------------------------------------------------------------#.
print(restab5,which.table="avail")
#-----------------------------------------------------------------------------------------------------------------------------------#.









# feina:[12.03.2018]
#-----------------------------------------------------------------------------------------------------------------------------------#.
#i) 	 [No pacs]		   			UNICURTA 	/	[taxa.ING,taxa.Exitus,Taxa.Fuga,Taxa.Retorn]
#ii)	 [Nomes Hospitals]			IRU	   	/	[taxa.ING,taxa.Exitus,Taxa.Fuga,Taxa.Retorn]
#iii)	 [Nomes Hospitals]			IRU	   	/	[(nivelprior1,nivelprior2,nivelprior3)GREUS,(nivelprior4,nivelprior5)NO_GREUS]
#iv)	 [all]					PAT/No_PAT	/	[taxa.ING,taxa.Exitus,Taxa.Fuga,Taxa.Retorn]
#-----------------------------------------------------------------------------------------------------------------------------------#.




#i) 	 [No pacs]		   			UNICURTA 	/	[taxa.ING,taxa.Exitus,Taxa.Fuga,Taxa.Retorn]


#UNICORTA	<> [tasaexitus,tasafuga,tasaretorno,tasaingr	] 
#-----------------------------------------------------------------------------------------------------------------------------------#.
res6<-compareGroups (unicorta~
tasaexitus+
tasafuga+
tasaretorno+
tasaingr,
method=c(
tasaexitus		=NA,
tasafuga		=NA,
tasaretorno		=NA,
tasaingr		=NA), 
data=UCI2 ,simplify=FALSE)
#-----------------------------------------------------------------------------------------------------------------------------------#.
#res6
#summary(res6)
#-----------------------------------------------------------------------------------------------------------------------------------#.
kk6		<-createTable(res6,show.all=TRUE)
restab6	<-update(kk6,hide="888")
#-----------------------------------------------------------------------------------------------------------------------------------#.
restab6
#-----------------------------------------------------------------------------------------------------------------------------------#.
#-----------------------------------------------------------------------------------------------------------------------------------#.
print(restab6,which.table="avail")
#-----------------------------------------------------------------------------------------------------------------------------------#.




















# ERROR######

# [FER-HO DEM?!!!!!]


#ii
#IRU	<> [tasaexitus,tasafuga,tasaretorno,tasaingr	] 
#-----------------------------------------------------------------------------------------------------------------------------------#.
res7<-compareGroups (iru~
tasaexitus+
tasafuga+
tasaretorno+
tasaingr,
method=c(
tasaexitus		=NA,
tasafuga		=NA,
tasaretorno		=NA,
tasaingr		=NA), 
data=UCI2 ,simplify=FALSE)
#-----------------------------------------------------------------------------------------------------------------------------------#.
#res7
#summary(res7)
#-----------------------------------------------------------------------------------------------------------------------------------#.
kk7		<-createTable(res7,show.all=TRUE)
restab7	<-update(kk7,hide="888")
#-----------------------------------------------------------------------------------------------------------------------------------#.
restab7
#-----------------------------------------------------------------------------------------------------------------------------------#.



#iii
#UNICORTA<> [tasaexitus,tasafuga,tasaretorno,tasaingr	] 

UCI2$nivelprior123						<- UCI2$nivelprior1+UCI2$nivelprior2+UCI2$nivelprior3
UCI2$nivelprior45							<- UCI2$nivelprior4+UCI2$nivelprior5


#-----------------------------------------------------------------------------------------------------------------------------------#.
res8<-compareGroups (iru~nivelprior123+nivelprior45	,
method=c(
nivelprior123		=NA,
nivelprior45		=NA), 
data=UCI2 ,simplify=FALSE)

#-----------------------------------------------------------------------------------------------------------------------------------#.
#res8
#summary(res8)
#-----------------------------------------------------------------------------------------------------------------------------------#.
kk8		<-createTable(res8,show.all=TRUE)
restab7	<-update(kk8,hide="888")
#-----------------------------------------------------------------------------------------------------------------------------------#.
restab8
#-----------------------------------------------------------------------------------------------------------------------------------#.





#-----------------------------------------------------------------------------------------------------------------------------------#.
restab3
restab4
restab5
#-----------------------------------------------------------------------------------------------------------------------------------#.

#-----------------------------------------------------------------------------------------------------------------------------------#.
#i) 	 [No pacs]		   			UNICURTA 	/	[taxa.ING,taxa.Exitus,Taxa.Fuga,Taxa.Retorn]

#SPSS!!!!
#ii)	 [Nomes Hospitals]			IRU	   	/	[taxa.ING,taxa.Exitus,Taxa.Fuga,Taxa.Retorn]
#iii)	 [Nomes Hospitals]			IRU	   	/	[(nivelprior1,nivelprior2,nivelprior3)GREUS,(nivelprior4,nivelprior5)NO_GREUS]


#iv)	 [all]					PAT/No_PAT	/	[taxa.ING,taxa.Exitus,Taxa.Fuga,Taxa.Retorn]
#-----------------------------------------------------------------------------------------------------------------------------------#.




#triaje			<> [tasaexitus,tasafuga,tasaretorno,tasaingr	] 
#-----------------------------------------------------------------------------------------------------------------------------------#.
res7<-compareGroups (triaje ~
tasaexitus+
tasafuga+
tasaretorno+
tasaingr,
method=c(
tasaexitus		=NA,
tasafuga		=NA,
tasaretorno		=NA,
tasaingr		=NA), 
data=UCI2 ,simplify=FALSE)
#-----------------------------------------------------------------------------------------------------------------------------------#.
#res7
#summary(res7)
#-----------------------------------------------------------------------------------------------------------------------------------#.
kk7		<-createTable(res7,show.all=TRUE)
restab7	<-update(kk7,hide="888")
#-----------------------------------------------------------------------------------------------------------------------------------#.
restab7
#-----------------------------------------------------------------------------------------------------------------------------------#.





#-----------------------------------------------------------------------------------------------------------------------------------#.
res7a<-compareGroups (triaje ~
tasaexitus,
method=c(
tasaexitus		=NA), 
data=UCI2 ,simplify=FALSE)
#-----------------------------------------------------------------------------------------------------------------------------------#.
#res7a
#summary(res7a)
#-----------------------------------------------------------------------------------------------------------------------------------#.
kk7a		<-createTable(res7a,show.all=TRUE)
restab7a	<-update(kk7a,hide="888")
#-----------------------------------------------------------------------------------------------------------------------------------#.
restab7a

#-----------------------------------------------------------------------------------------------------------------------------------#.
res7b<-compareGroups (triaje ~
tasafuga,
method=c(
tasafuga		=NA), 
data=UCI2 ,simplify=FALSE)
#-----------------------------------------------------------------------------------------------------------------------------------#.
#res7b
#summary(res7b)
#-----------------------------------------------------------------------------------------------------------------------------------#.
kk7b		<-createTable(res7b,show.all=TRUE)
restab7b	<-update(kk7b,hide="888")
#-----------------------------------------------------------------------------------------------------------------------------------#.
restab7b
#-----------------------------------------------------------------------------------------------------------------------------------#.

res7c<-compareGroups (triaje ~
tasaretorno
,
method=c(
tasaretorno		=NA), 
data=UCI2 ,simplify=FALSE)
#-----------------------------------------------------------------------------------------------------------------------------------#.
#res7c
#summary(res7c)
#-----------------------------------------------------------------------------------------------------------------------------------#.
kk7c		<-createTable(res7c,show.all=TRUE)
restab7c	<-update(kk7c,hide="888")
#-----------------------------------------------------------------------------------------------------------------------------------#.
restab7c
#-----------------------------------------------------------------------------------------------------------------------------------#.



#triaje			<> [tasaexitus,tasafuga,tasaretorno,tasaingr	] 
#-----------------------------------------------------------------------------------------------------------------------------------#.
res7d<-compareGroups (triaje ~
tasaingr,
method=c(tasaingr		=NA), 
data=UCI2 ,simplify=FALSE)
#-----------------------------------------------------------------------------------------------------------------------------------#.
#res7d
#summary(res7d)
#-----------------------------------------------------------------------------------------------------------------------------------#.
kk7d		<-createTable(res7d,show.all=TRUE)
restab7d	<-update(kk7d,hide="888")
#-----------------------------------------------------------------------------------------------------------------------------------#.
restab7d
#-----------------------------------------------------------------------------------------------------------------------------------#.






















#-----------------------------------------------------------------------------------------------------------------------------------#.
#res <- createTable(compareGroups(year ~ . -id, regicor), hide = c(sex=1), hide.no = 'no')
#export2pdf(restab,file='TABLA.pdf', size="small"
#pdf(restab, file=kkk.pdf)
#export2pdf(restab, kkk, compile = TRUE)
#-----------------------------------------------------------------------------------------------------------------------------------#.
#Observem..............
#-----------------------------------------------------------------------------------------------------------------------------------#.
#-----------------------------------------------------------------------------------------------------------------------------------#.
#1  id             25 continuous normal ALL      
#2  centernamecat  25 continuous normal ALL      
#3  localidadcat   25 continuous normal ALL      
#4  comarcacat     25 continuous normal ALL      
#5  tipcenter      25 continuous normal ALL      
#6  titucenter     25 continuous normal ALL      
#7  activsu        25 continuous normal ALL      
#8  superfic       25 continuous normal ALL      
#9  difgrave       25 continuous normal ALL      
#10 uniobserv      25 continuous normal ALL      
#11 unicorta       25 continuous normal ALL      
#12 ncamas         25 continuous normal ALL      
#13 dependeucia    25 continuous normal ALL      
#14 soportecap     25 continuous normal ALL      
#15 analisec       25 continuous normal ALL      
#16 rx             25 continuous normal ALL      
#17 ambulancia     25 continuous normal ALL      
#18 percentambula  25 continuous normal ALL      
#19 uciaslab       25 continuous normal ALL      
#20 uciasfinde     25 continuous normal ALL      
#21 uciasmorning   25 continuous normal ALL      
#22 uciasafternoon 25 continuous normal ALL      
#23 uciasnight     25 continuous normal ALL      
#24 turismo        25 continuous normal ALL      
#25 incrcapacidad  25 continuous normal ALL      
#26 incrpersonal   25 continuous normal ALL      
#27 triaje         25 continuous normal ALL      
#28 triaje24       25 continuous normal ALL      
#29 systriaje      25 continuous normal ALL      
#30 quientria      25 continuous normal ALL      
#31 persoucia      25 continuous normal ALL      
#32 famy           25 continuous normal ALL      
#33 medint         25 continuous normal ALL      
#34 anestesia      25 continuous normal ALL      
#35 internsiva     25 continuous normal ALL      
#36 quirurgi       25 continuous normal ALL      
#37 noespecia      25 continuous normal ALL      
#38 percentria     25 continuous normal ALL      
#39 tasaexitus     25 continuous normal ALL      
#40 tasafuga       25 continuous normal ALL      
#41 tasaretorno    25 continuous normal ALL      
#42 iru            25 continuous normal ALL      
#43 nivelprior1    25 continuous normal ALL      
#44 nivelprior2    25 continuous normal ALL      
#45 nivelprior3    25 continuous normal ALL      
#46 nivelprior4    25 continuous normal ALL      
#47 nivelprior5    25 continuous normal ALL      
#48 aumenpuntos    25 continuous normal ALL      
#49 minespera1     25 continuous normal ALL      
#50 minespera2     25 continuous normal ALL      
#51 minespera3     25 continuous normal ALL      
#52 minespera4     25 continuous normal ALL      
#53 minespera5     25 continuous normal ALL      
#54 personpropio   25 continuous normal ALL      
#55 experienmin    25 continuous normal ALL      
#56 uciasprev      25 continuous normal ALL      
#57 nivelantes     25 continuous normal ALL      
#58 niveldespues   25 continuous normal ALL      
#59 existprotocol  25 continuous normal ALL      
#60 percderiv      25 continuous normal ALL      
#61 existconti     25 continuous normal ALL      
#62 existreder     25 continuous normal ALL      
#63 percreder      25 continuous normal ALL      
#64 impacvolum     25 continuous normal ALL      
#65 derinv         25 continuous normal ALL      
#66 qnderiv        25 continuous normal ALL      
#67 mesaterri      25 continuous normal ALL      
#68 qnmesaterri    25 continuous normal ALL      
#69 sonadeq        25 continuous normal ALL      
#70 medsufi        25 continuous normal ALL      
#71 mejoramed      25 continuous normal ALL      
#72 mejorainf      25 continuous normal ALL      
#73 mejorabox      25 continuous normal ALL      
#74 mejorarx       25 continuous normal ALL      
#75 mejoralab      25 continuous normal ALL      
#76 mejoraotr      25 continuous normal ALL      
#77 buenuso        25 continuous normal ALL      
#78 noxdescono     25 continuous normal ALL      
#79 noxnopodra     25 continuous normal ALL      
#80 noxcomodo      25 continuous normal ALL      
#81 funcmejora     25 continuous normal ALL      
#82 sicuap         25 continuous normal ALL      
#83 debepoten      25 continuous normal ALL      
#84 desconges      25 continuous normal ALL      
#85 comentfi       25 continuous normal ALL      
#86 tasaingr       25 continuous normal ALL  
#-----------------------------------------------------------------------------------------------------------------------------------#.




