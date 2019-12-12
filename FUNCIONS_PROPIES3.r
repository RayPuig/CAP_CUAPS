#26.3.2019
######################################
#install.packages ("data.table")
#install.packages ("SNPassoc")
#install.packages ("htmlwidgets")
#install.packages ("compareGroups")
#install.packages ("foreign")
#install.packages ("lattice")
#install.packages ("Hmisc")
# library("ggplot2")
#install.packages ("pander")
#install.packages ("readxl")
#install.packages ("rmarkdown")
#install.packages ("knitr")
#install.packages ("data.table")
#install.packages ("MatchIt")
#install.packages ("survival")
# library("plyr")
#install.packages ("dplyr")
#install.packages ("survminer")
#install.packages ("purrr")
#install.packages ("stringr")
# library("tidyverse")
#install.packages ("tidyr")
#install.packages ("purrr")
######################################
#18.12.2018
#
#######################       CODI DE FUNCIONS PROPIES     -------------

library("data.table")
library("SNPassoc")
library("htmlwidgets")
library("compareGroups")
library("foreign")
library("lattice")
library("Hmisc")
## library("ggplot2")
library("pander")
library("readxl")
library("rmarkdown")
library("knitr")
library("data.table")
library("MatchIt")
library("survival")
## library("plyr")
library("dplyr")
library("survminer")
library("purrr")
library("stringr")
## library("tidyverse")
library("tidyr")
library("purrr")


#
#
#  Canviar directori de treball subdirectori ------------

directori_treball<-function(subdirectori,directori) {
  
  # directori=c("C:/Users/Jordi/Google Drive",
  #              "C:/Users/usuari/Google Drive",
  #              "C:/Users/43728088M/Google Drive",
  #              "C:/Users/jreal/Google Drive",
  #              "D:/Google Drive")
  # subdirectori="CIBERDEM/GEDAPS/Cohort_RD"
  
  # directori[file.exists(directori)] %>% 
  
    pp<-file.path(directori[file.exists(directori)],subdirectori)
    setwd(pp)
  

}


#
#  Etiquetar les variables de les dades      #####
###
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


# dades<-etiquetar(dades)
# dades<-etiquetar(dades,"variables_R.xls")

#  Genera FORMULA A PARTIR DE VARIABLES----------------------
#####       hi envio la columna de variables amb que vull generar la formula pel compare
formula=function(x="taula1",y="grup",eliminar=c("idp",y)) {
  pepito<-paste("as.vector(variables[variables$",x,"==1,]$camp)[!as.vector(variables[variables$",x,"==1,]$camp)%in%eliminar]",sep="")
  llistataula<-eval(parse(text=pepito))
  y<-as.formula(paste(y, paste(llistataula, collapse=" + "), sep=" ~ "))
}

####   FORMULA MILLORADA --------------------------
#
# Te en compte l'Ordre que està posada en el conductor taulavariables
#
#
formula_compare=function(x="taula1",y="grup",elimina=c("IDP"),taulavariables="variables_R.xls") {
  
  # x="T1_GSK"
  # y="."
  # taulavariables ="VARIABLES.xls"
  # elimina=c("IDP")
  
  
  # 1. Llegir conductor analisis 
  
  variables <- readxl::read_excel(taulavariables)
  variables[is.na(variables)]<- 0
  
  # 2. DATA table filtrar ordenar llista de camps
  polio<-data.table::data.table(variables)

  mua<-polio[camp!=elimina] %>% 
    dplyr::filter_(paste0(x,">0")) %>% 
    dplyr::arrange_(x) %>% 
    dplyr::select(camp) %>% as.vector()
  
  # 3. Generar formula
  
  # y<-as.formula(paste(y, paste(llista$camp, collapse=" + "), sep=" ~ "))
  
  y<-as.formula(paste(y, paste(mua$camp, collapse=" + "), sep=" ~ "))
  
  y
  
}

#  Llistat de taules a partir de Llista de factors de Y's i em retorna una llista de taules -----
llista.compare.Ys<-function(dt=dades,llista.y=c("CODGLP1","CKDEPI_cat2"),llista.x=c("canvi612.pes.perc","canvi612M.pes"),show.ratio=F,byrow=T,show.n=T,show.all=T,show.descr=T,digits=NA,digits.ratio=NA,hide.no = c('NA','No'),ref.no=NA){
  
  # dt=dt.matched
  # llista.y = c("event")
  # llista.x=llistaPS
  # show.ratio=F
  # byrow=T
  
  restab.llista<-list()
  
  # 3. Generar formula
  
  for (i in 1:length(llista.y)) {
    
    # i<-1
    
    restab.llista[[i]]<-as.formula(paste(llista.y[[i]], paste(llista.x, collapse=" + "), sep=" ~ ")) %>% 
      compareGroups(data=dt,include.miss = F,include.label=T,byrow = byrow,ref.no=ref.no) %>% 
      createTable(show.ratio = show.ratio , hide.no = hide.no, show.p.overall=T,show.n=show.n,show.all=show.all,show.descr=show.descr,digits=digits,digits.ratio=digits.ratio)
    
  }
  
  restab.llista
  
}


#  Selector de Variables      -------
#
selectorvariables=function(taula="table1",taulavariables="variables_R.xls",dt=dadestotal) {
  
  # taula = "Total"
  # taulavariables = "variables.xls"
  # dt=dadestotal
  
  vector_variables<-extreure.variables(taula=taula,taulavariables = taulavariables)
  
  moco<-dt %>% dplyr::select(vector_variables)
  
  moco
  

}


#  Selector de variables TAULA DE--------
#
extreure.variables=function(taula="table1",taulavariables="variables_R.xls") {
  
  # taula="dates"
  # taulavariables="variables_R.xls"
  
  # taula="T2.GSK"
  # taulavariables = "VARIABLES.xls"
  
  
  ####  Llegir etiquetes i variables a analitzar ####
  variables <- data.frame(readxl::read_excel(taulavariables))
  variables[is.na(variables)]<- 0
  
  # filtratge 
  kk<-variables %>% dplyr::filter_(paste0(taula,">=1")) %>% dplyr::select(camp) %>% as.vector()
  kk<-as.vector(kk[[1]])
  
}


##########      factoritzar NO.Yes llista de variables "factor" situades a la taulavariables camp=factor
factoritzar.NO.YES<-function(dt=dadesDF,columna="factor",taulavariables="ETIQUETES_VARIABLES_V2.xls"){
  
  ###   Extreure variables  
  x<-extreure.variables(taula=columna,taulavariables=taulavariables) 
  
  ###   Factoritzar-les
  dt[x]<-lapply(dt[x],function(y) factor(y, labels=c("No","Yes")))
  
  dt
  
}


#  Recodifico EN FUNCIÓ DE UN CAMP -------------------
### RETORNA DADES AMB RECODIFICACIÓ 

recodificar<-function(dt=dades,taulavariables="VARIABLES.xls",criteris="recode1"){
  
  # dt=dadesDM
  # taulavariables="VARIABLES.xls"
  # criteris="recode1"
  
  # dt=dadestotal
  # taulavariables = conductor_variables
  # criteris="recode1"
  
  ##  Llegeix criteris de variables 
  variables <- readxl::read_excel(taulavariables)
  variables[is.na(variables)]<- 0
  
  ##  0. Filtro taula variables només variables implicades en el filtre i el genero 
  maco<-variables %>% 
    dplyr::filter_(paste0(criteris,"!=0")) %>% 
    dplyr::select_("camp",criteris) 
  
  ##### Generar recodificació en base info
  
  mamon<-stringr::str_split(maco[criteris],"/") %>% 
    unlist() %>% 
    as.numeric()
  mamon<-c(-Inf,mamon,Inf)
  
  ##### Fer la recodificació en base el rang 
  nomcamp<-maco["camp"] %>% as.character()
  nomrecode<-paste0(nomcamp,".cat")
  
  dt<-dt %>% mutate_(camp=nomcamp)

  dt<-dt %>% mutate(popes=cut(camp,breaks = mamon))

  colnames(dt)[colnames(dt)=="popes"] <- nomrecode
  
  dt<-dt %>% select(-camp)
  
  
}

#  formula COX ajustat per event="Yes" -------------
#
###       incorpora efecte cluster

###       incorpora variables a evaluar a=Llista de variables a avaluar     ###

formulaCOX=function(x="v.ajust",event="event",temps="temps",elimina="",cluster="",a="",taulavariables="variables.xls") {
  
  variables <- data.frame(readxl::read_excel(taulavariables))
  variables[is.na(variables)]<- 0
  
  pepito<-paste("as.vector(variables[variables$",x,">0,]$camp)[!as.vector(variables[variables$",x,">0,]$camp)%in%c('idp')]",sep="")
  
  llistataula<-eval(parse(text=pepito))
  if (a!="") llistataula<-c(llistataula,a)
  
  # resposta<-paste("Surv(",temps,", as.integer(",event,"=='Si'))")
  # resposta<-paste("Surv(",temps,", as.integer(",event,"=='Yes'))")
  resposta<-paste("Surv(",temps,", as.integer(",event,"=='1'))")
  
  #
  if (cluster!="") kk<-paste(paste(llistataula,collapse=" + "),paste("cluster(",cluster,")",sep=""),sep="+")
  if (cluster=="") kk<-paste(llistataula,collapse=" + ")
  #
  # y<-as.formula(paste(resposta, paste(llistataula, collapse=" + "), sep=" ~ "))
  if (sum(elimina==llistataula)>0) y<-as.formula(paste(paste(resposta, kk , sep=" ~ "),elimina,sep=" - "))
  if (sum(elimina==llistataula)==0) y<-as.formula(paste(resposta, kk , sep=" ~ "))
  #
  
  y
  
}

#  Hadj retorna Ngran, Events, coef, HR, IC95, IC95, se.coef, p ---------

HRadj=function(x="v.ajust",event="EV.INSUF_CARD",t="tmp_insuf_card",e="",c="",d=dadesDF,taulavariables="variables.xls") { 
  pepito<-paste("sum(d$",t,")",sep="")
  PT<-eval(parse(text=pepito))
  
  if (c=="") posicio_p=5
  if (c!="") posicio_p=6
  
  result=tryCatch({
    pp<-coxph(formulaCOX(x=x,event=event,temps=t,elimina=e,cluster=c,taulavariables = taulavariables),data=d)    
    
    cbind(PT.Year=PT/365.25,
          N=pp$n,
          EVENTS=pp$nevent,
          coef=summary(pp)$coef[1,1],
          HR=summary(pp)$coef[1,2],
          IC951=summary(pp)$conf.int[1,3],
          IC952=summary(pp)$conf.int[1,4],
          se.coef=summary(pp)$coef[1,3],
          p=summary(pp)$coef[1,posicio_p])}
    
    ,error = function(e)  {
      cbind(PT.Year=PT/365.25,
            N=0,
            EVENTS=0,
            coef=NA,
            HR=NA,
            IC951=NA,
            IC952=NA,
            se.coef=NA,
            p=NA)}
    
  )
  result
}

#  HRestratificats  ----------------------
###   FUNCIiÓ QUE LLANÇO event, temps adjusted i em retorna un data frame amb tot global+ estratificat  ###
###     ENVIO exitus, temps i dades i em retorna data frame amb estratificats
####    camp estratificat conte variables estratificades tipo="v.ajust" / "crude"
HRestratificats<-function(event="exitus",t="temps",tipo="v.ajust",c="",taulavariables='variables.xls') {
  
  HRestratificats=data.frame()
  outDf<-data.frame(Subgroup="Total",HRadj(x=tipo,event=event,t=t,d=dades,c=c))

  variables2 <- data.frame(readxl::read_excel(taulavariables))
  variables2[is.na(variables2)]<- 0
  
  # row.names(outDf)<-label(dades$exitus)
  row.names(outDf)<-eval(parse(text=paste("Hmisc::label(dades$",event,")",sep="")))
  
  HRestratificats <-rbind(HRestratificats,outDf)
  
  N<-length(variables2[variables2$estrat==1,]$camp)
  
  for (i in 1:N) {
    outDf <-ddply(dades, variables2[variables2$estrat==1,]$camp[i], function(df)  HRadj(x=tipo,event=event,t=t,d=df,c=c))
    
    row.names(outDf)<-c(paste(Hmisc::label(eval(parse(text=paste("dades$",names(outDf)[1],sep="")))),"Adj1",sep=""),
                        paste(Hmisc::label(eval(parse(text=paste("dades$",names(outDf)[1],sep="")))),"Adj2",sep=""))
    names(outDf)[1]<-paste("Subgroup")  
    
    HRestratificats <-rbind(HRestratificats,outDf)
    
  }
  #   retorna 
  return(HRestratificats)
}


#  Formula segos LLISTA DE VARIABLES  D'AJUST     #######################
#####       hi envio la columna de variables amb que vull generar la formula pel compare

#####     x= variables d'ajust / y = resposta / eliminar /  a = Avaluar 

formula.LOGIT=function(x="taula1",y="resposta",eliminar=c("IDP"), a="",taulavariables='variables.xls') {
  # pepito<-paste("as.vector(variables[variables$",x,"==1,]$camp)[!as.vector(variables[variables$",x,"==1,]$camp)%in%c('idp','grup')]",sep="")
  
  x="ajust"
  y=Y
  taulavariables = "VARIABLES.xls"
  eliminar=c("IDP")
  a=""
  
  variables <- data.frame(readxl::read_excel(taulavariables))
  variables[is.na(variables)]<- 0
  
  variables<-variables %>% 
    dplyr::filter_(paste0(x,">0")) %>% 
    dplyr::arrange_(x)
  
  
  pepito<-paste("as.vector(variables[variables$",x,">0,]$camp)[!as.vector(variables[variables$",x,">0,]$camp)%in%eliminar]",sep="")
  
  llistataula<-eval(parse(text=pepito)) 
  
  # if (a!="") llistataula<-c(llistataula,a)
  if (a!="") llistataula<-c(a,llistataula,a)
  
  y<-as.formula(paste(y, paste(llistataula, collapse=" + "), sep=" ~ "))
  
}

#  Formula segos LLISTA DE VARIABLES  D'AJUST     #######################
#####       hi envio la columna de variables amb que vull generar la formula pel compare

#####     x= variables d'ajust / y = resposta / eliminar /  a = Avaluar 

formula.text=function(x="taula1",y="resposta",eliminar=c("IDP"), a="",taulavariables='variables.xls') {
  # pepito<-paste("as.vector(variables[variables$",x,"==1,]$camp)[!as.vector(variables[variables$",x,"==1,]$camp)%in%c('idp','grup')]",sep="")
  
  variables <- data.frame(readxl::read_excel(taulavariables))
  variables[is.na(variables)]<- 0
  
  variables<-variables %>% 
    dplyr::filter_(paste0(x,">0")) %>% 
    dplyr::arrange_(x)
  
  
  pepito<-paste("as.vector(variables[variables$",x,">0,]$camp)[!as.vector(variables[variables$",x,">0,]$camp)%in%eliminar]",sep="")
  

  llistataula<-eval(parse(text=pepito))
  
  # if (a!="") llistataula<-c(llistataula,a)
  if (a!="") llistataula<-c(a,llistataula,a)
  
  y<-paste(y, paste(llistataula, collapse=" + "), sep=" ~ ")
  
}



#  OR.ajustats(x,ajust,y)         ###########
#

OR.ajustats=function(x="lipos",ajust="V.ajust",y="prediabetis",d=dadestotal,taulavariables='variables.xls') {
  #
  # d=dades
  # taulavariables = "VARIABLES.xls"
  # x="lipos"
  # ajust="v.ajust"
  # y="Prediabetes"
  
  # d=dades
  # taulavariables="VARIABLES.xls"
  # x="lipos2"
  # ajust="v.ajust"
  # y="Prediabetes"
  
  
  #
  variables <- data.frame(readxl::read_excel(taulavariables))
  variables[is.na(variables)]<- 0
  # inicialitzar 
  num<-paste("length(variables[variables$",x,">0,]$camp)",sep="")
  num<-eval(parse(text=num))
  ORadj<-matrix(data=NA,ncol=4,nrow = num)
  # noms de columnes en matriu ORadj
  listvariables<-paste("list(variables[variables$",x,">0,]$camp[1:",num,"],c('OR','Linf','Lsup','p valor'))",sep="")
  dimnames(ORadj)<-eval(parse(text=listvariables))
  #
  #### extrec la variable que vull ajustar
  xtext<-paste("variables[variables$",x,">0,]",sep="")
  #
  
  ##  inicio bucle amb totes les variables que vull ajustar
  for (i in 1:num) {
    # i=1
    xeval<-eval(parse(text=xtext))$camp[i]
    # genero la forumla del model 
    # myFormula<-paste(y,"~",xeval,"+",variables.ajust(x=ajust),sep="")
    
    myFormula<-formula.LOGIT(x=ajust,y=y,eliminar="",a=xeval)
    
    # ajusto models
    model<-glm(formula= myFormula, family = binomial, data=d)
    model
    
    # extrec Coeficients dels models i IC i coloco dins de ORadj
    lolo<-cbind(OR=exp(summary.glm(model)$coef[,1]),Linf=exp(summary.glm(model)$coef[,1]-1.96*summary.glm(model)$coef[,2]),Lsup=exp(summary.glm(model)$coef[,1]+1.96*summary.glm(model)$coef[,2]),p_value=summary.glm(model)$coef[,4])
    ORadj[i,]<-cbind(OR=exp(summary.glm(model)$coef[2,1]),Linf=exp(summary.glm(model)$coef[2,1]-1.96*summary.glm(model)$coef[2,2]),Lsup=exp(summary.glm(model)$coef[2,1]+1.96*summary.glm(model)$coef[2,2]),p_value=summary.glm(model)$coef[2,4])
    
  }
  
  ORadj<-rownames(ORadj) %>% cbind(ORadj)
  ORadj<-as_tibble(ORadj)
  nomscol<-c("Variable","OR","Linf","Lsup","pvalor")
  ORadj<-ORadj %>% setNames(nomscol)
  
  ORadj
}


#  Variables.ajust   -----------------
#####       hi envio la columna de variables amb que vull generar la formula pel compare
#             FUNCIO variables.ajust
variables.ajust=function(x="taula1",variables=variables) {
  pepito<-paste("as.vector(variables[variables$",x,"==1,]$camp)[!as.vector(variables[variables$",x,"==1,]$camp)%in%c('idp','grup')]",sep="")
  llistataula<-eval(parse(text=pepito))
  z<-paste(llistataula, collapse=" + ")
}


#  GLM  COEFICIENTS      ###########################################################
#################   EXTREU COEFICIENTS glm, IC95 , p valors  GLM a partir de llista d'outcomes, X, i llista de v.ajust 
extreure_coef_glm<-function(dt=dades,outcomes="OFT_WORST",x="DM",z=""){
  
  # dt=dades
  # outcomes="OFT_WORST"
  # x="DM"
  # # z="ajust0"
  # z=""
  
  # Número de categories de X
  Ncat.x<-sum(table(dt[x])!=0)
  if (is.numeric(dt[[x]])) Ncat.x=1


  ### Si hi ha variables d'ajust genero llista
  if (z!="") mam<-names(selectorvariables(z,dt=dt,taulavariables="variables_R.xls"))  ### Genero llista de variables 
  if (z!="") x<-paste0(paste0(mam,collapse = "+"),"+",x)
  
  
  models1_oft<-names(selectorvariables(outcomes,dt=dt,taulavariables="variables_R.xls"))%>%         
    paste('~',x) %>%
    purrr::map(~glm(as.formula(.x), data= dt))%>%
    purrr::map(summary) %>% 
    purrr::map(coefficients) 
  
  noms_var_X<-models1_oft[[1]] %>% 
    rownames %>%        #
    tail(Ncat.x-1)      # Capturo nom categories de X
  
  # names(table(dt[x]))[2:Ncat.x]
  
  models1_oft<-models1_oft %>%                    # Select només num de coeficients necessaris de X
    purrr::map(tail,Ncat.x-1) %>% 
    purrr::map_dfr(data.table)
  
  variables<-names(selectorvariables(outcomes)) %>%  ##  Noms dels outcomes 
    rep(each=Ncat.x-1) %>%                                  ##  Cada Num de coeficients   
    data.table()      # Outcomes 
  colnames(variables)<-"Outcome"
  
  
  models_taula<-cbind(variables,Cat.X=noms_var_X,models1_oft) 
  
  models_taula
  
  list(models_taula,paste("Coeficient ajustat per:", x))
  
  
}


#

#  K-M   plot     #####
plotKM=function(y=exitus.surv,grup=grup,d=dades,caption="",llegenda=c("No","Yes")) {
  
  # y=dadesDF$exitus_surv
  # grup=dadesDF$hta
  # d=dadesDF
  # caption=Hmisc::label(dadesDF$hta)
  # llegenda=c("No","Yes")
  # 
  # fit<- survfit(y ~ grup, data = d) 

  # Basic survival curves
  p <- survminer::ggsurvplot(survfit(y ~ grup, data = d), data = d,
                  main = "Survival curve",
                  title= caption,
                  size = 0.5,
                  ylim = c(0,1),
                  xlim = c(0,60),
                  break.x.by=12,
                  xlab = "Time in months",
                  risk.table = F,
                  censor.shape="|", censor.size = 1
                  ,legend.labs=llegenda)
  p
}

#  Box-plot -----------------
boxplot_variables_grup<-function(dt=dades,variables="OFT_WORST",grup="DM", taulavariables="variables_R.xls") {
  
  # dt=dades
  # variables="OFT_WORST"
  # grup="DM"
  # taulavariables="variables_R.xls"
  
  ###   extrect variables 
  paco<-extreure.variables(variables,taulavariables=taulavariables)
  
  
  ###   Genero taula llarga
  popes<-dt %>% 
    dplyr::select(c(paco,grup)) %>% 
    gather_(key=variables,value="valor",setdiff(paco, grup)) 
  
  
  ###   FAi ggplot 
  figura1<-popes %>% ggplot2::ggplot(aes_string(x=variables, y="valor",fill=grup))+geom_boxplot()
  
  figura1
  
  
}

#  HR.COX  --------------------
####      funció que retorna MATRIU-->Ngran, Events, HR, IC951, IC952, p 
HR.COX=function(x="v.ajust",event="EV.INSUF_CARD",t="tmp_insuf_card",e="",d=dadesDF,taulavariables="variables.xls") { 
  pepito<-paste("sum(d$",t,")",sep="")
  PT<-eval(parse(text=pepito))
  
  result=tryCatch({
    pp<-survival::coxph(formulaCOX(x=x,event=event,temps=t,elimina=e,taulavariables = taulavariables),data=d)    
    
    
    cbind(N=pp$n,
          EVENTS=pp$nevent,
          HRadjusted=summary(pp)$coef[,2],
          IC951=summary(pp)$conf.int[,3],
          IC952=summary(pp)$conf.int[,4],
          p=summary(pp)$coef[,5])}
    
    ,error = function(e)  {
      cbind(N=0,
            EVENTS=0,
            HRadjusted=NA,
            IC951=NA,
            IC952=NA,
            p=NA)}
    
  )
  result
}

#  HR CRUS ------------------

HR.COX.CRU=function(x="lipos",event="EVENT_MCV",t="temps_exitus",e="",d=dadesDF,variables="variables_R.xls",evento="Si") {
  
  # x="Baseline"
  # event="RD"
  # t="TEMPS_RD2"
  # d=dadestotal
  # variables=conductor_variables
  # evento="1"
  
  
  bd.camps<-selectorvariables(x,dt=d,taulavariables=variables)
  camps<-names(bd.camps)
  num_camps<-length(names(bd.camps))
  
  poco<-cbind() 

  for (i in 1:num_camps) {
    
    # i<-1
    
    xx<-camps[i] 
    
    rr<-paste("Surv(",t,", as.integer(",event," == ",evento,"))~",xx,sep="")
    pp<-coxph(eval(parse(text=rr)),data=d) 
    
   
    mama<-cbind(N=pp$n,
                EVENTS=pp$nevent,
                HRcrude=summary(pp)$coef[,2],
                IC951=summary(pp)$conf.int[,3],
                IC952=summary(pp)$conf.int[,4],
                p=summary(pp)$coef[,5])
    
    rownames(mama)<-names(pp$coefficients)
    poco<-rbind(poco,mama)
  }
  
  poco
  
}

#################     FUNCIO PER EXTREURE CORRELACIONS, P VALORS ENTRE var1 i llista de quantis de dades 
extreure_cor=function(var1="CD36",var="quantis",d="dades") {
  
  llistavariables<-eval(parse(text=paste("variables$camp[variables$",var,"==1]",sep="")))
  
  # llistavariables<-variables$camp[variables$var==1]
  x<-eval(parse(text=paste(d,"$",var1,sep="")))
  
  ppp<-cbind()
  for (i in 2:length(llistavariables)) {
    
    var2<-paste(d,llistavariables[i],sep="$")
    y<-eval(parse(text=var2))
    cor.test(x,y)$estimate
    correlacio<-cor.test(x,y)$estimate
    pvalor<-cor.test(x,y)$p.value
    
    pp<-cbind(correlacio,pvalor)
    row.names(pp)<-llistavariables[i]
    ppp<-rbind(ppp,pp)
  }
  
  ppp
}

#  Extreure OR  --------------------
#       LLANÇO UNA FORMULA les dades per executar un model i retorno OR , CI95% i p-valor en una matriu

extreure_OR<- function (formu="AnyPlaqueBasal~CD5L",dades=dades) {
  
  fit<-stats::glm(formu, family = binomial, data=dades)
  my_coefficients <- fit %>% coef 
  ci<-fit %>% confint 
  
  OR<-my_coefficients %>% exp()
  OR_linf<-ci %>% exp()
  pvalors<-coef(summary(fit))[,'Pr(>|z|)']
  
  ret_val <- cbind(OR,OR_linf,pvalors)
  colnames(ret_val) <- c("OR","Linf", "Lsup", "p-value")
  # rownames(ret_val) <- names(coef(fit))
  as.data.frame(ret_val)
}

#  Resum d'un data.table (Mitjana, DT, N etc...)  --------------------

######         RESUM D'UN DATA.TABLE 

###   LLANÇO UN DT, VARIABLE I UNA ESTRATIFICACIó I EM TORNA UN DT AMB un resum

### mitjana, DT, N etc... per cada ESTRAT

resum3<-function(dt=dades,x="val_last.HBA1C",estrat="constant"){
  
  dt$constant<-1
  
  e<-parse(text=x)
  
  resum3<-dt[, .(
    Mean=mean(eval(e),na.rm=T),
    SD=sd(eval(e),na.rm=T),
    Nmenor7=sum(eval(e)<7,na.rm=T),
    Perc_menor7=(sum(eval(e)<7,na.rm=T)/length(which(eval(e) != "NA")))*100,
    N=length(eval(e))
  )
  ,by=estrat]
  
  resum3
} 

#  Resum quanti  -------------------------
#####     funció que retorna un summary (mean, sd) de y en funció d'un grup

resum_quanti<-function(dt=dades,y="valor_basal.GLICADA",grup="constant") {
  
  dt$constant=1
  
  # dt=dades
  # y="valor_basal.GLICADA"
  # grup="CODGLP1"
  
  ### extrect p valor 
  pepito=paste0("summary(aov(",y,"~",grup,",data=dt))[[1]][['Pr(>F)']]",sep="")
  pvalor<-eval(parse(text=pepito))[1]
  
  summ1 <- paste0('mean(', y, ',na.rm=T)')
  summ2<-paste0('sd(',y,',na.rm=T)')
  
  dt %>% dplyr::group_by_(grup) %>% 
    dplyr::summarise_(mean=summ1,
               sd=summ2,
               n="n()") %>% 
    dplyr::mutate(p=pvalor)
  
}

#  ESTADISTICS RESUMS ----------------------
# RETORNA ESTADISTICS RESUMS (mean, sd, p-valor --> ANOVA/t-test) X GRUP  X ESTRAT 

resum_quanti_estrat<-function(dt=dades,y="valor_basal.GLICADA",grup="CODGLP1",estrat="HBA1C_cat4"){
  # dt=dades
  # y="valor_basal.GLICADA"
  # grup="CODGLP1"
  # estrat="HBA1C_cat4"
  
  dt %>% 
    tidyr::drop_na(y) %>% 
    dplyr::group_by_(estrat) %>% 
    dplyr::do(resum_quanti(dt=.,y=y,grup=grup))
  
}


#  Resum events  ----------------------
###################         Llan?o dades, event i temps i me fa un resum 


resum_events<-function(dades=dadestotal,evento="RD",temps="temps",valorevent="Si") {
  
  # dades=dadesDF
  # evento="EVENT_MORT2014"
  # temps="temps_mortalitat"
  # valorevent="1"
  # dades=dades
  # evento="RD"
  # temps="TEMPS_RD2"

  Patients=length(dades[[evento]])
  PYears=sum(dades[[temps]])
  temps_seguiment=mean(dades[[temps]])
  N.Events=sum(dades[[evento]]==valorevent)
  Event.rate=((N.Events/PYears)*100)
  IA=(N.Events/Patients)
  resum<-cbind(Patients,PYears,temps_seguiment,N.Events,Event.rate,IA)
  resum
}


#  Resum events  ----------------------
resum_events_v2<-function(dades=dades,evento="RD",temps="temps") {
  
  # dades=dadestotal
  # evento="RD"
  # temps="TEMPS_RD2"
  
  Patients=length(dades[[evento]])
  PYears=sum(dades[[temps]])
  temps_seguiment=mean(dades[[temps]])
  N=mean(dades[["N_BREAK"]])
  min=min(dades[["N_BREAK"]])
  max=max(dades[["N_BREAK"]])
  N.Events=sum(dades[[evento]])
  Event.rate=(N.Events/PYears)*100 
  IA=(N.Events/Patients)*100
  
  ### Fusionar tot 
  resum<-cbind(Patients,PYears,temps_seguiment,N,min,max,N.Events,Event.rate,IA)
  resum
}


#  Resum events per grup  ------------------
##########              Llanço dades, event, temps , grup i retorno un resum d'events per grups 

resum_events_grup=function(d=dadestotal,evento="ECV1",temps="MesesHastaEventoFinal",grup="CD5L_Q4") {
  
  # d=dadesDF
  # evento="EVENT_MORT2014"
  # temps="temps_mortalitat"
  # valorevent="1"
  # grup="hta"
  
  # d=dadestotal
  # evento="RD"
  # temps="TEMPS_RD2"
  # grup="sexe"
  
  
  pepito=paste0("as.factor(d$",grup,")")
  dadesgrups<-d %>% split(eval(parse(text=pepito)))
  
  resumgrups=data.frame()
  
  for (i in 1:length(dadesgrups)) {
    
    out<- resum_events_v2(dades=dadesgrups[[i]],evento=evento,temps=temps)
    
    resumgrups<-rbind(out,resumgrups)
  }
  
  pepito=paste0("levels(d$",grup,")")
  nomscat<-eval(parse(text=pepito)) 
  
  
  resumgrups<-resumgrups %>% 
    mutate(variable=paste0(grup,":",nomscat)) %>% 
    as_tibble
  
  # variables a seleccionar 
  popo<-c("variable","Patients", "PYears", "temps_seguiment","N","min","max","N.Events","Event.rate","IA")  
  
  resumgrups %>% select(popo)
  
  
}

#  Llistat de Taules compare ------------------
#   LLISTA DE noms de taules i retorna llista de taules comparatives

#    Llanço una LLISTA de noms de taules que estan en el Conductor Variables i em retorna una llista de taules ###
llistadetaules.compare<-function(tablero=c("taula1","taula2","taula3","taula4","taula5"),y="sexe",variables = "variables.xls",dt=dades){
  restab.llista<-list()
  for (i in 1:length(tablero)) {
    restab.llista[[i]]<-tablero[i] %>% 
      formula_compare(y=y,taulavariables = variables) %>% 
      compareGroups(data=dt,include.miss = F,include.label=T) %>% 
      createTable(show.ratio = F, hide.no = c('NA','No'), show.p.overall=T,show.n=T,show.all=T)
  }
  
  restab.llista
  
}

# P-valors ajustats segons multiple test Comparations desde un objecte Compare groups  ------------------

### Llanço un objecte compare groups i em retorna els p-valors + els ajustats en una taula 

# p.adjust.methods
# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")
##    Ajust BH 
# The "BH" (aka "fdr") and "BY" method of Benjamini, Hochberg, and Yekutieli control the false discovery rate, 
# the expected proportion of false discoveries amongst the rejected hypotheses. 
# The false discovery rate is a less stringent condition than the family-wise error rate, so these methods are more powerful than the others.

Pvalors_ajustats_compare<-function(objecte_compare=T1.1.2, metodo="BH") {

  # objecte_compare=T1.1.2
  # metodo="bonferroni"
  
  # 1. Extrect els p-valors 
  pvalors <- compareGroups::getResults(objecte_compare, "p.overall")
  
  # 2. Tauls 
  pvals<-data.table(pvalors)
  
  # 3. Posa noms
  pvals$variable<-names(pvalors)
  
  # 4. Ajusta p- valors 
  pvals$Adjpvalor<-stats::p.adjust(pvalors, method = metodo)

  # 5. Punt de tall
  pvals<-pvals %>% mutate (sigBH=ifelse(Adjpvalor<0.05,"Sig","NS"))

  # 6. Canviar noms
  pvals<-pvals %>% setNames(c("P.crude","Variable",paste0("Padj.",metodo), paste0("Sig.",metodo)))

}

Pvalors_ajustats_taula<-function(objecte_taula=OR.ajust, p.valors='p valor', metodo="BH") {
  
  # objecte_taula=OR.ajust
  # metodo="bonferroni"
  # p.valors='pvalor'
  
  # 0 Genero noms de l'objecte a crear  
  nomsnous<-c(names(objecte_taula),paste0("Padj.",metodo),paste0("Sig.",metodo))
  
  # 1. Extrec p-valors 
  pvalors <-objecte_taula[p.valors]
  p.num<-pvalors[[1]] %>% as.numeric()
  
  
  # 2. Calculo els p valors ajustats
  pvals_adj<-p.adjust(p.num, method = metodo) 
  
  # 3. Ho fusiono amb la taula 
  objecte_taula<-objecte_taula %>% cbind(pvals_adj)
  
  # 4. Punt de tall
  objecte_taula<-objecte_taula %>% mutate (sigBH=ifelse(pvals_adj<0.05,"Sig","NS"))
  
  # 6. Canviar noms
  objecte_taula<-objecte_taula %>% setNames(nomsnous)
  
  objecte_taula %>% as_tibble()
  
  
}





#  Afegeix dataindex Dinamica o / Constant si no existeix------------

######      Funció que Afegeix dataindex Dinamica o / Constant si no existeix

###     Entra BD Historic i surt BD Historic + dataindex

afegir_dataindex<-function(dt_historic,bd.dindex="20161231") {
  
  # dt_historic=dt
  # bd.dindex=piolo
  
  if (is.data.table(bd.dindex) | is.data.frame(bd.dindex)) {
    
    rrr<-dt_historic %>% 
      dplyr::inner_join(bd.dindex, by="idp") %>% 
      rename(dtindex=tidyselect::last_col()) %>%          ## Renomenar dtindex (última columna de bd.index)
      data.table::data.table()
    
  } else {
    ###  Si NO Existeix la base de dades -->  Afegim data index constant bdades de variables
    rrr<-dt_historic %>% 
      dplyr::mutate_("dtindex"=as.character(bd.dindex)) %>% 
      data.table::data.table()
    
  }
  
  
}


#  Agregar analitiques -----------------

####################      FUNCIÓ QUE LLANÇO 1. Data.table, 
#                                           2. dataindex constant, / o data.frame amb idp + dataindex (caracter) ,
#                                           3. finestra de temps previ en dies 
####################      RETORNA UN data.table amb dades agregades

agregar_analitiques<-function(dt=ANALITIQUES,bd.dindex="20161231",finestra.dies=c(-Inf,Inf)){
  
  # finestra.dies=c(-365,0)
  # dt=VARIABLES
  # bd.dindex="20091231"
  # bd.dindex=piolo

  #### Afegir + data index (+dtindex) en l'historic de variables
  
  dt<-afegir_dataindex(dt,bd.dindex)
  
  ##### filtrar per intervals de dates 

  dt<-dt[data.table::between(
        lubridate::ymd(dat),
        lubridate::ymd(dtindex)+finestra.dies[1],
        lubridate::ymd(dtindex)+finestra.dies[2])]
  
  ##   prova sense Lubridate
  # dt<-dt[data.table::between(
  #   as.Date(as.character(dat),format="%Y%m%d"),
  #   as.Date(as.character(dtindex),format="%Y%m%d")+finestra.dies[1],
  #   as.Date(as.character(dtindex),format="%Y%m%d")+finestra.dies[2])]
  
  
  ##  Selecciono un unic registre i agrego amb valor valid més proper dins de la finestra 
  paco<-dt[val!=-9] %>% 
    dplyr::mutate(dies=lubridate::ymd(dtindex)-lubridate::ymd(dat)) %>%     # Calculo els dies fins data index 
    dplyr::group_by(idp,cod) %>%                                            # Agafo fila tal que dies sigui mínim (Valor mes proper)         
    dplyr::slice(which.min(dies)) %>%                                       # Fila que els dies sigui més propera a data index
    dplyr::ungroup()  

  # RESHAPE valors d'Analitiques 
  analitiques.valor <- paco[,c("idp","cod","val")] %>% 
    tidyr::spread(cod,val)
  
  # RESHAPE Dies 
  analitiques.dies <- paco[,c("idp","cod","dies")] %>% 
    tidyr::spread(cod,dies)
  
  # JOINT Valors i dies
  analitiques.idp<-full_join(analitiques.valor, analitiques.dies, by="idp",suffix = c(".valor", ".dies"))
  
  analitiques.idp
  
}

#  Agregar_problemes -----------------
###################     LLANCO PROBLEMES LONG + UNA DATA INDEX / BD ab data index, finestra temporal -> 
#                       RETORNO UNA TAULA AGREGADA / TAMBÉ POSO LA TAULA CATALEG

agregar_problemes<-function(dt=PROBLEMES,bd.dindex="20161231",dt.agregadors=CATALEG,finestra.dies=c(-Inf,0),prefix="DG.",camp_agregador="agr") {

  # dt=PROBLEMES_total
  # # bd.dindex =bdades_index
  # dt.agregadors=CATALEG
  # finestra.dies=c(-Inf,0)
  # bd.dindex="20161231"
  # prefix=""
  # camp_agregador="agr2"

  ## afegir en dataindex de BDINDEX si bd.dindex<>""
  #### Afegir + data index (+dtindex) en l'historic de problemes
  
  dt<-afegir_dataindex(dt,bd.dindex)
  
  ## filtrar per intervals de dates 
  
  dt<-dt[data.table::between(
    lubridate::ymd(dat),
    lubridate::ymd(dtindex)+finestra.dies[1],
    lubridate::ymd(dtindex)+finestra.dies[2])]
  
  ## Filtrar CATALEG PER CAMP AGREGADOR 
  dt.agregadors<-dt.agregadors %>% 
    select_("cod","agr"=camp_agregador) %>% 
    filter(!is.na(agr))
  
  ## Capturar agregador 
  
  dt.temp<-dt %>% 
    # camps mínims que necessito per agregar 
    dplyr::select(c(idp,cod,dat)) %>%                                             # Selecciono camps mínims
    # Capturo Agregador de CATALEG
    dplyr::inner_join(dplyr::select(dt.agregadors,c(cod,agr)), by="cod") %>%      # Capturo agregador del cataleg
    # Eliminar duplicats agafant el primer registre (dat minima)
    # Agrupar= unic reg per idp-agr (mes antic segons data)
    dplyr::group_by(idp,agr) %>%                                                  # Agrupo per idp agr
    dplyr::slice(which.min(dat)) %>%                                              # Selecciono més antic 
    dplyr::ungroup() # desagrupo
  
    # RESHAPE una data per agregador  
    # seleccionar camps i Reshape  
    dt.agregat<-dt.temp %>% 
      dplyr::select(idp,agr,dat) %>%  # Selecciono agregador i data
    # RESHAPE per agregador i em quedo la data
      tidyr::spread(agr,dat,sep=".")                                                        # Reshape
  
  names(dt.agregat) <- sub("agr.", prefix, names(dt.agregat))   # Afegir prefix en noms de variables 

  dt.agregat
  
  
}

#  Agregar_problemes un sol agregador  -----------------
###################     LLANCO PROBLEMES LONG + UNA DATA INDEX / Un agregador / BD ab data index, finestra temporal -> 
#                       RETORNO UNA TAULA AGREGADA / DAta i Codi/ TAMBÉ POSO LA TAULA CATALEG

agregar_problemes_agr<-function(dt=PROBLEMES,agregador="ECV",camp_agregador="AGR_TER",bd.dindex="20161231",dt.agregadors=CATALEG,finestra.dies=c(-Inf,0),prefix="") {
  
  # dt=PROBLEMES_total
  # bd.dindex="20161231"
  # agregador ="ECV_TER"
  # dt.agregadors=CATALEG
  # finestra.dies=c(-Inf,0)
  # prefix=""
  # camp_agregador="AGR_TER"
  
  ## afegir en dataindex de BDINDEX si bd.dindex<>""
  #### Afegir + data index (+dtindex) en l'historic de problemes

  dt<-afegir_dataindex(dt,bd.dindex)
  
  ## filtrar per intervals de dates 
  
  dt<-dt[data.table::between(
    lubridate::ymd(dat),
    lubridate::ymd(dtindex)+finestra.dies[1],
    lubridate::ymd(dtindex)+finestra.dies[2])]
  
  ## Filtrar CATALEG 
  dt.agregadors<-dt.agregadors %>% select_("cod","agr"=camp_agregador)
  dt.agregadors<-dt.agregadors %>% filter(agr==agregador)
  
  ## Capturar agregador 
  
  dt.temp<-dt %>% 
    # camps mínims que necessito per agregar 
    dplyr::select(c(idp,cod,dat)) %>%                                             # Selecciono camps mínims
    # Capturo Agregador de CATALEG
    dplyr::inner_join(dplyr::select(dt.agregadors,c(cod,agr)), by="cod") %>%      # Capturo agregador del cataleg
    # Eliminar duplicats agafant el primer registre (dat minima)
    # Agrupar= unic reg per idp-agr (mes antic segons data)
    dplyr::group_by(idp,agr) %>%                                                  # Agrupo per idp agr
    dplyr::slice(which.min(dat)) %>%                                              # Selecciono més antic 
    dplyr::ungroup() # desagrupo
  
  # RESHAPE una data per agregador  
  # seleccionar camps i Reshape  
  dt.agregat<-dt.temp %>% 
    dplyr::select(idp,agr,dat,cod) %>%  # Selecciono agregador i data
    # RESHAPE per agregador i em quedo la data
    tidyr::spread(agr,dat,sep=".")                                                        # Reshape
  
  names(dt.agregat) <- sub("agr.", prefix, names(dt.agregat))   # Afegir prefix en noms de variables 
  
  dt.agregat
  
}



#  agregar_prescripcions ----------------------
#        LLANCO PRESCRIPCIONS , data index / bd , agregadors i retorna la mitjana de les nddds per idp 
agregar_prescripcions<-function(dt=PRESCRIPCIONS,bd.dindex=20161231,dt.agregadors=CATALEG,prefix="FP."){
  
  # dt=FX.PRESCRITS
  # dt.agregadors=CATALEG
  # bd.dindex=20161231
  # bd.dindex=bdades_index
  # prefix="FP."

  ## afegir en dataindex de BDINDEX si bd.dindex<>""
  #### Afegir + data index (+dtindex) en l'historic de problemes
  dt<-afegir_dataindex(dt,bd.dindex)
  
  ##### Arreglar dades
  dt<-dt %>% mutate(
    dat=lubridate::ymd(dat),
    dbaixa=lubridate::ymd(dbaixa),
    dbaixa=ifelse(is.na(dbaixa),Inf,dbaixa))
  
  ##### filtrar si dtindex cau dins de l'interval de prescripció (dat-dbaixa) 
  dt<-dt %>% filter(dtindex<=dbaixa & dtindex>=dat)
  

  prescripcions_agr<-dt %>% 
    dplyr::select(idp,cod,dat,dbaixa) %>% 
    dplyr::inner_join(dplyr::select(dt.agregadors,c(cod,agr)), by="cod") %>%      # Capturo agregador del cataleg
    # Selecciono camps
    dplyr::select(c(idp,agr,dat)) %>% 
    # Eliminar duplicats PER IDP-AGR agafant unic registre per idp-agr 
    dplyr::group_by(idp,agr) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    # dplyr::mutate(FX=1) %>% 
    # Aplanamenta
    tidyr::spread(agr,dat,sep=".")
  
  names(prescripcions_agr) <- sub("agr.", prefix, names(prescripcions_agr))   # Afegir prefix en noms de variables 
  
  prescripcions_agr
  
}

#  agregar_facturacio -------------------
#        LLANCO PRESCRIPCIONS , data index i agregadors i retorna la mitjana de les nddds per idp 
agregar_facturacio<-function(dt=PRESCRIPCIONS,finestra.dies=c(-365,0),dt.agregadors=CATALEG,camp_agregador="agr",bd.dindex="20161231",prefix="FD."){
  
  # dt=FX.FACTURATS_1000000
  # prefix = "FD."
  # bd.dindex ="20171231"
  # finestra.dies=c(-Inf,+Inf)
  # dt.agregadors=conductor_variables
  # camp_agregador="GRUP"

  # Recode els numeros infinits
  finestra.dies=ifelse(finestra.dies==+Inf,99999,finestra.dies)
  finestra.dies=ifelse(finestra.dies==-Inf,-99999,finestra.dies)

  #### Afegir data index en l'historic 
    
  dt<-afegir_dataindex(dt,bd.dindex)

  ## Modifico CATALEG 
  dt.agregadors<-dt.agregadors %>% select_("cod","agr"=camp_agregador)
  dt.agregadors<-dt.agregadors %>% filter(!is.na(agr))
  
  
  #### Filtrar per finestra temporal i agregar
  ##  
  pepito<-dt %>%  dplyr::mutate (
        data=lubridate::ymd(paste0(as.character(dat),15)),    # data arrodonida al dia 15
        datafi=data+lubridate::days(env*30),                  # data fi 
        dtindex=lubridate::ymd(dtindex),                      # Data index en format data
        interval_fac=lubridate::interval(data,datafi),        # Interval Dispensació
        interval_fin=lubridate::interval(dtindex+finestra.dies[1],dtindex+finestra.dies[2]),            # Interval finestra
        pp=lubridate::int_overlaps(interval_fac,interval_fin)) %>%      # Genero filtre dins finestra 
    # Netejo variables i aplico filtre  
      dplyr::select(-agr,-env,-dat,-interval_fac,-interval_fin) %>%     # Netejo variables
      dplyr::filter(pp) %>%                                             # Aplico Filtro dins finestra
      dplyr::select(-pp) %>% 
    # Capturo Agregador de CATALEG
      dplyr::inner_join(dplyr::select(dt.agregadors,c(cod,agr)), by="cod") %>%    # Capturo agregador del cataleg
      
      dplyr::select(c(idp,agr,data)) %>%                                              # Selecciono camps
        # Eliminar duplicats PER IDP-AGR agafant unic registre per idp-agr 
      dplyr::group_by(idp,agr) %>% 
      dplyr::slice(1) %>% 
      dplyr::ungroup() %>% 
    # Aplanamenta
      tidyr::spread(agr,data,sep=".") %>% 
      mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

  names(pepito) <- sub("agr.", prefix, names(pepito))   # Afegir prefix a noms de variables 
  
  pepito

}

#  AGREGADOR DE VISITES      ###############
### Envio la historic de visites i retorno numero de visites en la finestra de temps 

agregar_visites<-function(dt=VISITES,bd.dindex=20161231,finestra.dies=c(-365,0)){
  
  # dt=VISITES
  # bd.dindex=bdades_index
  # finestra.dies=c(-365,0)
  
  
  ## Afegir en dataindex (+dtindex) en historic de Visites
  dt<-afegir_dataindex(dt,bd.dindex)
  
  ##### filtrar per intervals de dates 
  dt<-dt[data.table::between(
    lubridate::ymd(dat),
    lubridate::ymd(dtindex)+finestra.dies[1],
    lubridate::ymd(dtindex)+finestra.dies[2])]  
  
  ##### Agregar (Suma de visites en interval independentment del tipus)
  
  paco<-dt %>% 
    dplyr::group_by(idp,cod) %>%                    # Agrupo per id 
    dplyr::count() %>%           # Conto el numero visites per codi 
    dplyr::ungroup()  
  
  # RESHAPE per idp 
  visites <- paco[,c("idp","cod","n")] %>% 
    dplyr::select(idp,visites=cod,n) %>% 
    tidyr::spread(visites,n,sep = "_")
  
  paco <- paco %>% 
    dplyr::select(idp,visites=cod,n) %>% 
    tidyr::spread(visites,n,sep = "_")
  
  # NA = 0
  visites[is.na(paco)]<-0
  
  ###  Computo visites globals
  paco<-paco %>% select(idp)  # Separo id de visites 
  
  visites<-visites %>%        #  Sumo totes les visites
    select(starts_with("visites")) %>% 
    mutate(visites_TOT=rowSums(.) )
  
  paco<-paco %>% cbind(visites) %>% as_tibble()
  
  paco
  
}



#  APLICA CRITERIS D'EXCLUSIÓ A dades  -----------------------
criteris_exclusio<-function(dt=dades,taulavariables="VARIABLES_R3b.xls",criteris="exclusio1") {
  
  # dt=dades
  # taulavariables="VARIABLES_R3b.xls"
  # criteris="exclusio1"

  ##  Llegeix criteris de variables 
  variables <- readxl::read_excel(taulavariables)
  variables[is.na(variables)]<- 0
  
  ##  0. Filtro taula variables només variables implicades en el filtre i el genero 
  maco<-variables %>% 
    dplyr::filter_(paste0(criteris,"!=0")) %>% 
    dplyr::select_("camp",criteris) %>%
  # Genero la llista de filtres 
    tidyr::unite_("filtres", c("camp", criteris),sep="") 
 
     # Concateno condicions amb un OR
  maco<-str_c(maco$filtres,collapse=" | ")
  
  ## 1. Genera filtre en base a columna exclusio1   popes

  popes<-str_c("!(",maco,")")

  ##  2. Eliminar els espais en blanc de les variables factors del data.frame

  dt<-dt %>% 
    dplyr::mutate_if(is.factor,funs(str_trim(.)))
  
  ##  3. Aplicar filtre: popes a dt
  
  dt<-dt %>% 
    dplyr::filter(eval(parse(text=popes)))
  
  
}

criteris_exclusio_diagrama<-function(dt=dades,taulavariables="VARIABLES_R3b.xls",criteris="exclusio1"){
  
  dt=dades
  taulavariables="VARIABLES_R3b.xls"
  criteris="exclusio1"

  ##  Elimino els espais en blanc de les variables factor

  dt<-dt %>% dplyr::mutate_if(is.factor,funs(str_trim(.))) %>% as.data.table()

  ##  Llegeix criteris de variables 
  variables <- readxl::read_excel(taulavariables)
  variables[is.na(variables)]<- 0
  
  ##  0. Filtro taula variables només variables implicades en el filtre i el genero 
  maco<-variables %>% 
    dplyr::filter_(paste0(criteris,"!=0")) %>% 
    dplyr::select_("camp",criteris) %>%
    # Genero la llista de filtres 
    tidyr::unite_("filtres", c("camp", criteris),sep="") 


  dades_criteris<-c()
  for (i in 1: length(maco$filtres)){
    
  dades_criteris[i]<-dt %>% 
    dplyr::filter(eval(parse(text=maco[i,1]))) %>% 
    count(.) 
  
    }

  dades_criteris<-unlist(dades_criteris)
  
  exc_num<-dades_criteris %>% as.vector()
  
  exc_lab_kk<-str_remove_all(maco$filtres ,"'")
  
  pob0<-dades %>% summarise (n()) %>% as.numeric()
  
  
  
  # Concateno condicions amb un OR
  maco<-str_c(maco$filtres,collapse=" | ")
  ## 1. Genera filtre en base a columna exclusio1   popes
  popes<-str_c("!(",maco,")")
  ##  2. Eliminar els espais en blanc de les variables factors del data.frame
  dt<-dt %>% 
    dplyr::mutate_if(is.factor,funs(str_trim(.)))
  ##  3. Aplicar filtre: popes a dt
  dt<-dt %>% 
    dplyr::filter(eval(parse(text=popes)))
  ###
  
  pob.f<-dt %>% summarise (n()) %>% as.numeric()
  
  
  # popes<list(pob,exc,ex_lab)
  
  ### Ara falta dibuixar el diagrama de fluxe basat amb les dades_criteris
  

  pob=c(pob0,pob.f)
  exc=exc_num
  exc_lab=exc_lab_kk
  
  diagrama<-diagramaFlowchart(pob=c(pob0,pob.f),exc=exc_num,exc_lab=exc_lab)
  
  list(pob.f,diagrama)

}



#  CALCULA LA PROPORCIÓ -- RETORNA N I % fila ----------------

calcular_proporcio<-function(dt=dades,factor="canvi612M.glicadaCAT2"){
  
  # dt=dades
  # factor="canvi612M.glicadaCAT2"
  # cat="Yes"
  
  moco<-dt %>% 
    tidyr::drop_na(factor) %>% 
    dplyr::group_by_(factor) %>% 
    dplyr::summarise_(n="n()") %>% 
    dplyr::mutate_(freq="n/sum(n)*100") 
  
  moco
  
}


#  CALCULA PROPORCIO PER GRUPS I RETORNA P VALOR    --------------     

proporcions_grups<-function(dt=dades,factor="canvi612M.glicadaCAT2",estrat="SEXE"){
  
  # dt=dades
  # factor="canvi612M.glicadaCAT2"
  # estrat="CODGLP1"
  
  ##  extrec p-valor
  pepito=paste0("chisq.test(dt$",factor,",dt$",estrat,")$p.value",sep="")
  pvalor<-eval(parse(text=pepito))
  
  resultat<-
    dt %>% 
    tidyr::drop_na(factor) %>% 
    dplyr::group_by_(estrat) %>% 
    dplyr::do(calcular_proporcio(dt=.,factor=factor)) %>% 
    dplyr::mutate(p=pvalor)
  
  resultat
  
}


#  RETORNA UNA LLISTA DE TAULES DE PROPORCIONS PER GRUPS ESTRATIFICAT PER estratificat ----------

proporcio_grups_estratificat<-function(dt=dades,factor.Y="canvi612M.glicadaCAT2",grup=c("SEXE","CODGLP1","anys_DMcat4"),estratificat="HBA1C_cat4") {
  
  # dt=dades
  # factor.Y="canvi612M.glicadaCAT2"
  # grup=c("SEXE","anys_DMcat4")
  # estratificat="HBA1C_cat4"
  
  pepe<-list()
  
  for (i in 1:length(grup))  {
    pepe[[i]]<-
      dt %>% 
      tidyr::drop_na(estratificat) %>% 
      dplyr::group_by_(estratificat) %>%
      dplyr::do(proporcions_grups(dt=.,factor=factor.Y,estrat=grup[i]))
    
  }
  
  pepe
  
  
}



#  REDUCCIÓ AJUSTADA DIFERENTS METODES D'AJUST-----------------

##    BASAL , POST I RETORNA LA DIFERENCIA AJUSTA SEGONS EL BASAL I ERROR ESTANDARD

reduccio_ajustada<-function(dt=dades,v.basal,v.final,mean.basal=NA) {
  
  library(mgcv)
  
  # #  parametres 
  
  # dt=dades
  # v.basal="HBpreADD"
  # v.final="HBpostADD"
  # mean.basal=9.02
  
  ##  Si no poso la mitjana basal poso la mitjana de la base de dades
  if (is.na(mean.basal)) mean.basal=mean(dt[,v.basal],na.rm=T)
  
  #   Calculo la variable canvi  
  dt<-dt %>% 
    dplyr::mutate(canvi=dt[,v.basal]-dt[,v.final]) 
  # Genero quintils que no els faré servir de moment 
  dt<-dt %>% 
    dplyr::mutate(basal_cat5=cut2(dt[,v.basal], g=5))
  
  ## Elimino missings de taula i selecciono variables
  dt<-dt %>% 
    tidyr::drop_na(canvi) %>% 
    dplyr::select_(v.basal,v.final,"canvi","basal_cat5")

  ## canvio noms que tampoc caldria
  names(dt)<-c("pre","post","dif","basal_cat")
  
  ## model cru (descriptiu bàsic,+ mean, se )
  taula<-dt %>% summarise(
    n=n(),
    mean.basal=mean(pre),
    mean.canvi=mean(dif), 
    se=sd(dif)/sqrt(n())
  )
  
  ### arguments de funcions dels models amb les dades, junt amb la mean.basal
  pre<-dt$pre
  dif<-dt$dif
  
  # funcions dels models 
  model.lineal.w<-function(y=y,x=x)glm(y~x,weights =x,family = gaussian)
  model.lineal<-function(y=y,x=x) glm(y~x,family = gaussian) 
  model.nolineal<-function(y=y,x=x) glm(y~x+I(x^2)+I(x^3),family = gaussian) 
  model.gam1<-function(y=y,x=x) gam(y~s(x),family = gaussian)
  model.gam2<-function(y=y,x=x) gam(y~s(x,bs="cc",k=12),family = gaussian)
  
  # Genero els models que els poso en una llista
  llista.models<-list(
    lineal.w=model.lineal.w(x=pre,y=dif),
    # lineal=model.lineal(x=pre,y=dif),
    nolineal=model.nolineal(x=pre,y=dif),
    gam1=model.gam1(x=pre,y=dif)
    # , gam2=model.gam2(x=pre,y=dif)
    )

  predict(llista.models[[1]],data.frame(x=mean.basal))

  ## Genero les prediccions () en el punt basal mitg  i guardo el la SE
  maquina<-llista.models %>% 
    purrr::map_df(predict,data.frame(x=mean.basal),se.fit=T) %>% 
    as.data.frame()
  
  ## Calculo Rquadrat per cada model
  Rquadrat<-llista.models %>% 
    purrr::map_dbl(function(modelaco) (1-(modelaco$deviance/modelaco$null.deviance))) %>% 
    as.data.frame()

  ## Combino informació Rquadrat + prediccions de cada model
  taula.models<-cbind(Rquadrat,maquina)
  
  ## poso els noms dels models com una columna
  taula.models$model<-row.names(taula.models)
  
  ## enganxo la taula dels valors mitjans  i la N
  taula<-cbind(taula.models,taula,v.basal)
  
  ## Hauria de canviar el nom de les variables una mica i eliminar coses que no serveixen i tal pascual

  names(taula)[1] <- "R.Square"
  
  # Drop variables with -
  # taula<-select(taula, -("residual.scale"))

  taula

}

#  PLOT dispersió segons PRE-POST , FA DISPERSIÓ DE PRE VS CANVI I SOBREPOSA AJUST------

plot.dispersio.reduccio <-function(dt=dades,v.basal="HBpreADD",v.final="HBpostADD") {
  
  # library(mgcv)
  
  # #  parametres 
  
  # dt=dades
  # v.basal="HBpreADD"
  # v.final="HBpostADD"
  
  dt <-dt %>% dplyr::mutate(canvi=dt[,v.basal]-dt[,v.final]) 
  # Genero quintils
  dt<-dt %>% dplyr::mutate(basal_cat5=cut2(dt[,v.basal], g=5))
  
  ## Elimino missings de taula i selecciono variables
  
  dt<-dt %>% 
    tidyr::drop_na(canvi) %>% 
    dplyr::select_(v.basal,v.final,"canvi","basal_cat5")
  
  ## poso noms
  names(dt)<-c("pre","post","dif","basal_cat")
  
  lineal<-glm(dif~pre,weights = pre,family = gaussian, data=dt) %>% predict()
  lineal2<-glm(dif~pre,family = gaussian, data=dt) %>% predict()
  gam1<-mgcv::gam(dif~s(pre),family = gaussian, data=dt) %>% predict()
  gam2<-mgcv::gam(dif~s(pre,bs="cc",k=12),family = gaussian, data=dt) %>% predict()
  model.nolineal<-glm(dif~pre+I(pre^2)+I(pre^3),family = gaussian,data=dt) %>% predict()
  
  
  figuraZ<-dt %>% 
    ggplot2::ggplot(aes(x=pre, y=dif)) + 
    geom_point() + 
    ylab("Change at 6-12 months:HbA1c (%)") +
    xlab("HbA1c (%) Baseline") +
    geom_point(aes(y=lineal),color="red")+
    geom_point(aes(y=gam1),color="blue") +
    geom_point(aes(y=model.nolineal),color="green")+
    ggtitle(paste0(v.basal," Versus ",v.final)) # for the main title
  
  figuraZ
  
  
}

## Forest.plot --------------------

#######################  S'ha d'acabar

forest.plot<-function(data=df,label=rownames(df),mean=df$OR,lower=df$Linf,upper=df$Lsup) {
  
  # label=rownames(df)
  # df=modelaco
  # mean=df$OR
  # lower=df$Linf
  # upper=df$Lsup

  # 
  # label=df[label] 
  # mean=df[mean]
  # lower=df[lower]
  # upper=df[upper]
  # 
  # label<-as.vector(label)
  # mean<-mean %>% as.vector()
  # lower=lower %>% as.vector()
  # upper=upper %>% as.vector()
  
  # pp=data.frame(label,mean,lower,upper)

  fp <- ggplot(data=modelaco,aes(x=label, y=mean, ymin=lower, ymax=upper)) +
    geom_pointrange() + 
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab("Label") + ylab("Mean (95% CI)")
  fp
}

#  DATA RANDOM ENTRE DUES DATES (dataini i datafi) ---------------

data.random <- function(dataini=20120101, datafi=20121231) {
  
  # dataini=20120101
  # datafi=20161231
  
  dataini <- as.POSIXct(lubridate::ymd(dataini))
  datafi <- as.POSIXct(lubridate::ymd(datafi))
  temps <- as.numeric(difftime(datafi,dataini,unit="sec"))
  
  # Genera Data sumant temps random a dataini
  rt <- dataini + runif(1, 0, temps)
}

#  RETORNA UNA DATA A STRING  ------------------

data.to.string<-function(data) {
  
  data.string=paste0(year(data),
                     str_pad(lubridate::month(data),2,"left","0"),
                     str_pad(lubridate::day(data),2,"left","0"))
  
}


#  Random dates i marcar potencials CONTROLS-----------
##
## Genero N dates random entre 2010-2016 el mateix nombre que 

dt_index_data_random<-function(dt=PACIENTS) {
  
  # dt=PACIENTS
  # Necessito camp dtsortida (ymd)
  
  ####        Genero una data random entre 01/01/2010 i 31/12/2016
  
  set.seed(123)
  data_index_data<-dt %>% 
    nrow() %>% runif(as.Date("10/01/01", "%y/%m/%d"), as.Date("16/12/31", "%y/%m/%d")) %>% 
    data.table() %>% 
    setNames(.,c("dtindex.random")) %>% 
    mutate (
      dtindex.random=as.Date(dtindex.random, origin = "1970-01-01")
    )
  
  # Fusiono amb idp i selecciono POTENCIALS CONTROLS dins de periode de seguiment

  BD_PAC_DINDEX<-dt %>% 
    select(idp,dtsortida) %>%             
    cbind(data_index_data) %>%                                # Fusiono dates random
    filter(dtindex.random<=lubridate::ymd(dtsortida)) %>%     # Filtro només aquells que dins de la data de seguiment
    select (idp,dtindex.random) %>% 
    as_tibble()

}

#

#  GENERA UNA DATA INDEX SEGONS UNA DETERMINACIÓ ---------------------- 
## RETORNA DADES AMB idp + dtindex.semirandom

dt_index_data_semirandom<-function(dt=PACIENTS,dt.variables=VARIABLES,codi="EK201"){
  
  # dt=PACIENTS
  # dt.variables=VARIABLES
  # codi="EK201"
  
  # b) SEMI.RANDOM (amb un màxim de data a data sortida)
  
  # Una data entre tots Colesterol total (prèvies a data sortida) 
  # Si no hi ha cap Colesterol  alguna V clínica període (Random) 
  set.seed(123)
  ### Per cada pacient selecciono una dat random de entre tots els COLESTEROLS  (2010-2016)
  UN.COLESTEROL<-dt.variables %>%               
    filter(cod==codi) %>%                       # selecciono colesterols (Validar que es EK201)
    dplyr::left_join(dt,by="idp") %>%           # Junto pacients 
    select(idp,cod,dat,dtsortida) %>%           # Selecciono camps necessaris
    filter(!is.na(dtsortida)) %>%               # Filtro només pacients (amb dtsortida)
    filter (dat>=20100101 & dat<=dtsortida) %>%  # filtro Dates dins periode de seguiment 
    group_by(idp) %>%                           # Agafo un colesterol per cada idp
    sample_n(size = 1) %>%                      # Random
    ungroup %>% 
    select(idp, dat) %>% 
    rename(dat_col=dat) 
  
  ### Per cada pacient selecciono una dat random entre totes les VARIABLES 
  UNA.VARIABLE<-dt.variables %>%                # totes les variables  
    dplyr::left_join(dt,by="idp") %>%           # Junto pacients 
    select(idp,dat,dtsortida) %>%               # Selecciono camps necessaris
    filter(!is.na(dtsortida)) %>%               # Filtro només pacients amb dtsortida 
    filter (dat>=20100101 & dat<=dtsortida) %>% # Dates possibles dins el seguiment
    group_by(idp) %>%                           # Agafo unA fila per cada idp
    sample_n(size = 1) %>%                      # RAndom
    ungroup() %>% 
    select(idp, dat) %>% 
    rename(dat_var=dat)
  
  ### Fusió d'ambdos fitxers i selecciono una d'elles preferentment colesterol
  
  BDADES_DT_INDEX<-UNA.VARIABLE %>% 
    left_join(UN.COLESTEROL,by="idp") %>% 
    mutate(dtindex.semirandom=ifelse(is.na(dat_col),dat_var,dat_col)) %>% 
    select(idp,dtindex.semirandom)
  
}


#  MATCHING CAS-CONTROL SEGONS MÉTODE DENSITY-INCIDENCE ------------------

##  Retorna Subset matxejat per grup (event) en data index (dtindex.random, control) DE dt_pacients_dindex
##  Llista de variables variables.ps

matching_case_control<-function(dt=PACIENTS,variables.ps=llistaPS,dt_pacients_dindex=BD_PAC_DINDEX) {
  
  dt=PACIENTS
  variables.ps=c("edat","dtindex","sexe") # covaribles
  dt_pacients_dindex=BD_PAC_DINDEX

  # Es neceseciten camps com <dtsortida idp event> + llista de variables a matxejar
  # <idp, dtindex.random, control> en BD_PAC_DINDEX 
  
  # 2 Fusionar events i controls en una sola taula
  
  dt <-dt %>% 
    left_join(dt_pacients_dindex,by="idp")              # dt + dtindex.random (data random generada + de control
  
  # Selecciono events i mutar dataindex (event=1) en data d'event (dtsortida) 
  
  dtevents<-dt %>% filter(event==1) %>% mutate(dtindex=lubridate::ymd(dtsortida), event=1)         ## Els events data de sortida
  
  # Seleccionar controls i mutar dataindex en data index random 
  
  dtcontrols<-dt %>% filter(control==1) %>% mutate(dtindex=dtindex.random, event=0)     ## Els controls data random
  
  # Fusionar events + controls 
  dt.total<-dtevents %>% rbind(dtcontrols)  
  
  
  # 3 Agregar en data index (Edat)
  
  
  # Agrego en dtindex 
  
  dt.total<-dt.total %>% 
    mutate (edat=as.numeric((dtindex-lubridate::ymd(dnaix))/365.25))               # Calculo edat en dataindex
  
  
  # 4 Fer matching 
  
  # preparar dades per matching (idp + Llista matching)
  dadesmatching<-dt.total %>% select(idp,edat,dtindex,event,sexe)
  
  # Genero llista de covaraibles 
  formulaPS<-as.formula(paste("event", paste(variables.ps, collapse=" + "), sep=" ~ "))
  
  dt.matched<-formulaPS %>% 
    matchit(method="nearest",data=dadesmatching,ratio=4,caliper=0.01,distance = "logit") %>%    # FAig el matching 4 a 1
    weights() %>%                                                            # Guardo els pesos 
    data.table() %>% 
    'colnames<-'(c("PS")) %>% 
    bind_cols(dt.total) %>%                                                 # Ho junto al dt.total 
    filter(PS==1) %>% 
    as_tibble()
  
  
}

#  FLOW-CHART --------------------------

#   RETORNA UN FLOWCHART BASAT AMB ELS CRITERIS D'INCLUSIÓ, POBLACIÓ INICIAL I FINAL


diagramaFlowchart<-function(
  pob=c(1000,50),
  exc=c(10,1),
  exc_lab=c('Edat>90 anys','kkk'),
  colors=c('white','grey'),
  forma=c('box','box')) 
{
  
  if  (length(exc)<=10)
  {
    
    m1<-""
    for (i in 1:(length(exc) ))  
    { 
      m1<-paste0(m1,'->A',i) 
      i=i+1 }
    m1
    #"->A1->A2"#
    #---------------------------------------------------------------------------------------#
    m2<-""
    for (i in 1:(length(exc)))
    { 
      m2<-paste0(m2,' A',i,'->','E',i,'[color = black,dir=none]') 
      i=i+1 }
    m2
    #A1->E1[color = black,dir=none]A2->E2[color = black,dir=none]
    #---------------------------------------------------------------------------------------#
    m3<-""
    for (i in 1:(length(exc)))
    { 
      m3<-paste0(m3,' A',i,';') 
      i=i+1 }
    m3
    #A1;A2 
    #---------------------------------------------------------------------------------------#
    m4<-""
    for (i in 1:(length(exc)))
    { 
      m4<-paste0(m4,' E',i,';') 
      i=i+1 }
    m4
    #E1; E2;
    #---------------------------------------------------------------------------------------#
    m5<-""
    for (i in 1:(length(exc)))
    { 
      m5<-paste0(m5,'  subgraph {rank = same;',' A',i,';','E',i,'}') 
      i=i+1 }
    m5
    #subgraph {rank = same; A1;E1}  subgraph {rank = same; A2;E2}
    #---------------------------------------------------------------------------------------#
    m6<-""
    for (i in 1:(length(exc)))
    { 
      m6<-paste0(m6,'A',i,' [label =', "'@@",i+2,"']",';') 
      i=i+1 }
    m6
    #A1 [label = '@@3']
    #A2 [label = '@@4']
    #---------------------------------------------------------------------------------------#  
    m7<-""
    for (i in 1:(length(exc)))
    { 
      m7<-paste0(m7,'E',i,' [label =', "'@@",i+(length(exc)+2),"']",';') 
      i=i+1 }
    m7
    #E1 [label = '@@5']
    #E2 [label = '@@6']
    #---------------------------------------------------------------------------------------#
    m8<-""
    for (i in 1:(length(exc)) ) 
    { 
      m8<-paste0(m8," \n ","[",i+2,"]:paste0(' ')")
      i=i+1
    }
    m8 
    #" \n [3]:paste0(' ') \n [4]:paste0(' ')
    #---------------------------------------------------------------------------------------#
    
    paramet<-c(m1,m2,m3,m4,m5,m6,m7,m8)
    
    makao6<-paste0("digraph rai {","graph[layout = dot]",  
                   "node[shape=",forma[1],",","fontsize=12,fontname=Helvetica,width=0.9,
                   penwidth=0.9,color=black,style=filled,fillcolor=",colors[1],"]",
                   "P1;P2;", "node[shape=point,width =0,penwidth=0,color=black]",
                   paramet[3],
                   "node[shape=",forma[2],",","fontsize=8,fontname='Courier New',width=0.5,penwidth=0.5,style=filled,fillcolor=",colors[2],"]",
                   paramet[4],
                   " \n ","P1 [label = '@@1']","P2 [label = '@@2']",
                   paramet[6],
                   paramet[7],
                   " \n ","edge[width=0.5,penwidth=0.5,arrowhead=vee]",
                   " \n ","P1",
                   paramet[1],
                   "->P2[color = black,dir=none] ",
                   " \n ",
                   paramet[2],
                   " \n ",
                   paramet[5],"}",
                   
                   " \n ","[1]:paste0('Població Inicial RG 2008-16 [N = ',",pob[1],",']')"   ,
                   " \n ","[2]:paste0('Població Final Obj 1 [N = ',",pob[2],",']')"   ,
                   paramet[8],
                   
                   "\n[5]:paste0('", exc_lab[1], "[N = ',", exc[1], ",']')"   ,
                   "\n[6]:paste0('", exc_lab[2], "[N = ',", exc[2], ",']')"  ,
                   "\n[7]:paste0('", exc_lab[3], "[N = ',", exc[3], ",']')"   ,
                   "\n[8]:paste0('", exc_lab[4], "[N = ',", exc[4], ",']')"  ,
                   "\n[9]:paste0('", exc_lab[5], "[N = ',", exc[5], ",']')"   ,
                   "\n[10]:paste0('", exc_lab[6], "[N = ',", exc[6], ",']')"  ,
                   "\n[11]:paste0('", exc_lab[7], "[N = ',", exc[7], ",']')"   ,
                   "\n[12]:paste0('", exc_lab[8], "[N = ',", exc[8], ",']')"  ,
                   "\n[13]:paste0('", exc_lab[9], "[N = ',", exc[9], ",']')"   ,
                   "\n[14]:paste0('", exc_lab[10], "[N = ',", exc[10], ",']')"  
                   
    )
    
    
    #---------------------------------------------------------------------------------------#
    DiagrammeR::grViz(makao6)
    #---------------------------------------------------------------------------------------#
    
    
  }
  else
  {print("ERROR!, Les Exlusions han de ser iguals o inferiors a 10 !")
    
  }
}

### DiagrameR  -------------------------------------

diagramaFlowchar_BI<-function(
  
  pob=c(70211123),
  pob1=c(10088,50),
  exc1=c(1021,111,9),
  exc_lab1=c('Edat>90 anys','Cardio','J'),
  pob2=c(19002,599),
  exc2=c(1002,150,90),
  exc_lab2=c('Edat>76 anys','Rata','U'),
  colors=c('white','grey'),
  forma=c('box','box')            ) 
{
  
  if  (length(exc1)<=10 && length(exc2)<=10)
  {
    
    
    #node2
    #-----------------------------------------------------------------------------------#
    m3a<-""
    for (i in 1:(length(exc1)))
    { 
      m3a<-paste0(m3a,' A',i,';') 
      i=i+1 }
    m3a
    #A1;A2 
    #-----------------------------------------------------------------------------------#
    m3b<-""
    for (i in 1:(length(exc2)))
    { 
      m3b<-paste0(m3b,' B',i,';') 
      i=i+1 }
    m3b
    #B1;B2 
    
    #node3
    #-----------------------------------------------------------------------------------#
    m4a<-""
    for (i in 1:(length(exc1)))
    { 
      m4a<-paste0(m4a,' E_A',i,';') 
      i=i+1 }
    m4a
    #E_A1; E_A2
    #-----------------------------------------------------------------------------------#
    m4b<-""
    for (i in 1:(length(exc2)))
    { 
      m4b<-paste0(m4b,' E_B',i,';') 
      i=i+1 }
    m4b
    #E_B1; E_B2;
    #-----------------------------------------------------------------------------------#
    m6a<-""
    for (i in 1:(length(exc1)))
    { 
      m6a<-paste0(m6a,'A',i,'[label=', "'@@",i+5,"']",';') 
      i=i+1 }
    m6a
    #A1 [label ='@@6'];A2 [label ='@@7'];
    #-----------------------------------------------------------------------------------#
    m6b<-""
    for (i in 1:(length(exc2)))
    { 
      m6b<-paste0(m6b,'B',i,'[label=', "'@@",(i+length(exc2))+5,"']",';') 
      i=i+1 }
    m6b
    #B1[label='@@8'];B2[label='@@9']
    #-----------------------------------------------------------------------------------#
    
    #-----------------------#
    #(i+(2*length(exc1)))+5
    #-----------------------#
    m7a<-""
    for (i in 1:(length(exc1)))
    { 
      m7a<-paste0(m7a,'E_A',i,' [label =', "'@@",19+i,"']",';') 
      i=i+1 }
    m7a
    #"E_A1 [label ='@@20'];E_A2 [label ='@@21'];"
    #-----------------------------------------------------------------------------------#
    
    
    #------------------------#
    #(i+(3*length(exc1)))+5
    #------------------------#
    m7b<-""
    for (i in 1:(length(exc2)))
    { 
      m7b<-paste0(m7b,'E_B',i,' [label =', "'@@",29+i,"']",';') 
      i=i+1 }
    m7b
    #E_B1 [label ='@@30'];E_B2 [label ='@@31']
    
    #-----------------------------------------------------------------------------------#
    m1a<-""
    for (i in 1:(length(exc1) ))  
    { 
      m1a<-paste0(m1a,'->A',i) 
      i=i+1 }
    m1a
    #"->A1->A2"#
    #-----------------------------------------------------------------------------------#
    m1b<-""
    for (i in 1:(length(exc2) ))  
    { 
      m1b<-paste0(m1b,'->B',i) 
      i=i+1 }
    m1b
    #"->B1->B2"
    #-----------------------------------------------------------------------------------#
    m2a<-""
    for (i in 1:(length(exc1)))
    { 
      m2a<-paste0(m2a,' A',i,'->','E_A',i,'[color = black,dir=none]') 
      i=i+1 }
    m2a
    #A1->E_A1[color = black,dir=none] A2->E_A2[color = black,dir=none]
    #-----------------------------------------------------------------------------------#
    m2b<-""
    for (i in 1:(length(exc2)))
    { 
      m2b<-paste0(m2b,' B',i,'->','E_B',i,'[color = black,dir=none]') 
      i=i+1 }
    m2b
    #B1->E_B1[color = black,dir=none] B2->E_B2[color = black,dir=none]
    #-----------------------------------------------------------------------------------#
    m5a<-""
    for (i in 1:(length(exc1)))
    { 
      m5a<-paste0(m5a,'  subgraph {rank = same;',' A',i,';','E_A',i,';','}') 
      i=i+1 }
    m5a
    #"  subgraph {rank = same; A1;E_A1}  subgraph {rank = same; A2;E_A2}"
    #-----------------------------------------------------------------------------------#
    m5b<-""
    for (i in 1:(length(exc2)))
    { 
      m5b<-paste0(m5b,'  subgraph {rank = same;',' B',i,';','E_B',i,';','}') 
      i=i+1 }
    m5b
    #"  subgraph {rank = same; B1;E_B1}  subgraph {rank = same; B2;E_B2}"
    #-----------------------------------------------------------------------------------#
    
    
    #-----------------------------------------------------------------------------------#
    paramet2<-c(m1a,m1b,m2a,m2b,m3a,m3b,m4a,m4b,m5a,m5b,m6a,m6b,m7a,m7b)
    #-----------------------------------------------------------------------------------#
    
    length(paramet2)
    
    
    
    #node[shape=point,width =0,penwidth=0,color=black]",paramet2[5],
    
    makao7<-paste0("digraph rai {","graph[layout = dot]",  
                   "node[shape=",forma[1],",","fontsize=12,fontname=Helvetica,width=0.9,penwidth=0.9,color=black,style=filled,fillcolor=",colors[1],"]"," P_T;PA_I;PA_F;PB_I;PB_F ", 
                   
                   "node[shape=",forma[2],",","fontsize=8,fontname='Courier New',width=0,penwidth=0,style=filled,fillcolor=",colors[2],"]",
                   paramet2[7],paramet2[8],
                   
                   "node[shape=point,width =0,penwidth=0,color=black,fontname='Courier New']",paramet2[5],paramet2[6],
                   
                   " \n ","P_T[label='@@1']","PA_I[label='@@2']","PA_F[label='@@4']","PB_I[label='@@3']","PB_F[label='@@5']",
                   
                   paramet2[13],paramet2[14],
                   
                   paramet2[11],paramet2[12],
                   
                   " \n ","edge[width=0.5,penwidth=0.5,arrowhead=vee]",
                   " \n ","P_T->PA_I[color = black,arrowhead=vee]",
                   " \n ","P_T->PB_I[color = black,arrowhead=vee]",
                   " \n ","PA_I",paramet2[1],"->PA_F[color = black,dir=none] ",
                   " \n ",paramet2[3],
                   " \n ","PB_I",paramet2[2],"->PB_F[color = black,dir=none] ",
                   " \n ",paramet2[4],
                   " \n ",paramet2[9],
                   " \n ",paramet2[10],"}",
                   " \n "," [1]:paste0('Poblaci?                   [N = ',",pob[1], ",']')"   ,
                   " \n " ,"[2]:paste0('Poblaci? Inicial A         [N = ',",pob1[1],",']')"   ,
                   " \n " ,"[3]:paste0('Poblaci? Inicial B         [N = ',",pob2[1],",']')"   ,
                   " \n " ,"[4]:paste0('Poblaci? Final A           [N = ',",pob1[2],",']')"   ,
                   " \n " ,"[5]:paste0('Poblaci? Final B           [N = ',",pob2[2],",']')"   ,
                   " \n[6]:paste0('')"," \n[7]:paste0('')"," \n[8]:paste0('')"," \n[9]:paste0('')",
                   " \n[10]:paste0('')"," \n[11]:paste0('')"," \n[12]:paste0('')"," \n[13]:paste0('')",
                   " \n[14]:paste0('')"," \n[15]:paste0('')"," \n[16]:paste0('')"," \n[17]:paste0('')",
                   " \n[18]:paste0('')"," \n[19]:paste0('')",
                   " \n[20]:paste0('", exc_lab1[1], "[N = ',", exc1[1], ",']')",
                   " \n[21]:paste0('", exc_lab1[2], "[N = ',", exc1[2], ",']')",
                   " \n[22]:paste0('", exc_lab1[3], "[N = ',", exc1[3], ",']')",
                   " \n[23]:paste0('", exc_lab1[4], "[N = ',", exc1[4], ",']')",
                   " \n[24]:paste0('", exc_lab1[5], "[N = ',", exc1[5], ",']')",
                   " \n[25]:paste0('", exc_lab1[6], "[N = ',", exc1[6], ",']')",
                   " \n[26]:paste0('", exc_lab1[7], "[N = ',", exc1[7], ",']')",
                   " \n[27]:paste0('", exc_lab1[8], "[N = ',", exc1[8], ",']')",
                   " \n[28]:paste0('", exc_lab1[9], "[N = ',", exc1[9], ",']')",
                   " \n[29]:paste0('", exc_lab1[10], "[N = ',",exc1[10], ",']')",
                   " \n[30]:paste0('", exc_lab2[1], "[N = ',", exc2[1], ",']')",
                   " \n[31]:paste0('", exc_lab2[2], "[N = ',", exc2[2], ",']')",
                   " \n[32]:paste0('", exc_lab2[3], "[N = ',", exc2[3], ",']')",
                   " \n[33]:paste0('", exc_lab2[4], "[N = ',", exc2[4], ",']')",
                   " \n[34]:paste0('", exc_lab2[5], "[N = ',", exc2[5], ",']')",
                   " \n[35]:paste0('", exc_lab2[6], "[N = ',", exc2[6], ",']')",
                   " \n[36]:paste0('", exc_lab2[7], "[N = ',", exc2[7], ",']')",
                   " \n[37]:paste0('", exc_lab2[8], "[N = ',", exc2[8], ",']')",
                   " \n[38]:paste0('", exc_lab2[9], "[N = ',", exc2[9], ",']')",
                   " \n[39]:paste0('", exc_lab2[10], "[N = ',",exc2[10], ",']')"
    )
    
    
    
    #---------------------------------------------------------------------------------------#
    DiagrammeR::grViz(makao7)
    #---------------------------------------------------------------------------------------#
    
  }
  else
  {print("ERROR!, Les Exlusions han de ser iguals o inferiors a 10 !")
    
    
    
  }
}





#  NETEJA NOMS DE VARIABLES DE CARACTERS EXTRANYS ("/","(".....) ---------------

netejar.noms.variables<-function(dt=LIPOS_EORTEGA){
  
  paco<-names(dt) %>% 
    iconv("UTF-8","ASCII","") %>% 
    stringr::str_replace("/","") %>% 
    stringr::str_replace_all("\\(","") %>% 
    stringr::str_replace_all("\\)","") %>% 
    stringr::str_replace_all("\\/","") %>% 
    stringr::str_trim() %>% 
    stringr::str_replace_all(" ","_") %>% 
    stringr::str_replace_all("-","_")
  
  names(dt)<-paco
  dt
  
}


#      FI GENERAR FUNCION  

###########################################################
#                                                         #
#                                                         #
#                                                         #
#                                                         #
###########################################################



diagramaFlowchartFINAL<-function(
  
  grups=3,
  pob_lab=c("Poblaci? Total"),
  pob_lab1=c("Poblaci? Inicial A","Poblaci? Final A"),
  pob_lab2=c("Poblaci? Inicial B","Poblaci? Final B"),
  pob_lab3=c("Poblaci? Inicial C","Poblaci? Final C"),
  pob=c(70211123),
  pob1=c(10088,50),
  exc1=c(1021,111,9),
  exc_lab1=c('Edat>90 anys','Cardio','J'),
  pob2=c(19002,599),
  exc2=c(1002,150,90),
  exc_lab2=c('Edat>76 anys','Rata','U'),
  pob3=c(19002,599),
  exc3=c(1002,150,0),
  exc_lab3=c('Edat>91 anys','Pulm?','L'),
  colors=c('white','grey'),
  forma=c('box','box')    )


{
  
  if  (grups<1)
  {print("Error, posa els GRUPS, si us plau! al Flowchart!")  }
  else if  (grups==1)
  {diagramaFlowchart1G(
    pob1=pob1,
    pob_lab1=pob_lab1,
    exc1=exc1,
    exc_lab1=exc_lab1,
    colors=colors,
    forma=forma)  }
  else if (grups==2)
  {diagramaFlowchart2G(
    pob=pob,
    pob_lab=pob_lab,
    pob1=pob1,
    exc1=exc1,
    exc_lab1=exc_lab1,
    pob2=pob2,
    exc2=exc2,
    exc_lab2=exc_lab2,
    colors=colors,
    forma=forma ) }
  else if (grups==3)
  {diagramaFlowchart3G(
    pob=pob,
    pob_lab=pob_lab,
    pob1=pob1,
    exc1=exc1,
    exc_lab1=exc_lab1,
    pob2=pob2,
    exc2=exc2,
    exc_lab2=exc_lab2,
    pob3=pob3,
    exc3=exc3,
    exc_lab3=exc_lab3,
    colors=colors,
    forma=forma) }
  else if (grups>3)
  {
    print("Error no podem fer m?s de 3 Grups pel Flowchart!")
  }
}






#---------------------------------------------------------------------------------------# 
#diagramaFlowchartFINAL()
#---------------------------------------------------------------------------------------#

#diagramaFlowchartFINAL(
#  grups=1,
#  pob1=c(1000,50),
#  exc1=c(10),
#  exc_lab1=c('Edat>90 anys'),
#  colors=c('white','grey'),
#  forma=c('box','ellipse'))


#diagramaFlowchartFINAL(
#  grups=1,
#  pob_lab1=c("Poblaci? Inicial Aaaaaaaaa","Poblaci? Final A"),
#  pob1=c(1000,50),
#  exc1=c(10,1),
#  exc_lab1=c('Edat>90 anys','q'),
#  colors=c('white','grey'),
#  forma=c('box','ellipse'))




#diagramaFlowchartFINAL(
#  grups=2,
#  pob_lab=c("Poblaci? Total"),
#  pob_lab1=c("Poblaci? Inicial A","Poblaci? Final A"),
#  pob_lab2=c("Poblaci? Inicial B","Poblaci? Final B"),
#  pob=c(70211123),
#  pob1=c(10088,50),
#  exc1=c(1021,111,9),
#  exc_lab1=c('Edat>90 anys','Cardio','J'),
#  pob2=c(19002,599),
#  exc2=c(1002,150,90),
#  exc_lab2=c('Edat>76 anys','Rata','U'),
#  colors=c('white','grey'),
#  forma=c('box','ellipse')
#)




#diagramaFlowchartFINAL(
#  grups=10,
#  pob_lab1=c("Poblaci? Inicial A","Poblaci? Final A"),
#  pob1=c(1000,50),
#  exc1=c(10),
#  exc_lab1=c('Edat>90 anys'),
#  colors=c('white','grey'),
#  forma=c('box','ellipse')
#  
#)


#diagramaFlowchartFINAL(
#  grups=3,
#  pob=c(1000),
#  pob1=c(500,400),
#  exc1=c(50,25,25),
#  exc_lab1=c('Edat>90 anys','Cardio','Pulmonar'),
#  pob2=c(500,200),
#  exc2=c(100,50,150),
#  exc_lab2=c('Edat>76 anys','Diabetis2','Nefro'),
#  pob3=c(69,20),
#  exc3=c(10,5,15),
#  exc_lab3=c('Edat>23 anys','Obesitat','Peus'),
#  colors=c('white','grey'),
#  forma=c('ellipse','box') )


#----------------------------------------------------------------------------------------------#
#FLOW-CHART PRIMERA PART:####
#-------------------------------------------------------------------------------------#
#23.1.2019
#--------------------------------------------------------------------------------#
#10.12.2018 :[07:30]
#09.12.2018 :[21:33]
#--------------------------------------------------------------------------------#
#05.11.2018 :[17:30]                                                             #
#--------------------------------------------------------------------------------#
#30.11.2018#
#--------------------------------------------------------------------------------#
#[28.11.2018][07.12.2018]
#--------------------------------------------------------------------------------#
library(DiagrammeR)
library(DiagrammeRsvg)
library(magrittr)
library(svglite)
library(rsvg)
library(jpeg)
library(plotrix)
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



#----------------------------------------------------------------------------------------------#
#                 I                    P  A R T  
#----------------------------------------------------------------------------------------------#


diagramaFlowchart1G<-function(
  pob_lab1=c("Poblaci? Inicial","Poblaci? Final"),
  pob1=c(1000,50),
  exc1=c(10,1),
  exc_lab1=c('Edat>90 anys','kkk'),
  colors=c('white','grey'),
  forma=c('box','box')) 
{
  
  if  (length(exc1)<=10)
  {
    
    m1<-""
    for (i in 1:(length(exc1) ))  
    { 
      m1<-paste0(m1,'->A',i) 
      i=i+1 }
    m1
    #"->A1->A2"#
    #---------------------------------------------------------------------------------------#
    m2<-""
    for (i in 1:(length(exc1)))
    { 
      m2<-paste0(m2,' A',i,'->','E',i,'[color = black,dir=none]') 
      i=i+1 }
    m2
    #A1->E1[color = black,dir=none]A2->E2[color = black,dir=none]
    #---------------------------------------------------------------------------------------#
    m3<-""
    for (i in 1:(length(exc1)))
    { 
      m3<-paste0(m3,' A',i,';') 
      i=i+1 }
    m3
    #A1;A2 
    #---------------------------------------------------------------------------------------#
    m4<-""
    for (i in 1:(length(exc1)))
    { 
      m4<-paste0(m4,' E',i,';') 
      i=i+1 }
    m4
    #E1; E2;
    #---------------------------------------------------------------------------------------#
    m5<-""
    for (i in 1:(length(exc1)))
    { 
      m5<-paste0(m5,'  subgraph {rank = same;',' A',i,';','E',i,'}') 
      i=i+1 }
    m5
    #subgraph {rank = same; A1;E1}  subgraph {rank = same; A2;E2}
    #---------------------------------------------------------------------------------------#
    m6<-""
    for (i in 1:(length(exc1)))
    { 
      m6<-paste0(m6,'A',i,' [label =', "'@@",i+2,"']",';') 
      i=i+1 }
    m6
    #A1 [label ='@@3'];A2 [label ='@@4'];
    #---------------------------------------------------------------------------------------#  
    m7<-""
    for (i in 1:(length(exc1)))
    { 
      m7<-paste0(m7,'E',i,' [label =', "'@@",i+(length(exc1)+2),"']",';') 
      i=i+1 }
    m7
    #E1 [label ='@@5'];E2 [label ='@@6'];
    #---------------------------------------------------------------------------------------#
    m8<-""
    for (i in 1:(length(exc1)) ) 
    { 
      m8<-paste0(m8," \n ","[",i+2,"]:paste0(' ')")
      i=i+1 }
    m8 
    #" \n [3]:paste0(' ') \n [4]:paste0(' ')
    #---------------------------------------------------------------------------------------#
    paramet<-c(m1,m2,m3,m4,m5,m6,m7,m8)
    #---------------------------------------------------------------------------------------#  
    makao1<-paste0("digraph rai {","graph[layout = dot]",  
                   "node[shape=",forma[1],",","fontsize=12,fontname=Helvetica,width=0.9,
                   penwidth=0.9,color=black,style=filled,fillcolor=",colors[1],"]",
                   "P1;P2;", "node[shape=point,width =0,penwidth=0,color=black]",
                   paramet[3],
                   "node[shape=",forma[2],",","fontsize=8,fontname='Courier New',width=0.5,penwidth=0.5,style=filled,fillcolor=",colors[2],"]",
                   paramet[4],
                   " \n ","P1 [label = '@@1']","P2 [label = '@@2']",
                   paramet[6],
                   paramet[7],
                   " \n ","edge[width=0.5,penwidth=0.5,arrowhead=vee]",
                   " \n ","P1",
                   paramet[1],
                   "->P2[color = black,dir=none] ",
                   " \n ",
                   paramet[2],
                   " \n ",
                   paramet[5],"}",
                   "\n[1]:paste0('", pob_lab1[1], " \\n ", "[N = ',", pob1[1], ",']')"   ,
                   "\n[2]:paste0('", pob_lab1[2], " \\n ", "[N = ',", pob1[2], ",']')"   ,
                   paramet[8],
                   "\n[5]:paste0('", exc_lab1[1], " \\n ", "[N = ',", exc1[1], ",']')"   ,
                   "\n[6]:paste0('", exc_lab1[2], " \\n ", "[N = ',", exc1[2], ",']')"  ,
                   "\n[7]:paste0('", exc_lab1[3], " \\n ", "[N = ',", exc1[3], ",']')"   ,
                   "\n[8]:paste0('", exc_lab1[4], " \\n ", "[N = ',", exc1[4], ",']')"  ,
                   "\n[9]:paste0('", exc_lab1[5], " \\n ", "[N = ',", exc1[5], ",']')"   ,
                   "\n[10]:paste0('", exc_lab1[6], " \\n ", "[N = ',", exc1[6], ",']')"  ,
                   "\n[11]:paste0('", exc_lab1[7], " \\n ", "[N = ',", exc1[7], ",']')"   ,
                   "\n[12]:paste0('", exc_lab1[8], " \\n ", "[N = ',", exc1[8], ",']')"  ,
                   "\n[13]:paste0('", exc_lab1[9], " \\n ", "[N = ',", exc1[9], ",']')"   ,
                   "\n[14]:paste0('", exc_lab1[10], " \\n ", "[N = ',", exc1[10], ",']')"  
                   
    )
    
    
    #---------------------------------------------------------------------------------------#
    DiagrammeR::grViz(makao1)
    #---------------------------------------------------------------------------------------#
    
    
  }
  else
  {print("ERROR!, Les Exlusions han de ser iguals o inferiors a 10 !")
    
  }
}
#---------------------------------------------------------------------------------------# 

#diagramaFlowchart1G()
#---------------------------------------------------------------------------------------#
#i)
#---------------------------------------------------------------------------------------#
#diagramaFlowchart1G(
#  pob1=c(1000,50),
#  exc1=c(10),
#  exc_lab1=c('Edat>90 anys'),
#  colors=c('white','grey'),
#  forma=c('box','ellipse'))
#---------------------------------------------------------------------------------------#
#23.01.2019
#----------------------------------------------------------------------------------------------#
#                 I I                   P  A R T S 
#----------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------#
#07.01.2019                                                                                    #
#----------------------------------------------------------------------------------------------#
#4.1.2018#
#----------------------------------------------------------------------------------------------#

#FLOW-CHART [IDEA:]####

#----------------------------------------------------------------------------------------------#
#Per buscar una funci? primer de tot l'hem d'instal.lar, ?s com  comprar un llibre i 
#posar-l'ho a les estanteries, un cop posat, hem de carregar la funci? , ?s el
#mateix que agafar el llibre de l'estanteria i col.locar-l'ho a la taula per consultar.
#Podem crear les nostres funcions, ?s com escriure el nostre propi llibre, el llibre, 
#no ?s res m?s que un conjunt de codis, unes lletres unes frases, uns conceptes ,
#UNA IDEA.!!!!!
#----------------------------------------------------------------------------------------------#
#FLOW-CHART SEGONA PART:####
#-------------------------------------------------------------------------------------#
library(DiagrammeR)
library(DiagrammeRsvg)
#-------------------------------------------------------------------------------------#





diagramaFlowchart2G<-function(
  pob_lab=c("Poblaci? Total"),
  pob_lab1=c("Poblaci? Inicial A","Poblaci? Final A"),
  pob_lab2=c("Poblaci? Inicial B","Poblaci? Final B"),
  pob=c(70211123),
  pob1=c(10088,50),
  exc1=c(1021,111,9),
  exc_lab1=c('Edat>90 anys','Cardio','J'),
  pob2=c(19002,599),
  exc2=c(1002,150,90),
  exc_lab2=c('Edat>76 anys','Rata','U'),
  colors=c('white','grey'),
  forma=c('box','box')            ) 
{
  
  if  (length(exc1)<=10 && length(exc2)<=10)
  {
    
    #-----------------------------------------------------------------------------------#
    m1a<-""
    for (i in 1:(length(exc1) ))  
    { 
      m1a<-paste0(m1a,'->A',i) 
      i=i+1 }
    m1a
    #->A1->A2->A3    
    #-----------------------------------------------------------------------------------#
    m1b<-""
    for (i in 1:(length(exc2) ))  
    { 
      m1b<-paste0(m1b,'->B',i) 
      i=i+1 }
    m1b
    #->B1->B2->B3
    #-----------------------------------------------------------------------------------#
    m2a<-""
    for (i in 1:(length(exc1)))
    { 
      m2a<-paste0(m2a,' A',i,'->','E_A',i,'[color = black,dir=none]') 
      i=i+1 }
    m2a
    #A1->E_A1[color = black,dir=none] A2->E_A2[color = black,dir=none] A3->E_A3[color = black,dir=none]
    #-----------------------------------------------------------------------------------#
    m2b<-""
    for (i in 1:(length(exc2)))
    { 
      m2b<-paste0(m2b,' B',i,'->','E_B',i,'[color = black,dir=none]') 
      i=i+1 }
    m2b
    #B1->E_B1[color = black,dir=none] B2->E_B2[color = black,dir=none] B3->E_B3[color = black,dir=none]    
    #-----------------------------------------------------------------------------------#
    m3a<-""
    for (i in 1:(length(exc1)))
    { 
      m3a<-paste0(m3a,' A',i,';') 
      i=i+1 }
    m3a
    #A1; A2; A3;
    #-----------------------------------------------------------------------------------#
    m3b<-""
    for (i in 1:(length(exc2)))
    { 
      m3b<-paste0(m3b,' B',i,';') 
      i=i+1 }
    m3b
    #" B1; B2; B3;"
    #-----------------------------------------------------------------------------------#
    m4a<-""
    for (i in 1:(length(exc1)))
    { 
      m4a<-paste0(m4a,' E_A',i,';') 
      i=i+1 }
    m4a
    #E_A1; E_A2; E_A3;
    #-----------------------------------------------------------------------------------#
    m4b<-""
    for (i in 1:(length(exc2)))
    { 
      m4b<-paste0(m4b,' E_B',i,';') 
      i=i+1 }
    m4b
    #E_B1; E_B2; E_B3;
    #-----------------------------------------------------------------------------------#
    m5a<-""
    for (i in 1:(length(exc1)))
    { 
      m5a<-paste0(m5a,'  subgraph {rank = same;',' A',i,';','E_A',i,';','}') 
      i=i+1 }
    m5a
    # subgraph {rank = same; A1;E_A1;}  subgraph {rank = same; A2;E_A2;}  subgraph {rank = same; A3;E_A3;}
    #-----------------------------------------------------------------------------------#
    m5b<-""
    for (i in 1:(length(exc2)))
    { 
      m5b<-paste0(m5b,'  subgraph {rank = same;',' B',i,';','E_B',i,';','}') 
      i=i+1 }
    m5b
    #subgraph {rank = same; B1;E_B1;}  subgraph {rank = same; B2;E_B2;}  subgraph {rank = same; B3;E_B3;}
    #-----------------------------------------------------------------------------------#
    m6a<-""
    for (i in 1:(length(exc1)))
    { 
      m6a<-paste0(m6a,'A',i,'[label=', "'@@",i+5,"']",';') 
      i=i+1 }
    m6a
    #A1 A1[label='@@6'];A2[label='@@7'];A3[label='@@8'];
    #-----------------------------------------------------------------------------------#
    m6b<-""
    for (i in 1:(length(exc2)))
    { 
      m6b<-paste0(m6b,'B',i,'[label=', "'@@",(i+length(exc2))+5,"']",';') 
      i=i+1 }
    m6b
    #B1[label='@@9'];B2[label='@@10'];B3[label='@@11'];
    #-----------------------------------------------------------------------------------#
    m7a<-""
    for (i in 1:(length(exc1)))
    { 
      m7a<-paste0(m7a,'E_A',i,' [label =', "'@@",19+i,"']",';') 
      i=i+1 }
    m7a
    #E_A1 [label ='@@20'];E_A2 [label ='@@21'];E_A3 [label ='@@22'];
    #-----------------------------------------------------------------------------------#
    m7b<-""
    for (i in 1:(length(exc2)))
    { 
      m7b<-paste0(m7b,'E_B',i,' [label =', "'@@",29+i,"']",';') 
      i=i+1 }
    m7b
    #E_B1 [label ='@@30'];E_B2 [label ='@@31'];E_B3 [label ='@@32'];
    #-----------------------------------------------------------------------------------#
    #-----------------------------------------------------------------------------------#
    paramet2<-c(m1a,m1b,m2a,m2b,m3a,m3b,m4a,m4b,m5a,m5b,m6a,m6b,m7a,m7b)
    #-----------------------------------------------------------------------------------#
    makao2<-paste0("digraph rai {","graph[layout = dot]",  
                   "node[shape=",forma[1],",","fontsize=12,fontname=Helvetica,width=0.9,penwidth=0.9,color=black,style=filled,fillcolor=",colors[1],"]"," P_T;PA_I;PA_F;PB_I;PB_F ", 
                   
                   "node[shape=",forma[2],",","fontsize=8,fontname='Courier New',width=0,penwidth=0,style=filled,fillcolor=",colors[2],"]",
                   paramet2[7],paramet2[8],
                   
                   "node[shape=point,width =0,penwidth=0,color=black,fontname='Courier New']",paramet2[5],paramet2[6],
                   
                   " \n ","P_T[label='@@1']","PA_I[label='@@2']","PA_F[label='@@4']","PB_I[label='@@3']","PB_F[label='@@5']",
                   
                   paramet2[13],paramet2[14],
                   
                   paramet2[11],paramet2[12],
                   
                   " \n ","edge[width=0.5,penwidth=0.5,arrowhead=vee]",
                   " \n ","P_T->PA_I[color = black,arrowhead=vee]",
                   " \n ","P_T->PB_I[color = black,arrowhead=vee]",
                   " \n ","PA_I",paramet2[1],"->PA_F[color = black,dir=none] ",
                   " \n ",paramet2[3],
                   " \n ","PB_I",paramet2[2],"->PB_F[color = black,dir=none] ",
                   " \n ",paramet2[4],
                   " \n ",paramet2[9],
                   " \n ",paramet2[10],"}",
                   " \n[1]:paste0('", pob_lab[1], " \\n "," [N = ',",  pob[1], ",']')",
                   " \n[2]:paste0('", pob_lab1[1]," \\n ", " [N = ',", pob1[1], ",']')",
                   " \n[3]:paste0('", pob_lab2[1]," \\n ", " [N = ',", pob2[1], ",']')",
                   " \n[4]:paste0('", pob_lab1[2]," \\n ", " [N = ',", pob1[2], ",']')",
                   " \n[5]:paste0('", pob_lab2[2]," \\n ", " [N = ',", pob2[2], ",']')",
                   " \n[6]:paste0('')"," \n[7]:paste0('')"," \n[8]:paste0('')"," \n[9]:paste0('')",
                   " \n[10]:paste0('')"," \n[11]:paste0('')"," \n[12]:paste0('')"," \n[13]:paste0('')",
                   " \n[14]:paste0('')"," \n[15]:paste0('')"," \n[16]:paste0('')"," \n[17]:paste0('')",
                   " \n[18]:paste0('')"," \n[19]:paste0('')",
                   " \n[20]:paste0('", exc_lab1[1]," \\n ", "[N = ',", exc1[1], ",']')",
                   " \n[21]:paste0('", exc_lab1[2]," \\n ", "[N = ',", exc1[2], ",']')",
                   " \n[22]:paste0('", exc_lab1[3]," \\n ", "[N = ',", exc1[3], ",']')",
                   " \n[23]:paste0('", exc_lab1[4]," \\n ", "[N = ',", exc1[4], ",']')",
                   " \n[24]:paste0('", exc_lab1[5]," \\n ", "[N = ',", exc1[5], ",']')",
                   " \n[25]:paste0('", exc_lab1[6]," \\n ", "[N = ',", exc1[6], ",']')",
                   " \n[26]:paste0('", exc_lab1[7]," \\n ", "[N = ',", exc1[7], ",']')",
                   " \n[27]:paste0('", exc_lab1[8]," \\n ", "[N = ',", exc1[8], ",']')",
                   " \n[28]:paste0('", exc_lab1[9]," \\n ", "[N = ',", exc1[9], ",']')",
                   " \n[29]:paste0('", exc_lab1[10]," \\n ", "[N = ',",exc1[10], ",']')",
                   " \n[30]:paste0('", exc_lab2[1]," \\n ", "[N = ',", exc2[1], ",']')",
                   " \n[31]:paste0('", exc_lab2[2]," \\n ", "[N = ',", exc2[2], ",']')",
                   " \n[32]:paste0('", exc_lab2[3]," \\n ", "[N = ',", exc2[3], ",']')",
                   " \n[33]:paste0('", exc_lab2[4]," \\n ", "[N = ',", exc2[4], ",']')",
                   " \n[34]:paste0('", exc_lab2[5]," \\n ", "[N = ',", exc2[5], ",']')",
                   " \n[35]:paste0('", exc_lab2[6]," \\n ", "[N = ',", exc2[6], ",']')",
                   " \n[36]:paste0('", exc_lab2[7]," \\n ", "[N = ',", exc2[7], ",']')",
                   " \n[37]:paste0('", exc_lab2[8]," \\n ", "[N = ',", exc2[8], ",']')",
                   " \n[38]:paste0('", exc_lab2[9]," \\n ", "[N = ',", exc2[9], ",']')",
                   " \n[39]:paste0('", exc_lab2[10]," \\n ", "[N = ',",exc2[10], ",']')"
    )
    
    #---------------------------------------------------------------------------------------#
    DiagrammeR::grViz(makao2)
    #---------------------------------------------------------------------------------------#
    
  }
  else
  {print("ERROR!, Les Exlusions han de ser iguals o inferiors a 10 !")
    
    
    
  }
}
#---------------------------------------------------------------------------------------#

#diagramaFlowchart2G()
#---------------------------------------------------------------------------------------#
#i)
#diagramaFlowchart2G(pob=c(1000),
#                    pob1=c(500,400),
#                    exc1=c(50,25,25),
#                    exc_lab1=c('Edat>90 anys','Cardio','Pulmonar'),
#                    pob2=c(500,200),
#                    exc2=c(100,50,150),
#                    exc_lab2=c('Edat>76 anys','Diabetis2','Nefro'),
#                    colors=c('white','grey'),
#                    forma=c('ellipse','box') )
#---------------------------------------------------------------------------------------#

#23.01.2019
#----------------------------------------------------------------------------------------------#
#                 I I I                  P  A R T S 
#----------------------------------------------------------------------------------------------#
#22.01.2019
#----------------------------------------------------------------------------------------------#
#07.01.2019                                                                                    #
#----------------------------------------------------------------------------------------------#
#4.1.2018#
#----------------------------------------------------------------------------------------------#

#FLOW-CHART [IDEA:]####

#----------------------------------------------------------------------------------------------#
#Per buscar una funci? primer de tot l'hem d'instal.lar, ?s com  comprar un llibre i 
#posar-l'ho a les estanteries, un cop posat, hem de carregar la funci? , ?s el
#mateix que agafar el llibre de l'estanteria i col.locar-l'ho a la taula per consultar.
#Podem crear les nostres funcions, ?s com escriure el nostre propi llibre, el llibre, 
#no ?s res m?s que un conjunt de codis, unes lletres unes frases, uns conceptes ,
#UNA IDEA.!!!!!
#----------------------------------------------------------------------------------------------#



#3 PARTS!!


#FLOW-CHART SEGONA PART:####
#-------------------------------------------------------------------------------------#
library(DiagrammeR)
library(DiagrammeRsvg)
#-------------------------------------------------------------------------------------#

#[22.01.2019]

#----------------------------------------------------------------------------------------------#
#                               I I I             P A R T S !!
#----------------------------------------------------------------------------------------------#

diagramaFlowchart3G<-function(
  pob_lab=c("Poblaci? Total"),
  pob_lab1=c("Poblaci? Inicial A","Poblaci? Final A"),
  pob_lab2=c("Poblaci? Inicial B","Poblaci? Final B"),
  pob_lab3=c("Poblaci? Inicial C","Poblaci? Final C"),
  pob=c(70211123),
  pob1=c(10088,50),
  exc1=c(1021,111,9),
  exc_lab1=c('Edat>90 anys','Cardio','J'),
  pob2=c(19002,599),
  exc2=c(1002,150,90),
  exc_lab2=c('Edat>76 anys','Rata','U'),
  pob3=c(19002,599),
  exc3=c(1002,150,0),
  exc_lab3=c('Edat>91 anys','Pulm?','L'),
  colors=c('white','grey'),
  forma=c('box','box')            ) 
{
  
  if  (length(exc1)<=10 && length(exc2)<=10  && length(exc3)<=10    )
  {
    
    #-----------------------------------------------------------------------------------#
    m1a<-""
    for (i in 1:(length(exc1) ))  
    { 
      m1a<-paste0(m1a,'->A',i) 
      i=i+1 }
    m1a
    #"->A1->A2->A3"#
    #-----------------------------------------------------------------------------------#
    m1b<-""
    for (i in 1:(length(exc2) ))  
    { 
      m1b<-paste0(m1b,'->B',i) 
      i=i+1 }
    m1b
    #"->B1->B2->B3"
    #-----------------------------------------------------------------------------------#
    m1c<-""
    for (i in 1:(length(exc3) ))  
    { 
      m1c<-paste0(m1c,'->C',i) 
      i=i+1 }
    m1c
    #"->C1->C2->C3"
    #-----------------------------------------------------------------------------------#
    m2a<-""
    for (i in 1:(length(exc1)))
    { 
      m2a<-paste0(m2a,' A',i,'->','E_A',i,'[color = black,dir=none]') 
      i=i+1 }
    m2a
    #A1->E_A1[color = black,dir=none] A2->E_A2[color = black,dir=none] A3->E_A3[color = black,dir=none]
    #-----------------------------------------------------------------------------------#
    m2b<-""
    for (i in 1:(length(exc2)))
    { 
      m2b<-paste0(m2b,' B',i,'->','E_B',i,'[color = black,dir=none]') 
      i=i+1 }
    m2b
    #B1->E_B1[color = black,dir=none] B2->E_B2[color = black,dir=none]
    #-----------------------------------------------------------------------------------#
    m2c<-""
    for (i in 1:(length(exc3)))
    { 
      m2c<-paste0(m2c,' C',i,'->','E_C',i,'[color = black,dir=none]') 
      i=i+1 }
    m2c
    #C1->E_C1[color = black,dir=none] C2->E_C2[color = black,dir=none] C3->E_C3[color = black,dir=none]
    #-----------------------------------------------------------------------------------#    
    m3a<-""
    for (i in 1:(length(exc1)))
    { 
      m3a<-paste0(m3a,' A',i,';') 
      i=i+1 }
    m3a
    # A1; A2; A3;
    #-----------------------------------------------------------------------------------#
    m3b<-""
    for (i in 1:(length(exc2)))
    { 
      m3b<-paste0(m3b,' B',i,';') 
      i=i+1 }
    m3b
    #B1; B2; B3; 
    #-----------------------------------------------------------------------------------#
    m3c<-""
    for (i in 1:(length(exc3)))
    { 
      m3c<-paste0(m3c,' C',i,';') 
      i=i+1 }
    m3c
    #C1; C2; C3;
    #-----------------------------------------------------------------------------------#
    m4a<-""
    for (i in 1:(length(exc1)))
    { 
      m4a<-paste0(m4a,' E_A',i,';') 
      i=i+1 }
    m4a
    #E_A1; E_A2; E_A3;
    #-----------------------------------------------------------------------------------#
    m4b<-""
    for (i in 1:(length(exc2)))
    { 
      m4b<-paste0(m4b,' E_B',i,';') 
      i=i+1 }
    m4b
    #E_B1; E_B2; E_B3;"
    #-----------------------------------------------------------------------------------#
    m4c<-""
    for (i in 1:(length(exc3)))
    { 
      m4c<-paste0(m4c,' E_C',i,';') 
      i=i+1 }
    m4c
    #E_C1; E_C2; E_C3;
    #-----------------------------------------------------------------------------------#
    m5a<-""
    for (i in 1:(length(exc1)))
    { 
      m5a<-paste0(m5a,'  subgraph {rank = same;',' A',i,';','E_A',i,';','}') 
      i=i+1 }
    m5a
    #subgraph {rank = same; A1;E_A1;}  subgraph {rank = same; A2;E_A2;}  subgraph {rank = same; A3;E_A3;}"
    #-----------------------------------------------------------------------------------#
    m5b<-""
    for (i in 1:(length(exc2)))
    { 
      m5b<-paste0(m5b,'  subgraph {rank = same;',' B',i,';','E_B',i,';','}') 
      i=i+1 }
    m5b
    #subgraph {rank = same; B1;E_B1;}  subgraph {rank = same; B2;E_B2;}  subgraph {rank = same; B3;E_B3;}
    #-----------------------------------------------------------------------------------#
    m5c<-""
    for (i in 1:(length(exc3)))
    { 
      m5c<-paste0(m5c,'  subgraph {rank = same;',' C',i,';','E_C',i,';','}') 
      i=i+1 }
    m5c
    # subgraph {rank = same; C1;E_C1;}  subgraph {rank = same; C2;E_C2;}  subgraph {rank = same; C3;E_C3;}
    #-----------------------------------------------------------------------------------#
    m6a<-""
    for (i in 1:(length(exc1)))
    { 
      m6a<-paste0(m6a,'A',i,'[label=', "'@@",i+7,"']",';') 
      i=i+1 }
    m6a
    #A1[label='@@8'];A2[label='@@9'];A3[label='@@10'];
    #-----------------------------------------------------------------------------------#
    m6b<-""
    for (i in 1:(length(exc2)))
    { 
      m6b<-paste0(m6b,'B',i,'[label=', "'@@",(i+length(exc2))+7,"']",';') 
      i=i+1 }
    m6b
    #B1[label='@@11'];B2[label='@@12'];B3[label='@@13'];
    #-----------------------------------------------------------------------------------#
    m6c<-""
    for (i in 1:(length(exc3)))
    { 
      m6c<-paste0(m6c,'C',i,'[label=', "'@@",(i+length(exc3))+10,"']",';') 
      i=i+1 }
    m6c
    #C1[label='@@14'];C2[label='@@15'];C3[label='@@16'];"
    #-----------------------------------------------------------------------------------#
    m7a<-""
    for (i in 1:(length(exc1)))
    { 
      m7a<-paste0(m7a,'E_A',i,' [label =', "'@@",19+i,"']",';') 
      i=i+1 }
    m7a
    #"E_A1 [label ='@@20'];E_A2 [label ='@@21'];E_A3 [label ='@@22']"
    #-----------------------------------------------------------------------------------#
    m7b<-""
    for (i in 1:(length(exc2)))
    { 
      m7b<-paste0(m7b,'E_B',i,' [label =', "'@@",29+i,"']",';') 
      i=i+1 }
    m7b
    #E_B1 [label ='@@30'];E_B2 [label ='@@31'];E_B3 [label ='@@32']
    #-----------------------------------------------------------------------------------#
    m7c<-""
    for (i in 1:(length(exc3)))
    { 
      m7c<-paste0(m7c,'E_C',i,' [label =', "'@@",39+i,"']",';') 
      i=i+1 }
    m7c
    #"E_C1 [label ='@@40'];E_C2 [label ='@@41'];E_C3 [label ='@@42'];"
    #-----------------------------------------------------------------------------------#
    paramet2<-c(m1a,m1b,m1c,m2a,m2b,m2c,m3a,m3b,m3c,m4a,m4b,m4c,m5a,m5b,m5c,m6a,m6b,m6c,m7a,m7b,m7c)
    #-----------------------------------------------------------------------------------#
    makao3<-paste0("digraph rai {","graph[layout = dot]",  
                   "node[shape=",forma[1],",","fontsize=12,fontname=Helvetica,width=0.9,penwidth=0.9,color=black,style=filled,fillcolor=",colors[1],"]"," P_T;PA_I;PA_F;PB_I;PB_F;PC_I;PC_F ", 
                   
                   "node[shape=",forma[2],",","fontsize=8,fontname='Courier New',width=0,penwidth=0,style=filled,fillcolor=",colors[2],"]",
                   paramet2[10],paramet2[11],paramet2[12],
                   
                   "node[shape=point,width =0,penwidth=0,color=black,fontname='Courier New']",paramet2[7],paramet2[8],paramet2[9],
                   
                   " \n ","P_T[label='@@1']","PA_I[label='@@2']","PA_F[label='@@5']","PB_I[label='@@3']","PB_F[label='@@6']","PC_I[label='@@4']","PC_F[label='@@7']",
                   " \n ",paramet2[19],paramet2[20],paramet2[21],
                   " \n ",paramet2[16],paramet2[17],paramet2[18],
                   " \n ","edge[width=0.5,penwidth=0.5,arrowhead=vee]",
                   " \n ","P_T->PA_I[color = black,arrowhead=vee]",
                   " \n ","P_T->PB_I[color = black,arrowhead=vee]",
                   " \n ","P_T->PC_I[color = black,arrowhead=vee]",
                   " \n ","PA_I",paramet2[1],"->PA_F[color = black,dir=none] ",
                   " \n ",paramet2[4],
                   " \n ","PB_I",paramet2[2],"->PB_F[color = black,dir=none] ",
                   " \n ",paramet2[5],
                   " \n ","PC_I",paramet2[3],"->PC_F[color = black,dir=none] ",
                   " \n ",paramet2[6],
                   
                   " \n ",paramet2[13],
                   " \n ",paramet2[14],
                   " \n ",paramet2[15],"}",
                   
                   " \n[1]:paste0('", pob_lab[1], " \\n ", "  [N = ',",  pob[1], ",']')",
                   " \n[2]:paste0('", pob_lab1[1]," \\n ",  " [N = ',", pob1[1], ",']')",
                   " \n[3]:paste0('", pob_lab2[1]," \\n ", " [N = ',", pob2[1], ",']')",
                   " \n[4]:paste0('", pob_lab3[1]," \\n ", " [N = ',", pob3[1], ",']')",
                   " \n[5]:paste0('", pob_lab1[2]," \\n ", " [N = ',", pob1[2], ",']')",
                   " \n[6]:paste0('", pob_lab2[2]," \\n ", " [N = ',", pob2[2], ",']')",
                   " \n[7]:paste0('", pob_lab3[2]," \\n ", " [N = ',", pob3[2], ",']')",
                   " \n[8]:paste0('')"," \n[9]:paste0('')"," \n[10]:paste0('')"," \n[11]:paste0('')",
                   " \n[12]:paste0('')"," \n[13]:paste0('')"," \n[14]:paste0('')"," \n[15]:paste0('')",
                   " \n[16]:paste0('')"," \n[17]:paste0('')"," \n[18]:paste0('')"," \n[19]:paste0('')",
                   " \n[20]:paste0('", exc_lab1[1]," \\n ", "[N = ',", exc1[1], ",']')",
                   " \n[21]:paste0('", exc_lab1[2]," \\n ", "[N = ',", exc1[2], ",']')",
                   " \n[22]:paste0('", exc_lab1[3]," \\n ", "[N = ',", exc1[3], ",']')",
                   " \n[23]:paste0('", exc_lab1[4]," \\n ", "[N = ',", exc1[4], ",']')",
                   " \n[24]:paste0('", exc_lab1[5]," \\n ", "[N = ',", exc1[5], ",']')",
                   " \n[25]:paste0('", exc_lab1[6]," \\n ", "[N = ',", exc1[6], ",']')",
                   " \n[26]:paste0('", exc_lab1[7]," \\n ", "[N = ',", exc1[7], ",']')",
                   " \n[27]:paste0('", exc_lab1[8]," \\n ", "[N = ',", exc1[8], ",']')",
                   " \n[28]:paste0('", exc_lab1[9]," \\n ", "[N = ',", exc1[9], ",']')",
                   " \n[20]:paste0('", exc_lab1[10]," \\n ", "[N = ',",exc1[10], ",']')",
                   " \n[30]:paste0('", exc_lab2[1]," \\n ", "[N = ',", exc2[1], ",']')",
                   " \n[31]:paste0('", exc_lab2[2]," \\n ", "[N = ',", exc2[2], ",']')",
                   " \n[32]:paste0('", exc_lab2[3]," \\n ", "[N = ',", exc2[3], ",']')",
                   " \n[33]:paste0('", exc_lab2[4]," \\n ", "[N = ',", exc2[4], ",']')",
                   " \n[34]:paste0('", exc_lab2[5]," \\n ", "[N = ',", exc2[5], ",']')",
                   " \n[35]:paste0('", exc_lab2[6]," \\n ", "[N = ',", exc2[6], ",']')",
                   " \n[36]:paste0('", exc_lab2[7]," \\n ", "[N = ',", exc2[7], ",']')",
                   " \n[37]:paste0('", exc_lab2[8]," \\n ", "[N = ',", exc2[8], ",']')",
                   " \n[38]:paste0('", exc_lab2[9]," \\n ", "[N = ',", exc2[9], ",']')",
                   " \n[39]:paste0('", exc_lab2[10]," \\n ","[N = ',", exc2[10], ",']')",
                   " \n[40]:paste0('", exc_lab3[1]," \\n ", "[N = ',", exc3[1], ",']')",
                   " \n[41]:paste0('", exc_lab3[2]," \\n ", "[N = ',", exc3[2], ",']')",
                   " \n[42]:paste0('", exc_lab3[3]," \\n ", "[N = ',", exc3[3], ",']')",
                   " \n[43]:paste0('", exc_lab3[4]," \\n ", "[N = ',", exc3[4], ",']')",
                   " \n[44]:paste0('", exc_lab3[5]," \\n ", "[N = ',", exc3[5], ",']')",
                   " \n[45]:paste0('", exc_lab3[6]," \\n ", "[N = ',", exc3[6], ",']')",
                   " \n[46]:paste0('", exc_lab3[7]," \\n ", "[N = ',", exc3[7], ",']')",
                   " \n[47]:paste0('", exc_lab3[8]," \\n ", "[N = ',", exc3[8], ",']')",
                   " \n[48]:paste0('", exc_lab3[9]," \\n ", "[N = ',", exc3[9], ",']')",
                   " \n[49]:paste0('", exc_lab3[10]," \\n ", "[N = ',",exc3[10], ",']')"
    )                  
    
    #---------------------------------------------------------------------------------------#  
    DiagrammeR::grViz(makao3)
    #---------------------------------------------------------------------------------------#
    
  }
  else
  {print("ERROR!, Les Exlusions han de ser iguals o inferiors a 10 !")
    
    
    
  }
}
#---------------------------------------------------------------------------------------#

#diagramaFlowchart3G()
#---------------------------------------------------------------------------------------#
#i)
#diagramaFlowchart3G(pob=c(1000),
#                    pob1=c(500,400),
#                    exc1=c(50,25,25),
#                    exc_lab1=c('Edat>90 anys','Cardio','Pulmonar'),
#                    pob2=c(500,200),
#                    exc2=c(100,50,150),
#                    exc_lab2=c('Edat>76 anys','Diabetis2','Nefro'),
#                    pob3=c(69,20),
#                    exc3=c(10,5,15),
#                    exc_lab3=c('Edat>23 anys','Obesitat','Peus'),
#                    colors=c('white','grey'),
#                    forma=c('ellipse','box') )
#---------------------------------------------------------------------------------------#
