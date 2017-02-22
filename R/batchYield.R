#!/usr/bin/R
#extension for batch aplication of foreYield


#data importing front-end function (structure)
#allow for override with a saved detrended official dataset
importYield<-function(offiData,simuData,allData,crp,cnt,dec){
	if(missing(allData)){
		write.table(gsub(gsub(scan(file=offiData,what="text",sep="\n"),pattern='"',replacement='',fixed=TRUE),pattern=";",replacement=","),file="adjOffi.csv",quote=FALSE,row.names=FALSE,col.names=FALSE)
		write.table(gsub(gsub(scan(file=simuData,what="text",sep="\n"),pattern='"',replacement='',fixed=TRUE),pattern=";",replacement=","),file="adjSimu.csv",quote=FALSE,row.names=FALSE,col.names=FALSE)
	}else{write.table(gsub(gsub(scan(file=allData,what="text",sep="\n"),pattern='"',replacement='',fixed=TRUE),pattern=";",replacement=","),file="adjDtb.csv",quote=FALSE,row.names=FALSE,col.names=FALSE)
		longData<-read.csv("adjDtb.csv")
		library(tidyr)
		bothData<-spread(longData,INDICATOR_CODE,INDICATOR_VALUE)
		write.csv(bothData[,c(which(names(bothData)=="YEAR"),which(names(bothData)=="official.yield"|names(bothData)=="Officiall yield"|names(bothData)=="OFFICIAL_YIELD"|names(bothData)=="Official.yield"|names(bothData)=="official_yield"|names(bothData)=="Official_yield"))],"adjOffi.csv",quote=FALSE,row.names=FALSE)
		write.csv(bothData[,c(-which(names(bothData)=="official.yield"|names(bothData)=="Officiall yield"|names(bothData)=="OFFICIAL_YIELD"|names(bothData)=="Official.yield"|names(bothData)=="official_yield"|names(bothData)=="Official_yield"))],"adjSimu.csv",quote=FALSE,row.names=FALSE)
	}
		eurostat<-read.csv("adjOffi.csv")
	prev <- read.csv("adjSimu.csv")
	#here ends the importing data issue, from here they are only structured


	actualYield<-unique(eurostat)
	relatedModel<-unique(prev)

	#standarizing Official Yield Names...trying to, at least
			if(any(names(actualYield)=="official.yield")){names(actualYield)[names(actualYield)=="official.yield"]<-"OFFICIAL_YIELD"}
			if(any(names(actualYield)=="Official yield")){names(actualYield)[names(actualYield)=="Official yield"]<-"OFFICIAL_YIELD"}
			if(any(names(actualYield)=="Official.yield")){names(actualYield)[names(actualYield)=="Official.yield"]<-"OFFICIAL_YIELD"}
			if(any(names(actualYield)=="official_yield")){names(actualYield)[names(actualYield)=="official_yield"]<-"OFFICIAL_YIELD"}
			if(any(names(actualYield)=="Official _yield")){names(actualYield)[names(actualYield)=="Official_yield"]<-"OFFICIAL_YIELD"}
	#standarizing otherr variables
	if(any(names(actualYield)=="STAT_CROP_NO")&any(names(prev)=="CROP_NO")){names(actualYield)[names(actualYield)=="STAT_CROP_NO"]<-"CROP_NO"}else{
			if(any(names(prev)=="STAT_CROP_NO")&any(names(eurostat)=="CROP_NO")){names(prev)[names(prev)=="STAT_CROP_NO"]<-"CROP_NO"}}

	if(any(names(actualYield)=="CROP_NO")&any(names(relatedModel)=="CROP_NO")& !missing(crp)){actualYield<-subset(actualYield,actualYield$CROP_NO == crp)
		relatedModel<-subset(relatedModel,relatedModel$CROP_NO == crp)
		}
	if(any(names(actualYield)=="NUTS_CODE")&any(names(relatedModel)=="NUTS_CODE")& !missing(cnt)){
		actualYield<-subset(actualYield,actualYield$NUTS_CODE == cnt)
		relatedModel<-subset(relatedModel, relatedModel$NUTS_CODE == cnt)
	}

	yieldPrev$currentYear<- max(unique(relatedModel$YEAR))
	if(missing(dec)){
		currentDecade<- max(subset(relatedModel,relatedModel$YEAR==currentYear)$DECADE)
		}else{currentDecade<-dec}
	yieldPrev$relatedModel<-subset(relatedModel,relatedModel$DECADE == currentDecade)[,c(-which(names(prev)=="CROP_NO"),-which(names(prev)=="DECADE"),-which(names(prev)=="NUTS_CODE"))]

	actualYield<-actualYield[,c(which(names(actualYield)=="YEAR"),which(names(actualYield)=="OFFICIAL_YIELD"))]
	yieldPrev$actualYield<-actualYield[order(actualYield$YEAR),]
}

#
#impreadSet<-list(offiData,simuData,allData,crp,cnt,dec)
#impreadSet<-list(allData="../sugar/sugar.csv",dec=15)


batchFore<-function(impreadSet,modKind){
do.call(importYield,impreadSet)
yieldPrev$flatYield<-yieldPrev$actualYield
suppressMessages(suppressWarnings(try(autoDetrender())))
#modSel automated choice, it have to get edited for:
#	autoselection of proper model based on R2
#	receive argument for standard enhanced
dummy<-capture.output(suppressMessages(suppressWarnings(modSel(modKind,1))))

#response on Df: formula, PCA, fit, CV
	#PCA
	pcr_model<-pcr(OFFICIAL_YIELD ~ . ,data=yieldPrev$tableXregression,scale=TRUE,validation="LOO",ncomp=4)
	pcr4<-predict(pcr_model,newdata=subset(yieldPrev$relatedModel, yieldPrev$relatedModel$YEAR  == yieldPrev$currentYear),ncomp=4)


	#expected
	if(any(names(yieldPrev) =="breakPoint")){
		knoTime<-yieldPrev$breakPoint
		if(max(knoTime$finish)== (yieldPrev$currentYear -1)){
		trendMissing<-mean(knoTime$trend[which(knoTime$finish == max(knoTime$finish))])+yieldPrev$due2trend$trended[which(yieldPrev$due2trend$YEAR == (yieldPrev$currentYear -1) )]} else {trendMissing <- yieldPrev$due2trend$trended[which(yieldPrev$due2trend$YEAR == (yieldPrev$currentYear -1) )] }
	}else{trendMissing<-0}
	forED<-yieldPrev$expYield$fit[1]+trendMissing
	#CV
	cver<-yieldPrev$CVmsRes[1]
	results<-data.frame(expected=forED,pca4=pcr4[1],formula=yieldPrev$model_formula,R2=yieldPrev$CVmsRes[2],CV=cver)
	rownames(results)<-NULL
return(results)
}

#NB
#importYield is going to get feed by a over function wich will give it a a list as argument!