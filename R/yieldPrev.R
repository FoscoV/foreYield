	if(any(ls() == "yieldPrev")){}else{
	yieldPrev <-new.env()
	yieldPrev$.conflicts.OK<-c()
}
###CONFIGURATION
configure<-function(depth="base"){
	if(depth=="base"){
		cat(c("Do you have a single file database or official and simulation split in two different files? \n 1.single file \n 2. double files \n (answer by numer)"),fill=TRUE)
		numFiles<-scan(,nmax=1)
		while(numFiles != 1 & numFiles != 2){cat(c(" 1 and 2 are the only answers supported  \n 1.single file \n 2. double files \n "))
			numFiles<-scan(,nmax=1)
		}
		if(numFiles == 2){
			#find out data-files
			#cat("If you created your csv databases using MS Office, write <1> here, else <0>", fill=TRUE)
			#msoffice<-scan(,nmax=1)
			cat("Provide OFFICIAL yield database",fill=TRUE)
			offiData<-file.choose()
			write.table(gsub(gsub(scan(file=offiData,what="text",sep="\n"),pattern='"',replacement='',fixed=TRUE),pattern=";",replacement=","),file="adjOffi.csv",quote=FALSE,row.names=FALSE,col.names=FALSE)

		#	if(msoffice == 1) eurostat<-read.table(file.choose(), header=T, sep=";") else eurostat<-read.csv(file.choose())


			cat("Provide SIMULATE yield database",fill=TRUE)
			simuData<-file.choose()
			if(basename(simuData)=="PredictoIndicis.txt"){
				predInd<-read.csv(simuData)
				predInd$INDICATOR_VALUE<-as.numeric(as.character(predInd$INDICATOR_VALUE))
				predIndL<-spread(predInd,INDICATOR_CODE,INDICATOR_VALUE)
				write.csv(predIndL,file="adjSimu.csv",row.names=FALSE)
			}else{
				write.table(gsub(gsub(scan(file=simuData,what="text",sep="\n"),pattern='"',replacement='',fixed=TRUE),pattern=";",replacement=","),file="adjSimu.csv",quote=FALSE,row.names=FALSE,col.names=FALSE)
			}
		}
		if(numFiles == 1){
			cat(c("Point out your database \n"))
			allData<-file.choose()
			write.table(gsub(gsub(scan(file=allData,what="text",sep="\n"),pattern='"',replacement='',fixed=TRUE),pattern=";",replacement=","),file="adjDtb.csv",quote=FALSE,row.names=FALSE,col.names=FALSE)
			longData<-read.csv("adjDtb.csv")

			bothData<-spread(longData,INDICATOR_CODE,INDICATOR_VALUE)
			write.csv(bothData[,c(which(names(bothData)=="YEAR"),which(names(bothData)=="official.yield"|names(bothData)=="Officiall yield"|names(bothData)=="OFFICIAL_YIELD"|names(bothData)=="Official.yield"|names(bothData)=="official_yield"|names(bothData)=="Official_yield"))],"adjOffi.csv",quote=FALSE,row.names=FALSE)
			write.csv(bothData[,c(-which(names(bothData)=="official.yield"|names(bothData)=="Officiall yield"|names(bothData)=="OFFICIAL_YIELD"|names(bothData)=="Official.yield"|names(bothData)=="official_yield"|names(bothData)=="Official_yield"))],"adjSimu.csv",quote=FALSE,row.names=FALSE)
		}
	}
	eurostat<-read.csv("adjOffi.csv")
	prev <- read.csv("adjSimu.csv")
	#here ends the importing data issue, from here they are only structured


actualYield<-unique(eurostat)
relatedModel<-unique(prev)

		if(any(names(actualYield)=="STAT_CROP_NO")&any(names(relatedModel)=="CROP_NO")){names(actualYield)[names(actualYield)=="STAT_CROP_NO"]<-"CROP_NO"}else{
		if(any(names(relatedModel)=="STAT_CROP_NO")&any(names(actualYield)=="CROP_NO")){names(relatedModel)[names(relatedModel)=="STAT_CROP_NO"]<-"CROP_NO"}}

		#standarizing Official Yield Names...trying to, at least
		if(any(names(actualYield)=="official.yield")){names(actualYield)[names(actualYield)=="official.yield"]<-"OFFICIAL_YIELD"}
		if(any(names(actualYield)=="Official yield")){names(actualYield)[names(actualYield)=="Official yield"]<-"OFFICIAL_YIELD"}
		if(any(names(actualYield)=="Official.yield")){names(actualYield)[names(actualYield)=="Official.yield"]<-"OFFICIAL_YIELD"}
		if(any(names(actualYield)=="official_yield")){names(actualYield)[names(actualYield)=="official_yield"]<-"OFFICIAL_YIELD"}
		if(any(names(actualYield)=="Official _yield")){names(actualYield)[names(actualYield)=="Official_yield"]<-"OFFICIAL_YIELD"}



	if(any(names(actualYield)=="CROP_NO")){
		#choice of crop parameters
		cat(c("OFFICIAL yield data contains information for the following CROPs:\n",unique(actualYield$CROP_NO),"\n Choose one:",fill=TRUE))
		cropO<-scan(,nmax=1)
		while(any(unique(actualYield$CROP_NO) == cropO) == FALSE){cat("point an existing one \n ")
			cropO<-scan(,nmax=1)}
			actualYield<-subset(actualYield,actualYield$CROP_NO == cropO)
			}
	if(any(names(relatedModel)=="CROP_NO")){
		cat(c("SIMULATED yield data contains information for the following CROPs:\n",unique(relatedModel$CROP_NO),"\n Choose one:",fill=TRUE))
		cropS<-scan(,nmax=1)
		while(any(unique(relatedModel$CROP_NO) == cropS) == FALSE){cat("point an existing one \n ")
		cropS<-scan(,nmax=1)}
		yieldPrev$saveCrop<-cropS
		relatedModel<-subset(relatedModel, relatedModel$CROP_NO== cropS)
	}else{cat(c("It seems you have one crop only: no data named CROP_NO found \n"))}

	if(any(names(actualYield)=="NUTS_CODE")){
		#choice of country
		cat(c("The following countries are provided in the DataBases:\n","OFFICIAL:",levels(actualYield$NUTS_CODE),"(case sensitive) \n"),fill=TRUE)
		cat(" OFFICIAL COUNTRY:")
		countryO<-scan(,what="text",nmax=1)
		while(any(levels(actualYield$NUTS_CODE) == countryO) == FALSE){cat("point an existing one \n ")
		countryO<-scan(,what="text",nmax=1)}
		actualYield<-subset(actualYield,actualYield$NUTS_CODE == countryO)
	}
	if(any(names(prev)=="NUTS_CODE")){
		cat(c("The following countries are provided in the DataBases: \n SIMULATION:",levels(prev$NUTS_CODE)," \n "))
		countryS<-scan(,what="text",nmax=1)
		while(any(levels(prev$NUTS_CODE) == countryS) == FALSE){cat("point an existing one \n ")
		countryS<-scan(,what="text",nmax=1)}
		yieldPrev$saveCountry<-countryS
		relatedModel<-subset(relatedModel, relatedModel$NUTS_CODE == countryS)
	}else{cat(c("It seems you have one nation only: no data named NUTS_CODE found \n"))}
		#subsetting conforming the configuration set above
		actualYield<-actualYield[,c(which(names(actualYield)=="YEAR"),which(names(actualYield)=="OFFICIAL_YIELD"))]
		yieldPrev$actualYield<-actualYield[order(actualYield$YEAR),]

	#reading datas for suitable informations
	currentYear<- max(unique(relatedModel$YEAR))
	yieldPrev$currentYear <- currentYear
	if(any(names(relatedModel)=="DECADE")){
		currentDecade<- max(subset(relatedModel,relatedModel$YEAR==currentYear)$DECADE)
		cat(c("It seems forecasting the year",currentYear,"with data till the ",currentDecade,"th decade"),fill=TRUE)
		cat(c("Do you want to change Decade assumption? \n "))
		changeYD<-scan(,what="text",nmax=1)
		while(any(c(length(changeYD)==0,changeYD != "y", changeYD != "n"))){
			cat("answer y or n \n")
			changeYD<-scan(,what="text",nmax=1)
		}
		if(changeYD == "n"){
			yieldPrev$relatedModel<-subset(relatedModel,relatedModel$DECADE == currentDecade)[,c(-which(names(relatedModel)=="CROP_NO"),-which(names(relatedModel)=="DECADE"),-which(names(relatedModel)=="NUTS_CODE"))]
		}
		if(changeYD == "y"){
			cat(c("Digit desired Decade"))
			#suggest: loop checking that the desired decade is lower than the last one...
			currentDecade<-scan(,nmax=1)
			cat(c("Digit desired Year"))
			yieldPrev$currentYear <- scan(,nmax=1)
			yieldPrev$relatedModel<-subset(relatedModel,relatedModel$DECADE == currentDecade)[,c(-which(names(relatedModel)=="CROP_NO"),-which(names(relatedModel)=="DECADE"),-which(names(relatedModel)=="NUTS_CODE"))]
		}
	}
	yieldPrev$relatedModel<-yieldPrev$relatedModel[sapply(yieldPrev$relatedModel, function(x) !any(is.na(x)))]
}
		#save information for saveYieldSession()

####library(tidyr)
####library(spikeslab)

###working on official trends
####library(tseries)
####library(forecast)
####library(ggplot2)
#providiing function for check actual existance of trend
checkTrends<-function(){
	attach(yieldPrev)
	if(any(names(yieldPrev) == "flatYield")){}else{
		flatYield<-actualYield
		yieldPrev$flatYield<-actualYield }
	cat(c("\n Official yields have a " ,round((adf.test(flatYield$OFFICIAL_YIELD)$p.value)*100,2),"% of chances to have a trend."),fill=TRUE)
	cat(c(" \n Look the chart for the visual assessment. \n Plotted: \n BLACK:actual data \n BROWN:linear model \n RED:LOcal regrESSion \n ORANGE:spline regression"),fill=TRUE)
	offiPlot<-ggplot(flatYield,aes(x=YEAR, y=OFFICIAL_YIELD,group=1))+geom_point(color="black")+geom_line(color="black")+geom_smooth(method="lm",color="brown",se=FALSE)+geom_smooth(method="loess",color="red",se=FALSE)+geom_smooth(method="lm",formula= y~splines::bs(x,3),color="orange",se=FALSE)
	plot(offiPlot)
	cat(c(" \n Do you see a trend in the data?\n	(y / n) \n "),fill=TRUE)
	yieldPrev$flattyn<-scan(,what="text",nmax=1)
	while(any(c(length(yieldPrev$flattyn)==0,yieldPrev$flattyn != "y" , yieldPrev$flattyn != "n"))){
		cat("answer y or n \n ")
		yieldPrev$flattyn<-scan(,what="text",nmax=1)}
#	cat(c("The time serie is ",(unique(max(flatYield$YEAR))-unique(min(flatYield$YEAR))), "years long, since",unique(min(flatYield$YEAR)),"to",unique(max(flatYield$YEAR))),fill=TRUE)
	#checking if there are troubles in the official data series.
	if(length(c(setdiff(seq(min(flatYield$YEAR),max(flatYield$YEAR)),flatYield$YEAR),flatYield$YEAR[duplicated(flatYield$YEAR)])) == 0) cat("") else cat(c("By the way there are \n MISSING:",setdiff(seq(min(flatYield$YEAR),max(flatYield$YEAR)),flatYield$YEAR)," \n REPLICATED: ",flatYield$YEAR[duplicated(flatYield$YEAR)]," \n"))
	detach(yieldPrev)
}


#working on official data's trend
sewTrends<-function(inizio,fine){
	tempLimit<-data.frame(begin=inizio,finish=fine)
	#attach(yieldPrev)
	yieldPrev$flatOff<- merge(yieldPrev$flatYield,yieldPrev$relatedModel,by="YEAR")
	#yieldPrev$flatOff<-flatOff
	normalizingTrend<-function(campo){ #campo is numeric!
		yieldPrev$flatOff[,campo]<-(yieldPrev$flatOff[,campo]/mean(yieldPrev$flatOff[,campo]))*100
	}
	lapply(X=seq(2,length(yieldPrev$flatOff)),FUN=normalizingTrend)

	#removing NA values, they mess up the lm....
	#flatOff<-yieldPrev$flatOff
	yieldPrev$flatOff<-yieldPrev$flatOff[sapply(yieldPrev$flatOff,function(flatOff)!any(is.na(flatOff)))]
	#flatOff<-yieldPrev$flatOff
	trendAN<-new.env()
	trendAN$normPlot<-ggplot(yieldPrev$flatOff)+geom_line(aes(x=YEAR,y=OFFICIAL_YIELD,group=1),color="red")+geom_smooth(method="loess",color="red",aes(x=YEAR,y=OFFICIAL_YIELD,group=1),se=FALSE)
	trendPlot<-function(yvar){

		trendAN$normPlot<-trendAN$normPlot + geom_smooth(data=yieldPrev$flatOff,method="loess", color="cyan", aes_(x=~YEAR, y=as.name(yvar),group=1),se=FALSE)

	}
	lapply(names(yieldPrev$flatOff[,c(-1)]),FUN=trendPlot)
	trendAN$PlotNormA<-trendAN$normPlot+geom_line(aes(x=YEAR,y=OFFICIAL_YIELD,group=1),color="red",size=1.5)+geom_smooth(method="loess",color="orange",aes(x=YEAR,y=OFFICIAL_YIELD),se=FALSE,size=1.5)+labs(x="YEARS", y="TREND")+ geom_rect(data=tempLimit, aes(xmin=begin, xmax=finish, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.5)
	#detach(yieldPrev)
	plot(trendAN$PlotNormA)

	#yieldPrev$friendShip<-data.frame(trendCoef=lm(formula=OFFICIAL_YIELD ~ YEAR,data=flatOff)$coefficients[2])
	#rownames(yieldPrev$friendShip[1,])<-"OFFICIAL_YIELD"
	#flatOff1<-yieldPrev$flatOff
	yieldPrev$flatOff<-subset(yieldPrev$flatOff,yieldPrev$flatOff$YEAR >= inizio & yieldPrev$flatOff$YEAR <= fine)
	#yieldPrev$flatOff<-flatOff2
	#flatOff<-flatOff2
	yieldPrev$friendShip<-data.frame(param=as.character(names(yieldPrev$flatOff)[(names(yieldPrev$flatOff) == "OFFICIAL_YIELD")]),trendCoef=as.numeric(lm(formula=OFFICIAL_YIELD ~ YEAR,data=yieldPrev$flatOff)$coefficients[2]))
	plot(trendAN$PlotNormA+stat_smooth(data=yieldPrev$flatOff,method="lm",color="black",aes(x=YEAR,y=OFFICIAL_YIELD,group=1),fullrange=FALSE,se=FALSE,size=1))
	flatOff1<-merge(yieldPrev$flatYield,yieldPrev$relatedModel,by="YEAR")
	yieldPrev$flatOff<-subset(flatOff1,flatOff1$YEAR >= inizio & flatOff1$YEAR <= fine)
	#yieldPrev$flatOff<-flatOff2
	#flatOff<-flatOff2
	mayTrend<-names(yieldPrev$flatOff)[(names(yieldPrev$flatOff)!= "YEAR" &  names(yieldPrev$flatOff)!= "OFFICIAL_YIELD")]
	friendTest<-function(mate){
		allIn<-yieldPrev$flatOff
		#formulFriend<-as.formula(paste(as.name(mate)," ~ YEAR",sep=""))
		formulFriend<-as.formula(paste(as.name(mate)," ~ seq(1,length(yieldPrev$flatOff[,1]))",sep=""))
		linMod<-lm(formula=formulFriend,data=allIn)
		yieldPrev$friendShip <- rbind(yieldPrev$friendShip,data.frame(param=paste(c(as.character(mate)),sep=""),trendCoef=as.numeric(linMod$coefficients[2])),deparse.level=1)
		#rownames(yieldPrev$friendShip[length(yieldPrev$friendShip[,1]),])<-as.name(mate)
		#print(c(paste(c(as.character(mate)),sep=""),linMod$coefficients[2]))
	}
	sapply(X=mayTrend,FUN=friendTest,USE.NAMES=TRUE)
	trendShip<-yieldPrev$friendShip
	yieldPrev$YieldTrend<-trendShip[1,2]
	#yieldPrev$yieldTrend<-YieldTrend
	cat(c("The found trend for Official_Yield is ",round((trendShip[1,2]),2),". \n The following are the similar trends available between the predictors: \n ->Absolute values \n -> Sorted by difference with Yield trend \n \n "),fill=TRUE)
	trendShip$trendDiff<-abs(trendShip[,2]-trendShip[1,2])
	trendMates<-trendShip[c(-1),]
	trendMates<-(trendMates[order(trendMates$trendDiff),])
	trendMates$diff_perC <- round(abs(trendMates$trendDiff)/abs(yieldPrev$YieldTrend),2)
	trendMates$ID <- seq(1,length(trendMates[,1]))
	print(trendMates)

	#continue to cut?
	cat(c("\n Do you want to continue removing the trend in official yields"),fill=TRUE)
	continueToCut<-scan(,what="text",nmax=1)
	while(any(c(length(continueToCut)==0,continueToCut != "y" , continueToCut != "n"))){
		cat("answer y or n \n")
		continueToCut<-scan(,what="text",nmax=1)}
	if(continueToCut == "y"){
		#allow some sewing with Mates (in case which)
		cat(c("\n Does any of the predictors explain some of the Official's Trend? \n If yes, point which one(s) by ID (multiple answers allowed) \n If none of them does, return blank: \n"),fill=TRUE)
		mateList<-scan(,nmax=length(trendMates[,1]))
		if(length(mateList >= 1)){
			yieldPrev$safeTrend <- mean(abs(trendMates$trendCoef[c(mateList)]))
		}
		cutTrend(inizio,fine)
	}
}

breakTrends<-function(){ #this doesn't work
	attach(yieldPrev)
	flatPlot<-ggplot(yieldPrev$flatYield)+geom_point(color="black",aes(x=YEAR, y=OFFICIAL_YIELD,group=1))+geom_line(color="black",aes(x=YEAR, y=OFFICIAL_YIELD,group=1))+geom_smooth(method="lm",color="brown",se=FALSE,aes(x=YEAR, y=OFFICIAL_YIELD,group=1))+geom_smooth(method="loess",color="red",se=FALSE,aes(x=YEAR, y=OFFICIAL_YIELD,group=1))+geom_smooth(method="lm",formula= y~splines::bs(x,3),color="orange",se=FALSE,aes(x=YEAR, y=OFFICIAL_YIELD,group=1))
	if(any(names(yieldPrev) == "breakPoint")) flatPlot<-flatPlot + geom_rect(data=yieldPrev$breakPoint, aes(xmin=begin, xmax=finish, ymin=-Inf, ymax=+Inf), fill='yellow', alpha=0.3)
	plot(flatPlot)
	cat(c(" \n Time series starts in",unique(min(flatYield$YEAR)),"and ends in ",unique(max(flatYield$YEAR))),fill=TRUE)
		cat(c(" \n Point the trend's edges by year\n"),fill=TRUE)
		trendEdge<-scan(,nmax=2)
		tempLimit<-data.frame(begin=min(trendEdge),finish=max(trendEdge))
		#paint them and ask confirm.... this thing about asking confirm  is messy...
		cutPlot<-flatPlot + geom_rect(data=tempLimit, aes(xmin=begin, xmax=finish, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.5)
		#if(any(names(yieldPrev) == "breakPoint")) cutPlot<-cutPlotA + geom_rect(data=yieldPrev$breakPoint, aes(xmin=begin, xmax=finish, ymin=-Inf, ymax=+Inf), fill='yellow', alpha=0.3) else cutPlot <- cutPlotA
		plot(cutPlot)

	#confirm
	cat(c("\n Are the drawn lapse correct? \n (y/n)"),fill=TRUE)
	continCut <- scan(,what="text",nmax=1)
	while(any(c(length(continCut)==0,continCut != "y" , continCut != "n"))){
		cat("answer y or n")
	continCut <- scan(,what="text",nmax=1)}
	if(continCut == "y"){
	sewTrends(min(trendEdge),max(trendEdge))}
	detach(yieldPrev)

}
####library(MASS)
cutTrend<-function(inizio,fine){
	if(any(names(yieldPrev) == "due2trend")){} else {
		yieldPrev$due2trend<-data.frame(YEAR=yieldPrev$actualYield$YEAR,trended=rep(0,length=length(yieldPrev$actualYield$YEAR)))
	}
	notSoFlat <-yieldPrev$flatYield
	#cutting trend from inizio to fine
	preflat<-subset(notSoFlat,notSoFlat$YEAR >= inizio & notSoFlat$YEAR <= fine)
	flatLin<-lm(OFFICIAL_YIELD~YEAR,data=preflat)
	#developing the flat "officials"
	cutEnv<-new.env()
    cutEnv$modello<-flatLin
    cutEnv$flatting<-preflat
    if(any(names(yieldPrev)=="safeTrend")){cutEnv$trendCorr<-(flatLin$coefficients[2]-yieldPrev$safeTrend)} else{ cutEnv$trendCorr<-flatLin$coefficients[2]}
    print(cutEnv$trendCorr)
    smootherer<-function(num){
		#attach(cutEnv)
		model<-cutEnv$modello
		#flat<-(model$model[num,1])-(model$model[num,2]-model$model[1,2])* cutEnv$trendCorr
		yieldPrev$due2trend[which(yieldPrev$due2trend$YEAR == model$model[num,2]),2]<-yieldPrev$due2trend[which(yieldPrev$due2trend$YEAR == model$model[num,2]),2]+(model$model[num,2]-model$model[1,2])* cutEnv$trendCorr
		#cutEnv$flatting[num,2]<-flat
		#print(flatting[num,])
		#detach(cutEnv)
	}
	lapply(X=seq(1,length(preflat$OFFICIAL_YIELD)),FUN=smootherer)
	#preflat<-cutEnv$flatting
	if(any(names(yieldPrev) == "breakPoint")) yieldPrev$breakPoint<-rbind(yieldPrev$breakPoint,c(inizio,fine,as.numeric(flatLin$coefficients[2]))) else yieldPrev$breakPoint<- data.frame(begin=inizio,finish=fine,trend=as.numeric(flatLin$coefficients[2]))

	trendInt<-function(anno){
		model<-cutEnv$modello
		interTrend<-(model$model[length(model$model[,2]),2]-model$model[1,2])* cutEnv$trendCorr
		yieldPrev$due2trend[which(yieldPrev$due2trend$YEAR == anno),2]<- yieldPrev$due2trend[which(yieldPrev$due2trend$YEAR == anno),2] + interTrend
	}

	if(length(subset(notSoFlat$YEAR,notSoFlat$YEAR > fine)) > 0){
		lapply(X=seq((fine+1),max(notSoFlat$YEAR)),FUN=trendInt)
		}
	#now we have to grant no more safeTrend will influence further trends!
	#yieldPrev$safeTrend<- NULL
	rm(list=c("safeTrend"),envir=yieldPrev)

	yieldPrev$flatYield$OFFICIAL_YIELD<-yieldPrev$actualYield$OFFICIAL_YIELD - yieldPrev$due2trend$trended
}


####library(leaps)
####library(HH)
####library(relaimpo)
modSel <- function(standardModel,rcrit){
	tableXregression <-merge(yieldPrev$flatYield , yieldPrev$relatedModel , by="YEAR")
	#clean this table, 0 columns are going to mess it up
	coluClean<-function(cola){
		if(min(tableXregression[,cola]) == max(tableXregression[,cola])){return(cola)}
	}
	dirtyCol<-lapply(X=seq(1,length(names(tableXregression))),FUN=coluClean)
	if(!is.null(unlist(dirtyCol))){tableXregression<-tableXregression[,-unlist(dirtyCol)]}
	if(missing(standardModel)){
		cat("Are you looking for a standard additive model? \n a models accounting for combined predictors. \n Which do you prefer? \n ")
 		cat(" 1. standard \n 2. enhanced \n ")
 		standardModel<-scan(,what="text",nmax=1)
		while(standardModel != "1" & standardModel != "2" & standardModel != "standard" & standardModel != "enhanced" & length(standardModel)==0 ){
			cat(" 1. standard \n 2. enhanced \n ")
			standardModel<-scan(,what="text",nmax=1)
		}
	}
	if(standardModel == "1" | standardModel == "standard"){
		allSign <- regsubsets(OFFICIAL_YIELD~.,data=tableXregression[,c(-which(names(tableXregression)=="YEAR"))],nbest=2,method="exhaustive",nvmax=4, really.big=TRUE)}
	if(standardModel == "2" | standardModel == "enhanced"){
		allSign <- regsubsets(OFFICIAL_YIELD~.^2+.,data=tableXregression[,c(-which(names(tableXregression)=="YEAR"))],nbest=2,nvmax=4, really.big=TRUE)}#,method="exhaustive"
	#renaming predictors with numbers
	summaSign<-summaryHH(allSign,names=seq(1,length(allSign$xnames)),statistics="adjr2")
	if(missing(rcrit)){plot(summaSign,col="green",cex=0.8)}
	#plot(summaryHH(allSign,scale="r2"))
#calc.relimp(yieldPrev$regrSW,type="car")
	print(summaSign)
	#print(summaryHH(allSign,abbrev=3,statistics="adjr2"))
	cat("\n Note: one of the accounted parameter is (Intercept) \n Select a model")
	if(missing(rcrit)){
	modId<-scan(,nmax=1)}else{modId<-which(summaSign$bic == min(summaSign$bic))[1]}
	yieldPrev$model_formula<-c("OFFICIAL_YIELD ~ ")
	compleFormula<-function(parametro){
		yieldPrev$model_formula<- paste(yieldPrev$model_formula, names(coef(allSign,id= modId))[parametro],if(parametro == length(names(coef(allSign,id= modId)))) sep=" " else sep= " +")
	}
	#print(as.formula(yieldPrev$model_formula))
	lapply(X=seq(2,length(names(coef(allSign,id= modId)))),FUN=compleFormula)
	regrSW<-lm(as.formula(yieldPrev$model_formula),data=tableXregression)
	relatedModel <- yieldPrev$relatedModel
	expYield<-predict(regrSW,newdata=subset(relatedModel, relatedModel$YEAR  == yieldPrev$currentYear),se.fit=TRUE,type="response",level=0.95,interval="prediction")
	yieldPrev$expYield <- expYield
	yieldPrev$modelLM<-regrSW
	yieldPrev$tableXregression<-tableXregression
	#cross validating removing each year once
	attach(yieldPrev)
	validC<-CV(yieldPrev$modelLM)
#	validC<-cv.lm(data=yieldPrev$tableXregression, yieldPrev$modelLM, m=((length(unique(yieldPrev$tableXregression$YEAR))-1)),printit=FALSE,plotit=FALSE)
	detach(yieldPrev)
	yieldPrev$CVmsRes<-c(validC[1],validC[5])
	#yieldPrev$CVmsRes<-attributes(validC)$ms
	cat(c("SOME INFOs ABOUT THIS MODEL:"),fill=T)
	try(if(standardModel == 1){
		genizi<-as.matrix(calc.relimp(yieldPrev$modelLM,type="genizi")$genizi)
		colnames(genizi)<-as.list("R2")
			cat(c("\n Decomposition of R2 accordingly to (Genizi,1993): \n"),fill=TRUE)
		print(genizi)
		},silent=T)
		cat(c("\n Regression coefficients:\n"),fill=T)
		print(yieldPrev$modelLM$coefficients)
}
####library(DAAG)


randModel<-function(){
	#experimenting Bayesian variable selection: "spike and slab"
	yieldPrev$modelBay_formula<-c("OFFICIAL_YIELD~ ")
	puntone<-spikeslab(OFFICIAL_YIELD~ . ,data=yieldPrev$tableXregression,max.var=4,fast=F,n.iter1=1000,n.iter2=1000,mse=T)
	lastra<-predict(puntone,newdata=subset(yieldPrev$relatedModel, yieldPrev$relatedModel$YEAR  == yieldPrev$currentYear))
	compleBayFormula<-function(parametro){
		yieldPrev$modelBay_formula<- paste(yieldPrev$modelBay_formula, puntone$names[puntone$gnet.obj.vars][parametro],if(parametro == length(puntone$gnet.obj.vars)) sep=" " else sep= " +")
	}
	#print(as.formula(yieldPrev$model_formula))
	lapply(X=seq(1,length(puntone$gnet.obj.vars)),FUN=compleBayFormula)
	regrGNET<-lm(as.formula(yieldPrev$modelBay_formula),data=yieldPrev$tableXregression)

	expBAYield<-predict(regrGNET,newdata=subset(yieldPrev$relatedModel, yieldPrev$relatedModel$YEAR  == yieldPrev$currentYear),se.fit=TRUE,type="response",level=0.95,interval="prediction")
	#cross validating removing each year once
	validC<-c(CV(regrGNET)[c(1,5)])
	#yieldPrev$CVmsRes<-c(validC[1],validC[5])

	cat(c("Spike&Slam suggests as model:\n 	",yieldPrev$modelBay_formula,"\n which fits adopting: \n	"))
	print(regrGNET$coefficients)
	cat(c("\n and obtains:\n"))
	print(validC)
	cat(c("\n forecasting ",round(expBAYield$fit[1],2) ,"+/-",round(expBAYield$fit[1]-expBAYield$fit[2],2),".\n"))
}


####library(bartMachine)
####library(pls)
responseYield<-function(){
	expYield <- yieldPrev$expYield
	knoTime<-yieldPrev$breakPoint
	if(max(knoTime$finish)== (yieldPrev$currentYear -1)){
		trendMissing<-mean(knoTime$trend[which(knoTime$finish == max(knoTime$finish))])+yieldPrev$due2trend$trended[which(yieldPrev$due2trend$YEAR == (yieldPrev$currentYear -1) )]} else {trendMissing <- yieldPrev$due2trend$trended[which(yieldPrev$due2trend$YEAR == (yieldPrev$currentYear -1) )] }
	cat(c(" \n \n \n RESPONSE \n \n ","As it is, the forecasted yield for year",yieldPrev$currentYear,"is",round(expYield$fit[1],2),"+/-",round(expYield$fit[1]-expYield$fit[2],2),"."),fill=TRUE)
	cat(c("Confidence = 95% \n	\n CROSS-VALIDATION \n ",round(yieldPrev$CVmsRes[1],2),"as mean square error and ",round(yieldPrev$CVmsRes[2],2),"as R2."))

	try(scenarioAnalysis(),silent=T)

if(any(names(yieldPrev) == "due2trend")){cat(c("\n \n Due to the marked trends, the forecasted has to be corrected with ",round(trendMissing,2)," resulting, so, as ",round(expYield$fit[1]+trendMissing,2),". \n
"),fill=TRUE)}
try(bartolomeoMod(depthing=F),silent=TRUE)
cat(c("	\n
	TimeSeries statistical analysis over OFFICIAL_YIELD would bet on ",round(forecast(ets(yieldPrev$actualYield[,2]),h=1)$mean[1],2)," +/- ",round((forecast(ets(yieldPrev$actualYield[,2]),h=1)$upper[2]-forecast(ets(yieldPrev$actualYield[,2]),h=1)$mean[1]),2),"."))


	#last years everage
	lastResults<-yieldPrev$actualYield[which(yieldPrev$actualYield$YEAR>=yieldPrev$currentYear-5),which(names(yieldPrev$actualYield)=="OFFICIAL_YIELD")]
	cat(c("\n	In the last ",length(lastResults)," (",yieldPrev$currentYear-1,"-", yieldPrev$currentYear-5, ") the average yield was ",round(mean(lastResults),2)," +/- ",round(var(lastResults),2),"."))

	#crossVal plot
	#acquire loocv single data
	motoCross<-cv.lm(data=yieldPrev$tableXregression, formula(yieldPrev$modelLM), m=length(yieldPrev$tableXregression[,1]),printit=FALSE)
	#keep out Year and cvpred
	crValPre<-data.frame(YEAR=motoCross$YEAR,pred=motoCross$cvpred)
	respoPlot<- ggplot(crValPre)
	cat(c(" \n Plotted:\n GREEN: Predicted yield in Cross Validation \n RED: Data on which models are regressed"),fill=TRUE)
	if(any(names(yieldPrev) == "due2trend")){
		untrend<-yieldPrev$flatYield
		yieldPrev$omniYield<-merge(crValPre,yieldPrev$due2trend,by="YEAR")
		yieldPrev$omniYield<-rbind(yieldPrev$omniYield,c(yieldPrev$currentYear,expYield$fit[1],trendMissing))
		yieldPrev$omniYield$YIELD<-yieldPrev$omniYield$pred+yieldPrev$omniYield$trended

		respoPlot <- respoPlot+geom_line(aes(x=YEAR,y=OFFICIAL_YIELD),color="black",size=1.5,data=yieldPrev$actualYield)+geom_line(aes(x=YEAR,y=YIELD),color="blue",data=yieldPrev$omniYield)
		cat(c(" BLUE: Predicted yield, restored trend \n BLACK: Real data \n "),fill=TRUE)
	}
	crValPre<-rbind(crValPre,c(yieldPrev$currentYear,expYield$fit[1]))
	respoPlot<-respoPlot+geom_line(aes(x=YEAR,y=OFFICIAL_YIELD,group=1),color="red",size=1.5,data=yieldPrev$flatYield)+geom_line(aes(x=YEAR,y=pred,group=1),color="green",data=crValPre)
	plot(respoPlot)
	#dev.new()
	#plot(yieldPrev$PCmodel,line=1)
	if(any(names(yieldPrev)=="breakPoint")){try(valiTrend(),silent=TRUE)}
}

saveYieldSession<- function(){
	attach(yieldPrev)
	save(list=ls(yieldPrev),file=paste(Sys.Date(),".RData",sep=""))
	cat(c("Session saved in ",paste(Sys.Date(),saveCountry,saveCrop,".RData",sep=""),". \n Use loadYieldSession() to restore in future sessions."),fill=TRUE)
}
loadYieldSession<-function(){
	cat(c("Select the desired previous session saved \n "),fill=TRUE)
	oldYieldSession<-file.choose()
	load(file=oldYieldSession,envir=yieldPrev)
	cat(c("Session in oldYieldSession loaded \n "),fill=TRUE)
}

valiTrend<-function(){
	#obtaind clean informations for where the trend doesn't exist
	train<-subset(yieldPrev$due2trend,yieldPrev$due2trend$trended == 0)
	train<-merge(train,yieldPrev$actualYield, by="YEAR")
	train$trended<-NULL
	train<-merge(train,yieldPrev$relatedModel, by="YEAR")
	#getting a model only based on untrend data
	noTrendMod<-lm(as.formula(yieldPrev$model_formula),data=train)
	#test set
	test<-subset(yieldPrev$due2trend,yieldPrev$due2trend$trended != 0)
	test<-merge(test,yieldPrev$relatedModel,by="YEAR")
	test$trended<-NULL
	solution<-subset(yieldPrev$due2trend,yieldPrev$due2trend$trended != 0)
	solution<-merge(solution,yieldPrev$flatYield,by="YEAR")
	solution$trended<-NULL

	woTrend<-predict(noTrendMod,newdata=test,se.fit=TRUE,type="response",level=0.95,interval="prediction")

	predCfg<-merge(solution,yieldPrev$omniYield,by="YEAR")
	predCfg$clean<-woTrend$fit[,1]
	DnoTREND<-predCfg$YIELD-predCfg$pred
	DwTREND<-predCfg$OFFICIAL_YIELD-predCfg$clean

	sigNO<-sqrt(mean((DnoTREND - mean(DnoTREND))^2))
	sigW<-sqrt(mean((DwTREND - mean(DwTREND))^2))
	if(sigNO < sigW){cat(c("NOTE that the pointed Trends are afflicted by some kind of problem, a BETTER FITting model can be obtained WITHout any TREND removal. \n"))}
	asimErr<-skewness(woTrend$fit[,1]-solution$OFFICIAL_YIELD)
	predError<-woTrend$fit[,1]-solution$OFFICIAL_YIELD
	sigma<-sqrt(mean((predError - mean(predError))^2))
	danger<-asimErr/sigma
	if(danger >= 2.6){cat(c("\n ADVICE: \n The marked trend related dynamics don't fit with the data! \n "))}
	if(sigNO < sigW|danger >= 2.6){cat(c("Do you want to reset the trend marked and proceed again? (y/n) \n"),fill=T)
		reBea<-scan(,what="text",nmax=1)
		while(any(c(length(reBea)==0,reBea != "y" , reBea != "n"))){
			cat("answer y or n")
			reBea<-scan(,what="text",nmax=1)
		}
		if(reBea == "y"){
			#reset trend's stuff
			rm(list=c("breakPoint","flatYield","due2trend","friendShip","flattyn","safeTrend","yieldTrend","flatOff","tableXregression","model_formula","CVmsRes","expYield","omniYield","modelLM","PCmodel"),envir=yieldPrev)
			#then, start again
			virgilio()}
	}
}


bartolomeoMod<-function(cpusa=1,depthing=T){
	suppressMessages(library(bartMachine))
	if(cpusa>1){
		set_bart_machine_num_cores(cpusa)
	}
	anno<-yieldPrev$currentYear
	paese<-yieldPrev$saveCountry
	coltura<-yieldPrev$saveCrop
	if(depthing){
		BARmodel<-bartMachineCV(yieldPrev$tableXregression[,-c(1,2)],yieldPrev$tableXregression[,2])
	}else{
		BARmodel<-bartMachine(yieldPrev$tableXregression[,-c(1,2)],yieldPrev$tableXregression[,2],verbose=F)
	}
	correnti<-names(subset(yieldPrev$relatedModel, yieldPrev$relatedModel$YEAR  == yieldPrev$currentYear))
	predetti<-names(yieldPrev$tableXregression[,-c(1,2)])
	buono<-subset(yieldPrev$relatedModel, yieldPrev$relatedModel$YEAR  == yieldPrev$currentYear)[,unlist(lapply(X=correnti,FUN=function(nome){if(any(nome==predetti)){return(which(correnti==nome))}}))]
	BARpred<-calc_credible_intervals(BARmodel,buono)
	predCR<-mean(BARpred)
	errCR<-predCR-BARpred[1]

	#cat(c("=========================================================="),fill=T)
	#cat(c("=========================================================="),fill=T)
	#cat(c("Bayesian Additive Regression Tree (BART) running on provided data for crop ", coltura," in ",paese," esteems the yield for ",anno, " at ",round(predCR,2),"+/-",round(errCR,2),".\n"))
	cat(c("\nBayesian Additive Regression Tree (BART) running on provided data for crop ", coltura," in ",paese," esteems the yield for ",anno, " at ",round(predCR,2),"+/-",round(errCR,2),".\n"))
	#cat(c("=========================================================="),fill=T)
	#cat(c("=========================================================="),fill=T)
}

scenarioAnalysis<-function(){
	pcr_model<-pcr(OFFICIAL_YIELD ~ . ,data=yieldPrev$tableXregression,scale=TRUE,validation="LOO",ncomp=4)
	pcr4<-predict(pcr_model,newdata=subset(yieldPrev$relatedModel, yieldPrev$relatedModel$YEAR  == yieldPrev$currentYear),ncomp=4)
	pcr3<-predict(pcr_model,newdata=subset(yieldPrev$relatedModel, yieldPrev$relatedModel$YEAR  == yieldPrev$currentYear),ncomp=3)
	yieldPrev$PCmodel<-pcr_model
	cat(c("\n \n Principal Component Regression (PCR) predicted \n ",round(pcr4,2),"+/-",round(RMSEP(yieldPrev$PCmodel)$val[8],2)," using 4 components \n ",round(pcr3,2),"+/-",round(RMSEP(yieldPrev$PCmodel)$val[6],2)," using 3 components."))
}