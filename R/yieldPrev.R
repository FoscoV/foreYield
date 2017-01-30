	if(any(ls() == "yieldPrev")){}else{
	yieldPrev <-new.env()
	yieldPrev$.conflicts.OK<-c()
}
###CONFIGURATION
configure<-function(depth="base"){
	#find out data-files
	#cat("If you created your csv databases using MS Office, write <1> here, else <0>", fill=TRUE)
	#msoffice<-scan(,nmax=1)
	cat("Provide OFFICIAL yield database",fill=TRUE)
	offiData<-file.choose()
	write.table(gsub(gsub(scan(file=offiData,what="text",sep="\n"),pattern='"',replacement='',fixed=TRUE),pattern=";",replacement=","),file="adjOffi.csv",quote=FALSE,row.names=FALSE,col.names=FALSE)
	eurostat<-read.csv("adjOffi.csv")
#	if(msoffice == 1) eurostat<-read.table(file.choose(), header=T, sep=";") else eurostat<-read.csv(file.choose())

	if (depth == "advanced" && depth == "adv-offi"){
		#you are here only if the official tables are not in the format supposed... we are going to work on them
		freakoffi<-eurostat
		cat(c("Here are the columns in your file. Which one represents the crop code? \n ",names(freakoffi)," \n Write it here: \n "),fill=TRUE)
		scn<-scan(,what="text",nmax=1)

		cat(c("Here are the columns in your file. Which one represents the Nation (is going to be drop)? \n ",names(freakoffi)," \n Write it here: \n "),fill=TRUE)
		nut<-scan(,what="text",nmax=1)

		cat(c("Here are the columns in your file. Which one represents the Years? \n ",names(freakoffi)," \n Write it here: \n "),fill=TRUE)
		epoch<-scan(,what="text",nmax=1)

		cat(c("Here are the columns in your file. Which one represents the Official Yield? \n ",names(freakoffi)," \n Write it here: \n "),fill=TRUE)
		harv<-scan(,what="text",nmax=1)

		eurostat$STAT_CROP_NO<-freakoffi$as.name(scn)
		eurostat$NUTS_CODE<-freakoffi$as.name(nut)
		eurostat$YEAR<-freakoffi$as.name(epoch)
		eurostat$OFFICIAL_YIELD<-freakoffi$as.name(harv)
	}

	cat("Provide SIMULATE yield database",fill=TRUE)
	simuData<-file.choose()
	write.table(gsub(gsub(scan(file=simuData,what="text",sep="\n"),pattern='"',replacement='',fixed=TRUE),pattern=";",replacement=","),file="adjSimu.csv",quote=FALSE,row.names=FALSE,col.names=FALSE)
	prev <- read.csv("adjSimu.csv")
	#if(msoffice == 1) prev<-read.table(file.choose(), header=T, sep=";") else prev<-read.csv(file.choose())

	if (depth == "advanced" && depth == "adv-simul"){
		#you are here only if the prevision tables are not in the format supposed... we are going to work on them
		freakprev<-prev
		cat(c("Here are the columns in your file. Which one represents the crop code? \n ",names(freakprev)," \n Write it here: \n "),fill=TRUE)
		scn<-scan(,what="text",nmax=1)

		cat(c("Here are the columns in your file. Which one represents the Nation (is going to be drop)? \n ",names(freakprev)," \n Write it here: \n "),fill=TRUE)
		nut<-scan(,what="text",nmax=1)

		cat(c("Here are the columns in your file. Which one represents the Years? \n ",names(freakprev)," \n Write it here: \n "),fill=TRUE)
		epoch<-scan(,what="text",nmax=1)

		cat(c("Here are the columns in your file. Which one represents the decade (phase of the year, if you like: it doesn't matter)? \n ",names(freakprev)," \n Write it here: \n "),fill=TRUE)
		decad<-scan(,what="text",nmax=1)


		prev$CROP_NO<-freakprev$as.name(scn)
		prev$NUTS_CODE<-freakpre$as.name(nut)
		prev$YEAR<-freakoffi$pre(epoch)
		prev$DECADE<-freakoffi$as.name(decad)


	}

	#choice of crop parameters
	cat(c("OFFICIAL yield data contains information for the following CROPs:\n",unique(eurostat$STAT_CROP_NO),"\n Choose one:",fill=TRUE))
	cropO<-scan(,nmax=1)
	while(any(unique(eurostat$STAT_CROP_NO) == cropO) == FALSE){cat("point an existing one \n ")
		cropO<-scan(,nmax=1)}
	cat(c("SIMULATED yield data contains information for the following CROPs:\n",unique(prev$CROP_NO),"\n Choose one:",fill=TRUE))
	cropS<-scan(,nmax=1)
	while(any(unique(prev$CROP_NO) == cropS) == FALSE){cat("point an existing one \n ")
	cropS<-scan(,nmax=1)}

	#choice of country
	cat(c("Following countries are provided by the DataBases:\n","OFFICIAL:",levels(eurostat$NUTS_CODE),"\n SIMULATION:",levels(prev$NUTS_CODE),"(case sensitive) \n"),fill=TRUE)
	cat(" OFFICIAL COUNTRY:")
	countryO<-scan(,what="text",nmax=1)
	while(any(levels(eurostat$NUTS_CODE) == countryO) == FALSE){cat("point an existing one \n ")
	countryO<-scan(,what="text",nmax=1)}
	cat("\n SIMULATION COUNTRY")
	countryS<-scan(,what="text",nmax=1)
	while(any(levels(prev$NUTS_CODE) == countryS) == FALSE){cat("point an existing one \n ")
	countryS<-scan(,what="text",nmax=1)}
	#subsetting conforming the configuration set above
	actualYield<-subset(eurostat, eurostat$STAT_CROP_NO == cropO & eurostat$NUTS_CODE == countryO)[,c(which(names(eurostat)=="YEAR"),which(names(eurostat)=="OFFICIAL_YIELD"))]
	yieldPrev$actualYield<-actualYield[order(actualYield$YEAR),]
	relatedModel<-subset(prev, prev$CROP_NO ==cropS & prev$NUTS_CODE == countryS)

	#reading datas for suitable informations
	currentYear<- max(unique(relatedModel$YEAR))
	yieldPrev$currentYear <- currentYear
	currentDecade<- max(subset(relatedModel,relatedModel$YEAR==currentYear)$DECADE)
	cat(c("It seems forecasting the year",currentYear,"with data till the ",currentDecade,"th decade"),fill=TRUE)
	yieldPrev$relatedModel<-subset(relatedModel,relatedModel$DECADE == currentDecade)[,c(-which(names(prev)=="CROP_NO"),-which(names(prev)=="DECADE"),-which(names(prev)=="NUTS_CODE"))]

	#save information for saveYieldSession()
	yieldPrev$saveCrop<-cropS
	yieldPrev$saveCountry<-countryS

}



###working on official trends
library(tseries)
library(forecast)
library(ggplot2)
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
	while(yieldPrev$flattyn != "y" & yieldPrev$flattyn != "n"){
		cat("answer y or n \n ")
		yieldPrev$flattyn<-scan(,what="text",nmax=1)}
#	cat(c("The time serie is ",(unique(max(flatYield$YEAR))-unique(min(flatYield$YEAR))), "years long, since",unique(min(flatYield$YEAR)),"to",unique(max(flatYield$YEAR))),fill=TRUE)
	#checking if there are troubles in the official data series.
	if(length(c(setdiff(seq(min(flatYield$YEAR),max(flatYield$YEAR)),flatYield$YEAR),flatYield$YEAR[duplicated(flatYield$YEAR)])) == 0) cat("") else cat(c("By the way there are \n MISSING:",setdiff(seq(min(flatYield$YEAR),max(flatYield$YEAR)),flatYield$YEAR)," \n REPLICATED: ",flatYield$YEAR[duplicated(flatYield$YEAR)]))
	detach(yieldPrev)
}


#working on official data's trend
sewTrends<-function(inizio,fine){
	tempLimit<-data.frame(begin=inizio,finish=fine)
	attach(yieldPrev)
	flatOff<- merge(flatYield,relatedModel,by="YEAR")
	yieldPrev$flatOff<-flatOff
	normalizingTrend<-function(campo){ #campo is numeric!
		yieldPrev$flatOff[,campo]<-(flatOff[,campo]/mean(flatOff[,campo]))*100
	}
	lapply(X=seq(2,length(flatOff)),FUN=normalizingTrend)

	#removing NA values, they mess up the lm....
	flatOff<-yieldPrev$flatOff
	yieldPrev$flatOff<-flatOff[sapply(flatOff,function(flatOff)!any(is.na(flatOff)))]
	flatOff<-yieldPrev$flatOff
	trendAN<-new.env()
	trendAN$normPlot<-ggplot(yieldPrev$flatOff)+geom_line(aes(x=YEAR,y=OFFICIAL_YIELD,group=1),color="red")+geom_smooth(method="loess",color="red",aes(x=YEAR,y=OFFICIAL_YIELD,group=1),se=FALSE)
	trendPlot<-function(yvar){

		trendAN$normPlot<-trendAN$normPlot + geom_smooth(data=yieldPrev$flatOff,method="loess", color="cyan", aes_(x=~YEAR, y=as.name(yvar),group=1),se=FALSE)

	}
	lapply(names(yieldPrev$flatOff[,c(-1)]),FUN=trendPlot)
	trendAN$PlotNormA<-trendAN$normPlot+geom_line(aes(x=YEAR,y=OFFICIAL_YIELD,group=1),color="red",size=1.5)+geom_smooth(method="loess",color="orange",aes(x=YEAR,y=OFFICIAL_YIELD),se=FALSE,size=1.5)+labs(x="YEARS", y="TREND")+ geom_rect(data=tempLimit, aes(xmin=begin, xmax=finish, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.5)
	detach(yieldPrev)
	plot(trendAN$PlotNormA)

	#yieldPrev$friendShip<-data.frame(trendCoef=lm(formula=OFFICIAL_YIELD ~ YEAR,data=flatOff)$coefficients[2])
	#rownames(yieldPrev$friendShip[1,])<-"OFFICIAL_YIELD"
	flatOff1<-yieldPrev$flatOff
	flatOff2<-subset(flatOff1,flatOff1$YEAR >= inizio & flatOff1$YEAR <= fine)
	yieldPrev$flatOff<-flatOff2
	flatOff<-flatOff2
	yieldPrev$friendShip<-data.frame(param=as.character(names(flatOff)[(names(flatOff) == "OFFICIAL_YIELD")]),trendCoef=as.numeric(lm(formula=OFFICIAL_YIELD ~ YEAR,data=flatOff)$coefficients[2]))
	plot(trendAN$PlotNormA+stat_smooth(data=flatOff,method="lm",color="black",aes(x=YEAR,y=OFFICIAL_YIELD,group=1),fullrange=FALSE,se=FALSE,size=1))
	mayTrend<-names(flatOff)[(names(flatOff)!= "YEAR" &  names(flatOff)!= "OFFICIAL_YIELD")]
	friendTest<-function(mate){
		allIn<-yieldPrev$flatOff
		formulFriend<-as.formula(paste(as.name(mate)," ~ YEAR",sep=""))
		linMod<-lm(formula=formulFriend,data=allIn)
		yieldPrev$friendShip <- rbind(yieldPrev$friendShip,data.frame(param=paste(c(as.character(mate)),sep=""),trendCoef=as.numeric(linMod$coefficients[2])),deparse.level=1)
		#rownames(yieldPrev$friendShip[length(yieldPrev$friendShip[,1]),])<-as.name(mate)
		#print(c(paste(c(as.character(mate)),sep=""),linMod$coefficients[2]))
	}
	sapply(X=mayTrend,FUN=friendTest,USE.NAMES=TRUE)
	trendShip<-yieldPrev$friendShip
	YieldTrend<-trendShip[1,2]
	yieldPrev$yieldTrend<-YieldTrend
	cat(c("The found trend for Official_Yield is ",round((trendShip[1,2]),2),"%. \n The following are the similar trends available between the predictors: \n ->Absolute values \n -> Sorted by difference with Yield trend \n \n "),fill=TRUE)
	trendShip$trendDiff<-abs(trendShip[,2]-trendShip[1,2])
	trendMates<-trendShip[c(-1),]
	trendMates<-(trendMates[order(trendMates$trendDiff),])
	trendMates$diff_perC <- round(trendMates$trendDiff/YieldTrend,2)
	trendMates$ID <- seq(1,length(trendMates[,1]))
	print(trendMates)

	#continue to cut?
	cat(c("\n Do you want to continue removing the trend in official yields"),fill=TRUE)
	continueToCut<-scan(,what="text",nmax=1)
	while(continueToCut != "y" & continueToCut != "n"){
		cat("answer y or n")
		continueToCut<-scan(,what="text",nmax=1)}
	if(continueToCut == "y"){
		#allow some sewing with Mates (in case which)
		cat(c("\n Does any of the predictors explain some of the Official's Trend? \n If yes, point which one(s) by ID (multiple answers allowed) \n If none of them does, return blank: \n"),fill=TRUE)
		mateList<-scan(,nmax=length(trendMates[,1]))
		if(length(mateList >= 1)){
			yieldPrev$safeTrend <- mean(trendMates$trendCoef[c(mateList)])
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
	while(continCut != "y" & continCut != "n"){
		cat("answer y or n")
	continCut <- scan(,what="text",nmax=1)}
	if(continCut == "y"){
	sewTrends(min(trendEdge),max(trendEdge))}
	detach(yieldPrev)

}
library(MASS)
cutTrend<-function(inizio,fine){
	if(any(names(yieldPrev) == "due2trend")){} else {
		yieldPrev$due2trend<-data.frame(YEAR=yieldPrev$actualYield$YEAR,trended=rep(0,length=length(yieldPrev$actualYield$YEAR)))
	}
	notSoFlat <-yieldPrev$actualYield
	#cutting trend from inizio to fine
	preflat<-subset(notSoFlat,notSoFlat$YEAR >= inizio & notSoFlat$YEAR <= fine)
	flatLin<-lm(OFFICIAL_YIELD~YEAR,data=preflat)
	#developing the flat "officials"
	cutEnv<-new.env()
    cutEnv$modello<-flatLin
    cutEnv$flatting<-preflat
    if(any(names(yieldPrev)=="safeTrend)")){cutEnv$trendCorr<-(flatLin$coefficients[2]-yieldPrev$safeTrend)} else{ cutEnv$trendCorr<-flatLin$coefficients[2]}
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
	yieldPrev$safeTrend<- NULL


	#meet flatYield and preflat (now postFlat,but...by the way, i like that name!)

	yieldPrev$flatYield$OFFICIAL_YIELD<-yieldPrev$actualYield$OFFICIAL_YIELD - yieldPrev$due2trend$trended
}


library(leaps)
library(HH)
modSel <- function(){
	tableXregression <-merge(yieldPrev$flatYield , yieldPrev$relatedModel , by="YEAR")
	cat("Are you looking for a standard additive model? \n a models accounting for combined predictors. \n Which do you prefer? \n ")
	cat(" 1. standard \n 2. enhanced \n ")
	standardModel<-scan(,what="text",nmax=1)
	while(standardModel != "1" & standardModel != "2" & standardModel != "standard" & standardModel != "enhanced" ){
		cat(" 1. standard \n 2. enhanced \n ")
		standardModel<-scan(,what="text",nmax=1)}
	if(standardModel == "1" | standardModel == "standard"){
	allSign <- regsubsets(OFFICIAL_YIELD~.,data=tableXregression[,c(-1)],nbest=2,method="exhaustive",nvmax=4, really.big=TRUE)}
	if(standardModel == "2" | standardModel == "enhanced"){
	allSign <- regsubsets(OFFICIAL_YIELD~.^2+.,data=tableXregression[,c(-1)],nbest=2,method="exhaustive",nvmax=4, really.big=TRUE)}
	#renaming predictors with numbers
	summaSign<-summaryHH(allSign,names=seq(1,length(allSign$xnames)),statistics="adjr2")
	plot(summaSign,col="green",cex=0.8)
	#plot(summaryHH(allSign,scale="r2"))
#calc.relimp(yieldPrev$regrSW,type="car")
	print(summaSign)
	#print(summaryHH(allSign,abbrev=3,statistics="adjr2"))
	cat("\n Note: one of the accounted parameter is (Intercept) \n Select a model")
	modId<-scan(,nmax=1)
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
}
library(DAAG)

responseYield<-function(){
	expYield <- yieldPrev$expYield
	knoTime<-yieldPrev$breakPoint
	if(max(knoTime$finish)== (yieldPrev$currentYear -1)){
	trendMissing<-mean(knoTime$trend[which(knoTime$finish == max(knoTime$finish))])+yieldPrev$due2trend$trended[which(yieldPrev$due2trend$YEAR == (yieldPrev$currentYear -1) )]} else {trendMissing <- yieldPrev$due2trend$trended[which(yieldPrev$due2trend$YEAR == (yieldPrev$currentYear -1) )] }
	cat(c(" \n \n \n RESPONSE \n \n ","As it is, the forecasted yield for year",yieldPrev$currentYear,"is",round(expYield$fit[1],2),"+/-",round(expYield$fit[1]-expYield$fit[2],2),"."),fill=TRUE)
	cat(c("Confidence = 95% \n
	\n \n CROSS-VALIDATION returned ",round(yieldPrev$CVmsRes[1],2),"as mean square error
\n and ",round(yieldPrev$CVmsRes[2],2),"as R2."),fill=TRUE)
if(any(names(yieldPrev) == "due2trend")){cat(c("
Due to the marked trends, the forecasted has to be corrected with ",round(trendMissing,2)," resulting, so, as ",round(expYield$fit[1]+trendMissing,2),". \n
"),fill=TRUE)}
cat(c("	\n
	TimeSeries statistical analysis over OFFICIAL_YIELD would bet on ",round(forecast(ets(yieldPrev$actualYield[,2]),h=1)$mean[1],2)," +/- ",round((forecast(ets(yieldPrev$actualYield[,2]),h=1)$upper[2]-forecast(ets(yieldPrev$actualYield[,2]),h=1)$mean[1]),2)),fill=TRUE)

	#crossVal plot
	#acquire loocv single data
	motoCross<-cv.lm(data=yieldPrev$tableXregression, formula(yieldPrev$modelLM), m=length(yieldPrev$tableXregression[,1]),printit=FALSE)
	#keep out Year and cvpred
	crValPre<-data.frame(YEAR=motoCross$YEAR,pred=motoCross$cvpred)
	respoPlot<- ggplot(crValPre)
	cat(c("Plotted:\n GREEN: Predicted yield in Cross Validation \n RED: Data on which models are regressed"),fill=TRUE)
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
}

saveYieldSession<- function(){
	attach(yieldPrev)
	save(list=ls(yieldPrev),file=paste(Sys.Date(),saveCountry,saveCrop,".RData",sep=""))
	cat(c("Session saved in ",paste(Sys.Date(),saveCountry,saveCrop,".RData",sep=""),". \n Use loadYieldSession() to restore in future sessions."),fill=TRUE)
}
loadYieldSession<-function(){
	cat(c("Select the desired previous session saved \n "),fill=TRUE)
	oldYieldSession<-file.choose()
	load(file=oldYieldSession,envir=yieldPrev)
	cat(c("Session in oldYieldSession loaded \n "),fill=TRUE)
}

sewedCheck<-function(){
#save involved data that will be restored later
yieldPrev$bckflatYield <- yieldPrev$flatYield
yieldPrev$breakPoint
#refresh previously edited data as original
yieldPrev$flatYield<-yieldPrev$actualYield
#go throught the whole process by itself

#get the desired datas

#restore all

	}

virgilio<-function(){
	suppressWarnings(configure())
	suppressWarnings(checkTrends())
	while(yieldPrev$flattyn == "y"){
		suppressWarnings(breakTrends())
		suppressWarnings(checkTrends())
	}
	suppressWarnings(modSel())
	suppressWarnings(responseYield())
}

