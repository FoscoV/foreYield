####library(trend)

virgilio<-function(reload=F){
	if(reload=F){
		if(any(names(yieldPrev)== "actualYield")){}else{
		suppressWarnings(configure())}
	}else{
		suppressWarnings(configure(depth="reload"))
	}

	#cleaning working directory
	#unlink(c("adjOffi.csv","adjSimu.csv"))

	suppressWarnings(checkTrends())
	if(suppressWarnings(adf.test(yieldPrev$flatYield$OFFICIAL_YIELD)$p.value) > 0.25&yieldPrev$flattyn == "y") {suppressWarnings(try(autoProposal(),silent=T))}
	while(yieldPrev$flattyn == "y"){
		suppressWarnings(breakTrends())
		suppressWarnings(checkTrends())
	}
	suppressWarnings(modSel())
	suppressWarnings(responseYield())
}



autoProposal<-function(){
	if(yieldPrev$flattyn =="y"){
		#while(adf.test(yieldPrev$flatYield$OFFICIAL_YIELD)$p.value > 0.25){
			suppressMessages(suppressWarnings(autoDetrender()))
			#}
		dev.new()
		attach(yieldPrev)
			#offiPlot<-ggplot(flatYield,aes(x=YEAR, y=OFFICIAL_YIELD,group=1))+geom_point(color="black")+geom_line(color="black")+geom_smooth(method="lm",color="brown",se=FALSE)+geom_smooth(method="loess",color="red",se=FALSE)+geom_smooth(method="lm",formula= y~splines::bs(x,3),color="orange",se=FALSE)
			#plot(offiplot)

		flatPlot<-ggplot(yieldPrev$flatYield)+geom_point(color="black",aes(x=YEAR, y=OFFICIAL_YIELD,group=1))+geom_line(color="black",aes(x=YEAR, y=OFFICIAL_YIELD,group=1))+geom_smooth(method="lm",color="brown",se=FALSE,aes(x=YEAR, y=OFFICIAL_YIELD,group=1))+geom_smooth(method="loess",color="red",se=FALSE,aes(x=YEAR, y=OFFICIAL_YIELD,group=1))+geom_smooth(method="lm",formula= y~splines::bs(x,3),color="orange",se=FALSE,aes(x=YEAR, y=OFFICIAL_YIELD,group=1))
		flatPlot<-flatPlot + geom_rect(data=yieldPrev$breakPoint, aes(xmin=begin, xmax=finish, ymin=-Inf, ymax=+Inf), fill='yellow', alpha=0.3)
		plot(flatPlot)

		detach(yieldPrev)
		cat("This is the result of an automated de trending procedure. \n Do you want to use it? \n y : save it as a starting point \n n : drop it and remove trend from original data \n ")
		appAuto<-scan(,what="text",nmax=1)
		while(appAuto != "y" & appAuto != "n"){
			cat("answer y or n \n ")
			appAuto<-scan(,what="text",nmax=1)
		}
		if(appAuto == "n"){
			rm(list=c("flattyn","safeTrend","yieldTrend","flatOff","tableXregression","model_formula","CVmsRes","expYield","omniYield","modelLM","PCmodel"),envir=yieldPrev)
			yieldPrev$flatYield<-yieldPrev$actualYield
		}
		if(appAuto == "y"){
			suppressWarnings(checkTrends())
		}
		dev.off()

	}
}

autoDetrender<-function(){
	#acquire te data and clean'em
	tableXregression <-merge(yieldPrev$flatYield , yieldPrev$relatedModel , by="YEAR")
	#clean this table, 0 columns are going to mess it up
	coluClean<-function(cola){
		if(min(tableXregression[,cola]) == max(tableXregression[,cola])){return(cola)}
	}
	dirtyCol<-lapply(X=seq(1,length(names(tableXregression))),FUN=coluClean)
	if(!is.null(unlist(dirtyCol))){tableXregression<-tableXregression[,-unlist(dirtyCol)]}
	#check existance of break point
	if(pettitt.test(tableXregression$OFFICIAL_YIELD)$p.value <= 0.5 & pettitt.test(tableXregression$OFFICIAL_YIELD)$estimate > 1 ){
		untrendingIt(tableXregression$YEAR[pettitt.test(tableXregression$OFFICIAL_YIELD)$estimate],tableXregression)

		while(yieldPrev$refeedAutoTrend < yieldPrev$currentYear){

			try(untrendingIt(yieldPrev$refeedAutoTrend,merge(yieldPrev$flatYield , yieldPrev$relatedModel , by="YEAR")))}
	}
}

untrendingIt<-function(inizio,tableXregression){
	#find out pre-trend data
	tableXpast<-subset(tableXregression,tableXregression$YEAR<=inizio)
	#check pre-trend dynamics
			resarima<-auto.arima(tableXpast$OFFICIAL_YIELD,xreg=tableXpast[,-c(which(names(tableXpast) == "YEAR"),which(names(tableXpast) == "OFFICIAL_YIELD"))],seasonal=FALSE,allowdrift=F,allowmean=T)
			#model_full <- lm(OFFICIAL_YIELD ~ ., data = tableXpast[,-c(which(names(tableXpast) == "YEAR"))])
			#scopeformula <- formula(model_full)
			#resarima <- step(object=model_full, scope=scopeformula, direction="both", trace = 0) #<== larger K reduce the suit to involve more predictors
		#trended data
	tableXfuture<-subset(tableXregression,tableXregression$YEAR>inizio)
	#check if trend ended
	if(pettitt.test(tableXfuture$OFFICIAL_YIELD)$p.value <= 0.5 & pettitt.test(tableXfuture$OFFICIAL_YIELD)$estimate != length(tableXfuture$OFFICIAL_YIELD& pettitt.test(tableXfuture$OFFICIAL_YIELD)$estimate > 1)){
		fine<-tableXfuture$YEAR[pettitt.test(tableXfuture$OFFICIAL_YIELD)$estimate]
		print(fine)
		tableXfuture<-subset(tableXfuture,tableXfuture$YEAR <= fine)
		}else{fine<-yieldPrev$currentYear}
			futarima<-predict(resarima,newxreg=tableXfuture[,-c(which(names(tableXfuture) == "YEAR"),which(names(tableXfuture) == "OFFICIAL_YIELD"))])
			#futarima<-predict(resarima,newdata=tableXfuture[,-c(which(names(tableXfuture) == "YEAR"),which(names(tableXfuture) == "OFFICIAL_YIELD"))],se.fit=TRUE,type="response",level=0.95,interval="prediction")

			yieldPrev$safeTrend<-lm(futarima$pred~seq(1,length(futarima$pred)))$coefficients[2]
			#yieldPrev$safeTrend<-lm(futarima$fit[,1]~seq(1,length(futarima$fit[,1])))$coefficients[2]
	cutTrend(inizio,fine)
	yieldPrev$refeedAutoTrend<-fine

}
