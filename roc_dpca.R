library(pROC)

p <- length(ASIA)
prec<-matrix(nrow=20, ncol=p)
for(i in 1:20){
	prec[i,]<-mean_obs_prec[[i]][i,ASIA]}

	
	
time1= c(1:20) #all year
time2 = elyr #el nino
time3 = layr #la nina
time4 = nryr #neutral year
time5 = monsoon #positive EASMI
time6 = monsoon_below #negative EASMI
timelist = list(time1, time2, time3, time4, time5, time6)

for(t in 1:6){
	
	time <- timelist[[t]]

obss<-prec
a<-quantile(prec[time,],probs = seq(0, 1, 0.1))[6]

categorized_obs<-matrix(nrow=length(time),ncol=p)

for(k in 1:p){
		for(i in 1:length(time)){
			if (obss[time[i],k]>=a)
				categorized_obs[i,k]<-1
			if (obss[time[i],k]<a)
				categorized_obs[i,k]<-0

		}
	}



quartz()
par(mfrow=c(1,2), mar=c(3,3,4,6), cex.main=1, cex.axis=1,omi=c(0.2,0.5,0.5,0.3))

pred<-pca_ridge[time,ASIA]
plot.roc(categorized_obs,pred,print.auc=TRUE,print.auc.cex= 1.2,main='PCA/ridge')

pred<-dpca_ridge[time,ASIA]
plot.roc(categorized_obs,pred,print.auc=TRUE,print.auc.cex= 1.2, main='DPCA/ridge')


	if(t==1){
	mtext('ROC for precipitation : All years',3,outer=TRUE,line=0.7,cex=1.2) 
	}else if(t==2){
		mtext('ROC for precipitation : El Nino years',3,outer=TRUE,line=0.7,cex=1.2) 
	}else if(t==3){
		mtext('ROC for precipitation : La Nina years',3,outer=TRUE,line=0.7,cex=1.2) 
	}else if(t==4){
		mtext('ROC for precipitation : Neutral years',3,outer=TRUE,line=0.7,cex=1.2) 
	}else if(t==5){
		mtext('ROC for precipitation : positive EASMI years',3,outer=TRUE,line=0.7,cex=1.2) 
	}else if(t==6){
		mtext('ROC for precipitation : negative EASMI years',3,outer=TRUE,line=0.7,cex=1.2) 
	}
}