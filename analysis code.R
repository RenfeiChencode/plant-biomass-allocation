library(ggplot2)
library(gridExtra)
library(basicTrendline)
library(lmodel2) 
library(scales)
library(tseries)# test the stability of time series

####################################################################
#Constant final yield
#corn
together1=together2=NA;j=1
for (i in c(1,4,7,10,13,16)){
  leaf[[i]][leaf[[i]]==0]=NA;  total[[i]][total[[i]]==0]=NA; density[[i]][density[[i]]==0]=NA#
  df=data.frame(leafmass=leaf[[i]],totalmass=total[[i]],den=density[[i]])
  df=df[complete.cases(df),]#removing NA
  Meantotal=aggregate(df$totalmass,by=list(type=df$den),mean)
  timepoint=rep(j,length(Meantotal$type))
  j=j+1
  together1=c(together1,timepoint)
  together2=rbind(together2,Meantotal)
}
wholemass=(together2$type)*(together2$x)
Dense=together2$type
df=data.frame(wholemass=wholemass/2.25,Dense=Dense/2.25,class=together1)
df=df[complete.cases(df),]

pcorn=ggplot(data=df,aes(x=Dense,y=wholemass,group=class,color=class))+scale_colour_gradientn(colours = hcl.colors(length(1:6)))+geom_point(size=3)+ 
  geom_smooth(method = "nls", se = FALSE, method.args = list(formula= y~x*W/(1+a*x), start = list(W=6000/2.25,a=1),trace = T))+
  labs(title="Corn 2011",x=expression("Density (no./" ~ m^{2}~")"),y=expression("Whole biomass (g/" ~ m^{2}~")"))+
theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.1,0.8), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0.0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.7, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,900,300),limits=c(0,900))+
  scale_y_continuous(breaks=seq(0,4000,1000),limits=c(0,4000))
#ggsave("FigSconstantfinalyield.pdf",plot=grid.arrange(pcorn,pflax,pwheat,ncol = 3),width = 60, height = 20, units = "cm",dpi=300) 
######################################################################
#mixture #flaxwheat
library(sqldf)
library(dplyr)
KTcorn=c(0.3944298/ 0.0001042902,2.697651/ 0.0005533997,4.724898 /0.0009294475,6.715837 /0.001197204,30.41216 /0.008298492,63.61039/ 0.01184737)
KTwheat=c(0.7351187 /0.002737025,0.8173058 /0.000125521,1.49474/ 0.0008362794,0.7530507 /0.0004692125,1.045022/ 0.0004251083)
KTflax=c(0.02541527/ 3.662604e-06,0.6095139 /0.001206913,1.229125/ 0.000765852,1.09179 /0.0007150112,1.810612 /0.0006060676,1.947565/ 0.0006021042)
Alltimepoint=list(F20111,F20112,F20113,F20114,F20115)
together1=together2=0;MLT=matrix(nrow=5,ncol=18)
together3=together4=together5=together6=together7=together8=NA
for (i in 1:5){
  AT=Alltimepoint[[i]];AT[AT==0]=NA;
  stem=as.numeric(AT$stemflaxwheat); leaf=as.numeric(AT$leafflaxwheat)
  if (i==1){root=as.numeric(AT$rootflaxwheat); total=root+stem+leaf;
  }else{HDD=log(AT$dflaxwheat*AT$dflaxwheat*AT$heightflaxwheat)
  LT=TOT=0
  for (ii in 1:length(AT$IDflaxwheat2)){if (AT$IDflaxwheat2[ii]=="小麦"){TOT[ii]=exp(0.82*HDD[ii] -1.1)}else{TOT[ii]=exp(0.82*HDD[ii] -0.73)}}
  total=TOT
  }
  df=data.frame(leaf=leaf,total=total,leaffraction=leaf/total,densityflaxwheat=AT$densityflaxwheat,id1=AT$IDflaxwheat1,id2=AT$IDflaxwheat2)
  df=df[complete.cases(df),]
  MeanLT=aggregate(df$leaffraction,by=list(type=df$id1),mean)
  hel=sqldf("select id1,id2,avg(total) as total_avg,avg(densityflaxwheat) as density_avg from df group by id1,id2")
  newwheat=group_by(hel,id1)%>% slice(1)
  newflax=group_by(hel,id1)%>% slice(2)
  position <- Reduce(intersect,list(newflax$id1,newwheat$id1))
  Mtotalwheat=Mtotalflax=densewheat=denseflax=meanleaf=0
 for (ij in 1:length(position)){
   dfsub=subset(newwheat,newwheat$id1==position[ij])
   Mtotalwheat[ij]=dfsub$total_avg;densewheat[ij]=dfsub$density_avg
   dfsub=subset(newflax,newflax$id1==position[ij])
   Mtotalflax[ij]=dfsub$total_avg;denseflax[ij]=dfsub$density_avg
   dfsub=subset(MeanLT,MeanLT$type==position[ij])
   meanleaf[ij]=dfsub$x;
 }
MLT[i,]=meanleaf
  Kflax=KTflax[i]/Mtotalflax
  Kwheat=KTwheat[i]/Mtotalwheat
  q1=(Kflax-denseflax)*Kwheat/(densewheat*Kflax)
  q2=(Kwheat-densewheat)*Kflax/(denseflax*Kwheat)
  together1=together1+q1
  together2=together2+q2

together3=c(together3,df$leaf);together4=c(together4,df$total);together5=c(together5,df$leaffraction);together6=c(together6,df$densityflaxwheat);  
together7=c(together7,df$id1);together8=c(together8,df$id2);
result=lmodel2(log(df$leaf)~log(df$total)) #for each timepoint
print(result)
}
mlt=as.data.frame(MLT)

df2=data.frame(leaf=together3,total=together4,leaffraction=together5,density=together6,id1=together7,id2=together8)
leafflaxwheat=df2$leaf;totalflaxwheat=df2$total;classflaxwheat=length(df2$total)
df2=df2[complete.cases(df2),]

lmodel2(log(df2$leaf)~log(df2$total)) #for pooled data

competition4=exponent4=CVLT4=0;j=1
for (i in 1:length(position)){ 
  dfsub=subset(df2,df2$id1==position[i]);
  total=dfsub$total;leaf=dfsub$leaf;density=dfsub$density;leaffraction=dfsub$leaffraction

  competition4[j]=mean(density)
  result=lmodel2(log(leaf)~log(total))
  exponent4[j]=result$regression.results[1,3]
  CVLT4[j]=sd(leaffraction,na.rm=T)/mean(leaffraction,na.rm=T) 
  j=j+1
} 
ontogeny=leaffraction=exponent=comp=0
for (i in 1:length(mlt[1,])){
  ontogeny[((i-1)*5+1):((i-1)*5+5)]=c(30,40,50,60,70)
  leaffraction[((i-1)*5+1):((i-1)*5+5)]=mlt[,i]
  exponent[((i-1)*5+1):((i-1)*5+5)]=rep(exponent4[i],5)
  comp[((i-1)*5+1):((i-1)*5+5)]=rep(competition4[i],5)
}
df=data.frame(ontogeny=ontogeny,leaffraction=leaffraction,exponent=exponent,density=comp/2.25)
p1=ggplot(data = df,aes(x=ontogeny,y=leaffraction,group=density,color=density))+
  scale_colour_gradientn(breaks=c(70,360,650),limits=c(70,650),colours = hcl.colors(length(density)))+geom_point(shape=NA)+geom_path()+
  labs(title="Flax wheat mixture",x="Plant ontogeny (days)",y="Mean leaf mass ratio",color="Density")+#theme(plot.title = element_text(hjust = 0.5)) 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.15,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(30,70,10),limits=c(30,70))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-1),10^(-0.2))
  )

q1=together1/5;q2=together2/5;
class=c(rep("q1",length(q1)),rep("q2",length(q2)))
qq=c(q1,q2);CVLT=c(CVLT4,CVLT4)
df=data.frame(class=class,qq=qq,CVLT=CVLT)
plottfig4=ggplot(data = df,aes(x=qq,y=CVLT,group=class,color=class))+geom_point(size=1)+geom_smooth(method="lm",span=1)+labs(title="Flax wheat mixture",colour="",x="Plant interaction",y="CVLT")+theme(plot.title = element_text(hjust = 0.5))+
theme(axis.text=element_text(size=30,family="serif",colour = "black"),
      axis.text.x = element_text(vjust=0.0),
      axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
      axis.title.x = element_text(vjust=0.0),
      axis.line=element_line(size = 0.1, colour = "black", linetype=1),
      #text = element_text(family="Times New Roman",color = "black"),
      axis.ticks.length = unit(0.15, "cm"),
      axis.ticks = element_line(size = 0.5, color="black"),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
      panel.background = element_rect(fill = "white", color = NA),
      line = element_blank(),
      legend.position = c(0.9,0.9), 
      legend.background = element_rect(fill = "NA", color = NA),
      legend.text =element_text(size = 16,color='black'),
      legend.title = element_text(size=16, face = "bold"),
      legend.direction="vertical", #horizontal; vertical
      legend.key.size = unit(1.2,'line'), # space between legend text
      legend.key = element_blank(),
      plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
      plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(-105,195,100),limits=c(-105,195))+
  scale_y_continuous(breaks=seq(0.5,1.1,0.2),limits=c(0.5,1.1))

#ggsave("Fig 5Flax wheat.pdf",plot=grid.arrange(p1,Flaxwheat,ncol = 1),width = 18, height = 36, units = "cm",dpi=300) 
######################################################################
#leaf fraction vs. ontogeny
#2015 corn
ontogeny=meanfivedens=meanMLT=meanfivedensity=cvLT=0
HDDcorn2=log(corn2015$hdd2);HDDcorn3=log(corn2015$hdd3);HDDcorn4=log(corn2015$hdd4);HDDcorn5=log(corn2015$hdd5)
Totalcorn2=exp(0.74*HDDcorn2-0.29);Totalcorn3=exp(0.74*HDDcorn3-0.29);Totalcorn4=exp(0.74*HDDcorn4-0.29);Totalcorn5=exp(0.74*HDDcorn5-0.29)
leaffraction2=(corn2015$leaf2)/Totalcorn2;leaffraction3=(corn2015$leaf3)/Totalcorn3;leaffraction4=(corn2015$leaf4)/Totalcorn4;leaffraction5=(corn2015$leaf5)/Totalcorn5;
df=data.frame(ID=corn2015$ID2,repea=corn2015$repeat2,density=corn2015$density2,leaffraction=leaffraction2)
hel=sqldf("select ID,repea,avg(leaffraction) as leaffraction_avg,avg(density) as density_avg from df group by ID,repea")
meanPdMLT2=hel$leaffraction_avg;dense2=(hel$density_avg)/2.25

df=data.frame(ID=corn2015$ID3,repea=corn2015$repeat3,density=corn2015$density3,leaffraction=leaffraction3)
hel=sqldf("select ID,repea,avg(leaffraction) as leaffraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT3=hel$leaffraction_avg;dense3=(hel$density_avg)/2.25

df=data.frame(ID=corn2015$ID4,repea=corn2015$repeat4,density=corn2015$density4,leaffraction=leaffraction4)
hel=sqldf("select ID,repea,avg(leaffraction) as leaffraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT4=hel$leaffraction_avg;dense4=(hel$density_avg)/2.25

df=data.frame(ID=corn2015$ID5,repea=corn2015$repeat5,density=corn2015$density5,leaffraction=leaffraction5)
hel=sqldf("select ID,repea,avg(leaffraction) as leaffraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT5=hel$leaffraction_avg;dense5=(hel$density_avg)/2.25

for (i in 1:length(meanPdMLT2)){
  ontogeny[((i-1)*4+1):((i-1)*4+4)]=c(65,79,112,148)
  dense=c(dense2[i],dense3[i],dense4[i],dense5[i])
  meanfivedensity[i]=mean(dense)#每个样方五个时间点密度的算术平均值
  meanfivedens[((i-1)*4+1):((i-1)*4+4)]  =rep(mean(dense),4)
  mlt=c(meanPdMLT2[i],meanPdMLT3[i],meanPdMLT4[i],meanPdMLT5[i])
  meanMLT[((i-1)*4+1):((i-1)*4+4)]  =mlt
  cvLT[i]=sd(mlt)/mean(mlt)
}
df=data.frame(
  meanfivedensity=meanfivedensity,
  cvLT=cvLT,
  ontogeny=as.numeric(ontogeny),
  meanfivedens=as.numeric(meanfivedens),
  meanMLT=as.numeric(meanMLT))
fig2p1=ggplot(data = df,aes(x=ontogeny,y=meanMLT,group=meanfivedens,color=meanfivedens))+scale_colour_gradientn(breaks=c(1,90,180,265),colours = hcl.colors(length(meanfivedens)))+geom_point(shape=NA)+geom_path()+
  labs(title="Corn 2015",x="Plant ontogeny (days)",y="Mean leaf mass ratio",color="Density")+#theme(plot.title = element_text(hjust = 0.5)) 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(60,160,by=25),limits=c(60,160)
  )+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-1.2),10^(-0.4))
  )


############################################################
#simulation analysis
library(ggplot2)
library(patchwork)
library(tseries)# test the stability of time series
###############
C4=1;p0=-5;c=-1.5;g=8;pv=2*seq(1,2,by=0.1)-0.5
#corn2015
alphacorn2015OLS=c(NA,0.79, 1.27, 0.93, 0.92)#OLS
alphacorn2015SMA=c(NA,0.87, 1.28, 0.98, 0.96)#SMA
tcorn2015=c(NA,65,79,112,148)/30
#soybean2015
alphasoybean2015OLS=c(0.62, 0.83, 0.68, 0.93, 1.37)#OLS
alphasoybean2015SMA=c(0.73, 0.90, 0.73, 1.18, 1.50)#SMA
tsoybean2015=c(30,62,79,113,148)/30
#corn2016
alphacorn2016OLS=c(0.76, 1.03, 1.06, 0.98, 0.80)#OLS
alphacorn2016SMA=c(0.94, 1.12, 1.09, 1.02, 0.89)#SMA
tcorn2016=c(20,58,78,119,159)/30
#soybean2016
alphasoybean2016OLS=c(0.39, 0.37, 0.86, 1.05, 1.15)#OLS
alphasoybean2016SMA=c(0.73, 0.72, 0.96, 1.12, 1.21)#SMA
tsoybean2016=c(20,58,81,118,159)/30
leafratio=matrix(nrow=8,ncol=10);cvLf=matrix(nrow=8,ncol=length(pv))
for (ij in 1:8){
  if (ij==1){alphav=alphacorn2015OLS;tv=tcorn2015}#corn2015 OLS
  else if (ij==2){alphav=alphacorn2015SMA;tv=tcorn2015}#corn2015 SMA
  else if (ij==3){c=-0.5;g=3;alphav=alphasoybean2015OLS;tv=tsoybean2015}
  else if (ij==4){c=-0.5;g=3;alphav=alphasoybean2015SMA;tv=tsoybean2015}
  else if (ij==5){c=-2;alphav=alphacorn2016OLS;tv=tcorn2016}
  else if (ij==6){c=-2;alphav=alphacorn2016SMA;tv=tcorn2016}
  else if (ij==7){C4=-100;p0=-5;c=-2;g=1;pv=2*seq(1,2,by=0.1)-0.1
  alphav=alphasoybean2016OLS;tv=tsoybean2016}
  else if (ij==8){C4=-100;p0=-5;c=-2;g=1;pv=2*seq(1,2,by=0.1)-0.1
  alphav=alphasoybean2016SMA;tv=tsoybean2016}
  F=matrix(nrow=length(pv),ncol=length(alphav));Feq=F
  par(mfrow = c(2, 2))
  for (i in 1:length(pv)){
    for (j in 1:length(alphav)){
      p=pv[i]
      alpha=alphav[j]
      t=tv[j]
      F[i,j]=(g*(alpha - 1)*(tanh((g*(C4 + t/(alpha - 1))*(alpha - 1))/2) + (alpha*g - g + 2*c*p + 2*c*p0)/(g*(alpha - 1))))/(2*p + 2*p0)
      Feq[i,j]=c+g*(alpha-1)/(p+p0) 
    }
    fC=F[i,]
    cvLf[ij,i]=sd(exp(fC),na.rm =TRUE)/mean(exp(fC),na.rm =TRUE)
  }
  ff=F[4,]
  ffeq=Feq[4,]
  leafratio[ij,]=c(ff,ffeq)#leaf biomass fraction
  
}

pvone=2*seq(1,2,by=0.1)-0.5;pvtwo=2*seq(1,2,by=0.1)-0.1
CVLeafratio=c(cvLf[1,],cvLf[2,],cvLf[3,],cvLf[4,],cvLf[5,],cvLf[6,],cvLf[7,],cvLf[8,])
ppv=c(rep(pvone,6),pvtwo,pvtwo);NUM=length(pvone)
class=c(rep("OLS corn 2015",NUM),rep("SMA corn 2015",NUM),rep("OLS soybean 2015",NUM),rep("SMA soybean 2015",NUM),
        rep("OLS corn 2016",NUM),rep("SMA corn 2016",NUM),rep("OLS soybean 2016",NUM),rep("SMA soybean 2016",NUM))
df=data.frame(CVLeafratio=CVLeafratio,ppv=ppv,class=class)
CVsimulation=ggplot(data = df,aes(x=ppv,y=CVLeafratio,group=class,color=class))+geom_point(size=3)+geom_smooth(method="loess",span=1)+labs(title="CV",colour="",x="Plant competition",y="CVLF")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = 'none', 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(1,4,1),limits=c(1,4))+
  scale_y_continuous(breaks=seq(0,1.5,0.5),limits=c(0,1.5))


######detrend analyses
deCVLT=matrix(nrow=8,ncol=length(pv));CVLTcheck=matrix(nrow=8,ncol=length(pv)-3)
for (i in 1:8){
  print(adf.test(cvLf[i,])) #test whether it is necessary to further detrending
  deCVLT[i,]=c(NA,NA,NA,diff(cvLf[i,],differences=3))
  CVLTcheck[i,]=diff(cvLf[i,],differences=3)
  print(adf.test(CVLTcheck[i,]))
}
detrendCVLT=c(deCVLT[1,],deCVLT[2,],deCVLT[3,],deCVLT[4,],deCVLT[5,],deCVLT[6,],deCVLT[7,],deCVLT[8,])
df=data.frame(CVLeafratio=detrendCVLT,ppv=ppv,class=class)
detrendCVsim=ggplot(data = df,aes(x=ppv,y=CVLeafratio,group=class,color=class))+geom_point(size=3)+geom_smooth(method="loess",span=1)+labs(title="detrended CV",colour="",x="Plant competition",y="CVLF")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.3,0.7), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.5,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(2,4,1),limits=c(2,4))+
  scale_y_continuous(breaks=seq(0,1.5,0.5),limits=c(0,1.5))

###################
Leafratio=c(leafratio[1,],leafratio[2,])
time=c(tcorn2015,tcorn2015)*30
class=c(rep("OLS",5),rep("OLS EQ",5),rep("SMA",5),rep("SMA EQ",5))
df=data.frame(Leafratio=Leafratio,time=time,class=class)
Dyncorn2015=ggplot(data = df,aes(x=time,y=exp(Leafratio),group=class,color=class))+geom_point(size=3)+geom_line(size=1)+labs(title="Corn 2015",colour="",x="Plant ontogeny",y="Leaf mass ratio")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.8,0.3), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(60,160,25),limits=c(60,160))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-1.2),10^(-0.4))
  )




