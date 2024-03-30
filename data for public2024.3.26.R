#2011corn
total=c(total2011[[1]],total2011[[4]],total2011[[7]],total2011[[10]],total2011[[13]],total2011[[16]])
leaf=c(leaf2011[[1]],leaf2011[[4]],leaf2011[[7]],leaf2011[[10]],leaf2011[[13]],leaf2011[[16]])
density=c(density2011[[1]],density2011[[4]],density2011[[7]],density2011[[10]],density2011[[13]],density2011[[16]])
density=density/2.25

timepoints=c(rep(1,length(total2011[[1]])),rep(2,length(total2011[[4]])),rep(3,length(total2011[[7]])),
             rep(4,length(total2011[[10]])),rep(5,length(total2011[[13]])),rep(6,length(total2011[[16]])))
species=rep("corn",length(total))
plantedyear=rep("2011",length(total))
plantedtype=rep("monoculture",length(total))

leaf[leaf==0]=NA;total[total==0]=NA;density[density==0]=NA;
df=data.frame(leaf=leaf,total=total,density=density,timepoints=timepoints,species=species,plantedyear=plantedyear,plantedtype=plantedtype)
dfcorn=df[complete.cases(df),]#去掉包含NA的行

#2011wheat
total=c(total2011[[2]],total2011[[5]],total2011[[8]],total2011[[11]],total2011[[14]])
leaf=c(leaf2011[[2]],leaf2011[[5]],leaf2011[[8]],leaf2011[[11]],leaf2011[[14]])
density=c(density2011[[2]],density2011[[5]],density2011[[8]],density2011[[11]],density2011[[14]])
density=density/2.25

timepoints=c(rep(1,length(total2011[[2]])),rep(2,length(total2011[[5]])),rep(3,length(total2011[[8]])),
             rep(4,length(total2011[[11]])),rep(5,length(total2011[[14]])))
species=rep("wheat",length(total))
plantedyear=rep("2011",length(total))
plantedtype=rep("monoculture",length(total))

leaf[leaf==0]=NA;total[total==0]=NA;density[density==0]=NA;
df=data.frame(leaf=leaf,total=total,density=density,timepoints=timepoints,species=species,plantedyear=plantedyear,plantedtype=plantedtype)
dfwheat=df[complete.cases(df),]#去掉包含NA的行


#2011flax
total=c(total2011[[3]],total2011[[6]],total2011[[9]],total2011[[12]],total2011[[15]],total2011[[17]])
leaf=c(leaf2011[[3]],leaf2011[[6]],leaf2011[[9]],leaf2011[[12]],leaf2011[[15]],leaf2011[[17]])
density=c(density2011[[3]],density2011[[6]],density2011[[9]],density2011[[12]],density2011[[15]],density2011[[17]])
density=density/2.25

timepoints=c(rep(1,length(total2011[[3]])),rep(2,length(total2011[[6]])),rep(3,length(total2011[[9]])),
             rep(4,length(total2011[[12]])),rep(5,length(total2011[[15]])),rep(6,length(total2011[[17]])))
species=rep("flax",length(total))
plantedyear=rep("2011",length(total))
plantedtype=rep("monoculture",length(total))

leaf[leaf==0]=NA;total[total==0]=NA;density[density==0]=NA;
df=data.frame(leaf=leaf,total=total,density=density,timepoints=timepoints,species=species,plantedyear=plantedyear,plantedtype=plantedtype)
dfflax=df[complete.cases(df),]#去掉包含NA的行

#################################################
#2015 corn or soybean
hddcorn1516=c(corn2015$hdd2,corn2015$hdd3,corn2015$hdd4,corn2015$hdd5)
leafcorn1516=c(corn2015$leaf2,corn2015$leaf3,corn2015$leaf4,corn2015$leaf5)
hddsoybean1516=c(soybean2015$hdd1,soybean2015$hdd2,soybean2015$hdd3,soybean2015$hdd4,soybean2015$hdd5)
leafsoybean1516=c(soybean2015$leaf1,soybean2015$leaf2,soybean2015$leaf3,soybean2015$leaf4,soybean2015$leaf5)
HDDcorn=log(hddcorn1516);        HDDsoybean=log(hddsoybean1516)
Totalcorn1516=exp(0.74*HDDcorn-0.29);Totalsoybean1516=exp(HDDsoybean-0.7) 
IDcorn1516=c(corn2015$ID2,corn2015$ID3,corn2015$ID4,corn2015$ID5)
IDsoybean1516=c(soybean2015$ID1,soybean2015$ID2,soybean2015$ID3,soybean2015$ID4,soybean2015$ID5)
densitycorn1516=c(corn2015$density2,corn2015$density3,corn2015$density4,corn2015$density5)
densitysoybean1516=c(soybean2015$density1,soybean2015$density2,soybean2015$density3,soybean2015$density4,soybean2015$density5)
densitycorn1516=densitycorn1516/2.25
densitysoybean1516=densitysoybean1516/2.25

leaf=leafcorn1516;total=Totalcorn1516;density=densitycorn1516

timepoints=c(rep(1,length(corn2015$leaf2)),rep(2,length(corn2015$leaf3)),rep(3,length(corn2015$leaf4)),
             rep(4,length(corn2015$leaf5)))
species=rep("corn",length(total))
plantedyear=rep("2015",length(total))
plantedtype=rep("monoculture",length(total))

leaf[leaf==0]=NA;total[total==0]=NA;density[density==0]=NA;
df=data.frame(leaf=leaf,total=total,density=density,timepoints=timepoints,species=species,plantedyear=plantedyear,plantedtype=plantedtype)
dfcorn2015=df[complete.cases(df),]#去掉包含NA的行

leaf=leafsoybean1516;total=Totalsoybean1516;density=densitysoybean1516
timepoints=c(rep(1,length(soybean2015$leaf1)),rep(2,length(soybean2015$leaf2)),rep(3,length(soybean2015$leaf3)),
             rep(4,length(soybean2015$leaf4)),rep(5,length(soybean2015$leaf5)))
species=rep("soybean",length(total))
plantedyear=rep("2015",length(total))
plantedtype=rep("monoculture",length(total))
leaf[leaf==0]=NA;total[total==0]=NA;density[density==0]=NA;
df=data.frame(leaf=leaf,total=total,density=density,timepoints=timepoints,species=species,plantedyear=plantedyear,plantedtype=plantedtype)
dfsoybean2015=df[complete.cases(df),]#去掉包含NA的行
#######
#2016 corn or soybean
hddcorn1516=c(corn2016$hdd1,corn2016$hdd2,corn2016$hdd3,corn2016$hdd4,corn2016$hdd5)
leafcorn1516=c(corn2016$leaf1,corn2016$leaf2,corn2016$leaf3,corn2016$leaf4,corn2016$leaf5)
hddsoybean1516=c(soybean2016$hdd1,soybean2016$hdd2,soybean2016$hdd3,soybean2016$hdd4,soybean2016$hdd5)
leafsoybean1516=c(soybean2016$leaf1,soybean2016$leaf2,soybean2016$leaf3,soybean2016$leaf4,soybean2016$leaf5)
HDDcorn=log(hddcorn1516);        HDDsoybean=log(hddsoybean1516)
Totalcorn1516=exp(0.74*HDDcorn-0.29);Totalsoybean1516=exp(HDDsoybean-0.7) 
IDcorn1516=c(corn2016$ID1,corn2016$ID2,corn2016$ID3,corn2016$ID4,corn2016$ID5)
IDsoybean1516=c(soybean2016$ID1,soybean2016$ID2,soybean2016$ID3,soybean2016$ID4,soybean2016$ID5)
densitycorn1516=c(corn2016$density1,corn2016$density2,corn2016$density3,corn2016$density4,corn2016$density5)
densitysoybean1516=c(soybean2016$density1,soybean2016$density2,soybean2016$density3,soybean2016$density4,soybean2016$density5)
densitycorn1516=densitycorn1516/2.25
densitysoybean1516=densitysoybean1516/2.25


leaf=leafcorn1516;total=Totalcorn1516;density=densitycorn1516

timepoints=c(rep(1,length(corn2016$leaf1)),rep(2,length(corn2016$leaf2)),rep(3,length(corn2016$leaf3)),
             rep(4,length(corn2016$leaf4)),rep(5,length(corn2016$leaf5)))
species=rep("corn",length(total))
plantedyear=rep("2016",length(total))
plantedtype=rep("monoculture",length(total))

leaf[leaf==0]=NA;total[total==0]=NA;density[density==0]=NA;
df=data.frame(leaf=leaf,total=total,density=density,timepoints=timepoints,species=species,plantedyear=plantedyear,plantedtype=plantedtype)
dfcorn2016=df[complete.cases(df),]#去掉包含NA的行

leaf=leafsoybean1516;total=Totalsoybean1516;density=densitysoybean1516
timepoints=c(rep(1,length(soybean2016$leaf1)),rep(2,length(soybean2016$leaf2)),rep(3,length(soybean2016$leaf3)),
             rep(4,length(soybean2016$leaf4)),rep(5,length(soybean2016$leaf5)))
species=rep("soybean",length(total))
plantedyear=rep("2016",length(total))
plantedtype=rep("monoculture",length(total))
leaf[leaf==0]=NA;total[total==0]=NA;density[density==0]=NA;
df=data.frame(leaf=leaf,total=total,density=density,timepoints=timepoints,species=species,plantedyear=plantedyear,plantedtype=plantedtype)
dfsoybean2016=df[complete.cases(df),]#去掉包含NA的行

#############################2011mixture

#flaxwheat
Alltimepoint=list(F20111,F20112,F20113,F20114,F20115)
together3=together4=together5=together6=together7=together8=NA
for (i in 1:5){
  AT=Alltimepoint[[i]];AT[AT==0]=NA;
  #################################求变异系数
  stem=as.numeric(AT$stemflaxwheat); leaf=as.numeric(AT$leafflaxwheat)
  if (i==1){root=as.numeric(AT$rootflaxwheat); total=root+stem+leaf;
  }else{HDD=log(AT$dflaxwheat*AT$dflaxwheat*AT$heightflaxwheat)
  LT=TOT=0
  for (ii in 1:length(AT$IDflaxwheat2)){if (AT$IDflaxwheat2[ii]=="小麦"){TOT[ii]=exp(0.82*HDD[ii] -1.1)}else{TOT[ii]=exp(0.82*HDD[ii] -0.73)}}
  total=TOT
  }
  df=data.frame(leaf=leaf,total=total,densityflaxwheat=AT$densityflaxwheat,id1=AT$IDflaxwheat1,id2=AT$IDflaxwheat2)
  df=df[complete.cases(df),]#去掉包含NA的行
  together3=c(together3,df$leaf);together4=c(together4,df$total);together5=c(together5,rep(i,length(df$total)));together6=c(together6,df$densityflaxwheat);  
  together7=c(together7,df$id1);together8=c(together8,df$id2);
}
plantedyear=rep("2011",length(together3))
plantedtype=rep("mixture",length(together3))
df=data.frame(leaf=together3, total=together4,density=together6,timepoints=together5,species=together8,plantedyear=plantedyear,plantedtype=plantedtype)
dfflaxwheat=df[complete.cases(df),]

#cornwheat
Alltimepoint=list(F20111,F20112,F20113,F20114,F20115)
together3=together4=together5=together6=together7=together8=NA
for (i in 1:5){
  AT=Alltimepoint[[i]];AT[AT==0]=NA;
  #################################求变异系数
  stem=as.numeric(AT$stemcornwheat); leaf=as.numeric(AT$leafcornwheat)
  if (i==1){root=as.numeric(AT$rootcornwheat); total=root+stem+leaf;
  }else{HDD=log(AT$dcornwheat*AT$dcornwheat*AT$heightcornwheat)
  LT=TOT=0
  for (ii in 1:length(AT$IDcornwheat2)){if (AT$IDcornwheat2[ii]=="小麦"){TOT[ii]=exp(0.82*HDD[ii] -1.1)}else{TOT[ii]=exp(0.93*HDD[ii] -1.1)}}
  total=TOT
  }
  df=data.frame(leaf=leaf,total=total,densitycornwheat=AT$densitycornwheat,id1=AT$IDcornwheat1,id2=AT$IDcornwheat2)
  df=df[complete.cases(df),]#去掉包含NA的行
  together3=c(together3,df$leaf);together4=c(together4,df$total);together5=c(together5,rep(i,length(df$total)));together6=c(together6,df$densitycornwheat);  
  together7=c(together7,df$id1);together8=c(together8,df$id2);
}
plantedyear=rep("2011",length(together3))
plantedtype=rep("mixture",length(together3))
df=data.frame(leaf=together3, total=together4,density=together6,timepoints=together5,species=together8,plantedyear=plantedyear,plantedtype=plantedtype)
dfcornwheat=df[complete.cases(df),]

#flaxcorn
Alltimepoint=list(F20111,F20112,F20113,F20114,F20115)
together3=together4=together5=together6=together7=together8=NA
for (i in 1:5){
  AT=Alltimepoint[[i]];AT[AT==0]=NA;
  #################################求变异系数
  stem=as.numeric(AT$stemflaxcorn); leaf=as.numeric(AT$leafflaxcorn)
  if (i==1){root=as.numeric(AT$rootflaxcorn); total=root+stem+leaf;
  }else{HDD=log(AT$dflaxcorn*AT$dflaxcorn*AT$heightflaxcorn)
  LT=TOT=0
  for (ii in 1:length(AT$IDflaxcorn2)){if (AT$IDflaxcorn2[ii]=="胡麻"){TOT[ii]=exp(0.82*HDD[ii] -0.73)}else{TOT[ii]=exp(0.93*HDD[ii] -1.1)}}
  total=TOT
  }
  df=data.frame(leaf=leaf,total=total,densityflaxcorn=AT$densityflaxcorn,id1=AT$IDflaxcorn1,id2=AT$IDflaxcorn2)
  df=df[complete.cases(df),]#去掉包含NA的行
  together3=c(together3,df$leaf);together4=c(together4,df$total);together5=c(together5,rep(i,length(df$total)));together6=c(together6,df$densityflaxcorn);  
  together7=c(together7,df$id1);together8=c(together8,df$id2);
}
plantedyear=rep("2011",length(together3))
plantedtype=rep("mixture",length(together3))
df=data.frame(leaf=together3, total=together4,density=together6,timepoints=together5,species=together8,plantedyear=plantedyear,plantedtype=plantedtype)
dfflaxcorn=df[complete.cases(df),]


#write.csv(dfcorn,"PNAS上传corn2011monoculture.csv")
#write.csv(dfwheat,"PNAS上传wheat2011monoculture.csv")
#write.csv(dfflax,"PNAS上传flax2011monoculture.csv")
#write.csv(dfcorn2015,"PNAS上传corn2015monoculture.csv")
#write.csv(dfsoybean2015,"PNAS上传soybean2015monoculture.csv")
#write.csv(dfcorn2016,"PNAS上传corn2016monoculture.csv")
#write.csv(dfsoybean2016,"PNAS上传soybean2016monoculture.csv")

#write.csv(dfflaxwheat,"PNAS上传flaxwheat2011mixture.csv")
#write.csv(dfcornwheat,"PNAS上传cornwheat2011mixture.csv")
#write.csv(dfflaxcorn,"PNAS上传flaxcorn2011mixture.csv")
