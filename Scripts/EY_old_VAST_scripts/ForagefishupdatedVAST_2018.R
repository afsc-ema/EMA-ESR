#https://github.com/nwfsc-assess/geostatistical_delta-GLMM/blob/master/examples/Example--simple.R

devtools::install_github("james-thorson/VAST")
devtools::install_github("james-thorson/TMB")
#https://github.com/kaskr/TMB_contrib_R/tree/cdf5e4d3a9347924d5d6857b149cdaf8a2c880ad

devtools::install_github("james-thorson/VAST") 
devtools::install_github("james-thorson/FishData")
library("TMB", lib.loc="C:/~/R-3.4.4/library")
library("VAST", lib.loc="C:/~/R-3.4.4/library")

setwd("C:/Users/Alien/Documents/VAST EBS fish one time series")
temp<-read.csv("ForagefishNEBS.csv") #Data with 2017 and sst
temp<-read.csv("ForagefishSEBS.csv") #Data with 2017 and sst
temp<-read.csv("ForagefishJT.csv") #Data with 2017 and sst

print(temp)
dim(temp)
head(temp)
print(temp)
Sci=temp[,1]
year   = temp[,2] #Need KG catch data
vessel  = temp[,3]
AreaSwept  = temp[,4]
Lat = temp[,5]
Lon  	= temp[,6]
LAT=temp[,7]
LON=temp[,8]
Wt=temp[,9]
	LME=temp[,10]
	Station	=temp[,11]
	Temp_t=temp[,12]
	Lat_e=temp[,14]
	Lon_e=temp[,15]
	t_e=temp[,16]
	Cov_ep=temp[,17]

#3 SETTINGS
  #We use the latest version for CPP code
	Version = get_latest_version( package="VAST" )

	aggdata<-aggregate(Wt~year, data=temp, FUN=min)
  print(aggdata)  
  aggdata<-aggregate(Wt~year, FUN=length) #2005, 2008, 2013 with 100% encounter rates
  print(aggdata)

#3.1 Spatial Settings
Method = c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km = 25
n_x = 50   # Specify number of stations (a.k.a. "knots")

  #3.2 Model settings
  FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1) 
  RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) 
  OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
  ObsModel_ez = c(1,1)  
  #EBS 23
  #NEBS Obsmodel=c(1,1)
  #SEBS 21 Obsmodel=c(2,1)
 
   #Decide on which post-hoc calculations to include in the input
  Options =  c("SD_site_density"=0, "SD_site_logdensity"=0, "Calculate_Range"=1, "Calculate_evenness"=0, "Calculate_effective_area"=1, "Calculate_Cov_SE"=0, 'Calculate_Synchrony'=0, 'Calculate_Coherence'=0, 'normalize_GMRF_in_CPP'=TRUE)
  
#3.3 Stratification for results  
#strata.limits <- data.frame(STRATA = "EGOA")
strata.limits <- data.frame(STRATA = "EBS")

#3.4 Derived objects
Data_Set=temp
Region = "Other"
#Species_set=c( "YOY_ATF","YOY_P cod","YOY_pollock","YOY_rockfish")
#Species_set=c( "J Chinook","J chum","J pink","J sockeye")
#Species_set=c( "Herring","Squid","Wolf eel")
Species_set=c("Forage fish")
Species_set=c("Pollock")

#Table with # stations where
library(pander)
pander::pandoc.table(summary(Sci))
#3.5 Save settings
  # This is where all runs will be located
DateFile = paste0(getwd(),'/VAST_NEBSPollock 093018v5/')
dir.create(DateFile)

# Save options for future records
Record = list("Version"=Version,"Method"=Method,"grid_size_km"=grid_size_km,"n_x"=n_x,"FieldConfig"=FieldConfig,"RhoConfig"=RhoConfig,"OverdispersionConfig"=OverdispersionConfig,"ObsModel"=ObsModel_ez,"Region"=Region,"Species_set"=Species_set,"strata.limits"=strata.limits)
save( Record, file=file.path(DateFile,"Record.RData"))
capture.output( Record, file=paste0(DateFile,"Record.txt"))

#4 PREPARE DATA
#4.1 Data-frame for catch-rate data
Data_Geostat = data.frame(spp = temp[, "Sci"], Year = temp[, "year"], Catch_KG = temp[, "Wt"],AreaSwept_km2= temp[, "AreaSwept"],  Vessel= temp[, "vessel"], Lat = temp[, "Lat"], Lon = temp[, "Lon"])# Rename years and keep track of correspondance (for computational speed, given that there's missing years)
     pander::pandoc.table( head(Data_Geostat), digits=3 )
     pander::pandoc.table( tail(Data_Geostat), digits=3 ) 
 
     #4.2 Extrapolation grid
if( Region == "Other" ){
  Extrapolation_List = make_extrapolation_info( Region=Region, strata.limits=strata.limits, observations_LL=Data_Geostat[,c('Lat','Lon')], maximum_distance_from_sample=22.5 )
}
     
#4.3	Derived objects for spatio-temporal estimation
     #builds spatial information, make_spatial_info builds a tagged list with all the spatial information needed for Data_Fn
     Spatial_List = make_spatial_info( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=DateFile, Save_Results=FALSE )

     # Add knots to Data_Geostat
     Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i )

#Figure 1. Spatial extent and location of knots
#Figure 2. Spatial distribution of catch rates
SpatialDeltaGLMM::Plot_data_and_knots(Extrapolation_List = Extrapolation_List,
                                      Spatial_List=Spatial_List,Data_Geostat=Data_Geostat,
                                      PlotDir=DateFile)

# 5 BUILD AND RUN MODEL
#5.1 Build model
TmbData = Data_Fn("Version"=Version,   "FieldConfig"=FieldConfig, "OverdispersionConfig"=OverdispersionConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel_ez, "c_i"=as.numeric(Data_Geostat[,'spp'])-1, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )
# Build the TMB object
TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=DateFile, "Version"=Version, "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_x, "Method"=Method)
Obj = TmbList[["Obj"]]
#Estimate fixed effects and predict random effects
#Next, we use a gradient-based nonlinear minimizer to identify maximum likelihood estimates for fixed-effects
Opt = TMBhelper::Optimize( obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=DateFile, bias.correct=TRUE, newtonsteps=1, bias.correct.control=list(sd=FALSE, split=NULL, nsplit=1, vars_to_correct="Index_cyl") )

#Bundle and save
Report = Obj$report()
Save = list("Opt"=Opt, "Report"=Report, "ParHat"=Obj$env$parList(Opt$par), "TmbData"=TmbData)
save(Save, file=paste0(DateFile,"Save.RData"))

#6 DIAGNOSTIC PLOTS

#6.1 Plot data
#It is always good practice to conduct exploratory analysis of data. 
plot_data(Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, Data_Geostat=Data_Geostat, PlotDir=DateFile )

 #6.2 Convergence
library(pander)
pander::pandoc.table( Opt$diagnostics[,c('Param','Lower','MLE','Upper','final_gradient')] ) 
Enc_prob = plot_encounter_diagnostic( Report=Report, Data_Geostat=Data_Geostat, DirName=DateFile)

#Diagnostics for positive catch rate
Q = plot_quantile_diagnostic( TmbData=TmbData, Report=Report, FileName_PP="Posterior_Predictive",
                              FileName_Phist="Posterior_Predictive-Histogram", 
                              FileName_QQ="Q-Q_plot", FileName_Qhist="Q-Q_hist", DateFile=DateFile) 


  #6.5 Diagnostics for plotting residuals on map
# Get region-specific settings for plots
MapDetails_List = make_map_info( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
# Decide which years to plot                                                   
Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))
 
 #6.6 Model selection
  #  To select among models, we recommend using the Akaike Information Criterions,
  #AIC, via Ot$AIC=2.399\times 10^5
  
  #7 MODEL OUTPUT
  #Pre-defined plots to visualize the results
  
  #7.1 Direction of "geometric anistropy"
  #Determine which direction has faster or slower decorrelation.
  plot_anisotropy( FileName=paste0(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )

#7.3 Density surface for each year
#We can visualize many types of output from the model. Here I only show predicted density, but other options are obtained via other integers passed to plot_set as described in ?plot_maps
Dens_xt = plot_maps(plot_set=c(3), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)

#Figure 9: Density maps for each year for arrowtooth ???ounder
Dens_DF = cbind( "Density"=as.vector(Dens_xt), "Year"=Year_Set[col(Dens_xt)], "E_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'E_km'], "N_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'N_km'] )

#7.4 Index of abundance plus/mibnus 1 standard error
# Index = SpatialDeltaGLMM::PlotIndex_Fn(DirName = DateFile, 
# TmbData = TmbData, Sdreport = Opt[["SD"]], Year_Set = Year_Set, 
# Years2Include = Years2Include, strata_names = "EBS",  mar = c(2, 2, 2, 1), oma = c(1, 2, 0, 0),cex=.5,plot_legend=TRUE,
# use_biascorr = TRUE,cex=1,category_names=c("Forage fish"))

  Index = plot_biomass_index( DirName=DateFile, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Years2Include=Years2Include, strata_names=strata.limits[,1], use_biascorr=TRUE, category_names=levels(Data_Geostat[,'spp']) )
  pander::pandoc.table( Index$Table[,c("Category","Year","Estimate_metric_tons","SD_mt")] ) 


  setwd("C:/Users/Alien/Documents/VAST EBS fish one time series")
  
  temp2 <- read.csv("climate2.csv")    
  yr=temp2[,1]
  IRI	=temp2[,2]
  ICI=temp2[,3]	
  BT=temp2[,4]
  ST=temp2[,5]
  SEBS_Temp=temp2[,6]
  NEBS_Temp=temp2[,7]
 SEBS_mean_database	=temp2[,8]
 SEBS_SD_database	=temp2[,9]
 EBS_mean_database=temp2[,10]	
 EBS_SE_database=temp2[,11]	
 SEBS_mean	=temp2[,12]
 SEBS_SD	=temp2[,13]
 NEBS_mean=temp2[,14]	
 NEBS_SD=temp2[,15]
 EBS_mean=temp2[,16]	
 EBS_SD=temp2[,17]
 
  print(temp2)
  library(car)
  library(ggplot2)
  library(grid)
  library(gridBase)
  library(gridExtra)
  
 iff(file="Figure 2.tiff", height=8.5, width=11, units='in', pointsize=20,compression="lzw", res=800) 
    pdf(file="Figure 2.pdf", height=6, width=8.5, paper='special')
  #pdf(file="Figure Abundance and BT.pdf", height=8.5, width=11, paper='special')   
  Data <- data.frame(BT, ST,NEBSForageFish, SEBSForageFish, ForageFish2)
  #Surface temperature MODELS
  model_NEBS<- lm(NEBSForageFish~ ST, data = Data)
  model_NEBS
  summary(model_NEBS) 

    model_SEBS<- lm(SEBSForageFish~ ST, data = Data)
  model_SEBS
  summary( model_SEBS) 

  p2<-ggplot(Data, aes(ST,NEBSForageFish))+ ggtitle("NEBS  ") + labs(title=(expression(paste("NEBS  "  , R^2,"=0.29, P=0.05"))))+ theme(plot.title = element_text(size=22))+ geom_point(size=2)+ geom_smooth(method="lm", se=TRUE, na.rm=TRUE, show.legend = FALSE)+ ylab(NULL)+xlab(NULL) +theme(axis.text.y= element_text(size=12, colour = "black"), axis.text.x=element_text(size=12, colour = "black"), axis.text=element_text(size=16))
  p3<-ggplot(Data, aes(ST,SEBSForageFish)) + ggtitle("SEBS  ")+ labs(title=(expression(paste("SEBS  "  ,R^2,"=0.45, P=0.01"))))+ theme(plot.title = element_text(size=22))+ geom_point(size=2) + geom_smooth(method="lm", se=TRUE, na.rm=TRUE, show.legend = FALSE)+ ylab(NULL)+xlab(NULL)+theme(axis.text.y= element_text(size=12, colour = "black"), axis.text.x=element_text(size=12, colour = "black"), axis.text=element_text(size=16))
 grid.arrange(p2, p3,bottom="Sea surface temperature (Celsius)",left="Forage fish (metric tonnes)",nrow = 1, ncol=2)
  dev.off()
  
  model_SEBS<- lm(SEBS_mean~ ST, data = temp2) #R^0.45
  model_SEBS
  summary(model_SEBS) 
  
  model_SEBS<- lm(SEBS_mean~ ST, weights=1/(SEBS_SD^2),data = temp2) #R^0.55
  model_SEBS
  summary(model_SEBS) 
  
  
  model_NEBS<- lm(NEBS_mean~ ST, data = temp2) #R^0.45
  model_NEBS
  summary(model_NEBS) 
  
  model_NEBS<- lm(NEBS_mean~ ST, weights=1/(NEBS_SD^2),data = temp2) #R^0.55
  model_NEBS
  summary(model_NEBS) 
  
  model_EBS<- lm(EBS_mean~ ST, weights=1/(EBS_SD^2),data = temp2) #R^0.55
  model_EBS
  summary(model_EBS) 
  
    tiff(file="Figure 1 Forage fish ST.tiff", height=8.5, width=11, units='in', pointsize=16,compression="lzw", res=800) 
  #pdf(file="Figure Abundance and BT.pdf", height=8.5, width=11, paper='special')   
  Data <- data.frame(BT, ST,SEBS_mean, SEBS_SD, NEBS_mean)
  p1<-ggplot(temp2, aes(ST,NEBS_mean,size=1/(NEBS_SD^2)))+geom_point() + geom_smooth(method="gam", show.legend = FALSE)+ ylab(NULL)+xlab(NULL) +  labs(title=(expression(paste("NEBS"))))+theme(axis.text.y= element_text(size=12), axis.text.x=element_text(size=12))
  p2<-ggplot(temp2, aes(ST,SEBS_mean,size=1/(SEBS_SD^2)))+ geom_point() + geom_smooth(method="gam", show.legend = FALSE)+ ylab(NULL)+xlab(NULL) +  labs(title=(expression(paste("SEBS: ",R^2,"=0.55*"))))+theme(axis.text.y= element_text(size=12), axis.text.x=element_text(size=12))
  #p3<-ggplot(temp2, aes(ST,EBS_mean,size=1/(EBS_SD^2)))+ geom_point() + geom_smooth(method="gam", show.legend = FALSE)+ ylab(NULL)+xlab(NULL) +  labs(title=(expression(paste("EBS: ",R^2,"=0.*"))))+theme(axis.text.y= element_text(size=12), axis.text.x=element_text(size=12))
  
  grid.arrange(p1,p2, bottom="Surface temperature (Celsius)",left="Estimated biomass (metric tonnes)",nrow = 1, ncol=2)
  dev.off()
  
