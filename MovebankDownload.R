### Movebank data download and movement data analysis demo ###
### Orr Spiegel Sept 2019 ###
### the script here covers;
### using move package to download data from movebank
### movement visualization using moveVis package


#### loading packages #######
require(move)# if not installed:install.packages("move"); #for downloading data. info at https://www.rdocumentation.org/packages/move/versions/3.2.0
require(mapproj);require(ggmap) #these packages are necessary to work with google maps
require(moveVis);#install.packages("moveVis","rlang"): #if not installed

#### key parameter values used later in the code ######



################### reading data from movebank ###################
browseURL("movebank.org", browser = 'C:/Program Files/internet explorer/iexplore.exe') #just opening movebank
browseURL("movebank.org", browser = 'C:/Program Files (x86)/Google/Chrome/Application/chrome.exe') #just opening movebank

#movebank user credit for logging in
MB.LoginObject=movebankLogin(username='DemoUser',password='EasyPassword1'); 
# Unshared.credits <- movebankLogin() # specially usefull when sharing scripts with colleagues

#lets look for a vulture study in movebank#
searchMovebankStudies(x="vulture", login=MB.LoginObject) # we will get a list of ~40 studies from movebank, for our demo we will use number 35

StudyName= "Long-range adult movements of 3 vulture species (data from Spiegel et al. 2015)"#i chose this study of mine for this demonstration, but you can choose something else
#StudyName=2919708#until movebank fix the code i can work on this study.

#so lets get the metadata of this study
MetaDataThisStudy=getMovebankStudy(study=StudyName,login=MB.LoginObject)
print(MetaDataThisStudy)#what did the resaerchers wrote about this study?

# if we want to know whats included in the metadata use: str(MetaDataThisStudy)

MetaDataThisStudy$id#the movebank id of this study

# and what animals are included in this study 
AnimalsIncludedThisStudy=getMovebankAnimals(study=StudyName,login=MB.LoginObject) #if its the first time you try to do so you will have so sign the permission agreement in the movebank.org itself 
#lets look on a shorter version of this table:
AnimalsIncludedThisStudy[c('tag_id','animalName','taxon_canonical_name','sex')]

#Now that we know the study, lets DOWNLOAD the data itself . we can call study by name, Movebank ID	(33643212)
#MoveStackData=getMovebankData(study=2919708, login=MB.LoginObject)
MoveStackData=getMovebankData(study=MetaDataThisStudy$id, login=MB.LoginObject)
#
#
#MoveStackData=getMovebankData(study=33643212, login=MB.LoginObject, includeExtraSensors=FALSE, deploymentAsIndividuals=TRUE,removeDuplicatedTimestamps=F,#animalName
#     attributes=c('ground_speed','heading','location_lat','location_long','timestamp','update_ts'))

#a more complicated version: MoveStackData=getMovebankData(study=33643212, login=MB.LoginObject
#MoveStackData=getMovebankData(study=33643212, login=MB.LoginObject, includeExtraSensors=FALSE, deploymentAsIndividuals=FALSE,removeDuplicatedTimestamps=TRUE)#animalName

# getMovebankData(study=33643212, animalName=AnimalsIncludedThisStudy$animalName, login=MB.LoginObject, 
#                 removeDuplicatedTimestamps=FALSE,
#                 includeExtraSensors=FALSE, deploymentAsIndividuals=FALSE,
#                 includeOutliers=FALSE)


# its possible to download only a specific animal or time frame etc
#its possible to read csv files 

##### the most basic handling the downloaded data ######################

#lets first look on the data, hat have we downoaded?
show(MoveStackData)#what class of object is it?
summary(MoveStackData)#what do we have here?
citations(MoveStackData)

MoveStackData@idData[c('individual_id','local_identifier','taxon_canonical_name','sex')]#interesting metadata

print(paste('the data set has;',n.indiv(MoveStackData),' each track contains on average', round(mean(n.locs(MoveStackData)),digits = 1), 'locations, range:' , sep=' '))
range(n.locs(MoveStackData))


#timelag check?
MoveStackData$timeLag <- unlist(lapply(timeLag(MoveStackData, units="mins"),  c, NA))
median(MoveStackData$timeLag,na.rm=T);range(MoveStackData$timeLag,na.rm=T)


#getDuplicatedTimestamps(MoveStackData)


# how to convert a data.frame to move
#move_data <- df2move(move_data, proj = crs("+init=epsg:4326"), x = "coords.x1", y = "coords.x2",
#                     time = "timestamps", track_id = "trackId")

##### some initial plots of the data ######################

#too heavy but works: plot(MoveStackData,  lwd=2, xlab="location_long", ylab="location_lat",col='blue',pch=5)
mapObj <- get_map(bbox(extent(min(MoveStackData$location_long), 
                              max(MoveStackData$location_long), 
                              min(MoveStackData$location_lat), 
                              max(MoveStackData$location_lat) )*1.1), source="google", zoom=6)
Map=ggmap(mapObj)+
  geom_path(data=as.data.frame(MoveStackData), #read the data as data.frame.
            aes(x=location_long, y=location_lat,colour = trackId), size=1,show.legend = T) +
  theme(legend.text= element_text(size=14))+ggtitle('All tags together, colors by ID');
print(Map);rm(Map)#
#scale_colour_gradient2(low = "blue",mid='purple',high = "red",midpoint = mean(as.numeric(DatasetOhadF$DateOnly)))+
  #ggtitle(paste("Indiv=",indv, ', name=',TagsMetaData$name[indv],"duration",TagsMetaData$TrackDurationDays[indv],'day',sep=' '))
  

##### MoveVis of vulture data ######################


#for saving time let us choose only 3 tags:
#for namibia data set ChosenIndividuals <- MoveStackData[[c('X1','X9','X12')]]
ChosenIndividuals <- MoveStackData[[c('X1','X2','X3','X4','X6','X7')]]
#ChosenIndividuals <- MoveStackData[[c('X3','X4','X6','X7')]]

ChosenIndividuals@idData[c('individual_id','local_identifier','taxon_canonical_name','sex')]#interesting metadata
#ChosenIndividuals- making them all in one year
ChosenIndividuals$timestamp=  
as.POSIXct(strftime(as.character(ChosenIndividuals$timestamp),format="%m-%d %H:%M:%S"), format="%m-%d %H:%M:%S",tz='UTC')



#an update map
mapObj2 <- get_map(bbox(extent(min(ChosenIndividuals$location_long), 
                              max(ChosenIndividuals$location_long), 
                              min(ChosenIndividuals$location_lat), 
                              max(ChosenIndividuals$location_lat) )*1.1), source="google", zoom=6)
Map2=ggmap(mapObj2)+
      geom_path(data=as.data.frame(ChosenIndividuals), #read the data as data.frame.
                                aes(x=location_long, y=location_lat,colour = trackId), size=1,show.legend = T) +
      theme(legend.text= element_text(size=14))+ggtitle('just selected tags, colors by ID');
print(Map2);rm(Map2)#



# align move_data to a uniform time scale
move_data <- align_move(m=ChosenIndividuals, res = 'min', digit = 'min', unit = "secs", spaceMethod = "greatcircle")# memory limitation can be an issue with medium-large datasets, reduce resolution if needed!

# view data interactively
view_spatial(m=move_data,render_as = "mapview",stroke=T,path_legend=T,time_labels=T)#render_as = "leaflet")

# avaialble default maps
get_maptypes()#this lists all optional background maps

# create spatial frames with a selected background map
MapService='osm'
MapType = "terrain"
frames <- frames_spatial(m=move_data, #path_colours = c("red", "green", "blue"),
                         map_service = MapService, map_type = MapType, alpha = 0.5)

# animate your frames
#animate_frames(frames, out_file = paste('Vultures',MapService,MapType,".gif",sep='_'), overwrite = T)#"Warning: The number of frames exceeds 800 and the GIF format is used. This format may not be suitable for animations with a high number of frames, since it causes large file sizes. Consider using a video file format instead."
animate_frames(frames, out_file = paste('Vultures',MapService,MapType,".mp4",sep='_'), overwrite = T)




#### more drafts ########
#N vultures by day?
MoveStackData@data$Date=as.Date(MoveStackData@data$timestamp)#adding a date colum
DaysWithData=as.data.frame(unique(MoveStackData@data$Date)); names(DaysWithData)='Date'
DaysWithData$NvulturesToday=DaysWithData$NamesListvulturesToday=NA
DaysWithData$NvulturesToday=sapply(1:dim(DaysWithData)[1],function(i){length(unique(MoveStackData@data$tag_id[which(MoveStackData@data$Date==DaysWithData$Date[i])]))});
DaysWithData$NamesListvulturesToday=sapply(1:dim(DaysWithData)[1],function(i){list(unique(MoveStackData@data$tag_id[which(MoveStackData@data$Date==DaysWithData$Date[i])]))});
ggplot(data=DaysWithData,aes(x=Date, y=NvulturesToday)) + geom_line() +ggtitle('number of active tags by date')

MaxDays=which(DaysWithData$NvulturesToday==max(DaysWithData$NvulturesToday))
print(paste('the days with most vultures, (N=',max(DaysWithData$NvulturesToday),'=), are' ))
range(DaysWithData$Date[MaxDays])



#adding names to tags from the metadata
sort(unique(MoveStackData$tag_id))
MoveStackData$taxon_canonical_name=MoveStackData$animalName=MoveStackData$sex=NA
for (bird in 1:length(unique(MoveStackData$tag_id)))#loop on all value of the tag_id in the main data
{
  ind=which(MoveStackData$tag_id==AnimalsIncludedThisStudy$tag_id[bird])
  MoveStackData$taxon_canonical_name[ind]=as.character(AnimalsIncludedThisStudy$taxon_canonical_name[bird])
  MoveStackData$animalName[ind]=AnimalsIncludedThisStudy$animalName[bird]
  MoveStackData$sex[ind]=as.character(AnimalsIncludedThisStudy$sex[bird])
}
MoveStackData$sex[ind]=as.factor( MoveStackData$sex[ind])
MoveStackData$taxon_canonical_name[ind]=as.factor( MoveStackData$taxon_canonical_name[ind])

