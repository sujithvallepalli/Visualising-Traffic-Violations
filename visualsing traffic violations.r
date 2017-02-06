#####################################################################################
# Topic : Data Visualization in R                                                   #
# Packages Explored : ggplot2, plotly, azureMl, tm, wordcloud,RGoogleMaps			#
#####################################################################################

## Have fun running the code##

# removing all other environment variables if required
rm(list = ls())

# Import the raw dataset
# Change the path to the location in your pc where the file has been downloaded to
# Download the csv data file using below link
#https://catalog.data.gov/dataset/traffic-violations-56dda
Traffic_Violations <- read.csv("<File Path>/Traffic_Violations.csv")
#For effective visualizations we need our data to be as accurate as possible, so we have to remove redundant data and impute/delete the missing data.
#We observed missing values in our data and so we used omit function to remove all the missing values(less than 10% here) in the data
Tfv_data1=na.omit(Traffic_Violations)

#Now that we have our data ready, let us go ahead and make some graphical visualization on it.

#Pie-charts & bar graphs -- plotly
#First let us use the existing data in its simple and original form and apply some simple visualizations.
#Plotly is one among the amazing graphic packages which allows the plots and charts to be interactive. That is, we can get details 
#of a particular data point on chart/plot by hovering the mouse on it.

install.packages("plotly")
library(plotly)

#Subset the data into a data frame and naming the columns.

#Gender wise traffic violations using interactive Pie charts using plot_ly function.
pie1=as.data.frame(table(Tfv_data1$Gender))
colnames(pie1)<-c("Gender","Frequency")
plot_ly(data=pie1,values=pie1$Frequency,labels = pie1$Gender,type = "pie")%>%
  layout(title = "Traffic Violation by gender")

#Another similar pie chart with Race and traffic violations
pie2=as.data.frame(table(Tfv_data1$Race))
colnames(pie2)<-c("Race","Frequency")
plot_ly(data=pie2,values=pie2$Frequency,labels = pie2$Race,type = "pie")%>%
  layout(title = "Traffic Violation by Race")

#Now let us combine both the pie charts informations on a bar graph using similar structured data.
bar1=as.data.frame(table(Tfv_data1$Race,Tfv_data1$Gender))
colnames(bar1)<-c("Race","Gender","Frequency")

plot_ly(data=bar1, x = bar1$Race, y =bar1$Frequency, type = 'bar',color = bar1$Gender) %>%
  layout(title="Traffic Violation by Race and Gender")


#We have seen some plots and graphs using the existing data, now let us prepare more data from the existing ones
#to enrich our graphic learning.

#Extracting Weekday,Month, Hour and Year from exisitng Date and Time columns using "strptime","weekdays()","months()" function
as.character.Date(Tfv_data1$Date.Of.Stop)
Tfv_data1$Date<-strptime(Tfv_data1$Date.Of.Stop, format = '%m/%d/%Y')
Tfv_data1$weekday<-weekdays(Tfv_data1$Date)
Tfv_data1$Month<-months(Tfv_data1$Date)
Tfv_data1$Time<-strptime(Tfv_data1$Time.Of.Stop, format = '%H:%M:%S')
Tfv_data1$Hour<-format(Tfv_data1$Time,"%H")
Tfv_data1$Hour<-as.numeric(Tfv_data1$Hour)
Tfv_data1$Year2<-as.numeric(format(Tfv_data1$Date, "%Y"))

#Drop the Time,Date and Article columns as we do not need these in our analysis
Tfv_data1$Time<-NULL
Tfv_data1$Date=NULL
Tfv_data1$Article<-NULL

#We used above extracted data and plotted a simple interactive area plot using plotly function which shows hourly traffic violations
#area plot-plotly
area1=as.data.frame(table(Tfv_data1$Hour))
colnames(area1)<-c("Hour","Frequency")
plot_ly(data=area1,x = area1$Hour, y =area1$Frequency,fill="tonexty")%>%
  layout(title="Traffic Violation by Hour of Day")

##We used similar interactive area graph showing hourly violations on a daily basis using colors.
#We factored weekday and created sublevels
dailyViolations <- as.data.frame(table(Tfv_data1$weekday, Tfv_data1$Hour))
names(dailyViolations) <- c('Day', 'Hour', 'Frequency')
dailyViolations$Day<- factor(dailyViolations$Day, ordered = TRUE, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
plot_ly(data=dailyViolations, x = dailyViolations$Hour, y = dailyViolations$Frequency, type="area",color=dailyViolations$Day) %>%
  layout(title="Traffic Violation by Weekday and Hour")

#ggplot is another magnificient package for visualization, and called as grammar of graphics.
#We explored more on this traffic violations data using ggplot as shown below

install.packages("ggplot2")
library(ggplot2)

#area Plot which we had already drawn using plotly, let us show it in a different format using geom_tile and scale_fill_gradient function
#Here we also used theme funciton to format the title, axis labels and legend
##tile plot--ggplot
p=ggplot(data=dailyViolations, aes(x = dailyViolations$Hour, y = dailyViolations$Day)) + geom_tile(aes(fill = dailyViolations$Frequency))+ scale_fill_gradient(name = 'Number of Violtions', low = 'white', high = 'blue')+ggtitle("Tile Map: Traffic Violations on Weekdays every Hour")
p+ theme(plot.title=element_text(color="Brown",size=14,face="bold.italic"),axis.title.y =element_text(color="pink",size=10,face="bold.italic"),axis.title.x =element_text(color="green",size=10,face="bold.italic"),axis.text.y=element_text(colour="red",face="italic"),axis.text.x=element_text(colour="red",face="italic"),axis.ticks=element_line(colour="violet",size = 2))

#We have done plotly and ggplot independently, now let us combine them both and see. We will draw a plot using ggplot
#and then using ggplotly function, we will make it interactive.

#Here we compared Fatal and Non Fatal Violations over years.
fatal=table(Tfv_data1$Fatal,Tfv_data1$Year2)
#Since Fatal violations are very little in proportion compared to Non Fatal, let us make them row wise proportionate.
#That is we compare Fatal and Non Fatal violations over years respectively.
fatal_prop=prop.table(fatal,1)
fatal_prop=as.data.frame(fatal_prop*100)
names(fatal_prop)<-c("Fatal", "Year","Percentage")
#Using geom_bar function for bar graphs and ggplotly for making it interactive
bar3<-ggplot(data=fatal_prop,aes(x=fatal_prop$Year,y=fatal_prop$Percentage,fill=fatal_prop$Fatal))+geom_bar(stat = "identity",position = "dodge")+ggtitle("Fatal Traffic Violation over Years")
ggplotly(bar3)

#####################################################################################
#ggmaps
#We have seen graphics using bar plots, pie charts and tile chart. Now let us see the same graphics on Maps.
#We will use RgoogleMaps package which directly pulls the maps from Google API and then we can superimpose our data on the map and analyse

#Package called ggmap is used here to apply graphics on maps.


#install.packages("RgoogleMaps")
#install.packages("ggmap")
library(RgoogleMaps)
library(ggmap)

#Concentration related to time of the day

#Loading maps and its data with zoom level from Google API using geocode and get_map functions.
montgomery_county_gc <- geocode("montgomery county, MD")
montgomery_county_gc1 <- geocode("maryland state, MD")
montgomery_county_map <- get_map(location = c(montgomery_county_gc$lon, montgomery_county_gc$lat),zoom = 10)
montgomery_county_map1 <- get_map(location = c(montgomery_county_gc1$lon, montgomery_county_gc1$lat),zoom = 11)
#Montgomery County map extracted from Google API with zoom level 10
ggmap(montgomery_county_map)

#Using geom_point function similar to that of its usage in ggplot, let us superimpose our datapoints on to the map extracted from google.
#We used scale_color_gradient function to distinguish Hourly traffic violation in Montgomery County.

ggmap(montgomery_county_map)+ geom_point(data=Tfv_data1, aes(Tfv_data1$Longitude,Tfv_data1$Latitude, color=Tfv_data1$Hour))+scale_color_gradientn(colors=terrain.colors(12))+ggtitle("Traffic Violations every hour of the day in areas of Maryland")+labs(x = "Longitude", y = "Latitude")

#We also used stat_density2d function to plot and see where the density of violations are high. This is a 2d plot function which categorises density color by data points in that region.
ggmap(montgomery_county_map1) + stat_density2d(aes(x = Tfv_data1$Longitude, y = Tfv_data1$Latitude, fill = ..level..,alpha=..level..),size = 2, bins = 4, data = Tfv_data1, geom = "polygon")+ggtitle("Density Plot of Violations in Maryland region")+labs(x = "Longitude", y = "Latitude")

#In both these maps we used ggtitle function for title and labs function for labelling purposes

##WORD CLOUD##

#install.packages("tm")
#install.packages("wordcloud")
library(tm)
library(wordcloud)

#We have a field called description which has details of cause of Traffic Violations in Maryland.
#We will use this field and try study the frequent causes which led to Traffic Violations.

#Let's load the field description into a new variable called desc as a data frame.
desc=as.data.frame(Tfv_data1$Description)

#Since our data is huge and once we try to fit all the words in description field into rows,
#system demands more than 15Gb to crunch and process it. So we go ahead with random sample of the data.

#Let us take random sample of 10000 records, which is around 1.2% of the total data and take this data 
#for further processing
desc1=desc[sample(nrow(desc), 10000), ]
desc1=as.data.frame(desc1)

#For Text Mining, we need to form a corpus, which is the collection of documents on which we need to mine the text.
#In our case, we have only one document with number of records. So making it a Corpus.
#Let us also remove some unneccesasry words like numbers, punctuations, and standard English grammar which we are not interested to analyse.
corp=Corpus(DataframeSource(desc1))
a=tm_map(corp,removeNumbers)
a=tm_map(corp,removePunctuation)
a=tm_map(corp,removeWords,stopwords("english"))

#Now we have a corpus which is free from non standard words. Let us narrow our analysis to the words with 6 letters up to 15 letters.
#We now create a matrix which contains the words and its respective frequency and then convert it back to data frame.
tdm5=TermDocumentMatrix(a,control=list(wordLengths=c(6,15)))
#freq2=findFreqTerms(tdm5,lowfreq = 100)
tfreq= rowSums(as.matrix(tdm5))
tfreq1=subset(tfreq,tfreq>=150)
df1=data.frame(term=names(tfreq1),freq=tfreq1)

#Let us plot and see the number of words with frequency greater than 150 in a interactive bar graph using ggplot and plotly
b=ggplot(data=df1,aes(x=df1$term,y=df1$freq))+geom_bar(stat="identity",fill="orange")+xlab("Terms")+ylab("Count")+ggtitle("Frequency Count of Repeating Words")+coord_flip()
ggplotly(b)


#We have seen the list of words and its frequency on a graph, now let us see the same with 100 as minimum frequency using Word Cloud

#Sorting the frequency in decreasing order
wfreq=sort(tfreq,decreasing=T)

#preparing a wordcloud, with minimum frequency of 100 and applying colour and rotation percentage to 30% for good visual experience 
wordcloud(words=names(wfreq),freq=wfreq,min.freq = 100,random.order = F,rot.per = 0.3,colors = brewer.pal(8,"Dark2"))

##############Time series data Visualization########################

install.packages("xts")
library(xts)
#Change the datatype of date
Tfv_data1$Date<-strptime(Tfv_data1$Date.Of.Stop, format = '%m/%d/%Y')
datewise<-as.data.frame(table(Tfv_data1$Date.Of.Stop))
head(datewise)
datewise$date<-strptime(datewise$Var1, format = '%m/%d/%Y')
datewise<-xts(datewise,order.by = datewise$date)
install.packages("dygraphs")
library(dygraphs)
head(datewise)
datewise$date<-NULL
names(datewise)<-c("Date","No of Accidents")
dygraph(datewise, main = "Traffic Violations By Day") %>% 
  dyRangeSelector(dateWindow = c("2013-01-01", "2016-06-01")) %>%
  dySeries("No of Accidents",strokeWidth = 1, col = "green")

