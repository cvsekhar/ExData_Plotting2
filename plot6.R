#This function is used to answer
#Compare emissions from motor vehicle sources in Baltimore City(fips == "24510")  
#with emissions from motor vehicle sources in Los Angeles County, California 
#(fips == "06037"). Which city has seen greater changes over time in motor 
#vehicle emissions?
# Usage: 
#       plot6()
plot6 <- function(){
        
        #filename for the zip file
        zipfileName <- "data.zip"
        
        #url to download the zip file
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        
        #check if files exists
        if(!file.exists("summarySCC_PM25.rds") && 
                   !file.exists("Source_Classification_Code.rds")){
                #download the file
                download.file(fileUrl,destfile=zipfileName, method="curl")
                
                #unzip the contents of zipfile
                unzip(zipfileName)
                
        }
        
        #read the data
        NEI <- readRDS("summarySCC_PM25.rds")        
        
        SCC <- readRDS("Source_Classification_Code.rds")
        
        #Get only rows for Motor Vehicle from Source_Classification_Code
        cc <- SCC[grep("*Motor*|*Vehicle*", SCC$EI.Sector,ignore.case=TRUE),]
        
        #baltimore subset data
        bltmore <- subset(NEI, fips == "24510")
        
        # Filter all the rows, include only motor vehicle  for baltimore
        bltmoremv <- bltmore[bltmore$SCC %in% cc[,1],]
        
        # Sum the emissions from all sources per year for baltimore        
        pm0 <- with(bltmoremv, tapply(bltmoremv$Emissions,bltmoremv$year,sum)) 
        
        #convert array to data frame (baltimore) 
        bltdf <- data.frame(emission=as.numeric(pm0),year=as.numeric(names(pm0)),city=rep(c("Baltimore"),each=4))
        
        #losangeles, ca subset data
        laca <- subset(NEI, fips == "06037")
        
        # Filter all the rows, include only motor vehicle  for losangeles
        lacamv <- laca[laca$SCC %in% cc[,1],]
        
        # Sum the emissions from all sources per year for losangeles, ca       
        pm1 <- with(lacamv, tapply(lacamv$Emissions,lacamv$year,sum)) 
        
        #convert array to data frame (losangeles,ca) 
        ladf <- data.frame(emission=as.numeric(pm1),year=as.numeric(names(pm1)),city=rep(c("Los Angeles"),each=4))
        
        #calculate the percentage changes of total emission over time
        #using quantmod package and the function Delt
        library(quantmod) 
        bltdf$pctEmissions <- Delt(bltdf$emission)[,1] * 100
        
        ladf$pctEmissions <- Delt(ladf$emission)[,1] * 100
        
        #remove NA
        bltdf[1,c(4)] <- 0
        ladf[1,c(4)] <- 0
        
        #load library to plot 
        library(ggplot2)
        
        #plot the graph
        g <- ggplot(bltdf,aes(x=year,y=pctEmissions,color="Baltimore")) + geom_line()
        g <- g + geom_line(data=ladf,aes(color="Los Angeles"))
        g <- g + scale_x_continuous(breaks=bltdf$year)
        g <-  g + scale_y_continuous(breaks=c(ladf$pctEmissions,bltdf$pctEmissions))
        g <- g + labs(color="City", xlab="Year",
                      y=expression('Percentage change of PM'[2.5]*' emissions over years'),
                      title=expression('Change of motor vehicle PM'[2.5]*' emissions for Baltimore and LosAngeles over years'))
        
        #start to plot onto png device
        png("plot6.png", width=917, height=512)  
        
        print(g)
        
        #turn off the device
        dev.off()
}