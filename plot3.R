#This function is used to answer
#Of the four types of sources indicated by the type 
#(point, nonpoint, onroad, nonroad) variable, which of these four sources have 
#seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen 
#increases in emissions from 1999–2008? Use the ggplot2 plotting system to 
#make a plot answer this question.
# Usage: 
#       plot3()
plot3 <- function(){
        
        #library need to load ggplot
        library(ggplot2)
        
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
        
        #only gather baltimore data
        bltmore <- subset(NEI, fips == "24510")
        
        #subset data for types point,nonpoint,on-road,non-road
        # can also use aggregate, sqldf to get the total emission
        pt <- subset(bltmore, type=="POINT")
        npt <- subset(bltmore, type=="NONPOINT")
        rd <- subset(bltmore, type=="ON-ROAD")
        nrd <- subset(bltmore, type="NON-ROAD")
        
        #sum the total emission per year for each type 
        ptpm <- with(pt, tapply(pt$Emissions,pt$year,sum))
        nptpm <- with(npt, tapply(npt$Emissions,npt$year,sum))
        rdpm <- with(rd, tapply(rd$Emissions,rd$year,sum))
        nrdpm <- with(nrd, tapply(nrd$Emissions,nrd$year,sum))
        
        #construct data frame with all emissions per year for each type
        df <- data.frame(emission=as.numeric(ptpm),year=as.numeric(names(ptpm)),type=rep(c("POINT"),each=4))
        df <- rbind(df,data.frame(emission=as.numeric(nptpm),year=as.numeric(names(nptpm)),type=rep(c("NONPOINT"),each=4)))
        df <- rbind(df,data.frame(emission=as.numeric(rdpm),year=as.numeric(names(rdpm)),type=rep(c("ON-ROAD"),each=4)))
        df <- rbind(df,data.frame(emission=as.numeric(nrdpm),year=as.numeric(names(nrdpm)),type=rep(c("NON-ROAD"),each=4)))
               
        
        #plot the graph
        g <- ggplot(df, aes(x=year, y=emission)) + geom_line() + geom_point() 
        g <- g + facet_grid(. ~ type) + scale_y_continuous(breaks=df$emission)
        g <- g + scale_x_continuous(breaks=df$year) 
        g <- g + labs(x="Year", y=expression('Total PM'[2.5]*' emission in tons'), 
                      title=expression('Total PM'[2.5]*' emission per year for each type in Baltimore City'))
        
        #start to plot onto png device
        png("plot3.png", width=917, height=512) 
        
        print(g)        
       
        #turn off the device
        dev.off()
}