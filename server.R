library(ggplot2)
library(data.table)

function(input, output) {
  
  dt <-read.csv("DengueCases2011to2015byState.csv")
  
  #Sum No of Cases by Week in a Year
  dtsumWeek <- aggregate(. ~ State+Year+Week, data=dt, FUN=sum)
  columns<- c("State","Year","Week","Total_Cases")
  
  
  # Display No Of Cases By State By Year
    output$data_table <- renderTable({
    dtsum <- aggregate(. ~ State+Year, data=dt, FUN=sum)
    columns<- c("State","Year","Total_Cases")
    dtDisplay <- dtsum[,columns]
    dtDisplay <- as.data.table(dtDisplay) 
    dyear <- as.character(input$datayear)
    
    if (dyear == "2011"){
     
      dtbyYear <- dtDisplay[dtDisplay$Year =="2011" & order("Total_Cases",decreasing=TRUE)]
      
     }
    else if (dyear == "2012"){
      
      dtbyYear <- dtDisplay[dtDisplay$Year =="2012" & order("Total_Cases",decreasing=TRUE)]
    } 
    else if (dyear == "2013"){
      dtbyYear <- dtDisplay[dtDisplay$Year =="2013" & order("Total_Cases",decreasing=TRUE)]
    }
    else if (dyear == "2014"){
      dtbyYear <- dtDisplay[dtDisplay$Year =="2014" & order("Total_Cases",decreasing=TRUE)]
    }
    else if (dyear == "2015"){
      dtbyYear <- dtDisplay[dtDisplay$Year =="2015" & order("Total_Cases",decreasing=TRUE)]
    }
    else{
      dtbyYear <- dtDisplay[order("Year") & order("Total_Cases",decreasing=TRUE)]
      
    }
    dtbyYear
  }, caption="No of Cases by State",
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL))
  
    
    # Plotting Task based on User Input
    
    output$data_plot <- renderPlot({
      dat <- as.data.table(dt)
      dgraf <- as.character(input$graf1)
      if (dgraf == "By Year"){ 
        dat.by.year <- dat[,sum(Total_Cases),by=Year]
        names(dat.by.year) <- c('Year','Total_Cases')
       # graf1 <- barplot(as.numeric(dat.by.year$Total_Cases),names.arg=dat.by.year$Year, main="Total Dengue Cases from 2011 to 2015", xlab="Year", ylab="Total Cases", ylim=c(0,800000),col="blue")
       # text(x = graf1, y = dat.by.year$Total_Cases, label = dat.by.year$Total_Cases, pos = 3, cex = 0.8, col = "red")
          
        ggplot(data=dat.by.year, aes(x=factor(Year), y=Total_Cases))+geom_bar(stat="identity",position="dodge")+labs(title="Total Dengue Cases from 2011 to 2015", x="Year",y="Total Cases")+theme_bw()
          
      }  
        
      else if(dgraf =="By State")
      {
        
      }
      else if (dgraf =="Trend By Week")
      {
        
      }
   })
    
}