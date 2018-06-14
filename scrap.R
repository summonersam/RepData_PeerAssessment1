library(RColorBrewer)
fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  vi_pal = viridis_pal(option = 'D')(30)
  rdg <- brewer.pal("RdGy", n = 11)
  color.plotback = rdg[6]
  color.panelback = rdg[7]
  color.grid.major = rdg[6]
  color.grid.minor = rdg[6]
  color.axis.text = vi_pal[1]
  color.axis.title = vi_pal[18]
  color.title = vi_pal[1]
  color.lines = vi_pal[0]
  color.panelborder = vi_pal[18]
  color.grid.ticks = vi_pal[18]
  
  # Begin construction of chart
  
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    
    theme(panel.background=element_rect(fill=color.plotback, color=color.plotback)) +
    theme(plot.background=element_rect(fill=color.panelback, color=color.panelback)) +
    theme(panel.border=element_rect(color=color.panelborder, size = .5)) +
    
    # Format the grid
    
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_line(color=color.grid.minor,size=.25)) +
    theme(axis.ticks=element_line(color=color.grid.ticks,size=1)) +

    # Format the legend, but hide by default
    
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.plotback)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
}



library(ggplot2)
library(dplyr)
library(viridis)

dat <- read.csv("activity.csv")
vi_pal = viridis_pal(option = 'D')(30)
dat$intervalgroup = c(1:17568)

for(i in 1:17568)
{
  dat$intervalgroup[i] = dat$intervalgroup[i]%%288
}

intervalMeans = aggregate(steps ~ intervalgroup, data = dat, mean)
intervalData = as.data.frame(intervalMeans)
names(intervalData) = c("interval_group", "average_steps")

g <- ggplot(dat) + geom_line(aes(interval_group, average_steps), col = vi_pal[1]) +
        xlab("Interval Group") + ylab("Average Steps") +
        labs(title = "Interval Group Averages Over 61-Day Period") + fte_theme()



newdat = data.frame(dat)
for(i in 1:17568)
{
        if(is.na(newdat$steps[i]))
        {
                inde = i%%288 + 1
                newdat$steps[i] = intervalData$average_steps[inde]
        }
}

h <- ggplot(dat, aes(x = dat$intervalgroup, y = dat$steps)) + 
        geom_point(aes(colour = dat$steps), pch = 1) +
        xlab("Interval Group") + ylab("Steps") +
        labs(title = "Interval Group Steps Over 61-Day Period") +
        scale_y_log10(breaks = c(0,5,10,50,100,150,200,250,300,400,500,1000)) +
        scale_color_viridis() +
        fte_theme()
        
day = 0
standarddat$day = rep("day", 17568)
for(i in 1:61)
{
        if(i%%6==0 || i%%7==0)
        {
                for(k in 1:288)
                {
                        standarddat$day[day*288 + k] = "weekend"
                }
                day = day + 1
        }
        else
        {
                for(k in 1:288)
                {
                        standarddat$day[day*288 + k] = "weekday"
                }
                day = day + 1
        }
        
}
standarddat$day = as.factor(standarddat$day)

weekdaydata = subset(standarddat, day == "weekday")
weekenddata = subset(standarddat, day == "weekend")


##library(cowplot)

f <- ggplot(standarddat) + 
        geom_line(aes(intervalgroup, steps, col = day)) +
        xlab("Interval Group") + ylab("Steps") +
        labs(title = "Interval Group Steps Over 61-Day Period") +
        scale_y_log10(breaks = c(0,5,10,50,100,250,500,1000)) +
        facet_grid(day~.)
        fte_theme()
        
##v <- ggplot(weekenddata) + 
 ##       geom_line(aes(intervalgroup, steps, col = "red")) +
 ##       xlab("Interval Group") + ylab("Steps") +
 ##       labs(title = "Interval Group Steps Over 61-Day Period") +
 ##       scale_y_log10(breaks = c(0,5,10,50,100,250,500,1000)) +
 ##       fte_theme()

##plot_grid(f, v, labels=c("weekdays", "weekends"), nrow=2, align="v")



## imp_data <- transform(data, steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))






