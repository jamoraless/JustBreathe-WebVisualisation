# Course - CS 424 Visualization Analytics - Spring'19
# Project developed by Sai Krishnan Thiruvarpu Neelakantan - sthiru5@uic.edu
# http://sthiru5.people.uic.edu

# This project analyses the Air Quality data for different states and counties across the US from 1980-2018.  

#included libraries 

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(leaflet)
library(scales)
library(shinycssloaders)

#read all the data files and combine it as a single data

temp = list.files(pattern="*.csv")
Data <- lapply(temp, read.csv)
Datafinal <- do.call(rbind, Data)
years<-c(2018:1980)

# Create a Shiny dashboard with a sidebar for navigation purposes across the page

ui <- dashboardPage(
	skin = 'red',
    dashboardHeader(title = "Air Quality in the US"),
    dashboardSidebar(
    sidebarMenu(
    	selectInput("year", "Choose a year:",years,selected = 2010),
    	htmlOutput("choose_state"),
    	htmlOutput("choose_county"),
    	menuItem("AQI data for the year", tabName = "yeardata"),
    	menuItem("Over the years (1980-2018)", tabName = "overtheyeardata"),
    	menuItem("County Map", tabName = "countymap"),
    	menuItem("Compare different counties", tabName = "comparecounties"),
    	menuItem("Information", tabName = "information")    	
    )
    ),
    dashboardBody(
    	#custom css
    	tags$head( 
    		tags$style(
    			HTML(" #compare_state_option,#compare_year_option ,.compare-county-wrapper { display:flex; margin-bottom:-10px;}  
    				#compare_state_option > div, #compare_year_option > div, .compare-county-wrapper > div > div {padding-right: 15px;}
    				.shiny-output-error:before,.shiny-output-error{visibility: hidden !important;}
    				.compare-class > .tabbable > .nav-tabs {margin-bottom: 20px !important;} 
    				.box.box-solid.box-primary {border: 1px solid #dd4b39 !important;} 
    				.box.box-solid.box-primary>.box-header { background: #dd4b39 !important; background-color: #dd4b39 !important; }
    				.sidebar-menu>li {font-size:17px;}")
    	)
    ),	

    tabItems(
    tabItem(
        tabName = "yeardata",
        div(textOutput("dayswithaqi"),style="font-size:22px;font-weight:bold;margin-bottom:20px;"),
		  fluidRow(
		    column(6,
		    	box(title = "Categorized by Days", solidHeader = TRUE, status = "primary", width = 12,
		  				tabsetPanel(
		    				tabPanel("Pie Chart", withSpinner(plotOutput("piechart",height = 500))),
		    				tabPanel("Bar Chart", withSpinner(plotOutput("barchart",height = 500))),
		    				tabPanel("Table", withSpinner(dataTableOutput("table",height = 500)))

		  					)
		  			)	

		    ),
		    column(6,
		    	box(title = "Categorized by Pollutants", solidHeader = TRUE, status = "primary", width = 12,
		  				tabsetPanel(
		    				tabPanel("Pie Chart", withSpinner(plotOutput("piechartpollutant",height = 500))),
		    				tabPanel("Bar Chart", withSpinner(plotOutput("barchartpollutant",height = 500))),
		    				tabPanel("Table", withSpinner(dataTableOutput("tablepollutant",height = 500)))
		  					)
		  			)	

		    )
		    )
      ),
    tabItem(
    	tabName = "overtheyeardata",
        fluidRow(
        	column(12,
		    	  tabsetPanel(
		    				tabPanel("Change in AQI trend", withSpinner(plotOutput("linechart",height = 500))),
		    				tabPanel("Change in Pollutants", withSpinner(plotOutput("linechartpollutants",height = 500))),
		    				tabPanel("Table", withSpinner(dataTableOutput("tablepercentage",height = 500)))
		  			)	
		  		  )
        )
    ),
    tabItem(
        tabName = "comparecounties",
        htmlOutput("compare_year_option"),
        htmlOutput("compare_state_option"),
        div(class="compare-county-wrapper",
        htmlOutput("compare_county_option_1"),
        htmlOutput("compare_county_option_2"),
        htmlOutput("compare_county_option_3")),
        fluidRow(

        	column(12,
        		  div(class='compare-class',	
		    	  tabsetPanel(
		    	  	tabPanel('Change in AQI',

		    	  	tabsetPanel(
		    				tabPanel("90th Percentile", fluidRow(column(6,plotOutput("compare90th",height= 500)),column(6,plotOutput("comparebar90th",height= 500)))),
		    				tabPanel("Max AQI", fluidRow(column(6,plotOutput("comparemaxAqi",height= 500)),column(6,plotOutput("comparebarmaxAqi",height= 500)))),
		    				tabPanel("Median AQI", fluidRow(column(6,plotOutput("comparemedianAqi",height= 500)),column(6,plotOutput("comparebarmedianAqi",height= 500))))

		  			)	

		    	  	),
		    	  	tabPanel('Change in Pollutants',
		    	  		tabsetPanel(
		    				tabPanel("CO", fluidRow(column(6,plotOutput("compareCO",height= 500)),column(6,plotOutput("comparebarCO",height= 500)))),
		    				tabPanel("NO2", fluidRow(column(6,plotOutput("compareNO2",height= 500)),column(6,plotOutput("comparebarNO2",height= 500)))),
		    				tabPanel("Ozone", fluidRow(column(6,plotOutput("compareOzone",height= 500)),column(6,plotOutput("comparebarOzone",height= 500)))),
		    				tabPanel("SO2", fluidRow(column(6,plotOutput("compareSO2",height= 500)),column(6,plotOutput("comparebarSO2",height= 500)))),
		    				tabPanel("PM2.5", fluidRow(column(6,plotOutput("comparePM2.5",height= 500)),column(6,plotOutput("comparebarPM2.5",height= 500)))),
		    				tabPanel("PM10", fluidRow(column(6,plotOutput("comparePM10",height= 500)),column(6,plotOutput("comparebarPM10",height= 500))))

		  			)
		    	  	),
		    	  	tabPanel('Change in Days',
		    	  		tabsetPanel(
		    				tabPanel("Good", fluidRow(column(6,plotOutput("comparegood",height= 500)),column(6,plotOutput("comparebargood",height= 500)))),
		    				tabPanel("Moderate", fluidRow(column(6,plotOutput("comparemoderate",height= 500)),column(6,plotOutput("comparebarmoderate",height= 500)))),
		    				tabPanel("Sensitive for groups", fluidRow(column(6,plotOutput("comparesensitive",height= 500)),column(6,plotOutput("comparebarsensitive",height= 500)))),
		    				tabPanel("Unhealthy", fluidRow(column(6,plotOutput("compareunhealthy",height= 500)),column(6,plotOutput("comparebarunhealthy",height= 500)))),
		    				tabPanel("Very Unhealthy", fluidRow(column(6,plotOutput("comparevery",height= 500)),column(6,plotOutput("comparebarvery",height= 500)))),
		    				tabPanel("Hazardous", fluidRow(column(6,plotOutput("comparehazardous",height= 500)),column(6,plotOutput("comparebarhazardous",height= 500))))

		  			)
		    	  	)
        		)
        		)	
		    )

		    )

        ),
		    tabItem(
       		tabName = "countymap",
       		fluidRow(
        	column(9,
		    	    withSpinner(leafletOutput("leaf",height=500))
		  		  )
        		)

		    ),
		    tabItem(
       		tabName = "information",
        		div(htmlOutput("info"),style="font-size:22px;font-weight:bold;margin-bottom:20px;")

		    )	
		  )
      )
)	



server <- function(input, output) { 

# change default font size
theme_set(theme_light(base_size = 18))

#read the sites data for the getting the latitue/longitudes value

leafletdata <- read.csv("leaflet/aqs_sites.csv")
leafletdata <- leafletdata[!(leafletdata$Latitude == 0),]
leafletdata <- subset(leafletdata, select = c(Latitude, Longitude, State.Name, County.Name))

# selected year data retrieved from Datafinal variable 
selectedyeardata <- reactive({subset(Datafinal, Datafinal$Year == input$year)})	
# data filtered to compare for selected counties
finaldatacompare <- reactive({subset(Datafinal, (Datafinal$State == input$compare_state_1 & Datafinal$County == input$compare_county_1) | (Datafinal$State == input$compare_state_2 & Datafinal$County == input$compare_county_2) | (Datafinal$State == input$compare_state_3 & Datafinal$County == input$compare_county_3) )})
finaldatacompareyear <- reactive({subset(Datafinal, ( Datafinal$Year == input$compare_year_1 & Datafinal$State == input$compare_state_1 & Datafinal$County == input$compare_county_1) | ( Datafinal$Year == input$compare_year_2 & Datafinal$State == input$compare_state_2 & Datafinal$County == input$compare_county_2) | ( Datafinal$Year == input$compare_year_3 & Datafinal$State == input$compare_state_3 & Datafinal$County == input$compare_county_3) )})

# choose state for the current chosen year

output$choose_state = renderUI({
	selectedyeardata <- selectedyeardata()
	states = selectedyeardata$State
	selectInput(inputId = "state_sel", 
	 label = "Choose a state: ", 
	 choices = unique(states), 
	 selected = 'Illinois')
 
 })

# choose county for the current chosen year

output$choose_county = renderUI({
	selectedyeardata <- selectedyeardata()
	counties = subset(selectedyeardata$County, selectedyeardata$Year == input$year & selectedyeardata$State == input$state_sel)
	selectInput(inputId = "county_sel", 
	 label = "Choose a county: ", 
	 choices = unique(counties), 
	 selected = 'Cook')
 
 })
 
# create year,state and county dropdown to compare the data

output$compare_year_option = renderUI({

cyears<-c(2018:1980)
list(
	selectInput(inputId = "compare_year_1", 
		 label = "Year 1: ", 
		 choices = cyears),
	selectInput(inputId = "compare_year_2", 
		 label = "Year 2: ", 
		 choices = cyears),
	selectInput(inputId = "compare_year_3", 
		 label = "Year 3: ", 
		 choices = cyears)
	)

})

output$compare_state_option = renderUI({
	states = Datafinal$State
	unique_states = unique(states) 
	list(
	selectInput(inputId = "compare_state_1", 
		 label = "State 1: ", 
		 choices = unique(states),
		 selected = 'Illinois'),
	selectInput(inputId = "compare_state_2", 
		 label = "State 2: ", 
		 choices = unique(states),
		 selected = 'California'),
	selectInput(inputId = "compare_state_3", 
		 label = "State 3: ", 
		 choices = unique(states),
		 selected = 'Florida')
	)
})

output$compare_county_option_1 = renderUI({
	
	counties_1 = subset(Datafinal$County, Datafinal$State == input$compare_state_1)
	
		selectInput(inputId = "compare_county_1", 
			 label = "County 1: ", 
			 choices = unique(counties_1))
	
})

output$compare_county_option_2 = renderUI({
	
	counties_2 = subset(Datafinal$County, Datafinal$State == input$compare_state_2)
	
		selectInput(inputId = "compare_county_2", 
			 label = "County 2: ", 
			 choices = unique(counties_2))
	
})

output$compare_county_option_3 = renderUI({
	counties_3 = subset(Datafinal$County, Datafinal$State == input$compare_state_3)
	
		selectInput(inputId = "compare_county_3", 
			 label = "County 3: ", 
			 choices = unique(counties_3))
	
	
})	

#prints the days with AQI for the selected year
output$dayswithaqi <- renderText({
	selectedyeardata <- selectedyeardata()
	data <- subset(selectedyeardata, selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel)
	total <- data$Days.with.AQI
	paste0("Total days with AQI (Air Quality Index) - ",total)
	})

#Information dashboard
output$info <- renderText({
	text1 = paste0("Course - CS 424 Visualization Analytics - Spring'19")
	text2 = paste0("Project developed by Sai Krishnan Thiruvarpu Neelakantan - sthiru5@uic.edu")
	text3 = paste0("Libraries used - shiny, shinydashboard, ggplot2, lubridate, DT, grid ,leaflet, scales, shinycssloaders")
	text4 = paste0("Data source - https://aqs.epa.gov/aqsweb/airdata/download_files.html")

	HTML(paste(text1, text2, text3, text4, sep = '<br/><br/>'))
	})

# pie chart showing percentage of days for the selected year
output$piechart <- renderPlot({
	selectedyeardata <- selectedyeardata()
	data <- subset(selectedyeardata, selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel)
	pie = data.frame("Days" = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),"Count" = c(data$Good.Days,data$Moderate.Days,data$Unhealthy.for.Sensitive.Groups.Days,data$Unhealthy.Days,data$Very.Unhealthy.Days,data$Hazardous.Days))
    ggplot(pie[which(pie$Count>0),], aes(x=1, y=Count, fill=Days)) + geom_bar(width = 1, stat = "identity") + coord_polar("y") + 
    geom_text(aes(label = paste0(round((Count/sum(Count))*100,digits = 2), "%")), position = position_stack(vjust = 0.5)) + 
    labs(x = NULL, y = NULL, fill = NULL) + 
    scale_fill_brewer(palette="Blues")  + theme_classic() + theme(legend.title=element_text(size=15),legend.text=element_text(size=13),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#000000"))

    })

# bar chart showing values(categorized by days) for the selected year
output$barchart <- renderPlot({
	selectedyeardata <- selectedyeardata()
	data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
	pie = data.frame("Days" = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),"Count" = c(data$Good.Days,data$Moderate.Days,data$Unhealthy.for.Sensitive.Groups.Days,data$Unhealthy.Days,data$Very.Unhealthy.Days,data$Hazardous.Days))
    ggplot(data=pie[which(pie$Count>0),], aes(x=Days, y=Count, fill=Days)) + geom_bar(stat="identity") +
    scale_fill_brewer(palette="Blues") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
            axis.text.y = element_text(size = 10),
            panel.background = element_rect(fill = "white", color = "white"), 
            panel.grid.major = element_line(size = 0.25, linetype = "solid", color = "lightgray"),
            plot.title = element_text(size = 20))
    })

# Table showing values(categorized by days) for the selected year
output$table <- DT::renderDataTable(
    DT::datatable({ 
    selectedyeardata <- selectedyeardata()
	data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
	pie = data.frame("Days" = c("Good","Moderate","Unhealthy for sensitive","Unhealthy","Very Unhealthy","Hazardous"),"Count" = c(data$Good.Days,data$Moderate.Days,data$Unhealthy.for.Sensitive.Groups.Days,data$Unhealthy.Days,data$Very.Unhealthy.Days,data$Hazardous.Days))	
    pie
  }, 
  options = list(searching = TRUE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
  ), rownames = FALSE 
    ))

# pie chart showing percentage of pollutants for the selected year
output$piechartpollutant <- renderPlot({
	selectedyeardata <- selectedyeardata()
	data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
	pollutants = data.frame("Pollutant" = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),"Count" = c(data$Days.CO,data$Days.NO2,data$Days.Ozone,data$Days.SO2,data$Days.PM2.5,data$Days.PM10))
    ggplot(pollutants[which(pollutants$Count>0),], aes(x="", y=Count, fill=Pollutant)) + geom_bar(width = 1, stat = "identity") + coord_polar("y") + 
    geom_text(aes(label = paste0(round((Count/sum(Count))*100,digits = 2), "%")), position = position_stack(vjust = 0.5)) + 
    labs(x = NULL, y = NULL, fill = NULL) + 
    scale_fill_brewer(palette="Green") + theme_classic() + theme(legend.title=element_text(size=15),legend.text=element_text(size=13),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#000000"))
    })

# bar chart showing values(categorized by pollutants) for the selected year
output$barchartpollutant <- renderPlot({
	selectedyeardata <- selectedyeardata()
	data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
	pollutants = data.frame("Pollutant" = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),"Count" = c(data$Days.CO,data$Days.NO2,data$Days.Ozone,data$Days.SO2,data$Days.PM2.5,data$Days.PM10))
    ggplot(data=pollutants[which(pollutants$Count>0),], aes(x=Pollutant, y=Count, fill=Pollutant)) +geom_bar(stat="identity") +
    scale_fill_brewer(palette="Green") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
            axis.text.y = element_text(size = 10),
            panel.background = element_rect(fill = "white", color = "white"), 
            panel.grid.major = element_line(size = 0.25, linetype = "solid", color = "lightgray"),
            plot.title = element_text(size = 20))
    })

# Table showing values(categorized by pollutants) for the selected year
output$tablepollutant <- DT::renderDataTable(
    DT::datatable({ 
    selectedyeardata <- selectedyeardata()
	data <- selectedyeardata[ which(selectedyeardata$State==input$state_sel & selectedyeardata$County ==input$county_sel), ]
	pollutants = data.frame("Pollutant" = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),"Count" = c(data$Days.CO,data$Days.NO2,data$Days.Ozone,data$Days.SO2,data$Days.PM2.5,data$Days.PM10))	
    pollutants
  }, 
  options = list(searching = TRUE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
  ), rownames = FALSE 
    ))

#line chart to show the change in Median AQI, 90th Percentile AQI, Max AQI over the years for the selected county
output$linechart <- renderPlot({
	linestate = input$state_sel
	linecounty = input$county_sel
	linedata <- subset(Datafinal, Datafinal$State == linestate & Datafinal$County == linecounty)
	ggplot(data=linedata, aes(x=Year)) +
	geom_line(aes(y = Median.AQI,color="Median AQI")) + geom_point(aes(y = Median.AQI)) + 
	geom_line(aes(y = X90th.Percentile.AQI,color="90th Percentile AQI")) + geom_point(aes(y = X90th.Percentile.AQI)) +
	geom_line(aes(y = Max.AQI,color="Max AQI")) + geom_point(aes(y = Max.AQI)) +
	theme(legend.title = element_blank()) +
	ylab(label="Values")
    })

#line chart to show the change in Pollutants value over the years for the selected county
output$linechartpollutants <- renderPlot({
	linestate = input$state_sel
	linecounty = input$county_sel
	linedata <- subset(Datafinal, Datafinal$State == linestate & Datafinal$County == linecounty)
	ggplot(data=linedata, aes(x=Year)) +
	geom_line(aes(y = (Days.CO/Days.with.AQI)*100,color="CO")) + geom_point(aes(y = (Days.CO/Days.with.AQI)*100)) +
	geom_line(aes(y = (Days.NO2/Days.with.AQI)*100,color="NO2")) + geom_point(aes(y = (Days.NO2/Days.with.AQI)*100)) +
	geom_line(aes(y = (Days.Ozone/Days.with.AQI)*100,color="Ozone")) + geom_point(aes(y = (Days.Ozone/Days.with.AQI)*100)) +
	geom_line(aes(y = (Days.SO2/Days.with.AQI)*100,color="SO2")) + geom_point(aes(y = (Days.SO2/Days.with.AQI)*100)) +
	geom_line(aes(y = (Days.PM2.5/Days.with.AQI)*100,color="PM2.5")) + geom_point(aes(y = (Days.PM2.5/Days.with.AQI)*100)) +
	geom_line(aes(y = (Days.PM10/Days.with.AQI)*100,color="PM10")) + geom_point(aes(y = (Days.PM10/Days.with.AQI)*100)) +
	theme(legend.title = element_blank()) +
	ylab(label="Percentage")
    })

#Table to show the percentage change in Pollutants value over the years for the selected county
output$tablepercentage <- DT::renderDataTable(
    DT::datatable({ 
    linestate = input$state_sel
	linecounty = input$county_sel
	linedata <- subset(Datafinal, Datafinal$State == linestate & Datafinal$County == linecounty)
	linedata$CO <- round((linedata$Days.CO/linedata$Days.with.AQI)*100,digits=2)
	linedata$NO2 <- round((linedata$Days.NO2/linedata$Days.with.AQI)*100,digits=2)
	linedata$Ozone <- round((linedata$Days.Ozone/linedata$Days.with.AQI)*100,digits=2)
	linedata$SO2 <- round((linedata$Days.SO2/linedata$Days.with.AQI)*100,digits=2)
	linedata$PM2.5 <- round((linedata$Days.PM2.5/linedata$Days.with.AQI)*100,digits=2)
	linedata$PM10 <- round((linedata$Days.PM10/linedata$Days.with.AQI)*100,digits=2)

	final <- linedata[,c(3,4,20,21,22,23,24,25)]
  }, 
  options = list(searching = TRUE, pageLength = 10, lengthChange = FALSE, order = list(list(0, 'desc'))
  ), rownames = FALSE 
    ))

# Leaflet to plot the selected state,county

output$leaf <- renderLeaflet({

	dataleaf <- subset(leafletdata, leafletdata$State.Name == input$state_sel & leafletdata$County.Name == input$county_sel)
    map <- leaflet()
    map <- addTiles(map)
    # map <- setView(map, lng = dataleaf$Longitude[1], lat = dataleaf$Latitude[1], zoom=5)
    map <- addMarkers(map, lng = dataleaf$Longitude[1], lat = dataleaf$Latitude[1], label = paste0(input$county_sel,', ',input$state_sel),labelOptions = labelOptions(noHide = T, textOnly = TRUE), popup = paste0(input$county_sel,', ',input$state_sel))
    map
})

#line chart to compare change in 90th Percentile AQI value for the selected three counties over the years 1980-2018 
output$compare90th <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=X90th.Percentile.AQI,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Values")
    })

#line chart to compare change in Max AQI value for the selected three counties over the years 1980-2018
output$comparemaxAqi <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=Max.AQI,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Values")
    })

#line chart to compare change in Median AQI value for the selected three counties over the years 1980-2018
output$comparemedianAqi <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=Median.AQI,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Values")
    })

#line chart to compare change in CO percent for the selected three counties over the years 1980-2018
output$compareCO <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=(Days.CO/Days.with.AQI)*100,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Percentage")
    })

#line chart to compare change in NO2 percent for the selected three counties over the years 1980-2018
output$compareNO2 <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=(Days.NO2/Days.with.AQI)*100,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Percentage")
    })

#line chart to compare change in Ozone percent for the selected three counties over the years 1980-2018
output$compareOzone <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=(Days.Ozone/Days.with.AQI)*100,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Percentage")
    })

#line chart to compare change in SO2 percent for the selected three counties over the years 1980-2018
output$compareSO2 <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=(Days.SO2/Days.with.AQI)*100,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Percentage")
    })

#line chart to compare change in PM2.5 percent for the selected three counties over the years 1980-2018
output$comparePM2.5 <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=(Days.PM2.5/Days.with.AQI)*100,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Percentage")
    })

#line chart to compare change in PM10 percent for the selected three counties over the years 1980-2018
output$comparePM10 <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=(Days.PM10/Days.with.AQI)*100,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Percentage")
    })

#line chart to compare change in Good days percent for the selected three counties over the years 1980-2018
output$comparegood <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=(Good.Days/Days.with.AQI)*100,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Percentage")
    })

#line chart to compare change in Moderate days percent for the selected three counties over the years 1980-2018
output$comparemoderate <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=(Moderate.Days/Days.with.AQI)*100,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Percentage")
    })

#line chart to compare change in Sensitive for group days percent for the selected three counties over the years 1980-2018
output$comparesensitive <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=(Unhealthy.for.Sensitive.Groups.Days/Days.with.AQI)*100,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Percentage")
    })

#line chart to compare change in Unhealthy days percent for the selected three counties over the years 1980-2018
output$compareunhealthy <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=(Unhealthy.Days/Days.with.AQI)*100,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Percentage")
    })

#line chart to compare change in very unhealthy days percent for the selected three counties over the years 1980-2018
output$comparevery <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=(Very.Unhealthy.Days/Days.with.AQI)*100,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Percentage")
    })

#line chart to compare change in hazardous days percent for the selected three counties over the years 1980-2018
output$comparehazardous <- renderPlot({
	finaldata <- finaldatacompare()
	ggplot(data=finaldata, aes(x=Year,y=(Hazardous.Days/Days.with.AQI)*100,group=interaction(State,County),color=interaction(State,County))) + 
	geom_line() + geom_point() + 
	theme(legend.title = element_blank()) +
	ylab(label="Percentage")
    })

#Bar chart to compare change in 90th Percentile AQI value for three counties for the selected year
output$comparebar90th <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=X90th.Percentile.AQI, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

#Bar chart to compare change in Max AQI value for three counties for the selected year
output$comparebarmaxAqi <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=Max.AQI, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

#Bar chart to compare change in Median AQI value for three counties for the selected year
output$comparebarmedianAqi <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=Median.AQI, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

#Bar chart to compare change in CO value for three counties for the selected year
output$comparebarCO <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=Days.CO, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

#Bar chart to compare change in NO2 value for three counties for the selected year
output$comparebarNO2 <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=Days.NO2, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

#Bar chart to compare change in Ozone value for three counties for the selected year
output$comparebarOzone <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=Days.Ozone, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

#Bar chart to compare change in SO2 value for three counties for the selected year
output$comparebarSO2 <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=Days.SO2, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

#Bar chart to compare change in PM2.5 value for three counties for the selected year
output$comparebarPM2.5 <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=Days.PM2.5, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

#Bar chart to compare change in PM10 value for three counties for the selected year
output$comparebarPM10 <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=Days.PM10, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

#Bar chart to compare change in Good Days value for three counties for the selected year
output$comparebargood <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=Good.Days, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

#Bar chart to compare change in Moderate Days value for three counties for the selected year
output$comparebarmoderate <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=Moderate.Days, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

#Bar chart to compare change in Sensitive for group Days value for three counties for the selected year
output$comparebarsensitive <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=Unhealthy.for.Sensitive.Groups.Days, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

#Bar chart to compare change in Unhealthy Days value for three counties for the selected year
output$comparebarunhealthy <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=Unhealthy.Days, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

#Bar chart to compare change in Very Unhealthy Days value for three counties for the selected year
output$comparebarvery <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=Very.Unhealthy.Days, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

#Bar chart to compare change in Hazardous Days value for three counties for the selected year
output$comparebarhazardous <- renderPlot({
	finaldata <- finaldatacompareyear()
    ggplot(data=finaldata, aes(x=County, y=Hazardous.Days, fill=interaction(State,County,Year),group=interaction(State,County,Year))) + geom_bar(stat="identity",width=0.6) +
	theme(legend.title = element_blank()) +
	ylab(label="Value")
	})

}

# This runs the project 
shinyApp(ui = ui, server = server)
