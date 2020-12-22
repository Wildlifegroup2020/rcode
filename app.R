library(shiny)
library(highcharter)
library(countrycode)
library(shinydashboard)
library(dplyr)
library(openintro)
library(ggplot2)
library(readr)
library(tidyr)
library(ggthemes)
library(scatterpie)

# TODO - @Alex
# You guys should try to separate these data manipulations
# into their own files. Then export as finalized CSVs.
# Then in this file you can build your graphs, ui, and server variables.
# Ideally, this file is only "ui" and "server", and then saying to run it.

# Additionally, I know you want to add a slider for the date.
# You should move that slider to the "scatterpie" tab and away from the sidebar.

load("WildlifeStudy.Rda")
load("CamSum.Rda")

 CountNicheFreqWS <- function(WildlifeStudy, InputNiche) {
     b <- WildlifeStudy %>% group_by(WildlifeStudy$date3, WildlifeStudy$Area) 
     c <- sum(b$Indiv.[b$Niche == InputNiche],na.rm = TRUE)
     summarise(b,sum=sum(Indiv.[b$Niche == InputNiche],na.rm= TRUE))
 }
 
# PreyFreqWS <- CountNicheFreqWS(WildlifeStudy, "Prey")
# PredaFreqWS <- CountNicheFreqWS(WildlifeStudy, "Predator")
# MezoFreqWS <- CountNicheFreqWS(WildlifeStudy, "Mesocarnivore")
# HumanFreqWS <- CountNicheFreqWS(WildlifeStudy, "Human")
# DomesticFreqWS <- CountNicheFreqWS(WildlifeStudy, "Domestic")
# 
# names(PreyFreqWS)[3] <- "PreyFreq"
# names(PreyFreqWS)[2] <- "PreyArea"
# names(PreyFreqWS)[1] <- "PreyDate"
# 
# names(PredaFreqWS)[3] <- "PredatorFreq"
# names(PredaFreqWS)[2] <- "PredatorArea"
# names(PredaFreqWS)[1] <- "PredatorDate"
# 
# names(MezoFreqWS)[3] <- "MesoFreq"
# names(MezoFreqWS)[2] <- "MesoArea"
# names(MezoFreqWS)[1] <- "MesoDate"
# 
# names(HumanFreqWS)[3] <- "HumanFreq"
# names(HumanFreqWS)[2] <- "HumanArea"
# names(HumanFreqWS)[1] <- "HumanDate"
# 
# names(DomesticFreqWS)[3] <- "DomesticFreq"
# names(DomesticFreqWS)[2] <- "DomesticArea"
# names(DomesticFreqWS)[1] <- "DomesticDate"
# 
# #+ theme_solarized_2(light=FALSE)
# #use a time series plot instead of a scatter plot. Make x variable a date, then it will run as a time plot
# # gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
# 
# timegraphPred <- 
#     ggplot(PredaFreqWS,(aes(x=PredatorDate, y=PredatorFreq, group = PredatorArea, color = PredatorArea, na.rm = TRUE))) + 
#     geom_line (size = 2, alpha = 0.75) + labs (title = "Predator Niche Activity", x = "Date", y = "Number of Detections") + 
#     theme(axis.text.x = element_text(angle = 90)) + 
#     theme(plot.title = element_text(hjust = 0.5)) + 
#     geom_vline(xintercept = 13.3, color = "black") + 
#     theme(legend.position = "bottom")
# timegraphPred
# 
# #timegraphanimatePred <- timegraphPred + transition_reveal(PredatorDate) + view_follow(fixed_y = TRUE) + geom_point()
# #timegraphanimatePred
# timegraphMeso <- ggplot(MezoFreqWS,(aes(x=MesoDate, y=MesoFreq, group = MesoArea, color = MesoArea, na.rm = TRUE))) + 
#     geom_line (size = 2, alpha = 0.75) + labs (title = "Mesocarnivore Niche Activity", x = "Date", y = "Number of Detections") + 
#     theme(axis.text.x = element_text(angle = 90)) + theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept = 13.3, color = "black") + 
#     theme(legend.position = "bottom") + scale_color_colorblind()
# timegraphMeso
# 
# 
# timegraphPrey <- PreyFreqWS %>%  ggplot (aes(x= PreyDate, y= PreyFreq, group = PreyArea, color = PreyArea)) + 
#     geom_line (size = 2, alpha = 0.75) + 
#     labs (title = "Prey Niche Activity", x = "Date") + 
#     theme(axis.text.x = element_text(angle = 90)) + 
#     theme(plot.title = element_text(hjust = 0.5)) + 
#     geom_vline(xintercept = 13.3, color = "black") + 
#     theme(legend.position = "bottom") + 
#     scale_color_colorblind()
# timegraphPrey
# 
# timegraphDomestic <- DomesticFreqWS %>%  ggplot (aes(x= DomesticDate, y= DomesticFreq, group = DomesticArea, color = DomesticArea)) + 
#     geom_line (size = 2, alpha = 0.75) + 
#     labs (title = "Domestic Niche Activity", x = "Date", y = "Number of Detections") + theme(axis.text.x = element_text(angle = 90)) + 
#     theme(plot.title = element_text(hjust = 0.5)) + 
#     geom_vline(xintercept = 13.3, color = "black") + 
#     theme(legend.position = "bottom") + 
#     scale_color_colorblind()
# timegraphDomestic
# 
# timegraphHuman <- HumanFreqWS %>%  ggplot (aes(x= HumanDate, y= HumanFreq, group = HumanArea, color = HumanArea)) + 
#     geom_line (size = 2, alpha = 0.75) + 
#     labs (title = "Human Niche Activity", x = "Date", y = "Number of Detections") + 
#     theme(axis.text.x = element_text(angle = 90)) + theme(plot.title = element_text(hjust = 0.5)) + 
#     geom_vline(xintercept = 13.3, color = "black") + 
#     theme(legend.position = "bottom") + 
#     scale_color_colorblind()
# timegraphHuman
# 
# nicheToGraph = list()
# nicheToGraph[["Mesocarnivore"]] = timegraphMeso
# nicheToGraph[["Prey"]] = timegraphPrey
# nicheToGraph[["Domestic"]] = timegraphDomestic
# nicheToGraph[["Human"]] = timegraphHuman

ui <-
    dashboardPage(
        dashboardHeader(disable = TRUE),
        dashboardSidebar( 
            width = 0,
            sidebarMenu(
                
            )),
        dashboardBody(
            tabBox(
                title = 'Felidae Wildlife Activity Study',
                id = 'tabset1',
                width = 12,
                # tabPanel('Niche', 
                #          selectInput('niche','Select Niche', choices = c("Mesocarnivore", "Prey", "Domestic", "Human")),
                #          plotOutput('charts',height = '400px')),
                tabPanel('Scatterpie',
                                 selectInput('Month','Select Year for Study', choices = WildlifeStudy$date3, selected = 2017-10),
                         plotOutput('scatterpie', height='400')
                )
            )
        )
    )

server <- function(input, output, session) {
    output$charts <- renderPlot({
        nicheToGraph[[input$niche]]
    })
    
    output$scatterpie = renderPlot({
        ggplot() + 
            geom_scatterpie(
                aes(x=Longitude, y=Latitude, group=Camera, r=0.003), 
                data=subset(CamSum, Date3 == input$Month), 
                cols=c("HumanSum","DomesticSum","PredatorSum","MesoSum","PreySum"),
                color = "NA") + 
            coord_equal() + geom_hline(yintercept = 38.509, color = "black") + 
            geom_hline(yintercept = 38.5, color = "black") + 
            labs(title = "Niche Presence at Each Camera", x="Longitude", y="Latitude") + 
            guides(fill=guide_legend(title="Niche")) + 
            theme_clean() 
    })

    observeEvent(input$yearid, {
        updateTabItems(session,'tabset1','WorldSales')
    })
}

shinyApp(ui=ui, server=server)

