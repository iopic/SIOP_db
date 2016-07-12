#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(rsconnect)

library(shiny)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotflow)
library(scales)
library(randomcoloR)
library(ggthemes)

library(stringr)
library(tidyr)
library(directlabels)

###load helper functions
source("helper functions.R")


#set palette for 15 colors
#big_pal_15<-distinctColorPalette(15)

siop<-read.csv("C:\\Users\\Samantha\\Desktop\\sandbox\\siop_counts_all.csv",stringsAsFactors = FALSE)

siop$year_s<-siop$year
siop$year<-factor(siop$year)

siopText<-read.csv("C:\\Users\\Samantha\\Desktop\\sandbox\\siop_text.csv",stringsAsFactors = FALSE)

unique_sess<-unique(siop$typeS)




# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("SIOP Contributors over time: 2008-2016","HamSolland"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        ("") ,width=3,
        
        #select year (radio)
        selectInput("yearInput","Year",
                     choices=c(unique(siop$year_s),"All years"),
                               selected=unique(siop$year_s)[1]),
        
        #select session type (dropdown)
        uiOutput("sessionOutput"),
        
        #select letter
        selectInput("orgs", "Select Org", 
                    choices = sort(unique(siop$affiliation)), 
                    multiple = TRUE),
        

        #enter name of person (text)
     #   textInput("nameInput","Member Name",value="Steve Kozlowski")
     # ),
        selectInput("nameInput","Member Name",
                    choices=sort(unique(siop$name)),
                    selected="Eduardo Salas",
                    multiple=TRUE),
     
     tags$b("Enter up to 5 search terms in the boxes below to check out topic trends over time on the last tab."),
     tags$br(),
     ("First, select if search in abstract or title."),
     selectInput("text_typeInput","Text Type",
                 choices=c("title","abstract"),
                 selected=NULL,
                 multiple=FALSE),
     textInput("searchInput1",label=NULL),
     textInput("searchInput2",label=NULL),
     textInput("searchInput3",label=NULL),
     textInput("searchInput4",label=NULL),
     textInput("searchInput5",label=NULL)
     
      ),
      
      # Shows the plot of whatever
      mainPanel(
        tabsetPanel(
          tabPanel("Count - People",
                   plotOutput("topPlot",width="auto"),
                   plotOutput("personPlot",width="auto")),
          tabPanel("Count - Org by People",
                   plotOutput("topPlotOrg",width="auto"),
                   plotOutput("orgPlot",width="auto")),
          tabPanel("Count - Org by Present.",
                   plotOutput("topPlotOrg2",width="auto"),
                   plotOutput("orgPlot2",width="auto")),
          tabPanel("Word Search by Year",
                   plotOutput("textSearchPlot",width="auto"))
                     )
        

        
         
         
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output,session) {
   
  #feeds the session type to the input dropdown box
  output$sessionOutput<-renderUI({
    selectInput("sessionInput","Filter by Session Type (Optional)",
                sort(unique(siop$typeS)),
                selected=NULL,
                multiple=TRUE)
  }) 
  


  ####creates filter option
  sess_filter<-reactive({
    
    if(is.null(input$sessionInput)){
      unique_sess
    } else{
      unique_sess[unique_sess %in% input$sessionInput]
    }
    
  })
  
  ##create year selection
  YEAR<-reactive({
    logic<-ifelse(as.numeric(input$yearInput)!=9,FALSE,
                 TRUE)
    if(logic==TRUE|is.na(logic)){
      return(as.numeric(as.character((unique(siop$year)))))
    }else{
      return(as.numeric(input$yearInput))
    }
  })
  
  ##plot the top individual contributors for a give year
  output$topPlot<- renderPlot({
    
   # if (is.null(input$sessionInput)) {
   #   return(NULL)
   # } 

    top_10<-siop %>%
      filter(
        year %in% c(YEAR()),
        name!="",
        typeS %in% sess_filter())%>%
      group_by(name)%>%
      summarise(count=n()) %>%
      arrange(desc(count)) %>%
      ungroup() 
    
    top_10_names<-top_10$name[top_10$count>=top_10$count[10]]
    
    p2<-siop %>%
      filter(
        year %in% c(YEAR()),
        typeS %in% sess_filter())%>%
      group_by(name,typeS)%>%
      summarise(count=n()) %>%
      mutate(cum=cumsum(count)-.5*count)%>%
      ungroup() %>%
      group_by(name)%>%
      mutate(tot_count=sum(count))%>%
      ungroup()%>%
      arrange(desc(tot_count)) %>%
      filter(name %in% top_10_names)%>%
      mutate(name=factor(name)) %>%
      ggplot(aes(x=reorder(name,tot_count),y=count,fill=typeS))+
      geom_bar(stat='identity') +
      geom_text(aes(x=reorder(name,tot_count),y=cum,label=count,
                    fontface=3))+
      geom_text(aes(x=reorder(name,tot_count),y=tot_count,label=tot_count,
                    fontface=2),hjust=-.6)+
      coord_flip()+
      scale_fill_brewer(palette="Set3",drop=TRUE,
                        limits=levels(factor(siop$typeS)))+
      #scale_fill_manual(values=big_pal_15,
      #                  limits=levels(factor(siop$typeS)))+
      labs(
           y="# Presentations",
           title=paste(input$yearInput,"\nIndividual Top 10 Contributors (# Presentations), including ties"))+
      #scale_y_continuous(breaks=pretty_breaks())+
      theme(legend.position="right")+
      #guides(fill=guide_legend(ncol=1,
      #                         title="Sess\ntype"))+
      guides(fill=FALSE)+
      
      theme(axis.text.x=element_blank(),
            panel.background=element_blank(),
            legend.background=element_rect("black",fill=FALSE),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.ticks=element_blank())
      
      
      
    
    print(p2)
  })
  
  ##plot the top org contributors for a given year
  ##BY PEOPLE presenting
  output$topPlotOrg<- renderPlot({
    
    top_102<-siop %>%
      filter(
        year %in% c(YEAR()),
        typeS %in% sess_filter(),
        affiliation!="")%>%
      group_by(affiliation)%>%
      summarise(count=n()) %>%
      arrange(desc(count)) %>%
      ungroup() 
    
    top_102_names<-top_102$affiliation[top_102$count>=top_102$count[10]]
    
    p22<-siop %>%
      filter(
        year %in% c(YEAR()),
        typeS %in% sess_filter())%>%
      group_by(affiliation,typeS)%>%
      summarise(count=n()) %>%
      mutate(cum=cumsum(count)-.5*count)%>%
      ungroup() %>%
      group_by(affiliation)%>%
      mutate(tot_count=sum(count))%>%
      ungroup()%>%
      arrange(desc(tot_count)) %>%
      filter(affiliation %in% top_102_names)%>%
      mutate(affiliation=factor(affiliation)) %>%
      ggplot(aes(x=reorder(affiliation,tot_count),y=count,fill=typeS))+
      geom_bar(stat='identity') +
      geom_text(aes(x=reorder(affiliation,tot_count),y=cum,label=count,
                    fontface=3))+
      geom_text(aes(x=reorder(affiliation,tot_count),y=tot_count,label=tot_count,
                    fontface=2),hjust=-.6)+
      coord_flip()+
      scale_fill_brewer(palette="Set3",drop=TRUE,
                        limits=levels(factor(siop$typeS)))+
      #scale_fill_manual(values=big_pal_15,
      #                  limits=levels(factor(siop$typeS)))+
      labs(x="Top Contributor",
           y="# Presentations",
           title=paste(input$yearInput,"\nOrg. Top 10 Contributors (# People), including ties"))+
      #scale_y_continuous(breaks=pretty_breaks())+
      theme(legend.position="bottom")+
      #guides(fill=guide_legend(ncol=2,
      #                         title="Sess\ntype"))+
      guides(fill=FALSE)+
      theme(axis.text.x=element_blank(),
            panel.background=element_blank(),
            legend.background=element_rect("black",fill=FALSE),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.ticks=element_blank())
    
    print(p22)
  })
  
  ##plot the top org contributors for a given year
  ###by unique sessions
  output$topPlotOrg2<- renderPlot({
    
    
    
    top_102b<-siop %>%
      filter(
        year %in% c(YEAR()),
        typeS %in% sess_filter(),
        affiliation!="")%>%
      group_by(affiliation)%>%
      summarise(count=length(unique(session_id))) %>%
      arrange(desc(count)) %>%
      ungroup() 
    
    top_102b_names<-top_102b$affiliation[top_102b$count>=top_102b$count[10]]
    
    p22b<-siop %>%
      filter(
        year %in% c(YEAR()),
        typeS %in% sess_filter())%>%
      group_by(affiliation,typeS)%>%
      summarise(count=length(unique(session_id))) %>%
      mutate(cum=cumsum(count)-.5*count)%>%
      ungroup() %>%
      group_by(affiliation)%>%
      mutate(tot_count=sum(count))%>%
      ungroup()%>%
      arrange(desc(tot_count)) %>%
      filter(affiliation %in% top_102b_names)%>%
      mutate(affiliation=factor(affiliation)) %>%
      ggplot(aes(x=reorder(affiliation,tot_count),y=count,fill=typeS))+
      geom_bar(stat='identity') +
      geom_text(aes(x=reorder(affiliation,tot_count),y=cum,label=count,
                    fontface=3))+
      geom_text(aes(x=reorder(affiliation,tot_count),y=tot_count,label=tot_count,
                    fontface=2),hjust=-.6)+
      coord_flip()+
      scale_fill_brewer(palette="Set3",drop=TRUE,
                        limits=levels(factor(siop$typeS)))+
      #scale_fill_manual(values=big_pal_15,
      #                  limits=levels(factor(siop$typeS)))+
      labs(x="Top Contributor",
           y="# Presentations",
           title=paste(input$yearInput,"\nOrg. Top 10 Contributors (# Presentations), including ties"))+
      #scale_y_continuous(breaks=pretty_breaks())+
      theme(legend.position="bottom")+
      #guides(fill=guide_legend(ncol=2,
      #                         title="Sess\ntype"))+
      guides(fill=FALSE)+
      
      theme(axis.text.x=element_blank(),
            panel.background=element_blank(),
            legend.background=element_rect("black",fill=FALSE),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.ticks=element_blank())
    
    print(p22b)
  })
  
  #this combines all orgs together (i.e., for handling typos)
  ###by people
  output$orgPlot<- renderPlot({
    if (is.null(input$orgs)) {
      return(NULL)
    } 
    
    org_p<-siop %>%
      filter(affiliation %in% c(input$orgs),
             typeS %in% sess_filter()) %>%
      group_by(year,typeS)%>%
      summarise(count=n()) %>%
      mutate(cum=cumsum(count)-.5*count)%>%
      ungroup() %>%
      group_by(year)%>%
      mutate(tot_count=sum(count))%>%
      ungroup()%>%
      ggplot(aes(x=year,y=count,fill=typeS))+
      geom_bar(stat="identity")+
      geom_text(aes(x=year,y=cum,label=count,
                    fontface=3))+
      geom_text(aes(x=year,y=tot_count,label=tot_count,
                    fontface=2),vjust=-.8)+
      scale_fill_brewer(palette="Set3",drop=TRUE,
                        limits=levels(factor(siop$typeS)))+
      guides(fill=guide_legend(ncol=1,
                               title="Session type"))+
      #scale_x_discrete(labels=seq(2011,2015,1))+
      labs(title="Org Detail by # People")+
      theme(
            panel.background=element_blank(),
            legend.background=element_rect("black",fill=FALSE),
            legend.position="right",
            axis.title=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  
    
    print(org_p)
  })
  
  #this combines all orgs together (i.e., for handling typos)
  ###by unique sessions
  output$orgPlot2<- renderPlot({
    
  if (is.null(input$orgs)) {
      return(NULL)
        }    
    org_p<-siop %>%
      filter(affiliation %in% c(input$orgs),
             typeS %in% sess_filter()) %>%
      group_by(year,typeS)%>%
      summarise(count=length(unique(session_id))) %>%
      mutate(cum=cumsum(count)-.5*count)%>%
      ungroup() %>%
      group_by(year)%>%
      mutate(tot_count=sum(count))%>%
      ungroup()%>%
      ggplot(aes(x=year,y=count,fill=typeS))+
      geom_bar(stat="identity")+
      geom_text(aes(x=year,y=cum,label=count,
                    fontface=3))+
      geom_text(aes(x=year,y=tot_count,label=tot_count,
                    fontface=2),vjust=-.8)+
      scale_fill_brewer(palette="Set3",drop=TRUE,
                        limits=levels(factor(siop$typeS)))+
      guides(fill=guide_legend(ncol=1,
                               title="Session type"))+
      labs(title="Org Detail by # Presentations")+
      #scale_x_discrete(labels=seq(2011,2015,1))+
      theme(
        panel.background=element_blank(),
        legend.background=element_rect("black",fill=FALSE),
        legend.position="right",
        axis.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
    
    
    print(org_p)
  })
  
  #Individual person history
  output$personPlot<- renderPlot({
    
    check<-siop %>%
      filter(name %in% c(input$nameInput),
             typeS %in% sess_filter())
    
    #print(nrow(check))
    
    if (nrow(check)<1) {
      return(NULL)
    } 
    
    person_p<-check %>%
      group_by(year,typeS)%>%
      summarise(count=n()) %>%
      mutate(cum=cumsum(count)-.5*count)%>%
      ungroup() %>%
      group_by(year)%>%
      mutate(tot_count=sum(count))%>%
      ungroup()%>%
      ggplot(aes(x=year,y=count,fill=typeS))+
      geom_bar(stat="identity")+
      geom_text(aes(x=year,y=cum,label=count,
                    fontface=3))+
      geom_text(aes(x=year,y=tot_count,label=tot_count,
                    fontface=2),vjust=-.8)+
      scale_fill_brewer(palette="Set3",drop=TRUE,
                        limits=levels(factor(siop$typeS)))+
      guides(fill=guide_legend(ncol=1,
                               title="Session type"))+
      #scale_x_discrete(labels=seq(2011,2015,1))+
      labs(title=paste("Summary:",input$nameInput))+
      theme(
        panel.background=element_blank(),
        legend.background=element_rect("black",fill=FALSE),
        legend.position="right",
        axis.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
    
    
    print(person_p)
  })
  
  #Individual person history
  output$textSearchPlot<- renderPlot({
    
    terms<-c()
    for(i in c(input$searchInput1,
                      input$searchInput2,
                      input$searchInput3,
                      input$searchInput4,
                      input$searchInput5)){
      if(i==""){
        next
      }
      terms<-c(terms,i)
    }
    
    #terms<-terms[-1]
    
    text_type<-input$text_typeInput
    
    print(text_type)
    print(terms)
    
    if (length(terms)<1) {
      return(NULL)
    } 
    
    text_p<-run_search(terms,text_type,siopText)
    text_plot<-plot_data(text_p,text_type)

    print(text_plot)
  })

})

# Run the application 
shinyApp(ui = ui, server = server)

