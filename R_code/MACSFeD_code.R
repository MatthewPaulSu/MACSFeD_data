##load packages
library(readxl)
library(shiny)
library(shinyWidgets)
library(htmlwidgets)
library(bslib)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(tidyr)
library(naniar)
library(janitor)
library(stringr)
library(ggfittext)
library(openxlsx)
library(plotly)
library(htmltools)
library(data.table)
library(ggpubr)
library(DT)

df_swarm1 <- read.csv("https://raw.githubusercontent.com//MatthewPaulSu/MACSFeD_data/main/Data/Literature_database_Swarm.csv")
df_WBF1 <- read.csv("https://raw.githubusercontent.com//MatthewPaulSu/MACSFeD_data/main/Data/Literature_database_Wing_Beat_Frequency.csv")
df_MT1 <- read.csv("https://raw.githubusercontent.com//MatthewPaulSu/MACSFeD_data/main/Data/Literature_database_Mechanical_Tuning.csv")
df_ET1 <- read.csv("https://raw.githubusercontent.com//MatthewPaulSu/MACSFeD_data/main/Data/Literature_database_Electrical_Tuning.csv")
df_phono_all1 <- read.csv("https://raw.githubusercontent.com//MatthewPaulSu/MACSFeD_data/main/Data/Literature_database_Phonotaxis_All.csv")
df_phono_freq1 <- read.csv("https://raw.githubusercontent.com//MatthewPaulSu/MACSFeD_data/main/Data/Literature_database_Phonotaxis_Frequency_Responses.csv")

df_swarm <- df_swarm1
df_WBF <- df_WBF1
df_MT <- df_MT1
df_ET <- df_ET1
df_phono_all <- df_phono_all1
df_phono_freq <- df_phono_freq1

df_swarm$Tethered_Free <- rep("NA", length(df_swarm$Group))
df_MT$Tethered_Free <- rep("NA", length(df_MT$Group))
df_ET$Tethered_Free <- rep("NA", length(df_ET$Group))
df_phono_all$Tethered_Free <- rep("NA", length(df_phono_all$Group))
df_phono_freq$Tethered_Free <- rep("NA", length(df_phono_freq$Group))
df_phono_freq$Age <- as.character(df_phono_freq$Age)

df_swarm$Stimulus <- rep("NA", length(df_swarm$Group))
df_WBF$Stimulus <- rep("NA", length(df_WBF$Group))
df_phono_all$Stimulus <- rep("NA", length(df_phono_all$Group))
df_phono_freq$Stimulus <- rep("NA", length(df_phono_freq$Group))

df_swarm$Mechanical_State <- rep("NA", length(df_swarm$Group))
df_WBF$Mechanical_State <- rep("NA", length(df_WBF$Group))
df_phono_all$Mechanical_State <- rep("NA", length(df_phono_all$Group))
df_phono_freq$Mechanical_State <- rep("NA", length(df_phono_freq$Group))

df_swarm$'Swarming reports' <- rep("One",length(df_swarm$Group))
df_WBF$'Wing Beat Frequency reports' <- rep("One",length(df_WBF$Group))
df_MT$'Mechanical Tuning reports' <- rep("One",length(df_MT$Group))
df_ET$'Electrical Tuning reports' <- rep("One",length(df_ET$Group))
df_phono_all$'Phonotaxis reports' <- rep("One",length(df_phono_all$Group))

df <- full_join(x=df_WBF, y=df_swarm)[]
df <- full_join(x=df, y=df_MT)[]
df <- full_join(x=df, y=df_ET)[]
df <- full_join(x=df, y=df_phono_all)[]
df <- full_join(x=df, y=df_phono_freq)[]

colnames(df)[12] <- "Wing Beat Frequency (Hz)"
colnames(df)[22] <- "Wing Beat Frequency reports"
colnames(df)[25] <- 'Swarming duration (min)'
colnames(df)[26] <- 'Start (relative to sunrise)'
colnames(df)[27] <- 'Start (relative to sunset)'
colnames(df)[29] <- 'Males collected (field)'
colnames(df)[30] <- 'Swarm size by eye (field)'
colnames(df)[31] <- 'Height (m above ground)'
colnames(df)[32] <- "Swarming reports"
colnames(df)[37] <- "Mechanical Tuning Frequency (Hz)"
colnames(df)[43] <- "Mechanical Tuning reports"
colnames(df)[49] <- "Electrical Tuning Frequency (Hz)"
colnames(df)[51] <- "Electrical Tuning reports"
colnames(df)[66] <- "Phonotaxis reports"
colnames(df)[72] <- "Normalised frequency response"

suppressWarnings(df$'Swarming duration (min)' <- as.numeric(df$'Swarming duration (min)')) 
suppressWarnings(df$'Start (relative to sunrise)' <- as.numeric(df$'Start (relative to sunrise)'))
suppressWarnings(df$'Start (relative to sunset)' <- as.numeric(df$'Start (relative to sunset)'))
suppressWarnings(df$'Males collected (field)' <- as.numeric(df$'Males collected (field)')) 
suppressWarnings(df$'Swarm size by eye (field)' <- as.numeric(df$'Swarm size by eye (field)'))
suppressWarnings(df$'Height (m above ground)' <- as.numeric(df$'Height (m above ground)'))

df <- df %>% group_by(Group, Genus, Species, Paper, Experiment_location)

df_SR <- df[!is.na(df$'Swarming reports'), c(1,4,9,10,11,12,18,25,26,27,30,31,14,7,20,21)]
df_SR$Type <- rep("Swarming reports",length(df_SR$Group))
colnames(df_SR)[6] <- "Frequency (Hz)"
df_WBFR <- df[!is.na(df$'Wing Beat Frequency reports'), c(1,4,9,10,11,12,18,25,26,27,30,31,14,7,20,21)]
df_WBFR$Type <- rep("Wing Beat Frequency reports",length(df_WBFR$Group))
colnames(df_WBFR)[6] <- "Frequency (Hz)"
df_MTR <- df[!is.na(df$'Mechanical Tuning reports'), c(1,4,9,10,11,37,18,25,26,27,30,31,14,7,20,21)]
df_MTR$Type <- rep("Mechanical Tuning reports",length(df_MTR$Group))
colnames(df_MTR)[6] <- "Frequency (Hz)"
df_ETR <- df[!is.na(df$'Electrical Tuning reports'), c(1,4,9,10,11,49,18,25,26,27,30,31,14,7,20,21)]
df_ETR$Type <- rep("Electrical Tuning reports",length(df_ETR$Group))
colnames(df_ETR)[6] <- "Frequency (Hz)"
df_PR <- df[!is.na(df$'Phonotaxis reports'), c(1,4,9,10,11,12,18,25,26,27,30,31,14,7,20,21)]
df_PR$Type <- rep("Phonotaxis reports",length(df_PR$Group))
colnames(df_PR)[6] <- "Frequency (Hz)"
df_report <- rbind(df_SR,df_WBFR,df_MTR,df_ETR,df_PR)
for (i in 1:length(df_report$Sex)){
  if (df_report$Sex[i] == "Female"){
    df_report$Sex[i] <- intToUtf8(9792)}
  else{
    df_report$Sex[i] <- intToUtf8(9794)
  }
}

ui <- fluidPage(tags$head(
  tags$style(HTML("
      .Text pre {
        color: black;
        background-color: white;
        font-family: Helvetica;
        font-size: 14px
      }"))),
     fluidRow(
    column(width = 3,
           wellPanel(
             pickerInput("features",
                         label = NULL,
                         choices = list(Swarming = c("Swarming reports",
                                     "Swarming duration (min)", 
                                     "Start (relative to sunrise)", 
                                     "Start (relative to sunset)",
                                     "Swarm size by eye (field)", 
                                     "Height (m above ground)"),
                                     WBF = c("Wing Beat Frequency reports",
                                             "Wing Beat Frequency (Hz)"),
                                     Function = c("Mechanical Tuning reports",
                                       "Mechanical Tuning Frequency (Hz)",
                                       "Electrical Tuning reports",
                                     "Electrical Tuning Frequency (Hz)"),
                                     Phonotaxis = c("Phonotaxis reports",
                                                    "Normalised frequency response")),
                         selected = "Wing Beat Frequency (Hz)",
                         options = list(
                           title = "Select data type:"),
                         multiple = FALSE),
             div(uiOutput("input_genus", inline = TRUE), width = "100%",style = "font-style: bold;"),
             div(uiOutput("input_species", inline = TRUE), width = "100%",style = "font-style: bold;"),
             div(uiOutput("input_peer", inline = TRUE), width = "100%",style = "font-style: bold;"),
           )),
    column(width = 3,
           wellPanel( 
             conditionalPanel(
               condition = "input.features == 'Wing Beat Frequency reports'||input.features == 'Wing Beat Frequency (Hz)'||input.features == 'Mechanical Tuning reports' ||input.features == 'Mechanical Tuning Frequency (Hz)'||input.features == 'Electrical Tuning reports' ||input.features == 'Electrical Tuning Frequency (Hz)'||input.features == 'Phonotaxis reports'", 
               uiOutput("input_sex", inline = TRUE), width = "100%"),
             conditionalPanel(
               condition = "input.features != 'Mechanical Tuning reports' & input.features != 'Mechanical Tuning Frequency (Hz)'& input.features != 'Electrical Tuning reports'& input.features != 'Electrical Tuning Frequency (Hz)'", 
               uiOutput("input_location", inline = TRUE), width = "100%"),
             conditionalPanel(
               condition = "input.features == 'Wing Beat Frequency reports'||input.features == 'Wing Beat Frequency (Hz)'", 
               uiOutput("input_tether", inline = TRUE), width = "100%"),
             conditionalPanel(
               condition = "input.features == 'Mechanical Tuning reports' || input.features == 'Mechanical Tuning Frequency (Hz)'|| input.features == 'Electrical Tuning reports'|| input.features == 'Electrical Tuning Frequency (Hz)'", 
               uiOutput("input_state", inline = TRUE), width = "100%",
               uiOutput("input_stimulus", inline = TRUE), width = "100%"), 
             conditionalPanel(
               condition = "input.features == 'Wing Beat Frequency (Hz)' || input.features == 'Mechanical Tuning Frequency (Hz)'||input.features == 'Electrical Tuning Frequency (Hz)'", 
               uiOutput("input_facet_opt", inline = TRUE), width = "100%"),)
    ),
    column(width = 3,
           wellPanel(style = "center",
                     div(style = "display: inline-block;",downloadButton("downloadPlot", "Plot")), 
                     div(style = "display: inline-block;",downloadButton("downloadPlotData", "Plot Data")),
                     div(style = "display: inline-block;",downloadButton("downloadAllData", "All Data")),
                     br(),br(),
                     splitLayout(cellWidths = c("55%", "45%"), 
                                 div(class = "Text",style = "display: inline-block;",verbatimTextOutput(outputId = "date")),
                     div(style = "margin-top: 5px;display: inline-block;",a(actionButton(inputId = "email", label = "Contact authors", 
                                                                                     icon = icon("envelope")),
                                                                        href="https://forms.gle/Rxft47zq5hwLCCbR9", target="_blank"),)),
                    )
    )
    ),
 
 mainPanel(fluidPage(plotlyOutput("crossbar", height = 550, width = 1150), flex = 1, width = "100%", height = "100%", align = "center"), 
   DT::dataTableOutput("myTable")
 )
 )

server <- function(input, output, session) {
 
  df.reactive <- 
    reactive({
      df[!is.na(df[[input$features]]),]
      })
  
  #processing input Genus
  output$input_genus <- renderUI({
    
    selected_genus <- sort(unique(df.reactive()$Genus))
      
    pickerInput("Genus",
                choices = sort(unique(selected_genus)),
                selected = "Aedes",
                options = list(
                  title = "Select genus:",
                  `actions-box` = TRUE,
                  size = 10,
                  `live-search` = TRUE),
                choicesOpt = list(
                  content = sprintf("<i>%s</i>", sort(unique(selected_genus)))),
                multiple = TRUE,
                width = '100%')
    
  })

  #processing input location
  output$input_location <- renderUI({
    
    pickerInput(
      inputId = "Experiment_location",
   #   choices = sort(unique(na.omit(selected_location))),
  #    selected = unique(na.omit(selected_location)),
      choices = c("Lab","Field","Semi-field"),
      selected = c("Lab","Field","Semi-field"),
     options = list(
        title = "Select location:"), 
      multiple = TRUE,
      width = '100%'
    )

  })
  
  #processing input peer
  output$input_peer <- renderUI({
    
 #   selected_peer <- df %>% dplyr::filter(Genus %in% input$Genus) %>%
 #     pull(unique(.data$Peer_reviewed))
    
    pickerInput(
      inputId = "Peer_reviewed",
    #  choices = sort(unique(na.omit(selected_peer))),
    #  selected = unique(na.omit(selected_peer)),
       choices = c("N","Y"),
       selected = c("N","Y"),
      options = list(
        title = "Select literature type:"), 
      choicesOpt = list(
        content = c("Not peer reviewed","Peer reviewed")),
      multiple = TRUE,
      width = '100%'
    )
    
  })
  
  #processing input genus to give an output displaying input species
  output$input_species <- renderUI({
    
    selected_species <- df.reactive() %>% dplyr::filter(Genus %in% input$Genus) %>%
      pull(unique(.data$Group))
 
    pickerInput(
      inputId = "Group",
      choices = list(
        Aedes = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Aedes"]))),
        Anopheles = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Anopheles"]))),
        Armigeres = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Armigeres"]))),
        Culiseta = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Culiseta"]))),
        Culex = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Culex"]))),
        Deinocerites = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Deinocerites"]))),
        Hulecoetomyia = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Hulecoetomyia"]))),
        Mansonia = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Mansonia"]))),
        Mimomyia = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Mimomyia"]))),
        Orthopodomyia = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Orthopodomyia"]))),
        Psorophora = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Psorophora"]))),
        Toxorhynchites = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Toxorhynchites"]))),
        Uranotaenia = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Uranotaenia"]))),
        Verrallina = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Verrallina"]))),
        Wyeomyia = as.list(sort(unique(df.reactive()$Group[df.reactive()$Genus == "Wyeomyia"])))),
      selected =  selected_species,
      options = list(
        title = "Select species:",
        `actions-box` = TRUE,size = 10,
        `live-search` = TRUE),
      choicesOpt = list(
        content = sprintf("<i>%s</i>", sort(unique(df.reactive()$Group)))),
      multiple = TRUE,
      width = '100%'
    )
  })

  input_cny <- reactive({
    paste0(output$input_species)
  })
  
  #processing input genus to give an output displaying input sex
   output$input_sex <- renderUI({
    
    if (input$features == 'Phonotaxis reports' || input$features == 'Normalised frequency response' || input$features == "Swarming reports" || input$features == 'Swarming duration (min)' || input$features == 'Start (relative to sunrise)' || input$features == 'Start (relative to sunset)' || input$features == 'Swarm size by eye (field)' || input$features == 'Height (m above ground)' ){
      select_sex <- "Male"
    }else {
      select_sex <- c("Female","Male")
    }
    
    pickerInput(
      inputId = "Sex",
      #choices = sort(unique(na.omit(selected_sex))),
      #selected = unique(na.omit(selected_sex)),
      choices = c("Female","Male"),
      selected = c("Female","Male"),
      options = list(
        title = "Select sex:"), 
      multiple = TRUE,
      width = '100%'
    )

  })
  
  #processing input genus to give an output displaying input state
  output$input_state <- renderUI({
    
 #   selected_state <- df.reactive() %>% dplyr::filter(Genus %in% input$Genus) %>%
 #     pull(unique(.data$Mechanical_State))
    
    pickerInput(
      inputId = "Mechanical_State",
      choices = list(Active = c("Active quiescent","Active SSO"),
                    Passive = c("Passive pymetrozine","Passive sedation")),
      selected = c("Active quiescent","Active SSO"),
      options = list(
        title = "Select state:",
        `actions-box` = TRUE), 
      multiple = TRUE,
      width = '100%'
    )
    
  })
  
  #processing input genus to give an output displaying input stimulus
  output$input_stimulus <- renderUI({
    
#    selected_stimulus <- df.reactive() %>% dplyr::filter(Genus %in% input$Genus) %>%
#      pull(unique(.data$Stimulus))
    
    pickerInput(
      inputId = "Stimulus", 
      choices = list(Stimulated = c("Sweep","White noise"),
                     Unstimulated = c("Unstimulated")),
      selected = c("Sweep","White noise"),
      options = list(
        title = "Select stimulus type:",
        `actions-box` = TRUE), 
      multiple = TRUE,
      width = '100%'
    )

  })
  
  #processing input genus to give an output displaying input sex
  output$input_tether <- renderUI({
    
  #  selected_tether <- df.reactive() %>% dplyr::filter(Genus %in% input$Genus) %>%
  #    pull(unique(.data$Tethered_Free))
    
    pickerInput(
      inputId = "Tethered_Free", 
      choices = c("Free","Tethered"),
      #selected = unique(selected_tether),
      selected = c("Free"),
      options = list(
        title = "Select flight type:"), 
      multiple = TRUE,
      width = '100%'
    )

  })
  
  #processing input facet option to give an output displaying input facet option
  output$input_facet_opt <- renderUI({
    
    if (input$features == 'Swarming duration (min)' || input$features == 'Start (relative to sunrise)' || input$features == 'Start (relative to sunset)' || input$features == 'Swarm size by eye (field)' || input$features == 'Height (m above ground)' ){
      select_facet <- "Across species"
    }else {
      select_facet <- "Across species"
    }
    
    pickerInput(
      inputId = "Facet_Option", 
      choices = c("Within species","Across species","By temperature"),
      selected = select_facet,
      options = list(
        title = "Select how to group data:"), 
      multiple = FALSE,
      width = '100%'
    )
  })
  
  
  dataInput <- reactive({
    
    selected_feature <- switch(input$features,
                               "Swarming reports" = "Swarming reports",
                               "Swarming duration (min)" = "Swarming duration (min)",
                               "Start (relative to sunrise)" = "Start (relative to sunrise)", 
                               "Start (relative to sunset)" = "Start (relative to sunset)",
                               "Swarm size by eye (field)" = "Swarm size by eye (field)",
                               "Height (m above ground)" = "Height (m above ground)",
                               "Wing Beat Frequency reports" = "Wing Beat Frequency reports",
                               "Wing Beat Frequency (Hz)" = "Wing Beat Frequency (Hz)",
                               "Mechanical Tuning reports" = "Mechanical Tuning reports",
                               "Mechanical Tuning Frequency (Hz)"= "Mechanical Tuning Frequency (Hz)",
                               "Electrical Tuning reports" = "Electrical Tuning reports",
                               "Electrical Tuning Frequency (Hz)"= "Electrical Tuning Frequency (Hz)",
                               "Phonotaxis reports" = "Phonotaxis reports",
                               "Normalised frequency response" = "Normalised frequency response")
    
    selected_data <- df[df$Group %in% input$Group & df$Experiment_location %in% input$Experiment_location & df$Peer_reviewed %in% input$Peer_reviewed &df$Sex %in% input$Sex & df$Tethered_Free %in% c(input$Tethered_Free, "NA" ) & df$Mechanical_State %in% c(input$Mechanical_State,"NA") & df$Stimulus %in% c(input$Stimulus,"NA") &!is.na(df[[selected_feature]]), ]
    
  })
  
  plotInput <- reactive({
    selected_feature <- switch(input$features,
                               "Swarming reports" = "Swarming reports",
                               "Swarming duration (min)" = "Swarming duration (min)",
                               "Start (relative to sunrise)" = "Start (relative to sunrise)", 
                               "Start (relative to sunset)" = "Start (relative to sunset)",
                               "Swarm size by eye (field)" = "Swarm size by eye (field)",
                               "Height (m above ground)" = "Height (m above ground)",
                               "Wing Beat Frequency reports" = "Wing Beat Frequency reports",
                               "Wing Beat Frequency (Hz)" = "Wing Beat Frequency (Hz)",
                               "Mechanical Tuning reports" = "Mechanical Tuning reports",
                               "Mechanical Tuning Frequency (Hz)"= "Mechanical Tuning Frequency (Hz)",
                               "Electrical Tuning reports" = "Electrical Tuning reports",
                               "Electrical Tuning Frequency (Hz)"= "Electrical Tuning Frequency (Hz)",
                               "Phonotaxis reports" = "Phonotaxis reports",
                               "Normalised frequency response" = "Normalised frequency response")
    
    selected_data <- df[df$Group %in% input$Group & df$Experiment_location %in% input$Experiment_location & df$Peer_reviewed %in% input$Peer_reviewed & df$Sex %in% input$Sex & df$Tethered_Free %in% c(input$Tethered_Free, "NA" ) & df$Mechanical_State %in% c(input$Mechanical_State,"NA") & df$Stimulus %in% c(input$Stimulus,"NA") & !is.na(df[[selected_feature]]), ]
    selected_data$Genus <- sprintf("<i><b>%s</i></b>", selected_data$Genus)
    selected_data$Species <- sprintf("<i><b>%s</i></b>", selected_data$Species)
    selected_data$Group <- sprintf("<i><b>%s</i></b>", selected_data$Group)
    
    for (i in 1:length(selected_data$Sex)){
    if (selected_data$Sex[i] == "Female"){
      selected_data$Sex[i] <- intToUtf8(9792)}
      else{
      selected_data$Sex[i] <- intToUtf8(9794)
    }
    }
    
      if (length(unique(selected_data$Sex)) == 2){
        number_row <- 2}
      else{
        number_row <- 1
      }
    
    if (selected_feature == "Normalised frequency response"){
      Plot_title <- "Number of males responding to sound normalised by maximum responder number"
    } else if (selected_feature == "Swarming reports") {
      Plot_title <- "Number of reports per species containing information on swarming"
    } else if (selected_feature == "Wing Beat Frequency reports") {
      Plot_title <- "Number of reports per species containing Wing Beat Frequency estimates"
    } else if (selected_feature == "Mechanical Tuning reports") {
      Plot_title <- "Number of reports per species containing Mechanical Tuning estimates"
    } else if (selected_feature == "Electrical Tuning reports") {
      Plot_title <- "Number of reports per species containing Electrical Tuning estimates"
    }  else if (selected_feature == "Phonotaxis reports") {
      Plot_title <- "Number of reports per species containing information on phonotaxis"
    }  else if (selected_feature == "Swarming duration (min)") {
      Plot_title <- "Duration of swarm behavior in minutes"
    }  else if (selected_feature == "Start (relative to sunrise)") {
      Plot_title <- "Start time of swarm formation relative to sunrise in minutes"
    }  else if (selected_feature == "Swarm size by eye (field)")  {
      Plot_title <- "Estimated number of males in swarms in the field"
    }  else if (selected_feature == "Start (relative to sunset)")  {
      Plot_title <- "Start time of swarm formation relative to sunset in minutes"
    }   else if (selected_feature == "Height (m above ground)")  {
      Plot_title <- "Estimated height of swarm above ground in metres"
    } else if (selected_feature == "Wing Beat Frequency (Hz)")  {
      Plot_title <- "Estimated Wing Beat Frequency"
    }  else if (selected_feature == "Mechanical Tuning Frequency (Hz)")  {
      Plot_title <- "Estimated peak mechanical tuning (maximum vibration frequency of flagellum)"
    }  else if (selected_feature == "Electrical Tuning Frequency (Hz)")  {
      Plot_title <- "Estimated peak electrical tuning (frequency eliciting largest JO neuron response)"
    }  
      
    suppressWarnings(if(selected_feature == "Normalised frequency response"){
      p <- ggplot(selected_data, aes(x = round_any(Phono_stim_frequency,25), y = .data[[selected_feature]], group = Group,customdata = paste0(Sex,"__",Group,"__",Link,"__",.data[[selected_feature]]))) +
        stat_summary(fun="median",geom="crossbar",mapping = aes(color = Group,ymin=after_stat(y),ymax=after_stat(y)),width=35,lwd=2.8)+
        geom_jitter(aes(text = Paper, shape = Experiment_location),size=1.5, alpha=0.25)+
        labs(
           x = "",
            y = input$features,
           title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 18, face = "bold"),
              axis.text.x = element_text(size = 22, face = "italic", vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Sex," ",Genus)+Species, nrow = number_row, scales = "free_x")
    }    else if (selected_feature == "Swarming reports") {
      df1 <- selected_data[!is.na(selected_data$'Swarming reports'), ]
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species, Paper) %>% tally()  
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species) %>% tally() 
       p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group,customdata = paste0(Sex,"__",Group))) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6, color="black", size=4.75)+
       labs(
            x = "",
            y = input$features,
            title = Plot_title) +
        theme_classic()+
         theme(legend.position = "none",
               title = element_text(size = 18, face = "bold"),
              axis.text.x = element_text(size = 22, face = "italic", vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
         facet_wrap(~paste0(Sex," ",Genus)+Species, nrow = number_row, scales = "free_x")
    }   else if (selected_feature == "Wing Beat Frequency reports") {
      df1 <- selected_data[!is.na(selected_data$'Wing Beat Frequency reports'), ]
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species, Paper) %>% tally()  
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species) %>% tally()  
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group,customdata = paste0(Sex,"__",Group))) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6,  color="black", size=4.75)+
        labs(
          x = "",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 18, face = "bold"),
              axis.text.x = element_text(size = 22, face = "italic", vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Sex," ",Genus)+Species, nrow = number_row, scales = "free_x")
    }    else if (selected_feature == "Mechanical Tuning reports") {
      df1 <- selected_data[!is.na(selected_data$'Mechanical Tuning reports'), ]
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species, Paper) %>% tally()  
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species) %>% tally() 
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group,customdata = paste0(Sex,"__",Group))) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6,  color="black", size=4.75)+
        labs(
          x = "",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 18, face = "bold"),
              axis.text.x = element_text(size = 22, face = "italic", vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Sex," ",Genus)+Species, nrow = number_row, scales = "free_x")
    }  else if (selected_feature == "Electrical Tuning reports") {
      df1 <- selected_data[!is.na(selected_data$'Electrical Tuning reports'), ]
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species, Paper) %>% tally()  
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species) %>% tally() 
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group,customdata = paste0(Sex,"__",Group))) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6,    color="black", size=4.75)+
        labs(
          x = "",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 18, face = "bold"),
             axis.text.x = element_text(size = 22, face = "italic", vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Sex," ",Genus)+Species, nrow = number_row, scales = "free_x")
    }  else if (selected_feature == "Phonotaxis reports") {
      df1 <- selected_data[!is.na(selected_data$'Phonotaxis reports'), ]
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species, Paper) %>% tally()  
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species) %>% tally() 
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group,customdata = paste0(Sex,"__",Group))) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6, color="black", size=4.75)+
        labs(
          x = "",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 18, face = "bold"),
              axis.text.x = element_text(size = 22, face = "italic", vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Sex," ",Genus)+Species, nrow = number_row, scales = "free_x")
    } else if (input$Facet_Option == "Within species"){
      ggplot(selected_data, aes(x = Sex, y = .data[[selected_feature]],color = Group,customdata = paste0(Sex,"__",Group,"__",Link,"__",.data[[selected_feature]]))) +
        stat_summary(inherit.aes = FALSE,aes(x = Sex, y = .data[[selected_feature]],color = Group),fun="median",geom="crossbar",width=0.7,lwd=3.6)+
        geom_jitter(aes(text = Paper),size=4.5, alpha=0.25)+
        stat_summary(inherit.aes = FALSE,aes(x = Sex, y = .data[[selected_feature]],label = round(after_stat(y), 0)),fun = median, geom = "text", color="black", fontface = "bold", size=5.5) +
        labs(
          x = "",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 18, face = "bold"),
              axis.text.x = element_text(size = 28, face = "bold.italic", angle = 0, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
               strip.background = element_blank())+
        facet_wrap(~~Genus+Species, nrow = 1, scales = "free_x")
    }else if (input$Facet_Option == "By temperature"){
      selected_data <- selected_data[selected_data$Temperature > 0,]
      ggplot(selected_data, aes(x = Temperature, y = .data[[selected_feature]],color = Group,customdata = paste0(Sex,"__",Group,"__",Link,"__",.data[[selected_feature]]))) +
        geom_point(aes(text = Paper),size=4.5, alpha=0.55)+
        geom_smooth(inherit.aes = FALSE,aes(x = Temperature, y = .data[[selected_feature]]),method = lm,  size = 2, se=F, color = "black") +
         stat_regline_equation(inherit.aes = FALSE,aes(x = Temperature, y = .data[[selected_feature]],label =  paste0(gsub(" ","",str_sub(..eq.label..,start = -14,end =-12))," Hz/°C")),label.x.npc = "centre", label.y.npc = "top") +
        labs(
          x = "Temperature (°C)",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 21, face = "bold"),
              axis.text.x = element_text(size = 28, face = "bold.italic", angle = 0, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 18, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~Sex, nrow = 1, scales = "free_x")
    }else{
      ggplot(selected_data, aes(x = Group, y = .data[[selected_feature]],color = Group,customdata = paste0(Sex,"__",Group,"__",Link,"__",.data[[selected_feature]]))) +
      stat_summary(inherit.aes = FALSE,aes(x = Group, y = .data[[selected_feature]],color = Group),fun="median",geom="crossbar",width=0.7,lwd=3.6)+
      geom_jitter(aes(text = Paper),size=4.5, alpha=0.25)+
      stat_summary(inherit.aes = FALSE,aes(x = Group, y = .data[[selected_feature]],label = round(after_stat(y), 0)),fun = median, geom = "text", color="black", fontface = "bold", size=5.5) +
      labs(
            x = "",
           y = input$features,
           title = Plot_title) +
      theme_classic()+
       theme(legend.position = "none",
            title = element_text(size = 21, face = "bold"),
            axis.text.x = element_text(size = 22, face = "italic", angle = 0, vjust = 1, hjust = 1,colour="white"),
            axis.title.x = element_text(size = 1, face = "bold"),
            axis.text.y = element_text(size = 22),
            axis.title.y = element_text(size = 22, face = "bold"),
            strip.text.x = element_text(size = 22, face = "bold.italic"), 
            panel.spacing = unit(0.3, "lines"),
            strip.background = element_blank())+
        facet_wrap(~paste0(Sex," ",Genus)+Species, nrow = number_row, scales = "free_x")
           }
    )
    
  })
  
  plotInput1 <- reactive({
    selected_feature <- switch(input$features,
                               "Swarming reports" = "Swarming reports",
                               "Swarming duration (min)" = "Swarming duration (min)",
                               "Start (relative to sunrise)" = "Start (relative to sunrise)", 
                               "Start (relative to sunset)" = "Start (relative to sunset)",
                               "Swarm size by eye (field)" = "Swarm size by eye (field)",
                               "Height (m above ground)" = "Height (m above ground)",
                               "Wing Beat Frequency reports" = "Wing Beat Frequency reports",
                               "Wing Beat Frequency (Hz)" = "Wing Beat Frequency (Hz)",
                               "Mechanical Tuning reports" = "Mechanical Tuning reports",
                               "Mechanical Tuning Frequency (Hz)"= "Mechanical Tuning Frequency (Hz)",
                               "Electrical Tuning reports" = "Electrical Tuning reports",
                               "Electrical Tuning Frequency (Hz)"= "Electrical Tuning Frequency (Hz)",
                               "Phonotaxis reports" = "Phonotaxis reports",
                               "Normalised frequency response" = "Normalised frequency response")
    
    selected_data <- df[df$Group %in% input$Group & df$Experiment_location %in% input$Experiment_location & df$Peer_reviewed %in% input$Peer_reviewed & df$Sex %in% input$Sex & df$Tethered_Free %in% c(input$Tethered_Free, "NA" ) & df$Mechanical_State %in% c(input$Mechanical_State,"NA") & df$Stimulus %in% c(input$Stimulus,"NA") & !is.na(df[[selected_feature]]), ]
    
    for (i in 1:length(selected_data$Sex)){
      if (selected_data$Sex[i] == "Female"){
        selected_data$Sex[i] <- intToUtf8(9792)}
      else{
        selected_data$Sex[i] <- intToUtf8(9794)
      }
    }
    
    if (length(unique(selected_data$Sex)) == 2){
      number_row <- 2}
    else{
      number_row <- 1
    }
    
    if (selected_feature == "Normalised frequency response"){
      Plot_title <- "Number of males responding to sound normalised by maximum responder number"
    } else if (selected_feature == "Swarming reports") {
      Plot_title <- "Number of reports per species containing information on swarming"
    } else if (selected_feature == "Wing Beat Frequency reports") {
      Plot_title <- "Number of reports per species containing Wing Beat Frequency estimates"
    } else if (selected_feature == "Mechanical Tuning reports") {
      Plot_title <- "Number of reports per species containing Mechanical Tuning estimates"
    } else if (selected_feature == "Electrical Tuning reports") {
      Plot_title <- "Number of reports per species containing Electrical Tuning estimates"
    }  else if (selected_feature == "Phonotaxis reports") {
      Plot_title <- "Number of reports per species containing information on phonotaxis"
    }  else if (selected_feature == "Swarming duration (min)") {
      Plot_title <- "Duration of swarm behavior in minutes"
    }  else if (selected_feature == "Start (relative to sunrise)") {
      Plot_title <- "Start time of swarm formation relative to sunrise in minutes"
    }  else if (selected_feature == "Swarm size by eye (field)")  {
      Plot_title <- "Estimated number of males in swarms in the field"
    }  else if (selected_feature == "Start (relative to sunset)")  {
      Plot_title <- "Start time of swarm formation relative to sunset in minutes"
    }   else if (selected_feature == "Height (m above ground)")  {
      Plot_title <- "Estimated height of swarm above ground in metres"
    } else if (selected_feature == "Wing Beat Frequency (Hz)")  {
      Plot_title <- "Estimated Wing Beat Frequency"
    }  else if (selected_feature == "Mechanical Tuning Frequency (Hz)")  {
      Plot_title <- "Estimated peak mechanical tuning (maximum vibration frequency of flagellum)"
    }  else if (selected_feature == "Electrical Tuning Frequency (Hz)")  {
      Plot_title <- "Estimated peak electrical tuning (frequency eliciting largest JO neuron response)"
    }  
    
    suppressWarnings(if(selected_feature == "Normalised frequency response"){
      p <- ggplot(selected_data, aes(x = round_any(Phono_stim_frequency,25), y = .data[[selected_feature]], group = Group,customdata = paste0(Sex,"__",Group,"__",Link,"__",.data[[selected_feature]]))) +
        stat_summary(fun="median",geom="crossbar",mapping = aes(color = Group,ymin=after_stat(y),ymax=after_stat(y)),width=35,lwd=2.8)+
        geom_jitter(aes(text = Paper, shape = Experiment_location),size=1.5, alpha=0.25)+
        labs(
          x = "",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 18, face = "bold"),
              axis.text.x = element_text(size = 22, face = "italic", vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Sex," ",Genus)+Species, nrow = number_row, scales = "free_x")
    }    else if (selected_feature == "Swarming reports") {
      df1 <- selected_data[!is.na(selected_data$'Swarming reports'), ]
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species, Paper) %>% tally()  
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species) %>% tally() 
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group,customdata = paste0(Sex,"__",Group))) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6, color="black", size=4.75)+
        labs(
          x = "",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 18, face = "bold"),
              axis.text.x = element_text(size = 22, face = "italic", vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Sex," ",Genus)+Species, nrow = number_row, scales = "free_x")
    }   else if (selected_feature == "Wing Beat Frequency reports") {
      df1 <- selected_data[!is.na(selected_data$'Wing Beat Frequency reports'), ]
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species, Paper) %>% tally()  
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species) %>% tally()  
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group,customdata = paste0(Sex,"__",Group))) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6,  color="black", size=4.75)+
        labs(
          x = "",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 18, face = "bold"),
              axis.text.x = element_text(size = 22, face = "italic", vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Sex," ",Genus)+Species, nrow = number_row, scales = "free_x")
    }    else if (selected_feature == "Mechanical Tuning reports") {
      df1 <- selected_data[!is.na(selected_data$'Mechanical Tuning reports'), ]
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species, Paper) %>% tally()  
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species) %>% tally() 
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group,customdata = paste0(Sex,"__",Group))) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6,  color="black", size=4.75)+
        labs(
          x = "",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 18, face = "bold"),
              axis.text.x = element_text(size = 22, face = "italic", vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Sex," ",Genus)+Species, nrow = number_row, scales = "free_x")
    }  else if (selected_feature == "Electrical Tuning reports") {
      df1 <- selected_data[!is.na(selected_data$'Electrical Tuning reports'), ]
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species, Paper) %>% tally()  
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species) %>% tally() 
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group,customdata = paste0(Sex,"__",Group))) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6,    color="black", size=4.75)+
        labs(
          x = "",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 18, face = "bold"),
              axis.text.x = element_text(size = 22, face = "italic", vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Sex," ",Genus)+Species, nrow = number_row, scales = "free_x")
    }  else if (selected_feature == "Phonotaxis reports") {
      df1 <- selected_data[!is.na(selected_data$'Phonotaxis reports'), ]
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species, Paper) %>% tally()  
      df1 <- df1 %>%  group_by(Group, Sex, Genus, Species) %>% tally() 
      p <- ggplot(data=df1, aes(x=Group, y=n, fill=Group,customdata = paste0(Sex,"__",Group))) +
        geom_bar(stat="identity")  + 
        geom_text(aes(y=max(n)+1, label=n), vjust=1.6, color="black", size=4.75)+
        labs(
          x = "",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 18, face = "bold"),
              axis.text.x = element_text(size = 22, face = "italic", vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Sex," ",Genus)+Species, nrow = number_row, scales = "free_x")
    } else if (input$Facet_Option == "Within species"){
      ggplot(selected_data, aes(x = Sex, y = .data[[selected_feature]],color = Group,customdata = paste0(Sex,"__",Group,"__",Link,"__",.data[[selected_feature]]))) +
        stat_summary(inherit.aes = FALSE,aes(x = Sex, y = .data[[selected_feature]],color = Group),fun="median",geom="crossbar",width=0.7,lwd=3.6)+
        geom_jitter(aes(text = Paper),size=4.5, alpha=0.25)+
        stat_summary(inherit.aes = FALSE,aes(x = Sex, y = .data[[selected_feature]],label = round(after_stat(y), 0)),fun = median, geom = "text", color="black", fontface = "bold", size=5.5) +
        labs(
          x = "",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 18, face = "bold"),
              axis.text.x = element_text(size = 28, face = "bold.italic", angle = 0, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~~Genus+Species, nrow = 1, scales = "free_x")
    }else if (input$Facet_Option == "By temperature"){
      selected_data <- selected_data[selected_data$Temperature > 0,]
      ggplot(selected_data, aes(x = Temperature, y = .data[[selected_feature]],color = Group,customdata = paste0(Sex,"__",Group,"__",Link,"__",.data[[selected_feature]]))) +
        geom_point(aes(text = Paper),size=4.5, alpha=0.55)+
        geom_smooth(inherit.aes = FALSE,aes(x = Temperature, y = .data[[selected_feature]]),method = lm,  size = 2, se=F, color = "black") +
        stat_regline_equation(inherit.aes = FALSE,aes(x = Temperature, y = .data[[selected_feature]],label =  paste0(gsub(" ","",str_sub(..eq.label..,start = -14,end =-12))," Hz/°C")),label.x.npc = "centre", label.y.npc = "top") +
        labs(
          x = "Temperature (°C)",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 21, face = "bold"),
              axis.text.x = element_text(size = 28, face = "bold.italic", angle = 0, vjust = 1, hjust = 1),
              axis.title.x = element_text(size = 18, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~Sex, nrow = 1, scales = "free_x")
    }else{
      ggplot(selected_data, aes(x = Group, y = .data[[selected_feature]],color = Group,customdata = paste0(Sex,"__",Group,"__",Link,"__",.data[[selected_feature]]))) +
        stat_summary(inherit.aes = FALSE,aes(x = Group, y = .data[[selected_feature]],color = Group),fun="median",geom="crossbar",width=0.7,lwd=3.6)+
        geom_jitter(aes(text = Paper),size=4.5, alpha=0.25)+
        stat_summary(inherit.aes = FALSE,aes(x = Group, y = .data[[selected_feature]],label = round(after_stat(y), 0)),fun = median, geom = "text", color="black", fontface = "bold", size=5.5) +
        labs(
          x = "",
          y = input$features,
          title = Plot_title) +
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 21, face = "bold"),
              axis.text.x = element_text(size = 22, face = "italic", angle = 0, vjust = 1, hjust = 1,colour="white"),
              axis.title.x = element_text(size = 1, face = "bold"),
              axis.text.y = element_text(size = 22),
              axis.title.y = element_text(size = 22, face = "bold"),
              strip.text.x = element_text(size = 22, face = "bold.italic"), 
              panel.spacing = unit(0.3, "lines"),
              strip.background = element_blank())+
        facet_wrap(~paste0(Sex," ",Genus)+Species, nrow = number_row, scales = "free_x")
    }
    )
    
  })
  
  output$crossbar <- renderPlotly({
    ggplotly(plotInput()
             + theme(
               title = element_text(size = 14, face = "bold"),
               axis.text.x = element_text(size = ifelse(input$Facet_Option == "By temperature"|| input$features == "Normalised frequency response",16,1)),
                     axis.text.y = element_text(size = 16),
                     axis.title.x = element_text(size = 14),
                     axis.title.y = element_text(size = 18),
                     strip.text.x = element_text(size = ifelse(input$Facet_Option == "By temperature",20,9 + 9/length(unique(input$Group))), face = "bold")),
             tooltip = c(input$features,"Paper","color"),
    )%>%layout(hoverlabel = list(align = "left"),
               title = list(y = 0.99)) 
    })
  
  output$date <- renderText({"Version: 28th Jun 2024"})
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste(input$features, ".png", sep ="")},
    content = function(file) {
      ggsave(file, plot = plotInput1() , device = "png",
             width = 900, height = 600, dpi = 300, units = "mm")
    }
  )
  
  output$downloadPlotData <- downloadHandler(
    filename = function(){paste("Plot_data.csv", sep ="")},
    content = function(file) {
      write.csv(Filter(function(x)!all(is.na(x)), dataInput())[1:(length(Filter(function(x)!all(is.na(x)), dataInput()))-1)], file, row.names = FALSE)
    }
  )

  output$downloadAllData <- downloadHandler(
    filename = function(){paste("All_data.xlsx", sep ="")},
    content = function(file) {
      dataset_list <- list("Swarming" = df_swarm1, "WBF" = df_WBF1,"Mechanical Tuning" = df_MT1, "Electrical Tuning" = df_ET1,"Phonotaxis all" = df_phono_all1, "Phonotaxis frequency" = df_phono_freq1)
      write.xlsx(dataset_list, file)   
      }
  )
  
  output$myTable <- DT::renderDataTable({
    click <- event_data("plotly_click")
    click_string <- click$customdata[[1]]
    if (input$features == 'Swarming duration (min)'|| input$features == 'Start (relative to sunrise)' || input$features == 'Start (relative to sunset)' || input$features == 'Swarm size by eye (field)' || input$features == 'Height (m above ground)'){
      df_report <- df_report[df_report$Type == "Swarming reports",]
    } else if (input$features == 'Wing Beat Frequency (Hz)'){
      df_report <- df_report[df_report$Type == "Wing Beat Frequency reports",]
    } else if (input$features == 'Mechanical Tuning Frequency (Hz)'){
      df_report <- df_report[df_report$Type == "Mechanical Tuning reports",]
    } else if (input$features == 'Electrical Tuning Frequency (Hz)'){
      df_report <- df_report[df_report$Type == "Electrical Tuning reports",]
    } else if (input$features == 'Normalised frequency response'){
      df_report <- df_report[df_report$Type == "Phonotaxis reports",]
    }
    if (input$features == 'Swarming reports'|| input$features == 'Phonotaxis reports'){
     datatable(unique(df_report[sprintf("<i><b>%s</i></b>", df_report$Group) %in% strsplit(click_string,"__")[[1]][2] & df_report$Type %in% input$features & df_report$Sex %in% strsplit(click_string,"__")[[1]][1] & df_report$Experiment_location %in% input$Experiment_location & df_report$Peer_reviewed %in% input$Peer_reviewed,-c(6:13,15:17)]), extensions = 'Buttons', options = list(dom = 'Blrtip',buttons = c('copy', 'excel')),class = "display")   %>%
        formatStyle("Group",
                    fontStyle = "italic")
    } else if (input$features == 'Wing Beat Frequency reports'){
      datatable(unique(df_report[sprintf("<i><b>%s</i></b>", df_report$Group) %in% strsplit(click_string,"__")[[1]][2] & df_report$Type %in% input$features & df_report$Sex %in% strsplit(click_string,"__")[[1]][1] & df_report$Experiment_location %in% input$Experiment_location & df_report$Tethered_Free %in% input$Tethered_Free & df_report$Peer_reviewed %in% input$Peer_reviewed,-c(6:12,15:17)]), extensions = 'Buttons', options = list(dom = 'Blrtip',buttons = c('copy', 'excel')),class = "display")   %>%
        formatStyle("Group",
                    fontStyle = "italic")
    } else if (input$features == 'Mechanical Tuning reports'||input$features == 'Electrical Tuning reports'){
      datatable(unique(df_report[sprintf("<i><b>%s</i></b>", df_report$Group) %in% strsplit(click_string,"__")[[1]][2] & df_report$Type %in% input$features & df_report$Sex %in% strsplit(click_string,"__")[[1]][1] & df_report$Experiment_location %in% input$Experiment_location & df_report$Peer_reviewed %in% input$Peer_reviewed & df_report$Stimulus %in% input$Stimulus & df_report$Mechanical_State %in% input$Mechanical_State,-c(6:13,17)]), extensions = 'Buttons', options = list(dom = 'Blrtip',buttons = c('copy', 'excel')),class = "display")   %>%
        formatStyle("Group",
                    fontStyle = "italic")
    } else if (input$features == 'Swarming duration (min)'){
      datatable(unique(df_report[sprintf("<i><b>%s</i></b>", df_report$Group) %in% strsplit(click_string,"__")[[1]][2] & df_report$'Swarming duration (min)' %in% strsplit(click_string,"__")[[1]][4],-c(6:7,9:13,15:17)]), extensions = 'Buttons', options = list(dom = 'Blrtip',buttons = c('copy', 'excel')),class = "display")   %>%
        formatStyle("Group",
                    fontStyle = "italic")
    }else if (input$features == 'Start (relative to sunrise)'){
      datatable(unique(df_report[sprintf("<i><b>%s</i></b>", df_report$Group) %in% strsplit(click_string,"__")[[1]][2] & df_report$'Start (relative to sunrise)' %in% strsplit(click_string,"__")[[1]][4],-c(6:8,10:13,15:17)]), extensions = 'Buttons', options = list(dom = 'Blrtip',buttons = c('copy', 'excel')),class = "display")   %>%
        formatStyle("Group",
                    fontStyle = "italic")
    }else if (input$features == 'Start (relative to sunset)'){
      datatable(unique(df_report[sprintf("<i><b>%s</i></b>", df_report$Group) %in% strsplit(click_string,"__")[[1]][2] & df_report$'Start (relative to sunset)' %in% strsplit(click_string,"__")[[1]][4],-c(6:9,11:13,15:17)]), extensions = 'Buttons', options = list(dom = 'Blrtip',buttons = c('copy', 'excel')),class = "display")   %>%
        formatStyle("Group",
                    fontStyle = "italic")
    }else if (input$features == 'Swarm size by eye (field)'){
      datatable(unique(df_report[sprintf("<i><b>%s</i></b>", df_report$Group) %in% strsplit(click_string,"__")[[1]][2] & df_report$'Swarm size by eye (field)' %in% strsplit(click_string,"__")[[1]][4],-c(6:10,12:13,15:17)]), extensions = 'Buttons', options = list(dom = 'Blrtip',buttons = c('copy', 'excel')),class = "display")   %>%
        formatStyle("Group",
                    fontStyle = "italic")
    }else if (input$features == 'Height (m above ground)'){
      datatable(unique(df_report[sprintf("<i><b>%s</i></b>", df_report$Group) %in% strsplit(click_string,"__")[[1]][2] & df_report$'Height (m above ground)' %in% strsplit(click_string,"__")[[1]][4],-c(6:11,13,15:17)]), extensions = 'Buttons', options = list(dom = 'Blrtip',buttons = c('copy', 'excel')),class = "display")   %>%
        formatStyle("Group",
                    fontStyle = "italic")
    } else if (input$features == 'Normalised frequency response') {
      datatable(df_report[df_report$Link %in% strsplit(click_string,"__")[[1]][3] & sprintf("<i><b>%s</i></b>", df_report$Group) %in% strsplit(click_string,"__")[[1]][2] & df_report$Sex %in% strsplit(click_string,"__")[[1]][1],-c(6:13,15:17)], extensions = 'Buttons', options = list(dom = 'Blrtip',buttons = c('copy', 'excel')),class = "display")    %>%
        formatStyle("Group",
                    fontStyle = "italic")
      } else {datatable(df_report[df_report$Link %in% strsplit(click_string,"__")[[1]][3] & sprintf("<i><b>%s</i></b>", df_report$Group) %in% strsplit(click_string,"__")[[1]][2] & df_report$Sex %in% strsplit(click_string,"__")[[1]][1] & df_report$'Frequency (Hz)' %in% strsplit(click_string,"__")[[1]][4],-c(8:13,15:17)], extensions = 'Buttons', options = list(dom = 'Blrtip',buttons = c('copy', 'excel')),class = "display")    %>%
        formatStyle("Group",
                    fontStyle = "italic")}
     })
  
}

shinyApp(ui = ui, server = server)
