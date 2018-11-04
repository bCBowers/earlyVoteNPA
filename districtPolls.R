library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(lattice)
library(stringi)
library(reshape2)

# DFP plot theme 
theme_dfp <- function() {
  get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }
  if(get_os() == "osx") {
    theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.caption=element_text(hjust=1, size=9,
                                      margin=margin(t=10),
                                      face="italic"),
            plot.title=element_text(hjust=0, size=18,
                                    margin=margin(b=10),
                                    face="bold", family='FuturaBT-Heavy'),
            plot.subtitle=element_text(hjust=0,
                                       family='Montserrat-Regular'),
            axis.title.y=element_text(size=10,hjust=1,
                                      face="italic", family="Montserrat-Thin"),
            axis.title.x=element_text(hjust=1, size=10, face="italic", family="Montserrat-Thin",
                                      margin = margin(t = 10)), # , margin = margin(t = 10)
            legend.position = "bottom",
            legend.title = element_text(face="bold", family="Montserrat-Regular"),
            text=element_text(family="Montserrat-Regular"))
  }
  else {
    theme_bw() +
      theme(panel.border = element_blank(),
            plot.caption=element_text(hjust=1, size=9,
                                      margin=margin(t=10),
                                      face="italic"),
            plot.title=element_text(hjust=0, size=18,
                                    margin=margin(b=10),
                                    face="bold", family='FuturaBT-Heavy'),
            plot.subtitle=element_text(hjust=0,
                                       family='Montserrat-Regular'),
            axis.title.y=element_text(size=10,hjust=1,
                                      face="italic", family="Montserrat-Thin"),
            axis.title.x=element_text(hjust=1, size=10, face="italic", family="Montserrat-Thin",
                                      margin = margin(t = 10)), # , margin = margin(t = 10)
            legend.position = "bottom",
            legend.title = element_text(face="bold", family="Montserrat-Regular"),
            text=element_text(family="Montserrat-Regular"))
  }
}


# Compile all polls for their csv files
if (exists('dataset')){
  rm('dataset')
}

folder <- "../DFPPolls/Polls_11_4"
file_list <- list.files(folder)

pollDate <- read.csv('pollDates.csv')
#data$District <- as.character(data$District)

for (file in file_list){
  if (!exists("dataset")){
    dataset <- read.csv(file.path(folder,file))
    dataset$District <- stri_sub(str_split_fixed(file,'-', 3)[3],1,-7)
    dataset <- merge(dataset, pollDate, by='District', all.x=TRUE)
  }
  
  if (exists("dataset")){
    temp_dataset <-read.csv(file.path(folder,file))
    temp_dataset$District <- stri_sub(str_split_fixed(file,'-', 3)[3],1,-7)
    temp_dataset <- merge(temp_dataset, pollDate, by='District', all.x=TRUE)
    dataset<-bind_rows(dataset, temp_dataset)
    rm(temp_dataset)
  }
}


############################### Single Payer
payer <- dataset %>%
  select(SINGLEPAY, ager, file_race, partyid, w_LV)
payer <- na.omit(payer)
payer <- payer[payer$ager!='[DO NOT READ] Refused', ]
payer <- payer[payer$ager!="[DO NOT READ] Don't know/Refused", ]
payer <- payer[payer$partyid=='Democrat' | payer$partyid=='Republican' | payer$partyid=='Independent (No party)', ]
demo <- c('file_race', 'ager')

n <- 1
for (i in 1:length(demo)){
  var <- demo[[i]]
  inLoop <- payer
  #var <- ager
  inLoop$var <- inLoop[[var]]
  inLoop <- inLoop %>%
    group_by(partyid, var, SINGLEPAY) %>%
    tally()
  inLoop <- dcast(inLoop, partyid + var~SINGLEPAY)
  inLoop$perc <- inLoop$Support / (inLoop$Support + inLoop$Oppose)
  inLoop$perc[is.na(inLoop$perc)] <- 0
  
  if (n==1){
    plot1 <- ggplot(inLoop, aes(x = partyid, y = var, fill = perc)) + scale_y_discrete(limits=c("Unknown", "Other", "Asian", "Black", "Hispanic", "White")) +
      geom_tile(aes(fill = perc)) + scale_x_discrete(position = "top", labels=c('Democrat' = 'Dem', 'Independent (No party)' = 'Ind', 'Republican' = 'Rep')) +
      geom_text(aes(label = paste0(round(perc * 100), "%"))) +
 
      scale_fill_gradient('value', low='white', high='#124073', limits=c(0,1)) +
      xlab("") + ylab('') + theme_dfp() + 
      theme(legend.position="none")
  }
  else {
    plot2 <- ggplot(inLoop, aes(x = partyid, y = fct_rev(var), fill = perc)) +
      geom_tile(aes(fill = perc)) + 
      geom_text(aes(label = paste0(round(perc * 100), "%"))) + 
      scale_x_discrete(position = "top", labels=c('Democrat' = 'Dem', 'Independent (No party)' = 'Ind', 'Republican' = 'Rep')) + 
      scale_fill_gradient('value', low='white', high='#124073', limits=c(0,1)) +
      xlab("") + ylab('') + theme_dfp() + 
      theme(legend.position="none")
  }
    n = n + 1
}

g <- grid.arrange(plot1, plot2, nrow=1, top='Support for a Single Payer System\nAmong Swing Districts Voters')  
ggsave('singlePay.png', g)  

############################### CONIMMIG
payer <- dataset %>%
  select(CONIMMIG, ager, file_race, partyid, w_LV)
payer <- na.omit(payer)
payer <- payer[payer$ager!='[DO NOT READ] Refused', ]
payer <- payer[payer$ager!="[DO NOT READ] Don't know/Refused", ]
payer <- payer[payer$partyid=='Democrat' | payer$partyid=='Republican' | payer$partyid=='Independent (No party)', ]
demo <- c('file_race', 'ager')

n <- 1
for (i in 1:length(demo)){
  var <- demo[[i]]
  inLoop <- payer
  #var <- ager
  inLoop$var <- inLoop[[var]]
  inLoop <- inLoop %>%
    group_by(partyid, var, CONIMMIG) %>%
    tally()
  inLoop <- dcast(inLoop, partyid + var~CONIMMIG)
  inLoop$perc <- inLoop$Support / (inLoop$Support + inLoop$Oppose)
  inLoop$perc[is.na(inLoop$perc)] <- 0
  
  if (n==1){
    plot1 <- ggplot(inLoop, aes(x = partyid, y = var, fill = perc)) + scale_y_discrete(limits=c("Unknown", "Other", "Asian", "Black", "Hispanic", "White")) +
      geom_tile(aes(fill = perc)) + scale_x_discrete(position = "top", labels=c('Democrat' = 'Dem', 'Independent (No party)' = 'Ind', 'Republican' = 'Rep')) +
      geom_text(aes(label = paste0(round(perc * 100), "%"))) +
      
      scale_fill_gradient('value', low='white', high='#B71D1A', limits=c(0,1)) +
      xlab("") + ylab('') + theme_dfp() + 
      theme(legend.position="none")
  }
  else {
    plot2 <- ggplot(inLoop, aes(x = partyid, y = fct_rev(var), fill = perc)) +
      geom_tile(aes(fill = perc)) + 
      geom_text(aes(label = paste0(round(perc * 100), "%"))) + 
      scale_x_discrete(position = "top", labels=c('Democrat' = 'Dem', 'Independent (No party)' = 'Ind', 'Republican' = 'Rep')) + 
      scale_fill_gradient('value', low='white', high='#B71D1A', limits=c(0,1)) +
      xlab("") + ylab('') + theme_dfp() + 
      theme(legend.position="none")
  }
  n = n + 1
}

g <- grid.arrange(plot1, plot2, nrow=1, top='Support for Reduced Immigration and the Wall\nAmong Swing District Voters')  
ggsave('theWall.png', g)  

############################### Tax Reform
payer <- dataset %>%
  select(TAXREFORM, ager, file_race, partyid, w_LV)
payer <- na.omit(payer)
payer <- payer[payer$ager!='[DO NOT READ] Refused', ]
payer <- payer[payer$ager!="[DO NOT READ] Don't know/Refused", ]
payer <- payer[payer$partyid=='Democrat' | payer$partyid=='Republican' | payer$partyid=='Independent (No party)', ]
demo <- c('file_race', 'ager')

n <- 1
for (i in 1:length(demo)){
  var <- demo[[i]]
  inLoop <- payer
  #var <- ager
  inLoop$var <- inLoop[[var]]
  inLoop <- inLoop %>%
    group_by(partyid, var, TAXREFORM) %>%
    tally()
  inLoop <- dcast(inLoop, partyid + var~TAXREFORM)
  inLoop$perc <- inLoop$Support / (inLoop$Support + inLoop$Oppose)
  inLoop$perc[is.na(inLoop$perc)] <- 0
  
  if (n==1){
    plot1 <- ggplot(inLoop, aes(x = partyid, y = var, fill = perc)) + scale_y_discrete(limits=c("Unknown", "Other", "Asian", "Black", "Hispanic", "White")) +
      geom_tile(aes(fill = perc)) + scale_x_discrete(position = "top", labels=c('Democrat' = 'Dem', 'Independent (No party)' = 'Ind', 'Republican' = 'Rep')) +
      geom_text(aes(label = paste0(round(perc * 100), "%"))) +
      
      scale_fill_gradient('value', low='white', high='#B71D1A', limits=c(0,1)) +
      xlab("") + ylab('') + theme_dfp() + 
      theme(legend.position="none")
  }
  else {
    plot2 <- ggplot(inLoop, aes(x = partyid, y = fct_rev(var), fill = perc)) +
      geom_tile(aes(fill = perc)) + 
      geom_text(aes(label = paste0(round(perc * 100), "%"))) + 
      scale_x_discrete(position = "top", labels=c('Democrat' = 'Dem', 'Independent (No party)' = 'Ind', 'Republican' = 'Rep')) + 
      scale_fill_gradient('value', low='white', high='#B71D1A', limits=c(0,1)) +
      xlab("") + ylab('') + theme_dfp() + 
      theme(legend.position="none")
  }
  n = n + 1
}

g <- grid.arrange(plot1, plot2, nrow=1, top='Support for Trump Tax Cuts Among\nSwing District Voters')  
ggsave('TrumpTaxes.png', g)  

############################### Reverse Racism
payer <- dataset %>%
  select(REVERSERACIS, ager, file_race, partyid, w_LV)
payer <- na.omit(payer)
payer <- payer[payer$ager!='[DO NOT READ] Refused', ]
payer <- payer[payer$ager!="[DO NOT READ] Don't know/Refused", ]
payer <- payer[payer$partyid=='Democrat' | payer$partyid=='Republican' | payer$partyid=='Independent (No party)', ]
demo <- c('file_race', 'ager')

n <- 1
for (i in 1:length(demo)){
  var <- demo[[i]]
  inLoop <- payer
  #var <- ager
  inLoop$var <- inLoop[[var]]
  inLoop <- inLoop %>%
    group_by(partyid, var, REVERSERACIS) %>%
    tally()
  inLoop <- dcast(inLoop, partyid + var~REVERSERACIS)
  inLoop$perc <- inLoop$agree / (inLoop$agree + inLoop$disagree)
  inLoop$perc[is.na(inLoop$perc)] <- 0
  
  if (n==1){
    plot1 <- ggplot(inLoop, aes(x = partyid, y = var, fill = perc)) + scale_y_discrete(limits=c("Unknown", "Other", "Asian", "Black", "Hispanic", "White")) +
      geom_tile(aes(fill = perc)) + scale_x_discrete(position = "top", labels=c('Democrat' = 'Dem', 'Independent (No party)' = 'Ind', 'Republican' = 'Rep')) +
      geom_text(aes(label = paste0(round(perc * 100), "%"))) +
      
      scale_fill_gradient('value', low='white', high='#B71D1A', limits=c(0,1)) +
      xlab("") + ylab('') + theme_dfp() + 
      theme(legend.position="none")
  }
  else {
    plot2 <- ggplot(inLoop, aes(x = partyid, y = fct_rev(var), fill = perc)) +
      geom_tile(aes(fill = perc)) + 
      geom_text(aes(label = paste0(round(perc * 100), "%"))) + 
      scale_x_discrete(position = "top", labels=c('Democrat' = 'Dem', 'Independent (No party)' = 'Ind', 'Republican' = 'Rep')) + 
      scale_fill_gradient('value', low='white', high='#B71D1A', limits=c(0,1)) +
      xlab("") + ylab('') + theme_dfp() + 
      theme(legend.position="none")
  }
  n = n + 1
}

g <- grid.arrange(plot1, plot2, nrow=1, top='Swing District Voters who feel that\nReverse Racism is a Bigger Issue than Racism')  
ggsave('revRacism.png', g)  

############################### Immingrant Crime
payer <- dataset %>%
  select(IMMCRIME, ager, file_race, partyid, w_LV)
payer <- na.omit(payer)
payer <- payer[payer$ager!='[DO NOT READ] Refused', ]
payer <- payer[payer$ager!="[DO NOT READ] Don't know/Refused", ]
payer <- payer[payer$partyid=='Democrat' | payer$partyid=='Republican' | payer$partyid=='Independent (No party)', ]
demo <- c('file_race', 'ager')

n <- 1
for (i in 1:length(demo)){
  var <- demo[[i]]
  inLoop <- payer
  #var <- ager
  inLoop$var <- inLoop[[var]]
  inLoop <- inLoop %>%
    group_by(partyid, var, IMMCRIME) %>%
    tally()
  inLoop <- dcast(inLoop, partyid + var~IMMCRIME)
  inLoop$perc <- inLoop$agree / (inLoop$agree + inLoop$disagree)
  inLoop$perc[is.na(inLoop$perc)] <- 0
  
  if (n==1){
    plot1 <- ggplot(inLoop, aes(x = partyid, y = var, fill = perc)) + scale_y_discrete(limits=c("Unknown", "Other", "Asian", "Black", "Hispanic", "White")) +
      geom_tile(aes(fill = perc)) + scale_x_discrete(position = "top", labels=c('Democrat' = 'Dem', 'Independent (No party)' = 'Ind', 'Republican' = 'Rep')) +
      geom_text(aes(label = paste0(round(perc * 100), "%"))) +
            scale_fill_gradient('value', low='white', high='#B71D1A', limits=c(0,1)) +
      xlab("") + ylab('') + theme_dfp() + 
      theme(legend.position="none")
  }
  else {
    plot2 <- ggplot(inLoop, aes(x = partyid, y = fct_rev(var), fill = perc)) +
      geom_tile(aes(fill = perc)) + 
      geom_text(aes(label = paste0(round(perc * 100), "%"))) + 
      scale_x_discrete(position = "top", labels=c('Democrat' = 'Dem', 'Independent (No party)' = 'Ind', 'Republican' = 'Rep')) + 
      scale_fill_gradient('value', low='white', high='#B71D1A', limits=c(0,1)) +
      xlab("") + ylab('') + theme_dfp() + 
      theme(legend.position="none")
  }
  n = n + 1
}

g <- grid.arrange(plot1, plot2, nrow=1, top='Swing District Voters who Believe\nthat Immigrants Commit More Crime')  
ggsave('iCrime.png', g) 

############################### Assault Weapons
payer <- dataset %>%
  select(ASSAULTW, ager, file_race, partyid, w_LV)
payer <- na.omit(payer)
payer <- payer[payer$ager!='[DO NOT READ] Refused', ]
payer <- payer[payer$ager!="[DO NOT READ] Don't know/Refused", ]
payer <- payer[payer$partyid=='Democrat' | payer$partyid=='Republican' | payer$partyid=='Independent (No party)', ]
demo <- c('file_race', 'ager')

n <- 1
for (i in 1:length(demo)){
  var <- demo[[i]]
  inLoop <- payer
  #var <- ager
  inLoop$var <- inLoop[[var]]
  inLoop <- inLoop %>%
    group_by(partyid, var, ASSAULTW) %>%
    tally()
  inLoop <- dcast(inLoop, partyid + var~ASSAULTW)
  inLoop$perc <- inLoop$support / (inLoop$support + inLoop$oppose)
  inLoop$perc[is.na(inLoop$perc)] <- 0
  
  if (n==1){
    plot1 <- ggplot(inLoop, aes(x = partyid, y = var, fill = perc)) + scale_y_discrete(limits=c("Unknown", "Other", "Asian", "Black", "Hispanic", "White")) +
      geom_tile(aes(fill = perc)) + scale_x_discrete(position = "top", labels=c('Democrat' = 'Dem', 'Independent (No party)' = 'Ind', 'Republican' = 'Rep')) +
      geom_text(aes(label = paste0(round(perc * 100), "%"))) +
      scale_fill_gradient('value', low='white', high='#124073', limits=c(0,1)) +
      xlab("") + ylab('') + theme_dfp() + 
      theme(legend.position="none")
  }
  else {
    plot2 <- ggplot(inLoop, aes(x = partyid, y = fct_rev(var), fill = perc)) +
      geom_tile(aes(fill = perc)) + 
      geom_text(aes(label = paste0(round(perc * 100), "%"))) + 
      scale_x_discrete(position = "top", labels=c('Democrat' = 'Dem', 'Independent (No party)' = 'Ind', 'Republican' = 'Rep')) + 
      scale_fill_gradient('value', low='white', high='#124073', limits=c(0,1)) +
      xlab("") + ylab('') + theme_dfp() + 
      theme(legend.position="none")
  }
  n = n + 1
}

g <- grid.arrange(plot1, plot2, nrow=1, top='Support for an Assault Weapons Ban\nAmong Among Swing District Voters')  
ggsave('AssWeap.png', g) 