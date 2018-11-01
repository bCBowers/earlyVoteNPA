library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(lattice)
library(stringi)

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

folder <- "../DFPPolls/Polls_10_29"
file_list <- list.files(folder)

pollDate <- read.csv('pollDates.csv')
data$District <- as.character(data$District)

for (file in file_list){
  if (!exists("dataset")){
    dataset <- read.csv(file.path(folder,file))
    dataset$District <- stri_sub(str_split_fixed(file,'-', 3)[3],1,-5)
    dataset <- merge(dataset, pollDate, by='District', all.x=TRUE)
  }
  
  if (exists("dataset")){
    temp_dataset <-read.csv(file.path(folder,file))
    temp_dataset$District <- stri_sub(str_split_fixed(file,'-', 3)[3],1,-5)
    temp_dataset <- merge(temp_dataset, pollDate, by='District', all.x=TRUE)
    dataset<-bind_rows(dataset, temp_dataset)
    rm(temp_dataset)
  }
}


################################ Early Voting over Time
alreadyV <- dataset[dataset$likely =='Already voted', ]
alreadyV <- alreadyV[alreadyV$response== 'Dem' | alreadyV$response== 'Rep', ]

alreadyV <- alreadyV %>%
  group_by(response, file_party, poll_end) %>%
  tally() %>%
  group_by(response, file_party) %>% 
  mutate(csum = cumsum(n))
fix=data.frame('response'='Rep', 'file_party'='Democratic', 'poll_end'=c('10/29/2018'), 'n'=0, 'csum'=75) # fix Rep issue on 10/29
alreadyV <- bind_rows(alreadyV, fix)

plot1 <- ggplot(alreadyV[alreadyV$response=='Dem', ], aes(x=as.numeric(as.Date(poll_end, "%m/%d/%Y")), y=csum)) + 
  geom_area(aes(fill= file_party), position = 'stack') +
  xlab('') + ylab('Cumulative Demmocratic\nEarly Votes\n') + 
  theme_dfp()  + scale_y_continuous(breaks = c(0, 400, 800, 1200), labels=c('0', '400', '800', '1200')) +
  scale_fill_manual(name = '', labels=c('Democrat', 'Neither Party', 'Republican'), values=c("#124073", "#A8BF14", '#B71D1A')) + 
  ggtitle('Democratic Voters') + theme(axis.text.x = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank()) + 
  theme(legend.position = c(0.2, 0.7))

plot2 <- ggplot(alreadyV[alreadyV$response=='Rep', ], aes(x=as.numeric(as.Date(poll_end, "%m/%d/%Y")), y=csum)) + 
  geom_area(aes(fill= file_party), position = 'stack') +
  xlab('') + ylab('Cumulative Republican\nEarly Votes\n') + guides(fill=FALSE) +
  theme_dfp()  + scale_y_continuous(breaks = c(0, 400, 800, 1200), labels=c('0', '400', '800', '1200'), limits=c(0,1200)) +
  scale_x_continuous(breaks = c(17820, 17827, 17834), labels=c('10/16', '10/23', '10/30')) +
  scale_fill_manual(name = '', values=c("#124073", "#A8BF14", '#B71D1A')) + 
  ggtitle('Republican Voters') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

g <- grid.arrange(plot1, plot2, top='Early Voting Results by Voter File Registration\n')
ggsave('regAllVotes.png', g)

################ Who's winning NPAs
alreadyVI <- alreadyV[alreadyV$file_party=='Other', ] %>% 
  group_by(response) %>% mutate(csum = cumsum(n))

alreadyVI$csum[alreadyVI$response=='Rep'] <- -1 * alreadyVI$csum[alreadyVI$response=='Rep']
reg_delta <- dcast(alreadyVI, poll_end~response)
reg_delta$csum <- reg_delta$Dem + reg_delta$Rep

ggplot(alreadyVI, aes(x=as.numeric(as.Date(poll_end, "%m/%d/%Y")), y=csum)) + 
  geom_area(aes(fill= response, alpha=0.5)) +
  geom_area(data=reg_delta, fill='#124073') + guides(fill=FALSE) + guides(alpha=FALSE) +
  xlab('') + ylab('Cumulative Republican Early Vote                                                       Cumulative Democratic Early Votes\n') + 
  theme_dfp()  + scale_x_continuous(breaks = c(17820, 17827, 17834), labels=c('10/16', '10/23', '10/30')) +
  scale_y_continuous(breaks = c(-300, -150, 0, 150, 300), labels=c('300', '150', '0', '150', '300'), limits=c(-300,400)) +
  scale_fill_manual(name = 'Party Voted For', labels=c('Democratic', 'Republican'), values=c("#124073", '#B71D1A')) + 
  ggtitle('Early Voting by Voters\nReigstered to Neither Party') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(legend.position = c(0.2, 0.8)) 
ggsave('RegVote.png')