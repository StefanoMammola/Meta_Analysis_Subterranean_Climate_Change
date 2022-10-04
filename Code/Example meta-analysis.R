## ------------------------------------------------------------------------
## 'Gaming the system of article impact: The influence of non-scientific features on citations'
## ------------------------------------------------------------------------

# Stefano Mammola, Enrico Caprio, Alberto Doretto, Elena Piano, Dan Chamberlain

## ------------------------------------------------------------------------
# 'R script to reproduce the analyses'
## ------------------------------------------------------------------------

# Analysis performed with R (v. R 4.0.3) and R studio (v. 1.4.1103)
# Analysis by Stefano Mammola & Dan Chamberlain

#########################################################
## Workspace and Data preparation ##
#########################################################

# clean the workspace -----------------------------------------------------

rm(list=ls())

# Loading R packages ------------------------------------------------------

library("ggplot2")   # Create Elegant Data Visualisations Using the Grammar of Graphics
library("gridExtra") # Miscellaneous Functions for "Grid" Graphics
library("metafor")   # Meta-Analysis Package for R

# Custom theme for plot ---------------------------------------------------

theme_custom <-   theme(plot.title  = element_text(size=13,face = "bold"),
                        axis.title.x = element_text(size=14), 
                        axis.text.x  = element_text(size=10),
                        axis.title.y = element_text(size=14), 
                        axis.text.y  = element_text(size=10),
                        panel.grid.major = element_line(size = 0, linetype = 'solid', colour = "grey92"),
                        panel.background = element_rect(fill = "white", colour = "grey30",size = 1, linetype = "solid"))

# Set working directory ---------------------------------------------------

setwd("/Users/stefanomammola/Desktop/PAPERS IN CORSO/PERFECT PAPER_DAN/ZZ_PAPER/ANALYSIS/")

# Loading the data --------------------------------------------------------

Data <- read.csv("Final_data.csv", header = TRUE, dec = ",", sep= "\t", as.is = FALSE)

#Reads all as factors if I use dec=, and most (including Pearsons.r.1) if I use dec=,!

#Subsetting data by excluding not used measures
Data2 <- Data[Data$Analysis == "yes",] ; str(Data2)

# Extracting year for each study ------------------------------------------

#Calculating year range for the dataset analysed by each scientometric article

levels(Data2$Year.of.publication)

#cleaning messy cases
levels(Data2$Year.of.publication)[c(88,100,135,191:196)]   <- c("2000,2005,2010", #"2000/2005/2010"
                                                                "2005,2016",#"2005-2006; 2015-2016"
                                                                "2010,2016",#"2010 and 2016"
                                                                "2018",#"Jen Feb 2018"
                                                                "1965-2017", #"NA (all)" ----> all WoS range
                                                                NA,
                                                                NA,
                                                                "1965-2003", #"up to 2003" ----> all WoS range
                                                                "1965-2019") #"up to 2019" ----> all WoS range

#Extracting range with a loop
Yr_min   <- c()
Yr_max   <- c()
Yr_range <- c()

for (i in 1:nrow(Data2)){
  
  Data_i   <- Data2[i,]
  Year     <- as.character(Data_i$Year.of.publication) ; Year <- gsub(",","-",Year) #replace comma if needed
  Year     <- as.numeric(strsplit(Year, "-")[[1]]) #split the year range
  
  if(is.na(Year)==TRUE){ 
    Yr_min   <- c(Yr_min, NA)
    Yr_max   <- c(Yr_max, NA)
    Yr_range <- c(Yr_range, NA) } else {
    Yr_min   <- c(Yr_min, range(Year)[1])
    Yr_max   <- c(Yr_max, range(Year)[2])
    Yr_range <- c(Yr_range, (range(Year)[2]-range(Year)[1])) }
  
} #Warnings() occur when the range is just a single number (i.e. no range)

# check the values
range(Yr_max,na.rm=TRUE)
range(Yr_min,na.rm=TRUE)
range(Yr_range,na.rm=TRUE)

#Store the data
Data2 <- data.frame(Data2, Yr_min, Yr_max, Yr_range) ; head(Data2)

# Subsetting data ---------------------------------------------------------

Data2 <- Data2[,c("Reference", "Response.category", "Predictor.category","Discipline_macro","Discipline",
                  "N.journals","n", "Pearsons.r.1", "Yr_min", "Yr_max", "Yr_range")]

#Renaming columns
colnames(Data2) <- c("Reference", "Response", "Predictor", "Discipline_macro",
                     "Discipline", "n_journals" ,"n", "r", "Yr_min", "Yr_max", "Yr_range")

#Dropping unused levels
Data2 <- droplevels(Data2)

# Summary statistics ------------------------------------------------------

#Sample size
dim(Data2) #2628 standardized estimates
nlevels(Data2$Reference) #262 references 

levels(Data2$Response) #4 response variable
levels(Data2$Predictor) #28 predictors, some deemed not relevant like other, early citations)
levels(Data2$Discipline) #13 macro disciplines

#Summary of estimates in each combination of response and predictor
(table_tot <- table(Data2$Predictor,Data2$Response))

# We can only analyse citations as a response variable

#########################################################
## Analysis for citation - Full database ##
#########################################################

#Citations
data_citation <- Data2[Data2$Response == "Citations", ]
data_citation <- droplevels(data_citation) ; head(data_citation) 

dim(data_citation) #2361 standardized estimates
nlevels(data_citation$Reference) #250 references 

# average number of estimates per publication
mean(table(data_citation$Reference)) ; sd(table(data_citation$Reference))

# Derive Fischer's Z and its variance -------------------------------------

data_citation <- escalc(measure = "ZCOR", ri = r, ni = n, data = data_citation)

# Estimate models in a loop -----------------------------------------------

#Check sample size for each predictors
table_citation <- data.frame(predictor = NULL, n = NULL, n_papers = NULL)

for(i in 1:length(unique(levels(data_citation$Predictor))))
  table_citation <- rbind(table_citation, 
                          data.frame(predictor = levels(data_citation$Predictor)[i],
                                     n = nrow(data_citation[data_citation$Predictor == levels(data_citation$Predictor)[i], ]),
                                     n_papers = length(unique(data_citation[data_citation$Predictor == levels(data_citation$Predictor)[i], ]$Reference)))  
                          
  )

#Taking a 5 paper limit
table_citation[table_citation$n_papers>4,]

# Change with what you want 
predictors_to_analyse <- c("Altmetrics","Journal impact","Open Access","Fundings",
                           "No. Authors","Authors experience","Level of collaboration",
                           "Abstract length", "Abstract complexity",
                           "Title length","Title specificity","Title pleasantness","Title complexity",
                           "No. References","Reference list impact","No. Self citations","No. Keywords","Article length",
                           "No. Graphical items","Other presentation items",
                           "Gender bias","Linguistic injustice")

#Creating color vector for the plot
color_cat <- c(rep("darkorange2",4),
               rep("blue",3),
               rep("black",11),
               rep("grey50",2),
               rep("purple",2))

length(color_cat) == length(predictors_to_analyse) #Length should be the same as predictors_to_analyse (TRUE)

#Fitting the models
SUBSET    <- list()
MODEL     <- list()
MODEL_yr  <- list()

result_for_plot <- data.frame(label = NULL,
                              b     = NULL,
                              ci.lb = NULL,
                              ci.ub = NULL,
                              ES    = NULL,
                              L     = NULL,
                              U     = NULL)

table_sup_mat <-  data.frame(Predictor = NULL,
                             N         = NULL,
                             Beta_SE   = NULL,
                             CI        = NULL,
                             p         = NULL,
                             # model 2
                             Moderator_B_SE = NULL,
                             Moderator_CI   = NULL,
                             Moderator_p    = NULL)

for (i in 1:length(predictors_to_analyse)){  
  
  #subset the predictor
  data_i  <- data_citation[data_citation$Predictor == predictors_to_analyse[i], ]
  
  #fitting the model
  model_i <- rma.mv(yi, vi, random =  ~ 1 | Reference, data = na.omit(data_i)) 
  
  #fitting a second model considering Year as a moderator
  model_i_yr <- rma.mv(yi, vi, mods = ~ Yr_min, random =  ~ 1 | Reference, data = na.omit(data_i)) 
  
  #extracting coefficients
  result_for_plot_i <- data.frame(label = paste(predictors_to_analyse[i]," (" ,
                                                nrow(data_i),", ",
                                                length(unique(data_i$Reference)),")",sep=''),
                                  b     = model_i$b,
                                  ci.lb = model_i$ci.lb,
                                  ci.ub = model_i$ci.ub,
                                  ES    = ((exp(model_i$b)-1))/((exp(model_i$b)+1)),
                                  L     = ((exp(model_i$ci.lb)-1)/(exp(model_i$ci.lb)+1)),
                                  U     = ((exp(model_i$ci.ub)-1)/(exp(model_i$ci.ub)+1)))
  
  table_i <-  data.frame(Predictor = predictors_to_analyse[i],
             N       = nrow(data_i),
             Beta_SE = paste(round(model_i$beta,2),"+/-", round(model_i$se,2),sep=''),
             CI = paste(round(model_i$ci.lb,2), " | ", round(model_i$ci.ub,2),sep=''),
             p = round(model_i$pval,2),
             #model 2
             Moderator_B_SE = paste(round(model_i_yr$beta[2],5),"+/-", round(model_i_yr$se[2],5),sep=''),
             Moderator_CI   = paste(round(model_i_yr$ci.lb[2],5), " | ", round(model_i_yr$ci.ub[2],5),sep=''),
             Moderator_p    = round(model_i_yr$pval[2],5)
             )
  
  #store the data 
  SUBSET[[i]]     <- data_i
  MODEL[[i]]      <- model_i
  MODEL_yr[[i]]   <- model_i_yr
  result_for_plot <- rbind(result_for_plot,result_for_plot_i)
  table_sup_mat   <- rbind(table_sup_mat,table_i)
}

# Validation --------------------------------------------------------------

#Evaluation of publication bias via Rosenthal’s method.

ros_N <- c()
ros   <- c()

for (i in 1:length(predictors_to_analyse)){ 

Ros_i <- fsn(yi, vi, type = "Rosenthal", data = SUBSET[[i]])  

ros_N <- c(ros_N,Ros_i$fsnum)
ros   <- c(ros,round(Ros_i$pval,3))

message(paste("---", predictors_to_analyse[[i]], "---", sep = ' '))
print(Ros_i) #Rosenthal's failsafe number (rule-of-thumb: it should be larger than n*5 + 10)
}

#Based on Rosenthal number, following need to be approached with care:
# No. keywords (0.27)

# Title pleasantness (0.17)


# Saving model estimates and validation -----------------------------------

#Storing the data for Table S2
table_sup_mat <- data.frame(table_sup_mat, Rosenthal_FSnum = ros_N, Rosenthal_p = ros)
write.csv(table_sup_mat,"TABLE_S2.csv")

# Model summary -----------------------------------------------------------

for (i in 1:length(predictors_to_analyse)){ 
  message(paste("---", predictors_to_analyse[[i]], "---", sep = ' '))
  if(MODEL_yr[[i]]$pval[2] < 0.05)
    print(MODEL_yr[[i]]) 
  else
    print("year not significant")
}

#Year is significant only for:  Reference list impact, No. References, 
#Level of collaboration, No. Authors, Journal impact

# Generating the plot -----------------------------------------------------

#Color for the plot
result_for_plot <- data.frame(result_for_plot,color_cat) 
order <- result_for_plot$label
result_for_plot$label <- factor(result_for_plot$label,  levels  = order)
result_for_plot$label <- ordered(result_for_plot$label, levels  = order)

#Estimating the direction of the temporal trend, and other graphical parameters
arrow_direction <- c()
arrow_shade     <- c()
arrow_size      <- c()

for (i in 1:length(predictors_to_analyse)){ 
  
  if(MODEL_yr[[i]]$beta[2] > 0)
    arrow_direction <- c(arrow_direction, "last")
  else
    arrow_direction <- c(arrow_direction, "first")
  
  if(MODEL_yr[[i]]$pval[2] < 0.05){
    arrow_shade <- c(arrow_shade,1)
    arrow_size <- c(arrow_size,1)
  }else{ 
    arrow_shade <- c(arrow_shade,0.6)
    arrow_size <- c(arrow_size,0.5)}
}

#Making the plot
(plot1 <- ggplot(data= result_for_plot, aes(x=label, y=ES, ymin=L, ymax=U)) +
    geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
    #geom_hline(yintercept=0.373, lty=3, col="grey70") +  
    geom_pointrange(col= result_for_plot$color_cat, size= 1) + 
    ylim(-0.24,0.45) +
    annotate("segment", 
             x    = c(1:length(predictors_to_analyse)), 
             xend = c(1:length(predictors_to_analyse)), 
             y    = 0.44, 
             yend = 0.41, 
             colour = result_for_plot$color_cat, 
             size = arrow_size, 
             alpha = arrow_shade, 
             arrow=arrow(ends = arrow_direction,angle = 45, length = unit(.2,"cm")))+
    coord_flip() +  # flip coordinates (puts labels on y axis)
    labs(title = "(a) All disciplines")+
    xlab("") +
    ylab("Effect size [r]")+
    theme_bw() + theme_custom)

##################################################################
# SUB-SET ANALYSIS ONLY WITH STUDIES FOCUSING ON A SINGLE JOURNAL
##################################################################

data_citation$n_journals <- as.numeric(data_citation$n_journals)
data_citation_sub <- data_citation[data_citation$n_journals == 1, ]

#remove NA
data_citation_sub <- data_citation_sub[!is.na(data_citation_sub$n_journals),]
data_citation_sub <- droplevels(data_citation_sub)

#Sample size
nrow(data_citation_sub) # estimates
unique(levels(data_citation_sub$Reference))

# Derive Fischer's Z and its variance -------------------------------------

data_citation_sub <- escalc(measure = "ZCOR", ri = r, ni = n, data = data_citation_sub)

# Estimate models in a loop -----------------------------------------------

#Check sample size for each predictors
table_citation_sub <- data.frame(predictor = NULL, n = NULL, n_papers = NULL)

for(i in 1:length(unique(levels(data_citation_sub$Predictor))))
  table_citation_sub <- rbind(table_citation_sub, 
                          data.frame(predictor = levels(data_citation_sub$Predictor)[i],
                                     n = nrow(data_citation[data_citation_sub$Predictor == levels(data_citation_sub$Predictor)[i], ]),
                                     n_papers = length(unique(data_citation_sub[data_citation_sub$Predictor == levels(data_citation_sub$Predictor)[i], ]$Reference)))  
                          
  )

#Taking a 5 paper limit
table_citation_sub[table_citation_sub$n_papers>4,]

# Change with what you want 
predictors_to_analyse_sub <- c("Altmetrics",
                              "Level of collaboration", "No. Authors",
                               "Article length", 
                               "Title complexity",
                                "Title length")

#Creating color vector for the plot
color_cat <- c(rep("darkorange2",1),
               rep("blue",2),
               rep("black",3))

length(color_cat) == length(predictors_to_analyse) #Length should be the same as predictors_to_analyse (TRUE)

#Fitting the models
SUBSET    <- list()
MODEL     <- list()

result_for_plot_sub <- data.frame(label = NULL,
                              b     = NULL,
                              ci.lb = NULL,
                              ci.ub = NULL,
                              ES    = NULL,
                              L     = NULL,
                              U     = NULL)

for (i in 1:length(predictors_to_analyse_sub)){  
  
  #subset the predictor
  data_i  <- data_citation_sub[data_citation_sub$Predictor == predictors_to_analyse_sub[i], ]
  
  #fitting the model
  model_i <- rma.mv(yi, vi, random =  ~ 1 | Reference, data = na.omit(data_i)) 
  
  #extracting coefficients
  result_for_plot_i <- data.frame(label = paste(predictors_to_analyse_sub[i]," (" ,
                                                nrow(data_i),", ",
                                                length(unique(data_i$Reference)),")",sep=''),
                                  b     = model_i$b,
                                  ci.lb = model_i$ci.lb,
                                  ci.ub = model_i$ci.ub,
                                  ES    = ((exp(model_i$b)-1))/((exp(model_i$b)+1)),
                                  L     = ((exp(model_i$ci.lb)-1)/(exp(model_i$ci.lb)+1)),
                                  U     = ((exp(model_i$ci.ub)-1)/(exp(model_i$ci.ub)+1)))
  
  
  #store the data 
  SUBSET[[i]]     <- data_i
  MODEL[[i]]      <- model_i
  result_for_plot_sub <- rbind(result_for_plot_sub,result_for_plot_i)
}

# Model summary -----------------------------------------------------------

for (i in 1:length(predictors_to_analyse_sub)){ 
  message(paste("---", predictors_to_analyse_sub[[i]], "---", sep = ' '))
  print(MODEL_yr[[i]]) 
 
}

#Year is significant only for:  Reference list impact, No. References, 
#Level of collaboration, No. Authors, Journal impact

# Generating the plot -----------------------------------------------------

#Color for the plot
result_for_plot_sub <- data.frame(result_for_plot_sub,color_cat) 
order <- result_for_plot_sub$label
result_for_plot_sub$label <- factor(result_for_plot_sub$label,  levels  = order)
result_for_plot_sub$label <- ordered(result_for_plot_sub$label, levels  = order)

#Making the plot
(plotS2 <- ggplot(data= result_for_plot_sub, aes(x=label, y=ES, ymin=L, ymax=U)) +
    geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
    #geom_hline(yintercept=0.373, lty=3, col="grey70") +  
    geom_pointrange(col= result_for_plot_sub$color_cat, size= 1) + 
    ylim(-0.24,0.45) +
    coord_flip() +  # flip coordinates (puts labels on y axis)
    labs(title = "Subset analysis (only papers focusing on a single journal)")+
    xlab("") +
    ylab("Effect size [r]")+
    theme_bw() + theme_custom)

#########################################################
# ANALYSIS BY SUBDISCIPLINE
#########################################################

#Citations
data_discipline <- Data2[Data2$Response == "Citations", ] 
data_discipline <- droplevels(data_discipline) 

#Making balanced levels
table(data_discipline$Discipline)

levels(data_discipline$Discipline) <- c("Remove",
                                        "Natural sciences",
                                        "Natural sciences",
                                        "Soft sciences",
                                        "Natural sciences",
                                        "Natural sciences",
                                        "Mathematical sciences",
                                        "Soft sciences",
                                        "Mathematical sciences",
                                        "Medicine",
                                        "Soft sciences",
                                        "Soft sciences"
)

data_discipline <- droplevels(data_discipline[data_discipline$Discipline != "Remove", ])

#Check sample size for each predictors
table_data_discipline  <- data.frame(predictor = NULL, n = NULL, n_papers = NULL)

for(i in 1:length(unique(levels(data_discipline$Predictor))))
  table_data_discipline <- rbind(table_data_discipline, 
                                 data.frame(predictor = levels(data_discipline$Predictor)[i],
                                            n = nrow(data_discipline[data_discipline$Predictor == levels(data_discipline$Predictor)[i], ]),
                                            n_papers = length(unique(data_discipline[data_discipline$Predictor == levels(data_discipline$Predictor)[i], ]$Reference)))  
                                 
  )

table_data_discipline
table(data_discipline$Discipline)

# Derive Fischer's Z and its variance -------------------------------------

data_discipline <- escalc(measure = "ZCOR", ri = r, ni = n, data = data_discipline)

# Estimate models in a loop -----------------------------------------------

table_data_discipline_sub <-  table_data_discipline[table_data_discipline$n_papers > 6,] #removing predictors with n_papers > 2*n disciplines

#Selecting either n > 50 or n_papers > 15
table_data_discipline_sub <-  table_data_discipline_sub[table_data_discipline_sub$n > 50 | table_data_discipline_sub$n_papers > 10,]
predictors_to_analyse_discipline2 <- table_data_discipline_sub$predictor

#Sort predictors_to_analyse_discipline in the same order of predictor to analyse
predictors_to_analyse_discipline <-  predictors_to_analyse[predictors_to_analyse %in% predictors_to_analyse_discipline2]

# Calculating the model
SUBSET3 <- list()
MODEL2  <- list()
result_for_plot_discipline <- data.frame(label = NULL,
                              b     = NULL,
                              ci.lb = NULL,
                              ci.ub = NULL,
                              ES    = NULL,
                              L     = NULL,
                              U     = NULL)

table_sup_mat_2 <-  data.frame(Predictor = NULL,
                               N         = NULL,
                               Beta_SE   = NULL,
                               CI        = NULL,
                               p         = NULL)

for (i in 1:length(predictors_to_analyse_discipline)){  
  
  #subset the predictor
  data_i  <- data_discipline[data_discipline$Predictor == predictors_to_analyse_discipline[i], ]
  data_i  <- na.omit(data_i)
  
  #fitting the model
  model_i <- rma.mv(yi, vi, mods = ~ Discipline, random = list( ~1 | Reference), data = data_i)
  
  #extracting coefficients
  result_for_plot_i <- data.frame(label = paste(predictors_to_analyse_discipline[i]," (" ,
                                                nrow(data_i),", ",
                                                length(unique(data_i$Reference)),")",sep=''),
                                  b     = model_i$b,
                                  ci.lb = model_i$ci.lb,
                                  ci.ub = model_i$ci.ub,
                                  ES    = ((exp(model_i$b)-1))/((exp(model_i$b)+1)),
                                  L     = ((exp(model_i$ci.lb)-1)/(exp(model_i$ci.lb)+1)),
                                  U     = ((exp(model_i$ci.ub)-1)/(exp(model_i$ci.ub)+1)))
  
  L <- length(model_i$beta)
  
  table_i <-  data.frame(Predictor = rep(predictors_to_analyse_discipline[i], L),
                         Names = rownames(model_i$beta),
                         N       = rep(nrow(data_i),L),
                         Beta_SE = paste(round(model_i$beta,2),"+/-", round(model_i$se,2),sep=''),
                         CI = paste(round(model_i$ci.lb,2), " | ", round(model_i$ci.ub,2),sep=''),
                         p = round(model_i$pval,5)
  )
  
  #store the data 
  SUBSET[[i]]     <- data_i
  MODEL2[[i]]      <- model_i
  result_for_plot_discipline <- rbind(result_for_plot_discipline,result_for_plot_i)
  table_sup_mat_2   <- rbind(table_sup_mat_2,table_i)
}#warning message because certain predictors don't have all levels of discipline

# Saving model estimates and validation -----------------------------------

#Storing the data for Table S3
write.csv(table_sup_mat_2,"TABLE_S3.csv")

# Generating the plot -----------------------------------------------------

# Renaming disciplines
result_for_plot_discipline <- data.frame(result_for_plot_discipline, 
                              Discipline = gsub("[[:digit:]]", "", rownames(result_for_plot_discipline)))


result_for_plot_discipline$Discipline <- as.factor(result_for_plot_discipline$Discipline)
rownames(result_for_plot_discipline) <- NULL

order <- result_for_plot_discipline$label
result_for_plot_discipline$label <- factor(result_for_plot_discipline$label,  levels  = unique(order))
result_for_plot_discipline$label <- ordered(result_for_plot_discipline$label, levels  = unique(order))

(plot2 <- ggplot(data= result_for_plot_discipline, aes(x=label, y=ES, ymin=L, ymax=U, col= Discipline)) +
    geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
    geom_pointrange(size = 1, position = position_dodge(width = 0.6)) + 
    scale_color_manual(labels = c("Mathematical sciences", "Medicine", "Soft Sciences", "Natural Sciences"),
                       values = c("grey30", "darkblue","magenta4","chartreuse3"))+
    coord_flip() +  # flip coordinates (puts labels on y axis)
    labs(title = "(b) Analysis by discipline")+
    xlab("") +
    ylab("Effect size [r]")+
    theme_bw() + theme_custom +
    theme(legend.title = element_blank(),
          legend.position = c(0.22, 0.88),
          legend.background = element_rect(fill="white",
                                           size=0.5, linetype="solid", 
                                           colour ="grey70"))
)

# Arrange plots in a grid -------------------------------------------------

plots <- list(plot1,plot2) ; grobs <- list() ; widths <- list()

for (i in 1:length(plots)){
  grobs[[i]] <- ggplotGrob(plots[[i]])
  widths[[i]] <- grobs[[i]]$widths[2:5]
}

maxwidth <- do.call(grid::unit.pmax, widths)

for (i in 1:length(grobs)){
  grobs[[i]]$widths[2:5] <- as.list(maxwidth)
}

pdf(file = "FIGURE_2.pdf", width = 14, height = 7)

do.call("grid.arrange", c(grobs, nrow = 1, ncol = 2))

dev.off()

#########################################################
# REPEATING THE TEMPORAL ANALYSIS WITH YEAR MAX
#########################################################

#Fitting the models
SUBSET    <- list()
MODEL     <- list()
MODEL_yr  <- list()

result_for_plot <- data.frame(label = NULL,
                              b     = NULL,
                              ci.lb = NULL,
                              ci.ub = NULL,
                              ES    = NULL,
                              L     = NULL,
                              U     = NULL)

table_sup_mat <-  data.frame(Predictor = NULL,
                             N         = NULL,
                             Beta_SE   = NULL,
                             CI        = NULL,
                             p         = NULL,
                             # model 2
                             Moderator_B_SE = NULL,
                             Moderator_CI   = NULL,
                             Moderator_p    = NULL)

for (i in 1:length(predictors_to_analyse)){  
  
  #subset the predictor
  data_i  <- data_citation[data_citation$Predictor == predictors_to_analyse[i], ]
  
  #fitting the model
  model_i <- rma.mv(yi, vi, random =  ~ 1 | Reference, data = na.omit(data_i)) 
  
  #fitting a second model considering Year as a moderator
  model_i_yr <- rma.mv(yi, vi, mods = ~ Yr_max, random =  ~ 1 | Reference, data = na.omit(data_i)) 
  
  #extracting coefficients
  result_for_plot_i <- data.frame(label = paste(predictors_to_analyse[i]," (" ,
                                                nrow(data_i),", ",
                                                length(unique(data_i$Reference)),")",sep=''),
                                  b     = model_i$b,
                                  ci.lb = model_i$ci.lb,
                                  ci.ub = model_i$ci.ub,
                                  ES    = ((exp(model_i$b)-1))/((exp(model_i$b)+1)),
                                  L     = ((exp(model_i$ci.lb)-1)/(exp(model_i$ci.lb)+1)),
                                  U     = ((exp(model_i$ci.ub)-1)/(exp(model_i$ci.ub)+1)))
  
  table_i <-  data.frame(Predictor = predictors_to_analyse[i],
                         N       = nrow(data_i),
                         Beta_SE = paste(round(model_i$beta,2),"+/-", round(model_i$se,2),sep=''),
                         CI = paste(round(model_i$ci.lb,2), " | ", round(model_i$ci.ub,2),sep=''),
                         p = round(model_i$pval,2),
                         #model 2
                         Moderator_B_SE = paste(round(model_i_yr$beta[2],5),"+/-", round(model_i_yr$se[2],5),sep=''),
                         Moderator_CI   = paste(round(model_i_yr$ci.lb[2],5), " | ", round(model_i_yr$ci.ub[2],5),sep=''),
                         Moderator_p    = round(model_i_yr$pval[2],5)
  )
  
  #store the data 
  SUBSET[[i]]     <- data_i
  MODEL[[i]]      <- model_i
  MODEL_yr[[i]]   <- model_i_yr
  result_for_plot <- rbind(result_for_plot,result_for_plot_i)
  table_sup_mat   <- rbind(table_sup_mat,table_i)
}

# Model summary -----------------------------------------------------------

for (i in 1:length(predictors_to_analyse)){ 
  message(paste("---", predictors_to_analyse[[i]], "---", sep = ' '))
  if(MODEL_yr[[i]]$pval[2] < 0.05)
    print(MODEL_yr[[i]]) 
  else
    print("year not significant")
}

# Generating the plot -----------------------------------------------------

#Color for the plot
result_for_plot       <- data.frame(result_for_plot,color_cat) 
order                 <- result_for_plot$label
result_for_plot$label <- factor(result_for_plot$label,  levels  = order)
result_for_plot$label <- ordered(result_for_plot$label, levels  = order)

#Estimating the direction of the temporal trend, and other graphical parameters
arrow_direction <- c()
arrow_shade     <- c()
arrow_size      <- c()

for (i in 1:length(predictors_to_analyse)){ 
  
  if(MODEL_yr[[i]]$beta[2] > 0)
    arrow_direction <- c(arrow_direction, "first")
  else
    arrow_direction <- c(arrow_direction, "last")
  
  if(MODEL_yr[[i]]$pval[2] < 0.05){
    arrow_shade <- c(arrow_shade,1)
    arrow_size <- c(arrow_size,1)
  }else{ 
    arrow_shade <- c(arrow_shade,0.6)
    arrow_size <- c(arrow_size,0.5)}
}

#Making the plot
(plot3 <- ggplot(data= result_for_plot, aes(x=label, y=ES, ymin=L, ymax=U)) +
    geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
    #geom_hline(yintercept=0.373, lty=3, col="grey70") +  
    geom_pointrange(col= result_for_plot$color_cat, size= 1) + 
    ylim(-0.24,0.45) +
    annotate("segment", 
             x    = c(1:length(predictors_to_analyse)), 
             xend = c(1:length(predictors_to_analyse)), 
             y    = 0.44, 
             yend = 0.41, 
             colour = result_for_plot$color_cat, 
             size = arrow_size, 
             alpha = arrow_shade, 
             arrow=arrow(ends = arrow_direction,angle = 45, length = unit(.2,"cm")))+
    coord_flip() +  # flip coordinates (puts labels on y axis)
    labs(title = "Analysis with year maximum")+
    xlab("") +
    ylab("Effect size [r]")+
    theme_bw() + theme_custom)

pdf(file = "FIGURE_S2.pdf", width = 9, height = 7)

plot3

dev.off()
