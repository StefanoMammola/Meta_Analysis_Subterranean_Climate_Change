###############################################################

## A global meta-analysis on the biological impacts of climate change in subterranean ecosystems
# Ilaria Vaccarelli, Raquel Colado, David Sanchez-Fernandez, Diana M. P. Galassi, Susana Pallarés, Mattia Di Cicco, Melissa B. Meierhofer, Elena Piano, Stefano Mammola

## ------------------------------------------------------------------------
# 'R script to reproduce the analyses'
## ------------------------------------------------------------------------

# Analysis performed with R (v. R 4.1.0) and R studio (v. 1.4.1103)
# Authors (code): Stefano Mammola

###############################################################

# clean the workspace -----------------------------------------------------

rm(list = ls())

# Loading R package -------------------------------------------------------

library("dplyr")
library("metafor")   
library("ggplot2")
library("ggpubr")
library("ggthemes")
library("png")
library("tidyverse")
library("tidylog")

# Loading pictures ------------------------------------------------------

Badino_2004 <- png::readPNG("Pictures/Badino_2004.png")

# Sourcing useful functions ------------------------------------------------

source("Functions/Custom_functions.r")

###############################################################
## Data preparation:
###############################################################

# Loading the Databases ---------------------------------------------------

# Literature database
db.pub <-
  read.csv(
    file = "Data/publications.csv",
    sep = '\t',
    dec = ',',
    header = TRUE,
    as.is = FALSE
  )

str(db.pub)

# Database for meta analaysis
db.meta <-
  read.csv(
    file = "Data/meta_analysis.csv",
    sep = '\t',
    dec = ',',
    header = TRUE,
    as.is = FALSE
  )

str(db.meta)

# Database with only one estimate / paper
db.meta.distinct <- db.meta %>% 
                    dplyr::distinct(Paper_ID, .keep_all = TRUE) 

# Extracting temporal range of each study ---------------------------------
# Extracting year for each study ------------------------------------------

#Calculating year range for the dataset analysed by each scientometric article

levels(db.meta$Year)

#Extracting range with a loop
Yr_min   <- c()
Yr_max   <- c()
Yr_range <- c()

for (i in 1 : nrow(db.meta)){
  
  Data_i   <- db.meta[i,]
  Year     <- as.character(Data_i$Year)
  Year     <- gsub(";", "-", Year) #replace comma if needed
  Year     <- as.numeric(strsplit(Year, "-")[[1]]) #split the year range
  
  if(is.na(Year) == TRUE) { 
    Yr_min   <- c(Yr_min, NA)
    Yr_max   <- c(Yr_max, NA)
    Yr_range <- c(Yr_range, NA) } else {
    Yr_min   <- c(Yr_min, range(Year)[1])
    Yr_max   <- c(Yr_max, range(Year)[2])
    Yr_range <- c(Yr_range, (range(Year)[2] - range(Year)[1])) }
  
} #Warnings() occur when the range is just a single number (i.e. no range)

# check the values
range(Yr_max,   na.rm = TRUE)
range(Yr_min,   na.rm = TRUE)
range(Yr_range, na.rm = TRUE)

db.meta <- data.frame(db.meta, Yr_min, Yr_max, Yr_range) ; head(db.meta)

# Summary stats -----------------------------------------------------------

# How many papers?
db.pub %>% dplyr::distinct(Paper_ID, .keep_all = TRUE) %>% nrow()

# How many papers for meta-analysis?
db.meta.distinct %>% nrow()

# How many email requests?
db.pub[db.pub$Corresponding_emailed == "yes",] %>% dplyr::select(Answer) %>% table()

# What is the mean number of estimates/paper? 
mean(table(db.meta$Paper_ID)) ; SE(table(db.meta$Paper_ID))

# How many papers for different systems / taxa?
db.meta.distinct %>% dplyr::select(Domain) %>% table()
db.meta.distinct %>% dplyr::select(System_specific) %>% table()
db.meta %>% dplyr::select(Ecology_group) %>% table()

###############################################################
## Figures:
###############################################################

# Map ---------------------------------------------------------------------

#adding stas to be plotted
db.pub <- db.pub %>%
  dplyr::left_join(db.meta.distinct %>%
                     dplyr::select(Paper_ID, Domain),
                   by = "Paper_ID") %>%
  dplyr::left_join(db.meta %>%
                     group_by(Paper_ID) %>%
                     summarise(n = n()),
                   by = "Paper_ID")

# Load world map
world <- ggplot2::map_data("world")

(plot.map <- ggplot() +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           color = "grey20", fill = "grey40", size = 0.1) +
  ylim(-50,90)+
  geom_point(data = db.pub,
             aes(jitter(Longitude,4), 
                 jitter(Latitude,4), 
                 fill = Study_type), 
             size = 4,
             alpha = 0.7, 
             shape = 21, color = "black") +
  scale_fill_manual("Type of study",values = c("turquoise","orange","white","blue"))+
    guides(fill=guide_legend(title=NULL),
           size=guide_legend(title=NULL))+
    #scale_y_continuous(breaks = (-2:2) * 30) +
    scale_x_continuous(breaks = (-4:4) * 45) +
  ggthemes::theme_map()+
    theme(legend.position = c(0.05,0.2),
          legend.background=element_rect(fill = alpha("white", 0))))

# Temporal trend ----------------------------------------------------------

(plot.year <- ggplot(db.pub %>% 
                       dplyr::distinct(Paper_ID, .keep_all = TRUE) %>% 
                       group_by(Yr = Year_publication) %>% 
                       summarise(n = n()) %>% 
                       mutate(csum = cumsum(n)),
                     aes(x=Yr, y=n)) + 
    geom_line(linetype = 1, alpha = 1, col = "black")+
    geom_point(size =2, shape = 21, alpha = 1, col = "black", fill = "grey20")+
    # geom_smooth(method = "glm", se = FALSE,color= "orange", linetype = 2,
    #             method.args = list(family = "poisson"))+
    scale_x_continuous(breaks = seq(from=min(db.pub$Year_publication),
                                      to=max(db.pub$Year_publication),by=4))+ 
    scale_y_continuous(breaks = seq(from=0,to=10,by=1))+
   
   # annotation_custom(grid::rasterGrob(Badino_2004),
   #                   xmin = unit(1990, "native"), xmax = unit(2010,"native"),
   #                   ymin = unit(3,"npc"),  ymax = unit(5,"npc"))+
   # 
    labs(x = NULL,
         y = "Number of studies")+
    theme_classic())

# Data exploration --------------------------------------------------------

###############################################################

## Meta-Analysis

###############################################################

db.metafor <- db.meta %>% dplyr::select(Paper_ID, 
                                        Domain,
                                        Yr = Yr_range,
                                        Phylum,
                                        Ecology  = Ecology_group,
                                        Response = Response_Group,
                                        Type = Response_macrogroup,
                                        N,
                                        r = Pearson_r_conversion)

# Derive Fischer's Z and its variance
db.metafor <- metafor::escalc(measure = "COR", ri = r, ni = N, data = db.metafor)

#Check sample size for each predictors
table_estimates <- data.frame(predictor = NULL, n = NULL, n_papers = NULL)

for(i in 1:length(unique(levels(db.metafor$Response))))
  table_estimates <- rbind(table_estimates,
                          data.frame(
                            predictor = levels(db.metafor$Response)[i],
                            n = nrow(db.metafor[db.metafor$Response == levels(db.metafor$Response)[i],]),
                            n_papers = length(
                              unique(db.metafor[db.metafor$Response == levels(db.metafor$Response)[i],]$Paper_ID)
                          )
  ))

# Removing predictors with a single study
db.metafor <- db.metafor[!(db.metafor$Response %in% table_estimates[table_estimates$n_papers < 2,]$predictor),]
db.metafor <- droplevels(db.metafor) #dropped a study on Morphology

# Fitting the metafor models ----------------------------------------------

#Fitting the models
MODEL     <- list()
MODEL2    <- list()

i=13

for (i in 1 : nlevels(db.metafor$Response)){  
  
  # Subset the predictor
  data_i  <- db.metafor[db.metafor$Response == levels(db.metafor$Response)[i], ]
  
  # Fitting the model
  model_i <- metafor::rma.mv(yi, vi, random =  ~ 1 | Paper_ID, data = na.omit(data_i),
                             control=list(rel.tol=1e-8)) 
  
  # Validation
  failsafe_i <- fsn(yi, vi, type = "Rosenthal", data = na.omit(data_i)) 
  
  # Extracting coefficients
  result_for_plot_i <- data.frame(label = paste(levels(db.metafor$Response)[i],
                                                " (" ,
                                                nrow(data_i),", ",
                                                length(unique(data_i$Paper_ID)),")",sep=''),
                                  n_studies =  length(unique(data_i$Paper_ID)),
                                  Type = data_i$Type[1],
                                  b     = model_i$b,
                                  ci.lb = model_i$ci.lb,
                                  ci.ub = model_i$ci.ub,
                                  ES    = ((exp(model_i$b)-1))/((exp(model_i$b)+1)),
                                  L     = ((exp(model_i$ci.lb)-1)/(exp(model_i$ci.lb)+1)),
                                  U     = ((exp(model_i$ci.ub)-1)/(exp(model_i$ci.ub)+1)),
                                  p = round(model_i$pval,4),
                                  failsafe_N = failsafe_i$fsnum,
                                  failsafe_p = round(failsafe_i$pval,3))
  
  table_i <-  data.frame(Predictor = levels(db.metafor$Response)[i],
                         N       = nrow(data_i),
                         Beta_SE = paste(round(model_i$beta,2),"+/-", round(model_i$se,2),sep=''),
                         CI = paste(round(model_i$ci.lb,2), " | ", round(model_i$ci.ub,2),sep=''),
                         p = round(model_i$pval,4))
  
  # Fitting the model with moderathors
  model2_i <- rma.mv(yi, vi, mods = ~ Ecology, 
                     random = ~ 1 | Paper_ID, 
                     data = na.omit(data_i),
                     control = list(rel.tol=1e-8))

  #Adding missing level in the database
  data_i_eco  <- data_i[data_i$Paper_ID %in% model2_i$mf.r[[1]]$Paper_ID,]
  data_i_eco  <- droplevels(data_i_eco)
  
  if(nlevels(data_i_eco$Ecology) == 1) {
    Ecology_i <- levels(data_i_eco$Ecology)
  }
  else if(nlevels(data_i_eco$Ecology) < 3){
    non.baseline <- gsub('[Ecology]','', rownames(model2_i$b)[2])
    Ecology_i <- levels(data_i_eco$Ecology)
    
    baseline <- Ecology_i[! Ecology_i %in% non.baseline]
    Ecology_i <- c(baseline,non.baseline)
    
  } else { Ecology_i <- levels(data_i_eco$Ecology) }

  # Extracting coefficients
  result_for_plot2_i <- data.frame(label = paste(levels(db.metafor$Response)[i],
                                                " (" ,
                                                nrow(data_i),", ",
                                                length(unique(data_i$Paper_ID)),")",sep=''),
                                  Type = rep(data_i$Type[1],length(Ecology_i)),
                                  Ecology = Ecology_i,
                                  b     = model2_i$b,
                                  ci.lb = model2_i$ci.lb,
                                  ci.ub = model2_i$ci.ub,
                                  ES    = ((exp(model2_i$b)-1))/((exp(model2_i$b)+1)),
                                  L     = ((exp(model2_i$ci.lb)-1)/(exp(model2_i$ci.lb)+1)),
                                  U     = ((exp(model2_i$ci.ub)-1)/(exp(model2_i$ci.ub)+1)),
                                  p = round(model2_i$pval,4))
  
  table2_i <-  data.frame(Predictor = levels(db.metafor$Response)[i],
                         N       = nrow(data_i),
                         Beta_SE = paste(round(model2_i$beta,2),"+/-", round(model2_i$se,2),sep=''),
                         CI = paste(round(model2_i$ci.lb,2), " | ", round(model2_i$ci.ub,2),sep=''),
                         p = round(model_i$pval,2))
  
  
  # Store the data 
  MODEL[[i]]      <- model_i
  MODEL2[[i]]      <- model2_i
  
  # Store tables
  if(i > 1) {    
    result_for_plot <- rbind(result_for_plot,result_for_plot_i)
    table_sup_mat   <- rbind(table_sup_mat,table_i)
    result_for_plot2 <- rbind(result_for_plot2,result_for_plot2_i)
    table_sup_mat2   <- rbind(table_sup_mat2,table2_i)
    } else {
  result_for_plot <- result_for_plot_i
  table_sup_mat   <- table_i
  
  result_for_plot2 <- result_for_plot2_i
  table_sup_mat2   <- table2_i
    }
}
rm(i, result_for_plot_i, result_for_plot2_i, table_i, table2_i, data_i_eco, data_i)
#warnings()

# renaming Response group as in the result_for_plot
levels(db.metafor$Response) <- levels(as.factor(result_for_plot$label))
db.metafor$Yr <- log(db.metafor$Yr +1)

# Plotting ----------------------------------------------------------------

#Arrange
result_for_plot$Type <- 
  factor(result_for_plot$Type, levels = c("Behaviour","Physiology","Population/Community","Habitat")) #Sort

result_for_plot <- result_for_plot %>% arrange(Type, .by_group = FALSE)

result_for_plot$label <- 
  factor(result_for_plot$label, 
         levels = rev(as.character(result_for_plot$label))) #Sort

db.metafor$Response <- 
  factor(db.metafor$Response, 
         levels = result_for_plot$label) #Sort

colors.type <- c("darkorange","grey10","blue","darkmagenta")
color.num <- table(result_for_plot$Type)

color.axis.y <- c(rep(colors.type[2], color.num[4]),
                  rep(colors.type[4], color.num[3]),
                  rep(colors.type[3], color.num[2]), #
                  rep(colors.type[1], color.num[1])) #

face.axis.y <- ifelse(result_for_plot$p > 0.05, "plain", "bold")

(forest_plot1 <- 
    result_for_plot %>%
    ggplot2::ggplot(aes(x = ES, y = label, fill = Type, col = Type)) + 
    geom_vline(lty = 3, size = 0.5, col = "grey50", xintercept = 0) +
    geom_jitter(data = db.metafor, 
                aes(y = Response, x = yi, col = Type, shape = Domain), 
                alpha = 0.2, stroke = .8, size = 2,  height = 0.1, width = 0.5)+
    geom_errorbar(aes(xmin = L, xmax = U), size = 1.5, width = 0)+
    geom_point(size = 4, pch = 21) +
    
    # geom_text(aes(col = Type),label = paste0(round(table.plot.M1.2$Beta, 3), sign.M1.2, sep = "  "), 
    #           vjust = - 1, size = 3) +
    labs(x = expression(paste("Effect size [r]" %+-% "95% Confidence interval")),
         y = NULL) + 
    scale_color_manual("", 
                       values = colors.type)+
    scale_fill_manual("", 
                       values = colors.type)+
    
    scale_shape_manual("", values = c(21,24))+
    #guides(color = TRUE,fill = TRUE)+
    theme_bw() + 
    theme(legend.position = "right", 
          legend.direction = "vertical",
          legend.text = element_text(size = 8),
          axis.title = element_text(size = 12),
          axis.line.x = element_line(color="grey10"), 
          axis.line.y = element_line(color="grey10"),
          axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10, 
                                     color = rev(color.axis.y),
                                     face = face.axis.y),
          #panel.border = element_blank(),
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          # panel.grid.minor.y = element_blank(),
          # panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(rep(0.4,4)), units = , "cm")
          )
)

# Renaming disciplines

#Arrange
result_for_plot2$Type <- 
  factor(result_for_plot2$Type, levels = c("Behaviour","Physiology","Population/Community","Habitat")) #Sort

result_for_plot2 <- result_for_plot2 %>% arrange(Type, .by_group = FALSE)

result_for_plot2$label <- 
  factor(result_for_plot2$label, 
         levels = unique(result_for_plot2$label)) #Sort

(forest_plot2 <- 
    result_for_plot2 %>%
    ggplot2::ggplot(aes(x = ES, 
                        y = label,
                        shape = Ecology)) + 
    geom_vline(lty = 3, size = 0.5, col = "grey50", xintercept = 0) +
    
    geom_errorbar(aes(xmin = L, xmax = U),col = "grey10", size = .5, width = 0, position = position_dodge(width = 0.6))+
    geom_point(aes(fill = Ecology), size = 3, col = "grey10", stroke = .5, position = position_dodge(width = 0.6)) +
    
    labs(x = expression(paste("Effect size [r]" %+-% "95% Confidence interval")),
         y = NULL) +
    
    # scale_color_manual("", 
    #                    values = colors.type)+
    scale_fill_manual("",
                      values = c("grey10","mediumorchid2","lightcyan1"))+

    
    scale_shape_manual("", values = c(21,22,24))+
    theme_bw() + 
    theme(legend.position = "right", 
          legend.direction = "vertical",
          legend.text = element_text(size = 8),
          axis.title = element_text(size = 12),
          axis.line.x = element_line(color="grey10"), 
          axis.line.y = element_line(color="grey10"),
          axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10, 
                                     color = rev(color.axis.y)),
                                     #face = face.axis.y),
          #panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          # panel.grid.minor.y = element_blank(),
          # panel.grid.major.y = element_blank(),
          plot.margin = unit(c(rep(0.4,4)), units = , "cm")
    ))

pdf(file = "Figures/Figure_1.pdf", width = 8, height = 5)
plot.map
dev.off()

pdf(file = "Figures/Figure_2.pdf", width = 8, height = 5)
forest_plot1
dev.off()

pdf(file = "Figures/Figure_3.pdf", width = 8, height = 5)
forest_plot2
dev.off()

# result_for_plot2 <- data.frame(result_for_plot2,
#                                          Ecology2 = gsub("[[:digit:]]", "", rownames(result_for_plot2)))
# 
# result_for_plot2$Ecology2 <- as.factor(result_for_plot2$Ecology2)
# 
# (forest_plot2 <- 
#     result_for_plot2 %>%
#     ggplot2::ggplot(aes(x = ES, y = label, col = Ecology2, fill = Ecology2)) + 
#     geom_vline(lty = 3, size = 0.5, col = "grey50", xintercept = 0) +
#     
#     geom_errorbar(aes(xmin = L, xmax = U), size = 1, width = 0, position = position_dodge(width = 0.6))+
#     geom_point(size = 3, pch = 21,position = position_dodge(width = 0.6)) +
#     
#     labs(x = expression(paste("Effect size [r]" %+-% "95% Confidence interval")),
#          y = NULL) +
#     
#     theme_classic() + theme(#legend.position = "none",
#       
#       strip.text.x = element_text(size = 12),
#       #axis.text.y = element_text(colour = rev(color.axis),size = 12), 
#       axis.text.x = element_text(size = 11),
#       axis.title = element_text(size = 13))
#   
# )


# Modelling
for (j in 1:length(actions_to_analyse)){ 
  
  data_j  <- db_metafor[db_metafor$Predictor_Group == actions_to_analyse[j], ] ; data_j <- droplevels(data_j)
  
  predictors_to_analyse <- levels(data_j$Response_Group)
  
  for (i in 1:length(predictors_to_analyse)){  
    
    #subset the predictor
    data_i  <- data_j[data_j$Response_Group == predictors_to_analyse[i], ]
    
    if(nrow(data_i)<2) { NULL } else {
    
    #fitting the model
    model_i  <- rma.mv(yi, vi, random =  ~ 1 | ID, data = na.omit(data_i)) 
    
    failsafe_i <- fsn(yi, vi, type = "Rosenthal", data = na.omit(data_i)) 
    
    #extracting coefficients
    result_for_plot_i <- data.frame(label_action =  actions_to_analyse[j],
                                  label_pred = paste(predictors_to_analyse[i]," (" ,
                                                  nrow(data_i),", ",
                                                  length(unique(data_i$ID)),")",sep=''),
                                    size = length(unique(data_i$ID)),
                                    b     = model_i$b,
                                    beta  = round(model_i$beta[1],3),
                                    se    = round(model_i$se[1],3),
                                    z      = round(model_i$zval[1],3),
                                    p     = round(model_i$pval[1],3),
                                    ci.lb = round(model_i$ci.lb,3),
                                    ci.ub = round(model_i$ci.ub,3),
                                    ES    = ((exp(model_i$b)-1))/((exp(model_i$b)+1)),
                                    L     = ((exp(model_i$ci.lb)-1)/(exp(model_i$ci.lb)+1)),
                                    U     = ((exp(model_i$ci.ub)-1)/(exp(model_i$ci.ub)+1)),
                                  failsafe_N = failsafe_i$fsnum,
                                  failsafe_p = round(failsafe_i$pval,3)
                                    )
    
    #store the data 
    SUBSET[[j - 1 + num + i]]     <- data_i
    MODEL[[j - 1 + num + i]]      <- model_i
    result_for_plot <- rbind(result_for_plot,result_for_plot_i)
    }}
  
 num <- length(MODEL) - j
  
}

#Evaluation of publication bias via Rosenthal’s method.

ros_N <- c()
ros   <- c()

for (i in 1 : length(SUBSET)){ 
  
  Ros_i <- fsn(yi, vi, type = "Rosenthal", data = SUBSET[[i]])  
  
  ros_N <- c(ros_N,Ros_i$fsnum)
  ros   <- c(ros,round(Ros_i$pval,3))
  
  message(paste("---",  SUBSET[[i]]$Predictor_Group[1], "---",  SUBSET[[i]]$new_name[1],  sep = ' '))
  print(Ros_i) #Rosenthal's failsafe number (rule-of-thumb: it should be larger than n*5 + 10)
}

# plot -----------------------------

rownames(result_for_plot) <- NULL

ORDER <- as.character(result_for_plot$label_pred)

result_for_plot$label_pred <- factor(result_for_plot$label_pred, ORDER) #sort

#save a table

table_3 <- result_for_plot %>% dplyr::select(label_action, label_pred, beta, se, ci.lb, ci.ub, z, p, failsafe_N, failsafe_p)

table_3$ci.lb <- paste0("[",table_3$ci.lb, " — ",table_3$ci.ub, "]") 
table_3$beta <-  paste0(table_3$beta, " +/- ", table_3$se)
table_3 <- table_3 %>% dplyr::select(-c(se,ci.ub))
table_3$failsafe_p <- rep("<0.001",nrow(table_3))

colnames(table_3) <- c("Conservation action", 
                       "Response", 
                       "Beta +/- SE", "95% CI", "z", "p", "failsafe [N]", "failsafe [p]")

write.table(table_3, "Tables/Table_3.csv", append = FALSE, sep = "\t", dec = ".")

#Converting multiple families as multiple
genus_split <- strsplit(as.character(db_metafor$Genus_specific), ";")

genus <- c()
for(i in 1:length(genus_split))
  genus <- c(genus, ifelse(length(genus_split[[i]]) > 1, "Multiple", genus_split[[i]]) )

db_metafor$Genus_specific <- genus

# renaming Response group as in the result_for_plot
new_name <- factor(paste(db_metafor$Predictor_Group, db_metafor$Response_Group))
levels(new_name) <- ORDER

db_metafor <- data.frame(db_metafor,new_name)
colnames(db_metafor)

db_metafor$Genus_specific <- as.factor(db_metafor$Genus_specific)
db_metafor$Genus_specific <- relevel(db_metafor$Genus_specific , ref = "Multiple")

(meta_analysis <- ggplot(data= result_for_plot) +
     geom_hline(yintercept = 0, lty = 2, col = "grey50") +  # add a dotted line at x=1 after flip
     xlab("")+
     ylab("Effect size [r]")+
     geom_jitter(data = db_metafor, aes(x = new_name, y = r, shape = Genus_specific, col = Predictor_Group), 
                 size = 1.5, width = 0.2)+
     geom_pointrange(aes(x = label_pred, y = ES, ymin = L, ymax = U, col = label_action), size = 1) + 
     scale_color_manual("Conservation action", values = c("darkmagenta","grey10","darkcyan", "darkorange", "blue"))+
     scale_shape_manual("Taxon", values = c(1:8))+ #, guide = guide_legend(element_text(face = "italic")
     coord_flip() + 
     theme_custom() + theme(legend.position = "right", 
                            legend.direction = "vertical",
                            legend.text = element_text(face= c("italic")),
                            legend.title = element_text(size = 12, face = "bold"),
                            axis.text.y = element_text(face= c("plain","plain","bold","plain","bold", "plain", "plain"))))

#Save figure
pdf(file = "Figure/Figure_3.pdf", width = 9, height =5)
meta_analysis
dev.off()

# Action by region
COL =  c("grey20","darkorange")

geo_action <- semi_colon_splitter3(input1 = db$Higher_Geography,
                                   input2 = db$Conservation_Action,
                                   input3 = db$Tested_statistically,
                                    names = c("Geography","Action","Test")
                                   )

geo_action <- na.omit(geo_action)

bar_1 <- data.frame(table(geo_action$Geography,geo_action$Action,geo_action$Test))

colnames(bar_1) <- c("geo","action","test","N")

bar_1 <- bar_1[bar_1$geo != "Global",] ; bar_1 <- droplevels(bar_1)

bar_1$action <- factor(bar_1$action,levels = 
                       c("Assessment", "Education","Legislation","Monitoring","Prioritization",
                         "Risk assessment",
                         "Decontamination","Eradication","Gating",
                         "Habitat creation","Habitat restoration",
                         "Protected area",  "Regulate access"
                         ))

(bar_p1 <-  ggplot(bar_1, aes(x = action, y = N, fill = test)) +
    facet_wrap( ~ geo, nrow = 2, ncol = 3) +
    geom_bar(stat="identity",position= "stack", color = "grey10")+
    scale_fill_manual("",labels=c("Not tested", "Tested"),values = COL)+
    labs(title=NULL, subtitle = NULL,x=NULL, y = "Frequency")+
    geom_vline(xintercept = 6.5, linetype="dotted",
               color = "grey40", size=0.4)+
    theme_custom()+
    theme(legend.position =  "bottom",
          axis.text.x = element_text(angle = 0, vjust = 1, hjust=1),
          plot.margin = unit(c(0.2,0.2,0.2,0.2), 'cm'),
          strip.text.x=element_text(color = "grey10", face = "bold", size=12),
          strip.background = element_rect(colour=NA, fill=NA)
          ) + coord_flip()
)

pdf(file = "Figure/Figure_1.pdf", width = 12, height = 7)
bar_p1
dev.off()

# Action by region
geo_impact <- semi_colon_splitter3(input1 = db$Higher_Geography,
                                   input2 = db$Impact,
                                   input3 = db$Tested_statistically,
                                    names = c("Geography","Impact","Test")
                                   )

geo_impact <- na.omit(geo_impact)

bar_2 <- data.frame(table(geo_impact$Geography,geo_impact$Impact, geo_impact$Test))

colnames(bar_2) <- c("geo","Impact","test", "N")

bar_2 <- bar_2[bar_2$geo != "Global",] ; bar_2 <- droplevels(bar_2)

bar_2$Impact <- factor(bar_2$Impact,levels = 
                         c("None identified",
                           "Multiple",
                           "Alien species",
                           "Climate change",
                           "Pathogen",                    
                           "Poaching",
                           "Pollution",
                           "Subterranean habitat change",
                           "Surface habitat change",     
                           "Visitors"))

(bar_p2 <-  ggplot(bar_2, aes(x = Impact, y = N, fill = test)) +
    facet_wrap( ~ geo, nrow = 2, ncol = 3) +
    scale_fill_manual("",labels=c("Not tested", "Tested"),values = COL)+
    geom_bar(stat="identity",position= "stack", color = "grey10")+
    labs(title=NULL, subtitle = NULL,x=NULL, y = "Frequency")+
    theme_custom()+
    theme(legend.position =  "bottom",
          axis.text.x = element_text(angle = 0, vjust = 1, hjust=1),
          plot.margin = unit(c(0.2,0.2,0.2,0.2), 'cm'),
          strip.text.x=element_text(color = "grey10", face = "bold", size=12),
          strip.background = element_rect(colour=NA, fill=NA)) + coord_flip()
)

pdf(file = "Figure/Figure_2.pdf", width = 12, height = 7)
bar_p2
dev.off()

# Network -----------------------------------------------------------------

db_full <-
  read.csv(
    file = "Data/Master_Database_Cave_Conservation.csv",
    sep = '\t',
    dec = ',',
    header = TRUE,
    as.is = FALSE
  )

# Removing duplicates

db_full <- db_full[db_full$Remove != "yes",] ; db_full <- droplevels(db_full)

# removing aquatic organisms
db_full <- db_full[db_full$System != "Anchialine/Marine",] ; db_full <- droplevels(db_full)
db_full <- db_full[db_full$System != "Groundwater",] ; db_full <- droplevels(db_full)

# removing indirect conservation actions
db_full <- db_full[db_full$Conservation_Group != "Education",] ; db_full <- droplevels(db_full)
db_full <- db_full[db_full$Conservation_Group != "Assessment",] ; db_full <- droplevels(db_full)
db_full <- db_full[db_full$Conservation_Group != "Monitoring",] ; db_full <- droplevels(db_full)

db_full <- db_full[db_full$Taxon_Group != "Not specific",] ; db_full <- droplevels(db_full)

db_network <- semi_colon_splitter(input1 = db_full$Taxon_Group,
                                  input2 = db_full$Conservation_Action, 
                                  names = c("Taxon","Action"))

# Sample size
nrow(db_network) ; length(unique(levels(db_full$ID)))

# Renaming arthropoda
levels(db_network$Taxon)[1]  <- "Arthropods"

#Sorting
db_network$Taxon <- factor(db_network$Taxon, levels =
                             c("Bats",levels(db_network$Taxon)[c(-2)]))

Graph_bipartite <- db_network %>% 
  dplyr::select(Taxon,Action) %>% 
  table() %>% 
  igraph::graph_from_incidence_matrix(directed = TRUE) %>% 
  tidygraph::as_tbl_graph(directed = TRUE)

bipartite_matrix <- db_network %>% 
  dplyr::select(Taxon,Action) %>% 
  table()

bipartite_matrix <- matrix(bipartite_matrix, ncol = ncol(bipartite_matrix), dimnames = dimnames(bipartite_matrix))

animal_jaccard <- BAT::beta(bipartite_matrix, abund = TRUE)$Btot
animal_jaccard <- as.matrix(animal_jaccard)   
diag(animal_jaccard) <-0

animal_jaccard = 1-animal_jaccard


arthropods <- animal_jaccard[2,-c(2)]

animal_jaccard_net <- graph_from_adjacency_matrix(animal_jaccard, 
                                         mode = "undirected",
                                         weighted = TRUE) %>% as_tbl_graph()

#Add sample size
N <- data.frame(table(db_network$Taxon)) ; colnames(N) <- c("name","N")

N <- cbind(N, Bat = c(TRUE, rep(FALSE,nrow(N)-1)))

animal_jaccard_net <- animal_jaccard_net %>% activate(nodes) %>%  dplyr::left_join(N, by = "name")

animal_jaccard_net <- animal_jaccard_net %>% activate(edges) %>% mutate(Bat = edge_is_from(1))

(plot <- ggraph::ggraph(animal_jaccard_net, layout = "gem") +
  geom_edge_arc(aes(width = weight, 
                    col = Bat), alpha = 0.3, strength = .1) +
  
  geom_node_point(aes(size = N, col = Bat), 
                  
                  alpha = 0.6, 
                  shape = 19
                  ) + 
  
    geom_node_text(aes(label = name), size=6, color="gray10", repel=TRUE, force = 20) +
  scale_edge_width_continuous("Similarity",range=c(0.6,3))+
  scale_size_continuous("Sample size",range=c(3,8), breaks =c(10,100,400))+
  scale_edge_colour_manual(values = COL)+
  scale_colour_manual(values = COL)+
  
    theme_void() + theme(legend.position = "right",
                         legend.direction = "vertical", 
                         plot.margin = unit(c(3, 3, 3, 3), units = , "cm")) + guides(colour = "none", edge_colour ="none"))

pdf(file = "Figure/Network.pdf", width = 9, height = 7)
plot
dev.off()

## APPUNTI:::



# Conservation actions by Family ------------------------------------------

levels(db$Higher_Geography)
family_action <- semi_colon_splitter(input1 = db$Family,
                            input2 = db$Conservation_Action, 
                            names = c("Family","Action"))


family_action <- na.omit(family_action)

Graph_bipartite <- family_action %>% 
  dplyr::select(Family,Action) %>% 
  table() %>% 
  igraph::graph_from_incidence_matrix(directed = FALSE) %>% 
  tidygraph::as_tbl_graph(directed = FALSE)

# Graph_bipartite <- Graph_bipartite %>% tidygraph::activate(nodes) %>% 
#   left_join(rbind(data.frame(table(db$Taxon)),
#                   data.frame(table(db$Conservation_Action))), by = c("name" = "Var1"))


# Collapse it into an unipartite 
Graph_unipartite_full <- igraph::bipartite_projection(Graph_bipartite)

# Takes the unipartite project graph
Graph_unipartite <- Graph_unipartite_full$proj1  %>% as_tbl_graph(directed = FALSE) %>% 
  activate(edges) %>% #%>% mutate(weight = 1) 
  igraph::simplify(edge.attr.comb = "sum") %>% 
  as_tbl_graph


########################
# Plotting the network #
########################

#SpatialLayout
Graph_plot <- Graph_unipartite %>% 
  igraph::simplify(edge.attr.comb = "sum") 

# Graph_plot_bat <- to_subgraph(Graph_unipartite, to %in% c(9) | from %in% c(9), subset_by = "edges")$subgraph %>% 
#   igraph::simplify(edge.attr.comb = "sum") 

library("igraph")

ggraph::ggraph(Graph_plot,  layout_with_kk(Graph_plot)) +
   geom_edge_arc(aes(width=weight) , strength = .1,
                alpha = 0.1) +
  
  geom_node_point(fill="grey30", alpha = .8, 
                  shape = 21) + 
  geom_node_text(aes(label = name), size=4, color="gray10", repel=TRUE, force = 10) +
  scale_color_gradient2("Connection strength",
                        low= "#f7fcfd",
                        mid = "#8c96c6",
                        high = "#4d004b") +
   theme_void() + theme(legend.position = "bottom",legend.direction = "horizontal") + coord_fixed()

Graph_bipartite %>%  igraph::simplify(edge.attr.comb = "sum") %>% 
  ggraph::ggraph(.,  layout = "bipartite") +
  geom_edge_link0(edge_colour = "grey66")+
  # geom_node_point(alpha = .8, 
  #                 aes(size=Freq, fill=type), shape = 21) +
  scale_fill_manual(values=c("blue","red"))+
  scale_colour_manual(values=c("blue","red"))+
  geom_node_text(aes(label = name, color=type), size=4, repel=TRUE, force = 10) +
  
  theme_void() + theme(legend.position = "left",legend.direction = "horizontal") 

bipartite_matrix <- as_incidence_matrix(Graph_bipartite)  # Extract the matrix

animal_jaccard <- dist.binary(bipartite_matrix, method=1, upper=TRUE, diag = FALSE) # Method #1 is "Jaccard Index"
conservation_jaccard <- dist.binary(t(bipartite_matrix), method=1, upper=TRUE, diag = FALSE) 

animal_jaccard <- as.matrix(animal_jaccard)   
diag(animal_jaccard)<-0

# women_jaccard          # Look at the matrix before you binarize
animal_jaccard <- ifelse(animal_jaccard>0.7, 1, 0)     # Binarize

# jaccard_women      # Take a look at the matrix if you like.

animal_jaccard <- graph_from_adjacency_matrix(animal_jaccard,    # Create an igraph network
                                              mode = "undirected")
plot(animal_jaccard)

# Network test ------------------------------------------------------------

db_full <- db_full[db_full$System != "Anchialine/Marine",] ; db_full <- droplevels(db_full)


levels(db_full$Taxon_Group)[c(2,3,4)] <- "Arthropoda"

db <- db[db$Direction_of_effect != "Negative",] ; db <- droplevels(db)

Graph_bipartite <- db_full %>% 
  dplyr::select(Taxon_Group,Conservation_Action) %>% 
  table() %>% 
  igraph::graph_from_incidence_matrix(directed = FALSE) %>% 
  tidygraph::as_tbl_graph(directed = FALSE) 

Graph_bipartite <- Graph_bipartite %>% tidygraph::activate(nodes) %>% 
  left_join(rbind(data.frame(table(db$Taxon)),
                  data.frame(table(db$Conservation_Action))), by = c("name" = "Var1"))

# Collapse it into an unipartite 
Graph_unipartite_full <- igraph::bipartite_projection(Graph_bipartite)

# Takes the unipartite project graph
Graph_unipartite <- Graph_unipartite_full$proj1  %>% as_tbl_graph(directed = FALSE) %>% 
  activate(edges) %>% #%>% mutate(weight = 1) 
  igraph::simplify(edge.attr.comb = "sum") %>% 
  as_tbl_graph

########################
# Plotting the network #
########################

#SpatialLayout
Graph_plot <- Graph_unipartite %>% 
    igraph::simplify(edge.attr.comb = "sum") 

Graph_plot_bat <- to_subgraph(Graph_unipartite, to %in% c(2) | from %in% c(2), subset_by = "edges")$subgraph %>% 
  igraph::simplify(edge.attr.comb = "sum") 

ggraph::ggraph(Graph_plot_bat,  layout_with_kk(Graph_plot)) +
    #geom_edge_density(fill="orange", alpha=1) +
    geom_edge_arc(aes(width=weight) , strength = .1,
                  alpha = 0.1) +
    
    geom_node_point(fill="grey30", alpha = .8, 
                    aes(size=Freq), shape = 21) + 
    geom_node_text(aes(label = name), size=4, color="gray10", repel=TRUE, force = 10) +
    scale_color_gradient2("Connection strength",
                          low= "#f7fcfd",
                          mid = "#8c96c6",
                          high = "#4d004b") +
    #scale_fill_manual(values = c("blue", "orange", "pink","purple", "grey15")) +
    theme_void() + theme(legend.position = "bottom",legend.direction = "horizontal") + coord_fixed()

Graph_bipartite %>%  igraph::simplify(edge.attr.comb = "sum") %>% 
  ggraph::ggraph(.,  layout_with_kk(.)) +
   geom_edge_arc(
                 strength = .1,
                alpha = 0.3) +
   geom_node_point(alpha = .8, 
                  aes(size=Freq, fill=type), shape = 21) +
  scale_fill_manual(values=c("blue","red"))+
  scale_colour_manual(values=c("blue","red"))+
  geom_node_text(aes(label = name, color=type), size=4, repel=TRUE, force = 10) +
  
  theme_void() + theme(legend.position = "bottom",legend.direction = "horizontal") + coord_fixed()

library(ade4) # If you have not already done so

bipartite_matrix <- as_incidence_matrix(Graph_bipartite)  # Extract the matrix

animal_jaccard <- dist.binary(bipartite_matrix, method=1, upper=TRUE, diag = FALSE) # Method #1 is "Jaccard Index"
conservation_jaccard <- dist.binary(t(bipartite_matrix), method=1, upper=TRUE, diag = FALSE) 

animal_jaccard <- as.matrix(animal_jaccard)   
diag(animal_jaccard)<-0

# women_jaccard          # Look at the matrix before you binarize
animal_jaccard <- ifelse(animal_jaccard>0.8, 1, 0)     # Binarize

# jaccard_women      # Take a look at the matrix if you like.

animal_jaccard <- graph_from_adjacency_matrix(animal_jaccard,    # Create an igraph network
                                          mode = "undirected")
plot(animal_jaccard)



