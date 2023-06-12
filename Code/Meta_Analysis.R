###############################################################

## A global meta-analysis on the biological impacts of climate change in subterranean ecosystems
# Ilaria Vaccarelli, Raquel Colado, David Sanchez-Fernandez, Diana M. P. Galassi, Susana Pallarés, Mattia Di Cicco, Melissa B. Meierhofer, Elena Piano, Tiziana Di Lorenzo, Stefano Mammola

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
library("grid")
library("ggplot2")
library("ggpubr")
library("ggthemes")
library("glmmTMB")
library("performance")
library("png")
library("stringr")
library("tidyverse")
library("tidylog")

# Loading pictures ------------------------------------------------------

spider_png      <- png::readPNG("Pictures/spider.png")
insect_png      <- png::readPNG("Pictures/Leptodirus.png")
crustacean_png  <- png::readPNG("Pictures/Niphargus.png")

# Sourcing useful functions ------------------------------------------------

source("Functions/Custom_functions.r") #Useful custom functions and ggplot2 settings

###############################################################
## Data preparation:
###############################################################

# Loading the Databases ---------------------------------------------------

# Literature database
db.pub <-
  read.csv(
    file = "Data/publications.csv",
    sep = ';',
    dec = ',',
    header = TRUE,
    as.is = FALSE
  )

str(db.pub)

# Database for meta analaysis
db.meta <-
  read.csv(
    file = "Data/meta_analysis.csv",
    sep = ';',
    dec = ',',
    header = TRUE,
    as.is = FALSE
  )

#removing life history traits (only 2 estimates)

db.meta <- db.meta[db.meta$Response_macrogroup != "Life history",]

# Database with traits
db.trait <-
  read.csv(
    file = "Data/trait.csv",
    sep = ';',
    dec = ',',
    header = TRUE,
    as.is = FALSE
  )

str(db.meta)

# Database with only one estimate / paper
db.meta.distinct <- db.meta %>% 
                    dplyr::distinct(Paper_ID, .keep_all = TRUE) 

db.pub.distinct <- db.pub %>% 
  dplyr::distinct(Paper_ID, .keep_all = TRUE) 

# Extracting temporal range of each study ---------------------------------

#Calculating year range for the dataset analysed by each scientometric article

levels(db.meta$Year)

#Extracting Temporal span (years) of each study with a loop
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
    Yr_range <- c(Yr_range, NA) } 
  else if (length(Year) == 1) { 
    Yr_min   <- c(Yr_min, Year)
    Yr_max   <- c(Yr_max, Year)
    Yr_range <- c(Yr_range, 1)
  } else {
    Yr_min   <- c(Yr_min, range(Year)[1])
    Yr_max   <- c(Yr_max, range(Year)[2])
    Yr_range <- c(Yr_range, (range(Year)[2] - range(Year)[1])) }
  
} #Warnings() occur when the range is just a single number (i.e. no range)

# check the values
range(Yr_max,   na.rm = TRUE)
range(Yr_min,   na.rm = TRUE)
range(Yr_range, na.rm = TRUE)

db.meta <- data.frame(db.meta, Yr_min, Yr_max, Yr_range) ; head(db.meta)

rm(i, Year, Yr_min, Yr_max, Yr_range, Data_i) #clean

# Cleaning p value --------------------------------------------------------

db.meta$P.value <- stringr::str_replace(db.meta$P.value, "<", "")
db.meta$P.value <- stringr::str_replace(db.meta$P.value, ">", "")
db.meta$P.value <- stringr::str_replace(db.meta$P.value, ",", ".")
db.meta$P.value <- as.numeric(db.meta$P.value)

# Summary stats -----------------------------------------------------------

# How many papers and sources?
db.pub.distinct %>% nrow()
db.pub.distinct$Source %>% table()

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

# geography

db.pub.distinct$Geography %>% table()

db.meta$Response_macrogroup %>% table

db.meta$Response_macrogroup %>% table / sum(db.meta$Response_macrogroup %>% table)

###############################################################
## Figures:
###############################################################

### db.pub <- db.pub[db.pub$Paper_ID %in% db.meta$Paper_ID,]

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

levels(db.pub$Study_type)

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
    scale_x_continuous(breaks = (-4:4) * 45) +
  ggthemes::theme_map()+
    theme(legend.position = c(0.05,0.2),
          legend.background=element_rect(fill = alpha("white", 0))))

rm(world) #cleaning

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
    scale_y_continuous(breaks = seq(from=0,to=13,by=1))+
   
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
                                        Class,
                                        Ecology  = Ecology_group,
                                        Response = Response_Group,
                                        Type = Response_macrogroup,
                                        N,
                                        p = P.value,
                                        r = Pearson_r_conversion)


# Derive Fischer's Z and its variance
db.metafor <- metafor::escalc(measure = "COR", ri = r, ni = N, data = db.metafor)

# Fisher’s r-to-z transformation is a variance stabilizing transformation 
# for correlation coefficients with the added benefit of also being a rather effective 
# normalizing transformation (Fisher 1921). 
# The Fisher’s r-to-z transformed correlation coefficient is equal to:
# 1/2 * log((1 + ri)/(1 - ri)).

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

rm(i, table_estimates) #cleaning

# Fitting the metafor models ----------------------------------------------

#Fitting the models
MODEL     <- list()
MODEL2    <- list()
MODEL3    <- list()

for (i in 1 : nlevels(db.metafor$Response)){  
  
  # Subset the predictor
  data_i  <- db.metafor[db.metafor$Response == levels(db.metafor$Response)[i], ]
  
  # Fitting the model
  model_i <- metafor::rma.mv(yi, vi, random =  ~ 1 | Paper_ID, data = na.omit(data_i),
                             control=list(rel.tol=1e-8)) 
  
  # Extracting coefficients
  result_for_plot_i <- data.frame(original_label = levels(db.metafor$Response)[i],
                                  label = paste(levels(db.metafor$Response)[i],
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
                                  p = round(model_i$pval,4))

  # Fitting the model with moderator Ecology
  model2_i <- rma.mv(yi, vi, mods = ~ Ecology, 
                     random = ~ 1 | Paper_ID, 
                     data = na.omit(data_i),
                     control = list(rel.tol=1e-8))

  ?rma.mv
  
  # Adding missing level in the database
  data_i_eco  <- data_i[data_i$Paper_ID %in% model2_i$mf.r[[1]]$Paper_ID,]
  data_i_eco  <- droplevels(data_i_eco)
  
  if(nlevels(data_i_eco$Ecology) == 1) {
    Ecology_i <- levels(data_i_eco$Ecology)
  }
  else if(nlevels(data_i_eco$Ecology) < 3){
    non.baseline <- gsub('Ecology','', rownames(model2_i$b)[2])
    Ecology_i <- levels(data_i_eco$Ecology)
    
    baseline <- Ecology_i[! Ecology_i %in% non.baseline]
    Ecology_i <- c(baseline,non.baseline)
    
  } else { Ecology_i <- levels(data_i_eco$Ecology) }

  # Extracting coefficients
  result_for_plot2_i <- data.frame(original_label = rep(levels(db.metafor$Response)[i],length(Ecology_i)),
                                  label = rep(paste(levels(db.metafor$Response)[i],
                                                " (" , nrow(data_i),", ",
                                                length(unique(data_i$Paper_ID)),")",sep=''),length(Ecology_i)),
                                  Type = rep(data_i$Type[1],length(Ecology_i)),
                                  Ecology = Ecology_i,
                                  b     = model2_i$b,
                                  ci.lb = model2_i$ci.lb,
                                  ci.ub = model2_i$ci.ub,
                                  ES    = ((exp(model2_i$b)-1))/((exp(model2_i$b)+1)),
                                  L     = ((exp(model2_i$ci.lb)-1)/(exp(model2_i$ci.lb)+1)),
                                  U     = ((exp(model2_i$ci.ub)-1)/(exp(model2_i$ci.ub)+1)),
                                  p = round(model2_i$pval,4))
  
  # Fitting the model with moderator Domain
  model3_i <- rma.mv(yi, vi, mods = ~ Domain, 
                     random = ~ 1 | Paper_ID, 
                     data = na.omit(data_i),
                     control = list(rel.tol=1e-8))
  
  # Adding missing level in the database
  data_i_dom  <- data_i[data_i$Paper_ID %in% model3_i$mf.r[[1]]$Paper_ID,]
  data_i_dom  <- droplevels(data_i_dom)
  
  if(nlevels(data_i_dom$Domain) == 1) {
    Domain_i <- levels(data_i_eco$Domain)
  }
  else{
    non.baseline <- gsub('Domain','', rownames(model3_i$b)[2])
    Domain_i <- levels(data_i_eco$Domain)
    
    baseline <- Domain_i[! Domain_i %in% non.baseline]
    Domain_i <- c(baseline, non.baseline)
  }
  
  # Extracting coefficients
  result_for_plot3_i <- data.frame(original_label = rep(levels(db.metafor$Response)[i],length(Domain_i)),
                                   label = rep(paste(levels(db.metafor$Response)[i],
                                                 " (" , nrow(data_i),", ",
                                                 length(unique(data_i$Paper_ID)),")",sep=''),length(Domain_i)),
                                   Type = rep(data_i$Type[1],length(Domain_i)),
                                   Domain = Domain_i,
                                   b     = model3_i$b,
                                   ci.lb = model3_i$ci.lb,
                                   ci.ub = model3_i$ci.ub,
                                   ES    = ((exp(model3_i$b)-1))/((exp(model3_i$b)+1)),
                                   L     = ((exp(model3_i$ci.lb)-1)/(exp(model3_i$ci.lb)+1)),
                                   U     = ((exp(model3_i$ci.ub)-1)/(exp(model3_i$ci.ub)+1)),
                                   p = round(model3_i$pval,4))
  
  
  # Store the data 
  MODEL[[i]]  <- model_i
  MODEL2[[i]] <- model2_i
  MODEL3[[i]] <- model3_i
  
  # Store tables
  if(i > 1) {    
    result_for_plot <- rbind(result_for_plot,result_for_plot_i)
    result_for_plot2 <- rbind(result_for_plot2,result_for_plot2_i)
    result_for_plot3 <- rbind(result_for_plot3,result_for_plot3_i)
    } else {
  result_for_plot  <- result_for_plot_i
  result_for_plot2 <- result_for_plot2_i
  result_for_plot3 <- result_for_plot3_i
    }
}  ; rm(i, data_i_eco, data_i_dom, data_i, Domain_i, Ecology_i, baseline, non.baseline,
   model_i, model2_i, model3_i, 
   result_for_plot_i, result_for_plot2_i, result_for_plot3_i) #cleaning

# renaming Response group as in the result_for_plot
levels(db.metafor$Response) <- levels(as.factor(result_for_plot$label))

# Evaluation of publication bias --------------------------------------------------------

rosenthal_N <- c()
rosenthal_p <- c()

pdf(file = "Figures/Figure_S2.pdf", width = 12, height = 12)
par(mfrow= c(4,3), mar = c(rep(2,4)))
for (i in 1 : nlevels(db.metafor$Response)){  
  
  # Subset the predictor
  data_i  <- db.metafor[db.metafor$Response == levels(db.metafor$Response)[i], ]
  
  # Validation
  failsafe_rosenthal <- metafor::fsn(yi = yi, vi = vi, data = na.omit(data_i), 
                                     type = "Rosenthal") 
  
  rosenthal_N <- c(rosenthal_N, failsafe_rosenthal$fsnum)
  rosenthal_p <- c(rosenthal_p, round(failsafe_rosenthal$pval,3))
  
  funnel(MODEL2[[i]], main = levels(db.metafor$Response)[i])
  
} ; rm(i, data_i, failsafe_rosenthal) #cleaning
dev.off()

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

colors.type <- c("chocolate3","grey10","blue","darkmagenta")
color.num <- table(result_for_plot$Type)

color.axis.y <- c(rep(colors.type[2], color.num[4]),
                  rep(colors.type[4], color.num[3]),
                  rep(colors.type[3], color.num[2]), 
                  rep(colors.type[1], color.num[1])) 

face.axis.y   <- ifelse(result_for_plot$p > 0.05, "plain", "bold")
p.values.meta <- ifelse(result_for_plot$p > 0.05, " ", " *")

(forest_plot1 <- 
    result_for_plot %>%
    ggplot2::ggplot(aes(x = ES, y = label, fill = Type, col = Type)) + 
    geom_vline(lty = 3, size = 0.5, col = "grey10", xintercept = 0) +
    geom_jitter(data = db.metafor,
                aes(y = Response, x = yi, col = Type, shape = Domain),
                alpha = 0, stroke = .8, size = 0,  height = 0.1, width = 0.5)+
    geom_errorbar(aes(xmin = L, xmax = U), size = 1, width = 0)+
    geom_point(size = 3, pch = 21) +
    
    geom_text(aes(col = Type),label = paste0(round(result_for_plot$b, 3), p.values.meta, sep = " "), vjust = - 1, size = 3.5) +
    
    xlim(-1,1)+
    labs(x = expression(paste("Effect size [r]" %+-% "95% Confidence interval")),
         y = NULL) + 
    scale_color_manual("", 
                       values = colors.type)+
    scale_fill_manual("", 
                       values = colors.type)+
    
    #guides(color = TRUE,fill = TRUE)+
    theme_bw() + 
    theme(legend.position = "none",
          legend.text = element_text(size = 8),
          axis.title = element_text(size = 12),
          axis.line.x = element_line(color="grey10"), 
          axis.line.y = element_line(color="grey10"),
          axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10, 
                                     color = rev(color.axis.y)),
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          plot.margin = unit(c(rep(0.4,4)), units = , "cm")
          )
)

(boxplot.forest_plot1 <- db.metafor %>% 
    ggplot(aes(x = r, y = Response, col = Type, fill = Type)) +
    geom_vline(lty = 3, size = 0.5, col = "grey10", xintercept = 0) +
    geom_point(aes(size = Yr, shape = Domain), 
               position = position_jitter(width = 0.35), alpha = 0.3) +
    geom_boxplot(width = .8, outlier.shape = NA, alpha =0) +
    labs(x = "Effect size [r]", 
         y = NULL) +
  xlim(-1,1)+
  scale_color_manual("Response type", 
                     values = colors.type[c(2,4,3,1)],
                     breaks = c("Habitat", "Population/Community", "Physiology", "Behaviour"))+
  scale_fill_manual("Response type", 
                    values = colors.type[c(2,4,3,1)],
                    breaks = c("Habitat", "Population/Community", "Physiology", "Behaviour"))+
  scale_size("Temporal scale (Years)",
             breaks = c(1,10,50,100))+
  scale_shape_manual("Habitat", 
                     values = c(21,24))+
  
  theme_bw() + 
  theme(legend.position = "right", 
        legend.direction = "vertical",
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        axis.line.x = element_line(color="grey10"), 
        axis.line.y = element_line(color="grey10"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 0, 
                                   color = rev(color.axis.y),
                                   face = face.axis.y),
        panel.grid.major.x = element_blank(),                                          
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(rep(0.4,4)), units = , "cm")
  )
)

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
    
    xlim(-1,1)+
    
    scale_fill_manual("Ecology",
                      values = c("grey60","grey10","white"))+
    
    scale_shape_manual("Ecology", values = c(23,25,22))+
    
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
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.margin = unit(c(rep(0.4,4)), units = , "cm")
    ))

result_for_plot3$Type <- 
  factor(result_for_plot3$Type, levels = c("Behaviour","Physiology","Population/Community","Habitat")) #Sort

result_for_plot3 <- result_for_plot3 %>% arrange(Type, .by_group = FALSE)

result_for_plot3$label <- 
  factor(result_for_plot3$label, 
         levels = unique(result_for_plot3$label)) #Sort

(forest_plot3 <- 
    result_for_plot3 %>%
    ggplot2::ggplot(aes(x = ES, 
                        y = label,
                        shape = Domain)) + 
    geom_vline(lty = 3, size = 0.5, col = "grey50", xintercept = 0) +
    
    geom_errorbar(aes(xmin = L, xmax = U),col = "grey10", size = .5, width = 0, position = position_dodge(width = 0.6))+
    geom_point(aes(fill = Domain), size = 3, col = "grey10", stroke = .5, position = position_dodge(width = 0.6)) +
    
    labs(x = expression(paste("Effect size [r]" %+-% "95% Confidence interval")),
         y = NULL) +
    
    xlim(-1,1)+
    
    scale_fill_manual("Habitat",
                      values = c("steelblue1","grey30"))+
    
    scale_shape_manual("Habitat", values = c(21,24))+
    
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
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.margin = unit(c(rep(0.4,4)), units = , "cm")
    ))

# Saving the plots --------------------------------------------------------

pdf(file = "Figures/Figure_2.pdf", width = 10, height = 5)
plot.year + annotation_custom(ggplotGrob(plot.map),xmin = 1983, xmax = 2013,ymin = 3, ymax = 10)
dev.off()

pdf(file = "Figures/Figure_3.pdf", width = 14, height = 6)
ggpubr::ggarrange(forest_plot1, boxplot.forest_plot1, hjust = -0.2,
                  ncol = 2, nrow = 1, labels = c("A", "B"))
dev.off()

pdf(file = "Figures/Figure_4.pdf", width = 14, height = 6)
ggpubr::ggarrange(forest_plot2, forest_plot3, hjust = -0.2,
                  ncol = 2, nrow = 1, labels = c("A", "B"))
dev.off()

###############################################################

## Trait analysis

###############################################################

# LT 50
db.trait2 <- db.trait[db.trait$Response_revised == "LT50", ]
db.trait2 <- droplevels(db.trait2)

db.trait2$Ramping <- ifelse(db.trait2$Ramping..min. > 1, "Hours/Days", "Minutes")
db.trait2$Ramping <- as.factor(db.trait2$Ramping)

levels(db.trait2$Class)[c(2, 4)] <- "Crustacea"

#db.trait2$Class <- factor(db.trait2$Class, levels = c("Crustacea", "Arachnida", "Insecta")) #Sort
levels(db.trait2$Ecological_Classification) <- c("Low/Null","High","Low/Null")

db.trait2 <- droplevels(db.trait2)

(box_3 <- db.trait2 %>% ggplot2::ggplot(aes(y = Delta_Value, x = Class, 
                                  fill = Ecological_Classification)) +
  geom_boxplot(col = "grey10",
               outlier.shape = NA,
               alpha = 0.4,
               position = position_dodge2(0.65, preserve = "single")) +
  geom_point(aes(fill = Ecological_Classification, 
                 size = N, 
                 shape = Methodology),
             position = position_dodge2(0.65, preserve = "single"), color = "grey10",alpha = 0.3)+#shape = 21,
             # position = position_jitterdodge(jitter.width = 0.2, 
             #                                 dodge.width = 0.9)
  geom_vline(xintercept = 1.5, linetype = "dotted") +
  geom_vline(xintercept = 2.5, linetype = "dotted") +
  labs(
    x = NULL,
    y = expression(Delta * "LT50 [°C]"),
    title = NULL,
    subtitle = NULL
  ) +
  guides(fill = guide_legend(title.position = "top"),
         size = guide_legend(title.position = "top"),
         shape = guide_legend(title.position = "top")) +
  scale_size_continuous("Sample size") +
  scale_fill_manual("Subterranean specialisation", values = c("white", "deeppink4")) +
  scale_shape_manual("Experiment type", 
                     values = c(21,24))+
  
  annotation_custom(grid::rasterGrob(spider_png),
                   xmin = unit(0.9, "native"), xmax = unit(1.4,"native"),
                   ymin = unit(34,"npc"),  ymax = unit(43.5,"npc"))+

  annotation_custom(grid::rasterGrob(crustacean_png),
                    xmin = unit(1.9, "native"), xmax = unit(2.4,"native"),
                    ymin = unit(36,"npc"),  ymax = unit(43.5,"npc"))+

  annotation_custom(grid::rasterGrob(insect_png),
                    xmin = unit(3.1, "native"), xmax = unit(3.5,"native"),
                    ymin = unit(36,"npc"),  ymax = unit(43.5,"npc"))+
  ylim(0, 42)+
  theme_bw(base_family = "Arial") +
  theme(legend.position = "top", 
        legend.direction = "horizontal",
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        axis.line.x = element_line(color = "grey10"), 
        axis.line.y = element_line(color = "grey10"),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(rep(0.4,4)), units = "cm")
  ))

formula_m1 <- as.formula("Delta_Value ~ Ecological_Classification + Class + Methodology + (1 | Paper_ID)")
model <- glmmTMB::glmmTMB(formula_m1, data = db.trait2, family = gaussian,#Gamma(link ="log"),
                          control = glmmTMBControl(optimizer=optim,
                                                 optArgs=list(method="BFGS")))

performance::check_model(model)
summary(model)

# LT 100
db.trait3 <- db.trait[db.trait$Response_revised == "LT100", ]
db.trait3 <- droplevels(db.trait3)

levels(db.trait3$Class)[3] <- "Crustacea"
db.trait3$Class <- factor(db.trait3$Class, levels = c("Arachnida", "Crustacea" ,"Insecta")) #Sort

levels(db.trait3$Ecological_Classification) <- c("Low/Null","High","Low/Null")
db.trait3$Ecological_Classification <- factor(db.trait3$Ecological_Classification, levels = c("Low/Null", "High")) #Sort

(box_4 <- db.trait3 %>% ggplot2::ggplot(aes(y = Delta_Value, x = Class, 
                                            fill = Ecological_Classification)) +
    geom_boxplot(col = "grey10",
                 outlier.shape = NA,
                 alpha = 0.4,
                 position = position_dodge2(0.65, preserve = "single")) +
    geom_point(aes(fill = Ecological_Classification, 
                   size = N, 
                   shape = Methodology),
               position = position_dodge2(0.65, preserve = "single"), color = "grey10",alpha = 0.3)+#shape = 21,
    geom_vline(xintercept = 1.5, linetype = "dotted") +
    geom_vline(xintercept = 2.5, linetype = "dotted") +
    labs(
      x = NULL,
      y = expression(Delta * "LT100 [°C]"),
      title = NULL,
      subtitle = NULL
    ) +
    scale_size_continuous("Sample size") +
    scale_fill_manual("Subterranean specialisation", values = c("white", "deeppink4")) +
    scale_shape_manual("Experiment type", 
                       values = c(21,24))+
    theme_bw(base_family = "Arial") +
    theme(legend.position = "none", 
          legend.direction = "horizontal",
          legend.text = element_text(size = 8),
          axis.title = element_text(size = 12),
          axis.line.x = element_line(color="grey10"), 
          axis.line.y = element_line(color="grey10"),
          axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.margin = unit(c(rep(0.4,4)), units = "cm")
    ))

ggpubr::ggarrange(box_3, box_4, hjust = -0.1, heights = c(1,0.9),
                  ncol = 1, nrow = 2, labels = c("A", "B"))

ggsave("Figures/Figure_5.jpg", width = 8, height = 8)

# Saving tables -----------------------------------------------------------

rownames(result_for_plot) <- NULL
result_for_plot <- result_for_plot[,-c(2:3)]
colnames(result_for_plot)[1] <- "Predictor"
result_for_plot[,c(3:ncol(result_for_plot))] <- round(result_for_plot[,c(3:ncol(result_for_plot))],3)
result_for_plot$p <- ifelse(result_for_plot$p < 0.0001, "<0.001",result_for_plot$p)
result_for_plot <- result_for_plot %>% mutate_if(is.numeric, as.character)
result_for_plot <- result_for_plot %>% arrange(Type, .by_group = FALSE)

result_for_plot$CI = paste0("(",result_for_plot$ci.lb,", ",result_for_plot$ci.ub,")")

result_for_plot <- result_for_plot %>% 
                   dplyr::select(Predictor,
                                 Type,
                                 beta = b,
                                 SE = ES,
                                 CI,
                                 p)

result_for_plot <- data.frame(result_for_plot,
                              rosenthal_N, 
                              rosenthal_p = ifelse(rosenthal_p == 0.000, "<0.001", rosenthal_p))

xlsx::write.xlsx(result_for_plot, "Tables/Table_S1.xlsx")

rownames(result_for_plot2) <- NULL
result_for_plot2 <- result_for_plot2[,-c(2)]
colnames(result_for_plot2)[1] <- "Predictor"
result_for_plot2[,c(4:ncol(result_for_plot2))] <- round(result_for_plot2[,c(4:ncol(result_for_plot2))],3)
result_for_plot2$p <- ifelse(result_for_plot2$p < 0.0001, "<0.001",result_for_plot2$p)
result_for_plot2 <- result_for_plot2 %>% mutate_if(is.numeric, as.character)
result_for_plot2 <- result_for_plot2 %>% arrange(Type, .by_group = FALSE)

result_for_plot2$CI = paste0("(",result_for_plot2$ci.lb,", ",result_for_plot2$ci.ub,")")

result_for_plot2 <- result_for_plot2 %>% 
  dplyr::select(Predictor,
                Type,
                beta = b,
                SE = ES,
                CI,
                p)

xlsx::write.xlsx(result_for_plot2, "Tables/Table_S2.xlsx")

rownames(result_for_plot3) <- NULL
result_for_plot3 <- result_for_plot3[,-c(2)]
colnames(result_for_plot3)[1] <- "Predictor"
result_for_plot3[,c(4:ncol(result_for_plot3))] <- round(result_for_plot3[,c(4:ncol(result_for_plot3))],3)
result_for_plot3$p <- ifelse(result_for_plot3$p < 0.0001, "<0.001",result_for_plot3$p)
result_for_plot3 <- result_for_plot3 %>% mutate_if(is.numeric, as.character)
result_for_plot3 <- result_for_plot3 %>% arrange(Type, .by_group = FALSE)

result_for_plot3$CI = paste0("(",result_for_plot3$ci.lb,", ",result_for_plot3$ci.ub,")")

result_for_plot3 <- result_for_plot3 %>% 
  dplyr::select(Predictor,
                Type,
                beta = b,
                SE = ES,
                CI,
                p)

xlsx::write.xlsx(result_for_plot3, "Tables/Table_S3.xlsx")
#end
