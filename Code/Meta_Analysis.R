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
library("stringr")
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

# Cleaning p value --------------------------------------------------------

db.meta$P.value <- stringr::str_replace(db.meta$P.value, "<", "")
db.meta$P.value <- stringr::str_replace(db.meta$P.value, ">", "")
db.meta$P.value <- stringr::str_replace(db.meta$P.value, ",", ".")
db.meta$P.value <- as.numeric(db.meta$P.value)

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

# Fitting the metafor models ----------------------------------------------

#Fitting the models
MODEL     <- list()
MODEL2    <- list()

for (i in 1 : nlevels(db.metafor$Response)){  
  
  # Subset the predictor
  data_i  <- db.metafor[db.metafor$Response == levels(db.metafor$Response)[i], ]
  
  # Fitting the model
  model_i <- metafor::rma.mv(yi, vi, random =  ~ 1 | Paper_ID, data = na.omit(data_i),
                             control=list(rel.tol=1e-8)) 
  
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
                                  p = round(model_i$pval,4))
  
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
rm(i, model_i, model2_i; result_for_plot_i, result_for_plot2_i, table_i, table2_i, data_i_eco, data_i)
#warnings()

# renaming Response group as in the result_for_plot
levels(db.metafor$Response) <- levels(as.factor(result_for_plot$label))


preds <- predict(MODEL[[13]], transf = exp)
wi <- 1/sqrt(data_i$vi)
size <- 0.5 + 3 * (wi - min(wi))/(max(wi) - min(wi))
plot(data_i$Yr, exp(data_i$yi), pch = 19, cex = size,
     xlab = "Range yr", ylab = "r",
     las = 1, bty = "l", log = "y")
lines(min(data_i$Yr):max(data_i$Yr), preds$pred)
lines(min(data_i$Yr):max(data_i$Yr), preds$ci.lb, lty = "dashed")
lines(min(data_i$Yr):max(data_i$Yr), preds$ci.ub, lty = "dashed")
abline(h = 1, lty = "dotted")




# Evaluation of publication bias --------------------------------------------------------

rosenthal_N <- c()
rosenthal_p <- c()

for (i in 1 : nlevels(db.metafor$Response)){  
  
  # Subset the predictor
  data_i  <- db.metafor[db.metafor$Response == levels(db.metafor$Response)[i], ]
  
  # Validation
  failsafe_rosenthal <- metafor::fsn(yi = yi, vi = vi, data = na.omit(data_i), 
                                     type = "Rosenthal") 
  # failsafe_rosenberg <- metafor::fsn(yi = yi, vi = vi, data = na.omit(data_i), 
  #                                     type = "Rosenberg") 
  
  rosenthal_N <- c(rosenthal_N, failsafe_rosenthal$fsnum)
  rosenthal_p <- c(rosenthal_p, round(failsafe_rosenthal$pval,3))
  
  funnel(MODEL2[[i]], main = levels(db.metafor$Response)[i])
  #regtest(MODEL2[[i]], predictor = "ni")
  # rosenberg_N <- c(rosenberg_N, failsafe_rosenberg$fsnum)
  # rosenberg_p <- c(rosenberg_p, round(failsafe_rosenberg$pval,3))
}

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
    
    xlim(-0.6,0.6)+
    labs(x = expression(paste("Effect size [r]" %+-% "95% Confidence interval")),
         y = NULL) + 
    scale_color_manual("", 
                       values = colors.type)+
    scale_fill_manual("", 
                       values = colors.type)+
    
    #scale_shape_manual("", values = c(21,24))+
    #guides(color = TRUE,fill = TRUE)+
    theme_bw() + 
    theme(legend.position = "none", 
          #legend.direction = "vertical",
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
          )
)

(boxplot.forest_plot1 <- db.metafor %>% 
    ggplot(aes(x = r, y = Response, col = Type, fill = Type)) +
    geom_vline(lty = 3, size = 0.5, col = "grey10", xintercept = 0) +
    #geom_flat_violin(alpha = 0.8) +
    geom_point(aes(size = Yr, shape = Domain), 
               position = position_jitter(width = 0.35), alpha = 0.3) +
    geom_boxplot(width = .8, outlier.shape = NA, alpha =0) +
    labs(x = "Effect size [r]", 
         y = NULL) +
  xlim(-1,1)+
  scale_color_manual("Response type", 
                     values = colors.type)+
  scale_fill_manual("Response type", 
                    values = colors.type)+
  scale_size("Temporal scale (Years)",
             breaks = c(1,10,50,100))+
  scale_shape_manual("Domain", 
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
        #panel.border = element_blank(),
        panel.grid.major.x = element_blank(),                                          
        panel.grid.minor.x = element_blank(),
        # panel.grid.minor.y = element_blank(),
        # panel.grid.major.y = element_blank(),  
        plot.margin = unit(c(rep(0.4,4)), units = , "cm")
  )
)

pdf(file = "Figures/Figure_2.pdf", width = 14, height = 6)
ggpubr::ggarrange(forest_plot1, boxplot.forest_plot1, hjust = -0.2,
                  ncol = 2, nrow = 1, labels = c("A", "B"))
dev.off()


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
    
    xlim(-1.2,1.2)+
    
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

pdf(file = "Figures/Figure_1.pdf", width = 10, height = 5)
plot.map
dev.off()

pdf(file = "Figures/Figure_2.pdf", width = 8, height = 5)
forest_plot1
dev.off()

pdf(file = "Figures/Figure_3.pdf", width = 8, height = 5)
forest_plot2
dev.off()

(plotS2a <- db.metafor %>% 
    group_by(Class) %>%
    mutate(median_yi = median(yi, na.rm=T),
           n = n()) %>%
    ungroup() %>%
    arrange(desc(Phylum),Class) %>%
    mutate(Class = factor(Class, levels = unique(.$Class))) %>%
    ggplot(aes(x = yi, y = Class)) +
    geom_point(position = position_jitter(width = 0.35), size = 1, alpha = 0.3) +
    geom_boxplot(width = .8, outlier.shape = NA, alpha = 0.2, col = "grey20") +
    labs(x = "Estimates", y = NULL) +
    
    # ggimage::geom_phylopic(aes(x = 1, y = phylum, image = image),
    #                        size = .2, color = "grey20") +
    # scale_color_manual(values = custom_color)+
    # scale_fill_manual(values = custom_color)+
    scale_x_continuous(trans = scales::pseudo_log_trans(), breaks = c(0, 10, 100, 1000, 10000)) + 
    theme_classic() +
    theme(legend.position = "none", axis.text.y = element_text(size = 12)))

