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
library("metafor")   # Meta-Analysis Package for R
library("ggplot2") 
library("tidyverse")

# Sourcing useful functions ------------------------------------------------

source("Functions/Custom_functions.r")

###############################################################

## Data preparation:

###############################################################

# Loading the Database ----------------------------------------------------

db <-
  read.csv(
    file = "Data/data.csv",
    sep = '\t',
    dec = ',',
    header = TRUE,
    as.is = FALSE
  )

# Checking the database
str(db)

#Database only with distinct paper
db_unique <- dplyr::distinct(db_full, Paper_ID, .keep_all = TRUE) 

nrow(db)
nrow(db_unique)

mean(table(db$Paper_ID)) ; SE(table(db$Paper_ID)) # mean number of estimates/paper

# Data exploration --------------------------------------------------------

table(db$Domain)
table(db$System_specific)
table(db$Phylum)

table(db$Predictor_Group)


#Type of actions
table(db$Publication_type)

#Summary statistics (Literature)
table(db_unique$Source) ; sum(table(db_unique$Source)) # N° of unique sources

#Summary statistics (Testing)
table(db$Tested_statistically)[2] / sum(table(db$Tested_statistically)) #N° and % testing

#How many estimates would be usable for meta analysis?
n_studies            <- c() 
n_estimates_testing  <- c()
n_estimates_tot      <- c()
perc_testing         <- c()
usable               <- c()
unusable             <- c()
perc_usable          <- c()

for(i in 1:nlevels(db$Conservation_Action)){

  db_i_tot <- db[db$Conservation_Action == levels(db$Conservation_Action)[i],]
  db_i     <- db_i_tot[db_i_tot$Tested_statistically == "yes",]
  
  table_i        <- table(db_i$Pearson_r_conversion) #% of usable statistics
  n_studies      <- c(n_studies, nrow(distinct(db_i, ID, .keep_all = TRUE)) ) #unique studies
  n_estimates_testing    <- c(n_estimates_testing, nrow(db_i) ) #unique estimates
  n_estimates_tot    <- c(n_estimates_tot, nrow(db_i_tot) ) #unique estimates
  perc_testing   <- c(perc_testing, round(nrow(db_i)/nrow(db_i_tot),2)*100 )
  usable         <- c(usable, sum(table_i[1],table_i[3]))
  unusable       <- c(unusable, sum(table_i[2]))
  perc_usable    <- c(perc_usable, round((usable[i]/sum(table_i)),2)*100)
  
}

Table_1 <- data.frame(Intervention = levels(db$Conservation_Action), n_studies, n_estimates_testing, n_estimates_tot, perc_testing, usable, unusable, perc_usable)
Table_1[is.na(Table_1)] <- 0
colnames(Table_1) <- c("Intervention", "N° studies", "N° interventions testing","N° interventions tot", "% testing", "N° usable", "N° unusable", "% usable")

write.csv(Table_1,"Tables/Table_1.csv")

#How many estimates would be usable for meta analysis?
n_studies            <- c() 
n_estimates_testing  <- c()
n_estimates_tot      <- c()
perc_testing         <- c()
usable               <- c()
unusable             <- c()
perc_usable          <- c()

for(i in 1:nlevels(db$Impact)){
  
  db_i_tot <- db[db$Impact == levels(db$Impact)[i],]
  db_i     <- db_i_tot[db_i_tot$Tested_statistically == "yes",]
  
  table_i        <- table(db_i$Pearson_r_conversion) #% of usable statistics
  n_studies      <- c(n_studies, nrow(distinct(db_i, ID, .keep_all = TRUE)) ) #unique studies
  n_estimates_testing    <- c(n_estimates_testing, nrow(db_i) ) #unique estimates
  n_estimates_tot    <- c(n_estimates_tot, nrow(db_i_tot) ) #unique estimates
  perc_testing   <- c(perc_testing, round(nrow(db_i)/nrow(db_i_tot),2)*100 )
  usable         <- c(usable, sum(table_i[1],table_i[3]))
  unusable       <- c(unusable, sum(table_i[2]))
  perc_usable    <- c(perc_usable, round((usable[i]/sum(table_i)),2)*100)
  
}

Table_2 <- data.frame(Impact = levels(db$Impact), n_studies, n_estimates_testing, n_estimates_tot, perc_testing, usable, unusable, perc_usable)
Table_2[is.na(Table_2)] <- 0
colnames(Table_2) <- c("Impact", "N° studies", "N° interventions testing","N° interventions tot", "% testing", "N° usable", "N° unusable", "% usable")

write.csv(Table_2,"Tables/Table_2.csv")

#Redefining levels
levels(db$Impact)[2] <- "Multiple"

levels(db$Conservation_Action)[5] <- "Gating"

###############################################################

## Meta-Analysis

###############################################################

db_metafor <- db[db$Tested_statistically == "yes",]
db_metafor <- db_metafor[db_metafor$Pearson_r_conversion == "converted",]
db_metafor <- droplevels(db_metafor)

mean(table(db_metafor$ID)) ; sd(table(db_metafor$ID))

dim(db_metafor)
nlevels(db_metafor$ID) #250 references 

db_metafor <- db_metafor %>% select(ID, 
                             N,
                             Domain,
                             System,
                             Family,
                             Genus_specific,
                             Response_Group,
                             Predictor_Group,
                             r = Pearson.s_r)

# Derive Fischer's Z and its variance

db_metafor <- metafor::escalc(measure = "COR", ri = r, ni = N, data = db_metafor)

# Gate
db_metafor <- db_metafor[db_metafor$Predictor_Group == "Gate" | 
                         db_metafor$Predictor_Group == "Disturbance reduction" |
                         db_metafor$Predictor_Group == "Restoration" |
                         db_metafor$Predictor_Group == "Decontamination" |
                         db_metafor$Predictor_Group == "Monitoring", ] ; db_metafor <- droplevels(db_metafor)

table(db_metafor$Predictor_Group,db_metafor$Response_Group) # Disturbance reduction & Gate

# Removing combinations with 1 study only
db_metafor <- db_metafor[!c(db_metafor$Predictor_Group == "Disturbance reduction" & db_metafor$Response_Group == "Population"),]
db_metafor <- db_metafor[!c(db_metafor$Predictor_Group == "Monitoring" & db_metafor$Response_Group == "Pathogen"),]
db_metafor <- db_metafor[!c(db_metafor$Predictor_Group == "Restoration" & db_metafor$Response_Group == "Behavior"),]

#Check sample size for each predictors
table_n <- data.frame(predictor = NULL, n = NULL, n_papers = NULL)

for(i in 1:length(unique(levels(db_metafor$Response_Group))))
  table_n <- rbind(table_n, 
                          data.frame(predictor = levels(db_metafor$Response_Group)[i],
                                     n = nrow(db_metafor[db_metafor$Response_Group == levels(db_metafor$Response_Group)[i], ]),
                                     n_papers = length(unique(db_metafor[db_metafor$Response_Group == levels(db_metafor$Response_Group)[i], ]$ID)))  
                          
  )

actions_to_analyse    <- c("Decontamination", "Disturbance reduction", "Gate", "Monitoring", "Restoration")

SUBSET      <- list()
MODEL       <- list()

result_for_plot <- data.frame(label_action = NULL,
                              label_pred = NULL,
                              size = NULL,
                              b     = NULL,
                              beta  = NULL,
                              se    = NULL,
                              z = NULL,
                              p     = NULL,
                              ci.lb = NULL,
                              ci.ub = NULL,
                              ES    = NULL,
                              L     = NULL,
                              U     = NULL,
                              failsafe_N = NULL,
                              failsafe_p = NULL)

num <- 0

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


