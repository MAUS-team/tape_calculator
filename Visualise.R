#-------------------------------------------------------------------------------
# load the data and alter for visualistion
#-------------------------------------------------------------------------------

source('Sourced code/Source.R')
df <- all_TAPE()

#-------------------------------------------------------------------------------
# choose colors
#-------------------------------------------------------------------------------

# Install
#install.packages("wesanderson")
# Load
library(wesanderson)

names(wes_palettes) #find all the options for the colors

pal <- wes_palette(6, name = "GrandBudapest1", type = "continuous")

# Create a sequence of numbers from 1 to the length of the palette
x <- seq_len(length(pal))

# Create a bar plot to see the different colors
barplot(x, col = pal, border = NA)

cbPalette<-pal #set the palette for plotting once you are satisfied with the colors

#-------------------------------------------------------------------------------
# pie chart
#-------------------------------------------------------------------------------


df<-all_TAPE()
# Define the breaks for the categories
breaks <- c( 20,30, 40, 50, 60, 70, 80, Inf)

# Define the labels for the categories
labels <- c( "20-30", "30-40", "40-50", "50-60", "60-70", "70-80",">80")

# Create a new variable 'caet_cut' that categorizes 'caet_tot' into the specified groups
df$caet_cut <- cut(df$caet_tot, breaks = breaks, labels = labels, include.lowest = TRUE)

# Count the number of scores in each category
counts <- table(df$caet_cut)

# Compute the percentages
percentages <- round(counts / sum(counts) * 100, 1)

# Create labels for the pie chart
labels <- paste(names(percentages), ": ", percentages, "%", sep = "")

# Create a pie chart
pie(percentages, labels = labels, main = "Percentage of CAET score", col=cbPalette)



#-------------------------------------------------------------------------------
# correlation matrix
#-------------------------------------------------------------------------------

### Step 1 to Step 1

step1_to_1 <- all_TAPE() %>%
  select(c('caet_tot','div_score','syn_score','eff_score','rec_score','res_score','cultf_score','cocr_score', 'human_score', 'circ_score', 'respg_score'))

step1_to_1<- step1_to_1%>%
  rename(             CAET = "caet_tot",
                      Diversity = "div_score",
                      Synergy = "syn_score",
                      Efficiency = "eff_score",
                      Recycling = "rec_score",
                      Resilience = "res_score",
                      'Culture' = "cultf_score",
                      'Co-creation' = "cocr_score",
                      'Human values' = "human_score",
                      'Circularity' = "circ_score",
                      'Governance' = "respg_score"
  )

corelation_step1 <- cor(na.omit(step1_to_1), method = 'spearman')
p.mat <- cor.mtest(step1_to_1, method = 'spearman')$p

# Corrplot

corrplot(corelation_step1, method = "color", type = "lower",
         addCoef.col = "black",
         number.cex = 1.2, # Add correlation coefficient on the plot
         tl.col = "black", tl.srt = 100,
         p.mat = p.mat, # Add matrix of p-values
         sig.level = 0.05, # Set significance level
         insig = "blank" # Do not show insignificant correlations
)


#### Step 2 to Step 1

#install the packages if needed
#install.packages("Himsc")
library(Hmisc)



df<-all_TAPE()%>%
  select('caet_tot' ,"div_score" ,"syn_score","eff_score", "rec_score",  "res_score" , "cultf_score" , "cocr_score", "human_score", "circ_score",  "respg_score" ,
         'dietary_diversity' , 'soil_health' , 'landtenure', 'pest_score', 'Biodiversity_indicator', 'Income', 'ValueAdded', 'youth_score', 'wemp_score' )%>%
  rename(CAET = 'caet_tot',
         Diversity = "div_score",
         Synergy = "syn_score",
         Efficiency = "eff_score",
         Recycling = "rec_score",
         Resilience = "res_score",
         'Culture' = "cultf_score",
         'Co-creation' = "cocr_score",
         'Human' = "human_score",
         'Circularity' = "circ_score",
         'Governance' = "respg_score",
         CAET = 'caet_tot',
         'Dietary Diversity' = "dietary_diversity",
         'Soil Health' = "soil_health",
         Landtenure = "landtenure",
         'Exposure to Pesticides' = "pest_score",
         'Biodiversity Indicator' = "Biodiversity_indicator",
         'Value Added' = "ValueAdded",
         'Youth Empowerment' = "youth_score",
         'Womens Empowerment' = "wemp_score")


rcorrmat_st12st2 <- rcorr(as.matrix(df), type = 'spearman')

rvalues<- rcorrmat_st12st2$r
rvalues<- rvalues[1:11,12:20]
pvalues<-rcorrmat_st12st2$P
pvalues<- pvalues[1:11,12:20]


corrplot(rvalues, method = "color",
         addCoef.col = "black",
         number.cex = 0.6, # Add correlation coefficient on the plot
         tl.col = "black", tl.srt = 100,
         p.mat = pvalues, # Add matrix of p-values
         sig.level = 0.05, # Set significance level
         insig = "blank" # Do not show insignificant correlations
)


#-------------------------------------------------------------------------------
# spiderplot step 1 grouped by location (entered in column location 1)
#-------------------------------------------------------------------------------

CAET_Results <- all_TAPE() %>%
  select(c('key', 'caet_tot', 'div_score', 'syn_score', 'eff_score', 'rec_score', 'res_score', 'cultf_score', 'cocr_score', 'human_score', 'circ_score', 'respg_score', 'location1'))

CAET_Results <- CAET_Results %>%
  group_by(location1) %>%
 summarise_all(., mean, na.rm = T) 


i<-length(CAET_Results)
CAET_Results <- rbind(rep(100,i), rep(0, i), CAET_Results)


CAET_Results <- CAET_Results %>% select(-c('location1', 'key'))

CAET_Results <- CAET_Results %>%
  rename("CAET"= "caet_tot",
         Diversity = "div_score",
         "Synergy" = "syn_score",
         "Efficiency" = "eff_score",
         "Recycling" = "rec_score",
         "Resilience" = "res_score",
         'Culture and food traditions' = "cultf_score",
         'Co-creation and sharing of knowledge' = "cocr_score",
         'Human and social values' = "human_score",
         'Circular and solidarity economy' = "circ_score",
         'Responsible Governance' = "respg_score")



## Define colors and colorblind-friendly color palette
colors <- c('darkolivegreen4', 'darkorange', 'deepskyblue3', 'darkgrey')
# Colorblind-Friendly color palette
cbPalette <- c("#00AFBB","#FC4E07" )

## Plots
par(mfrow = c(1,1),
    mar = c(1,1,1,1))
radarchart(CAET_Results, 
           axistype = 0, # The type of axis label 
           #axislabcol = 'black', # color of axis label and numbers
           #caxislabels = c('0', '100'), # overwriting values specified in axistype
           pcol = cbPalette, # color codes for plot data
           pfcol = scales::alpha(cbPalette, 0.04), # color codes for filling polygons
           plty = 1, # line type for plot data
           plwd = 1,  #  line width for plot data
           cglty = 1, # line type for radar grid
           cglwd = 0.8, # line width for radar grid
           cglcol = 'grey', # line color for radar grids
           vlcex = 0.9, # Font size magnifiction for vlabels
           #title = paste('Bomet')
           #vlabels = " " # in case you want spiderplot without the lables
)

# Add a legend
legend("topleft", 
       legend = c("location name 1", "location name 2"), 
       fill = c("#FC4E07", "#00AFBB"),
       horiz = F, bty = 'u',  text.col = 'black', cex = 1)


#-------------------------------------------------------------------------------
# spiderplot step 2 
#-------------------------------------------------------------------------------

step2_Results <- all_TAPE() %>%
  select(c('key', 'soil_health', 'landtenure', 'pest_score','Biodiversity_indicator', 'youth_score', 'wemp_score',
           'Productivity_scaled', 'ValueAdded_scaled', 'Income_scaled', 'dietary_diversity', 'pest_score', 'location1'))

step2_Results <- step2_Results %>%
  group_by(location1) %>%
  summarise_all(., mean, na.rm = T) 

i <- length(step2_Results)
step2_Results <- rbind(rep(100,i), rep(0, i), step2_Results)


step2_Results <- step2_Results %>% select(-c('location1', 'key'))

step2_Results <- step2_Results %>%
  rename('Dietary Diversity' = "dietary_diversity",
         'Soil Health' = "soil_health",
         Landtenure = "landtenure",
         'Pesticides' = "pest_score",
         'Biodiversity Indicator' = "Biodiversity_indicator",
         'Youth' = "youth_score",
         'Productivity' = 'Productivity_scaled' ,
         'Income' = 'Income_scaled'  ,
         'Value added' = 'ValueAdded_scaled',
         'Women' = "wemp_score")



## Define colors and colorblind-friendly color palette
colors <- c('darkolivegreen4', 'darkorange', 'deepskyblue3', 'darkgrey')
# Colorblind-Friendly color palette
cbPalette <- c("#00AFBB","#FC4E07" )

## Plots
par(mfrow = c(1,1),
    mar = c(1,1,1,1))
radarchart(step2_Results, 
           axistype = 0, # The type of axis label 0 = no label
           #axislabcol = 'black', # color of axis label and numbers
           #caxislabels = c('0', '25', '50', '75', '100'), # overwriting values specified in axistype
           pcol = cbPalette, # color codes for plot data
           pfcol = scales::alpha(cbPalette, 0.04), # color codes for filling polygons
           plty = 1, # line type for plot data
           plwd = 1,  #  line width for plot data
           cglty = 1, # line type for radar grid
           cglwd = 0.8, # line width for radar grid
           cglcol = 'grey', # line color for radar grids
           vlcex = 0.9, # Font size magnifiction for vlabels
           #title = paste('Bomet')
           #vlabels = " " # in case you want spiderplot without the lables
)
# Add a legend
legend("topleft", 
       legend = c("location name 1", "location name 2"), 
       fill = c("#FC4E07", "#00AFBB"),
       horiz = F, bty = 'u',  text.col = 'black', cex = 1)





