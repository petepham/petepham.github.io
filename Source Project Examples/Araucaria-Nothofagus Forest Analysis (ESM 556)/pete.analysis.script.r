######## Libraries #######

library(caret)
library(leaps)
library(egg)
library(formatR)
library(broom)
library(plyr)
library(ggpubr)
library(ggthemes)
library(hrbrthemes)
library(kableExtra)
library(knitr)
library(magrittr)
library(tidyverse)
library(xtable)
library(pander) 
library(dunn.test)

# regresion tree packages
library(rsample)
library(rpart)
library(rpart.plot)
library(ipred)
library(randomForest)

######## Data Import and Manipulation #######
setwd("~/R/School/[ESM 566] Environmental Data Analysis [R]/Projects")
dt = data.frame(read_csv("ChinaMuerta.csv"))

spp<-subset(dt, origen !="none")
spp<-droplevels(spp)

# Total species richness
dim(tapply(spp$origen, spp$species, length))  #66 spp

# Species richness by treatment
ddply(spp, c("site","cod.sev"), function(df)
  return(c(species=length(unique(df$species)))))

# Species richness by subplot
plot.rich<-ddply(spp, c("site","n.plot" ,"cod.sev", "transect"), function(df)
  return(c(species=length(unique(df$species)))))

# spread function
#avoiding to lose subplot without species, and filling them with zeros
dt1<-spread(plot.rich, transect, species, fill=0)


# gather function & reorganizing subplots (transects) in one column
dt2<-gather(dt1, "east", "north", "south", "west", key="transect", value=species)

# Set cod.sev as a factor and assign a specific order
dt2$cod.sev = factor(dt2$cod.sev,
                     levels = c("UN","L","H"),
                     ordered=TRUE)

View(dt2)

######## DPLYR Equivalent Data Import and Manipulation #######
df = dt %>%
  filter(origen != "NA") %>%
  select(species, cod.sev, slope, aspect, elev.m, 
         cov.50N, cov.50E, cov.50S, cov.50W, cov.140N, cov.140E, cov.140S, 
         cov.140W) %>%
  rowwise() %>%
  mutate(c50.bar = mean(c(cov.50N, cov.50E, cov.50S, cov.50W), na.rm=TRUE),
         c140.bar = mean(c(cov.140N, cov.140E, cov.140S, cov.140W), na.rm=TRUE),
         cod.sev = recode(cod.sev,"UN" = "1", "L" = "2", "H" = "3")) %>%
  group_by(cod.sev, aspect, slope, elev.m) %>%
  summarize(c50.mean = mean(c50.bar, na.rm = TRUE),
            c140.mean = mean(c140.bar, na.rm = TRUE),
            species = length(species))

View(df)



####### Plots for Visual Data Descriptions #######
# Creating a boxplot for the groups
bp = ggplot(dt2, aes(y=species, fill=cod.sev, x=cod.sev)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.1, alpha=0.9) +
  theme_economist() +
  theme(legend.position="top",
        legend.text=element_text(size=10)) +
  scale_fill_manual("Variables:",
                    values=c("darkseagreen","darkgoldenrod3","darkred"),
                    labels = c("UN - Unburned",
                               "L - Low Burn", 
                               "H - High Burn")) +
  labs(y = expression(italic("Count of Unique Species")),
       x = expression(italic("Burn Severity Group")),
       subtitle = expression(
         bold(
           "")))

# Creating a histogram for the frequency of the species count 
# make the line thicker and fill 
hg = ggplot(dt2, aes(x=species, fill = cod.sev, color=cod.sev)) +
  geom_histogram(binwidth=1, alpha=0.5, position="identity")+
  theme_economist() +
  scale_fill_manual("Variable",
                    values=c("darkseagreen","darkgoldenrod3","darkred")) +
  scale_color_manual("Variable",
                    values=c("black","black","black")) +
  labs(y = expression(italic("Frequency of Occurence")),
       x = expression(italic("Count of Unique Species")),
       subtitle = expression(
         bold(
           "")
       )) + 
  theme(legend.position = "none")

# Using ggarrange to combine both plots into a single output
bph = ggarrange(bp, hg, 
                nrow=1, 
                common.legend=TRUE,
                legend="bottom")

# Annotate Figure
annotate_figure(bph,
                fig.lab = "Distribution of Species Richness by Burn Severity Group", 
                fig.lab.face="bold.italic",
                fig.lab.pos = "top.left",
                fig.lab.size=12,
                bottom = text_grob("Figure 1",
                                   hjust = 1,
                                   face = "italic", 
                                   size = 10),)



####### Tables for Numerical Data Descriptions #######
# Table 1: Summary Statistics for groups

summary = bind_rows( # We are creating two tables, this function will bind them together

  dt2 %>% # Produce summary statistics by burn group
    dplyr::group_by(cod.sev) %>%
    dplyr::summarise(
      Minimum = min(species),
      Q1 = quantile(species, 0.25),
      Median = median(species),
      Mean = mean(species),
      S.Error = sd(species)/sqrt(length(species)),
      S.D. = sd(species),
      Q3 = quantile(species, 0.75),
      Maximum = max(species)
    ),

  dt2 %>% # Produce summary statistics for all groups combined
    summarise(
      Minimum = min(species),
      Q1 = quantile(species, 0.25),
      Median = median(species),
      Mean = mean(species),
      S.Error = sd(species)/sqrt(length(species)),
      S.D. = sd(species),
      Q3 = quantile(species, 0.75),
      Maximum = max(species)
    ) %>%
    mutate(cod.sev = "All")
)

# Using KableExtra package to clean up table data and to add styling options
kable(summary,
      caption =
        "Summary Statistics for Species Richness by Burn Severity",
      col.names = c("Burn Severity", names(summary)[-1]),
      align="c",
      booktabs = T,
      digits=2) %>%
  kable_styling(position = "center", latex_options="hold_position") %>%
  row_spec(0, bold=TRUE)


####### Assessment of Normality & Constant Variance #####
hg.all = dt2 %>%
  mutate(cod.sev = fct_reorder(cod.sev,species)) %>%
  ggplot(aes(x=species, fill=cod.sev, color=cod.sev)) +
  geom_histogram(binwidth=1, alpha=0.6, position="identity")+
  theme_economist() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Number of Species Identified per Plot") +
  ylab("Frequency") +
  facet_wrap(~cod.sev)

hg.all


sw.all = dt2 %>%
  dplyr::group_by(cod.sev) %>%
  dplyr::summarize(
    n = length(species),
    SW = shapiro.test(species)$statistic)


sw.all

leveneTest(species ~ cod.sev, data=dt2)


####### Regression Tree #####

# First Step Model
rt1 = rpart(
  formula = species ~ .,
  data = df,
  control = list(minsplit = 10, maxdepth = 12, xval = 10))

rpart.plot(rt1)




# Seed & Setup
set.seed(9957)

sample.df = df$species %>%
  createDataPartition(p = 0.80,
                      list = FALSE)

train.df = df[sample.df, ]
test.df = df[-sample.df, ]

# Random Forest Model
rf1 = train(
  species ~ ., 
  data = df,
  method = "rf",
  trControl = trainControl("cv", number = 50)
  )

rf1$bestTune

rf1$finalModel

predict = rf1 %>%
  predict(test.df)

head(pclass)
RMSE(predict, test.df$species)

varImpPlot(rf1$finalModel, type = 1)

####### ANOVA Test ######
# ANOVA
anova = aov(species~cod.sev, data = dt2)

pander(anova)

# Kruskall-Wallce
kw = kruskal.test(species ~ cod.sev, data = dt2)
kw
pander(kw)

# Post Hoc Analysis
?dunn.test
dunn.test(dt2$species, g=dt2$cod.sev, kw=TRUE)



