
# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory

filename="ugrad_dis_2022.csv"
library(dplyr)
mydata=read.csv(filename)
mydata <- mydata %>%
  mutate(School_Type = case_when(Sector %in% c("Private for-profit, 2-year", "Private not-for-profit, 2-year", "Public, 2-year") ~ "2-year",
                                 TRUE ~ "4-year")) %>%
  mutate(Sector = case_when(Sector == "Private for-profit, 2-year" ~ "Private for-profit, 2-year",
                            Sector == "Private not-for-profit, 2-year" ~ "Private not-for-profit, 2-year",
                            Sector == "Public, 2-year" ~ "Public, 2-year",
                            Sector == "Public, 4-year or above" ~ "Public, 4-year",
                            Sector == "Private not-for-profit, 4-year or above" ~ "Private not-for-profit, 4-year",
                            Sector == "Private for-profit, 4-year or above" ~ "Private for-profit, 4-year"))
  
# see data ----------------------------------------------------------


head(mydata)
mean(mydata$Distance_Ed_Percent)
median(mydata$Distance_Ed_Percent)


# see data types ----------------------------------------------------------

str(mydata)

# deliverable 1 ----------------------------------------------------------
library(ggplot2)
library(dplyr)
sourceText='Source: National Center for Education Statistics, U.S. Department of Education'
Fill_Color="#EA90C1"

mydata1 <- mydata %>%
  filter(Distance_Ed_Percent > 21)

absoluteT = table(mydata1$Sector,
                  exclude = 'nothing')

absoluteT

propT = prop.table(absoluteT)*100

tableFreq=as.data.frame(absoluteT)
names(tableFreq)=c("Sector","Count")
tableFreq$Percent=as.vector(propT)
tableFreq

# thelabels<- paste0(tableFreq$Sector, ':  ', round(tableFreq$Percent,1), '%')

base= ggplot(data=tableFreq, aes(x = reorder(Sector, Percent),
                                 y = Percent)) 
del1Draft= base + geom_bar(fill=Fill_Color, stat = 'identity') + 
  labs(title="Which Sectors Have Over 1/5 of Undergrads Pursuing Distance Ed?",
       subtitle="Higher education institutions in the United States, 2022",
       caption = sourceText) + 
  coord_flip() +
  theme_void() + 
  geom_text(
    aes(label = paste0(Sector, ': ', round(Percent,0), '%'), 
        hjust = ifelse(Percent > 25, 1.08, -.025)
        ),
    color = "black", size = 4.5) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) 
del1Draft


# save del1Draft ----------------------------------------------------------
saveRDS(del1Draft, file = "del1Draft.rds")


# deliverable 2 ----------------------------------------------------------

base= ggplot(data=mydata) 
md <- median(mydata$Distance_Ed_Percent)
txtMedian=paste0('Median: ',round(md), '%')

del2Draft= base + geom_histogram(aes(x = Distance_Ed_Percent), binwidth = 1, fill=Fill_Color, color="black") +
  labs(title = "How Many Schools Have High Rates of Fully Dist-Ed Undergrads?",
       subtitle = "U.S. institutions with 1%+ of undergrads enrolled in distance education, 2022",
       x = "Percentage of Undergraduates Enrolled in Distanced Education", y = "Number of Schools",
       caption = sourceText) + 
  geom_vline(xintercept = md,color='darkblue') + 
  annotate(geom = 'text', color='darkblue',
           label=txtMedian, 
           y = 81, x = md+2, angle = 90) + 
  theme_minimal() 
del2Draft


# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft, file = "del2Draft.rds")


# deliverable 3 ----------------------------------------------------------

del3Draft= base + geom_point(aes(x=Distance_Ed_Percent,
                                 y=Instate_Tuition, color=School_Type)) +
  theme_minimal() + 
  labs(title = "How Much is Tuition for Undergrads Pursuing Distance Ed?",
       subtitle = "Distance undergrad education & tuition rates in U.S. schools, 2022",
       x = "Percent Undergraduates Enrolled Exclusively in Distance Education", y = "In-State Tuition for Full-Time Undergrads",
       caption = sourceText) +
  scale_color_manual(values = c("darkblue", "#F74AA9")) +
  guides(color = guide_legend(title = "School Type")) + 
  theme(legend.position = c(0.815, 0.77), legend.background = element_rect(fill="white",
                                                                          size=0.25, linetype = 1, color = "gray"))
del3Draft 

# save del3Draft ----------------------------------------------------------
saveRDS(del3Draft, file = "del3Draft.rds")


# deliverable 4  ----------------------------------------------------------

library(sf)
library(plotly)
library(usmap)
library(paletteer)
state_map <- usmap::us_map()

head(state_map)
head(mydata)

state_tu <- mydata %>% filter(School_Type=="4-year") %>% group_by(State) %>%
  summarize(Median_Instate_Tuition = median(Instate_Tuition, na.rm = TRUE))

# merge data into map ----------------------------------------------------------
myMapDistEd=merge(state_map,state_tu, by.x="full", "State")

# prepare plot

base=ggplot(myMapDistEd)

del4Draft=base + geom_sf(aes(fill=Median_Instate_Tuition), color = "#242424") + 
  scale_fill_viridis_c(option = "F", direction = -1) +
  # scale_fill_paletteer_c("viridis::plasma", direction = 1) +
  # scale_fill_gradient(high = "black", low = "white") +
  # scale_fill_gradient2(high = "navyblue", low = "deeppink", mid = "violet", midpoint = 20000) +
  labs(title = "Where does Distance Education at 4-Year Schools Cost the Most?",
       subtitle = "Median In-State Tuition (USD) for Full-Time Undergraduate Students, 2022",
       caption = sourceText, fill = "Tuition") +
  theme_void() + 
  theme(legend.position = c(0.065, 0.65))

del4Draft

# save del4Draft ----------------------------------------------------------
saveRDS(del4Draft, file = "del4Draft.rds")



# deliverable 4 ALT2 -------------------------------------------------------

# deliverable 4 ALT3 -------------------------------------------------------

# head(state_map)
# head(mydata)
# 
# state_tu <- mydata %>% filter(School_Type=="2-year") %>% group_by(State) %>%
#   summarize(Mean_Instate_Tuition = mean(Instate_Tuition, na.rm = TRUE))
# 
# # merge data into map ----------------------------------------------------------
# myMapDistEd=merge(state_map,state_tu, by.x="full", "State")
# 
# # prepare plot
# 
# base=ggplot(myMapDistEd)
# 
# alt3del4Draft=base + geom_sf(aes(fill=Mean_Instate_Tuition)) + 
#   scale_fill_viridis_c(direction = -1) +
#   # scale_fill_gradient2(high = "darkgreen", low = "white", mid = "seagreen", midpoint = 13000) +
#   labs(title = "Mean 2-Year Tuition by State, 2022",
#        caption = sourceText, legend = "Mean In-State Tuition") +
#   theme_void()
# 
# alt3del4Draft
# 
# # deliverable 4 ALT4 -------------------------------------------------------
# 
# head(state_map)
# head(mydata)
# 
# state_tu <- mydata %>% filter(School_Type=="2-year") %>% group_by(State) %>%
#   summarize(Median_Instate_Tuition = median(Instate_Tuition, na.rm = TRUE))
# 
# # merge data into map ----------------------------------------------------------
# myMapDistEd=merge(state_map,state_tu, by.x="full", "State")
# 
# # prepare plot
# 
# base=ggplot(myMapDistEd)
# 
# alt4del4Draft=base + geom_sf(aes(fill=Median_Instate_Tuition)) + 
#   scale_fill_viridis_c(direction = -1) +
#   # scale_fill_gradient2(high = "darkgreen", low = "white", mid = "seagreen", midpoint = 13000) +
#   labs(title = "Median 2-Year Tuition by State, 2022",
#        caption = sourceText, legend = "Median In-State Tuition") +
#   theme_void()
# 
# alt4del4Draft

# deliverable 4 ALT5 -------------------------------------
# 
# head(state_map)
# head(mydata)
# 
# state_tu <- mydata %>% filter(School_Type=="4-year") %>% group_by(State) %>%
#   summarize(Median_Instate_Tuition = median(Instate_Tuition, na.rm = TRUE))
# 
# # merge data into map ----------------------------------------------------------
# myMapDistEd=merge(state_map,state_tu, by.x="full", "State")

# prepare plot
# 
# base=ggplot(myMapDistEd)
# 
# alt5del4Draft=base + geom_sf(aes(fill=Median_Instate_Tuition)) + 
#   scale_fill_viridis_c(direction = -1) +
#   # scale_fill_gradient2(high = "darkgreen", low = "white", mid = "seagreen", midpoint = 13000) +
#   labs(title = "Median 4-Year Tuition by State, 2022",
#        caption = sourceText, legend = "Median In-State Tuition") +
#   theme_void()
# 
# alt5del4Draft

# deliverable 4 ALT6 ---------------------------------
# 
# head(state_map)
# head(mydata)
# 
# state_tu <- mydata %>% group_by(State) %>%
#   summarize(Median_Instate_Tuition = median(Instate_Tuition, na.rm = TRUE))
# 
# # merge data into map ----------------------------------------------------------
# myMapDistEd=merge(state_map,state_tu, by.x="full", "State")
# 
# # prepare plot
# 
# base=ggplot(myMapDistEd)
# 
# alt6del4Draft=base + geom_sf(aes(fill=Median_Instate_Tuition)) + 
#   scale_fill_viridis_c(direction = -1) +
#   # scale_fill_gradient2(high = "darkgreen", low = "white", mid = "seagreen", midpoint = 13000) +
#   labs(title = "Median Tuition by State, 2022",
#        caption = sourceText, legend = "Median In-State Tuition") +
#   theme_void()
# 
# alt6del4Draft