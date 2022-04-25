library(tidyverse)
library(summarytools)

data <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Data Analyst Certification\\Case Study\\Travel Insurance\\travel_insurance.csv")

summarytools::view(dfSummary(data))

# Cleaning data is not necessary, just adjust the types of variables
data[,c(2,3,6:9)] <- lapply(data[,c(2,3,6:9)], function(x){as.factor(x)})
data <- data %>% mutate(value = 1)

# Questions: difference in travel habits between customer and non customers
#1. travel often (buy tickets from frequent flyer miles)
#2. travel abroad

gg1 <- ggplot(data) +
  aes(x = FrequentFlyer, y = value, fill = TravelInsurance) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c(`0` = "#E6E6EA",`1` = "#F56476")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(colour = "black", face = "bold", size = 9),
        legend.position = "none")


gg2 <- ggplot(data) +
  aes(x = EverTravelledAbroad, y = value, fill = TravelInsurance) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c(`0` = "#E6E6EA",`1` = "#F56476")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_text(colour = "black", face = "bold", size = 9),
        legend.position = "none")

gridExtra::grid.arrange(gg1,gg2, ncol = 2)

chisq.test(data$TravelInsurance, data$FrequentFlyer)
chisq.test(data$TravelInsurance, data$EverTravelledAbroad)


# Relationship between continuous (numerical) variables
g1 <- ggplot(data) + # Density by Age
 aes(x = Age, fill = TravelInsurance) +
 geom_density(adjust = 1L, size = 0.72, alpha = 0.7, color = "white") +
 scale_fill_manual(values = c(`0` = "#E6E6EA", 
`1` = "#F56476")) +
 labs(x = "Age", y = "") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white", size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        axis.ticks.y = element_blank(),axis.text.y = element_blank(),
        axis.title.x = element_text(colour = "black", face = "bold", size = 9))

g2 <- ggplot(data) + # Density by Income
 aes(x = AnnualIncome, fill = TravelInsurance) +
 geom_density(adjust = 1L, size = 0.72, alpha = 0.7, color = "white") +
 scale_fill_manual(values = c(`0` = "#E6E6EA", 
`1` = "#F56476")) +
 labs(x = "Anual Income", y = "") +
  theme(legend.position =  "none",
        panel.background = element_rect(fill = "white", colour = "white", size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        axis.ticks.y = element_blank(),axis.text.y = element_blank(),
        axis.title.x = element_text(colour = "black", face = "bold", size = 9))

g3 <- ggplot(data) + # Density by Family members
 aes(x = FamilyMembers, fill = TravelInsurance) +
 geom_density(adjust = 1L, size = 0.72, alpha = 0.7, color = "white") +
 scale_fill_manual(values = c(`0` = "#E6E6EA", 
`1` = "#F56476")) + theme(legend.position = "none") +
 labs(x = "Family Members", y = "") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white", size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        axis.ticks.y = element_blank(),axis.text.y = element_blank(),
        axis.title.x = element_text(colour = "black", face = "bold", size = 9))

gridExtra::grid.arrange(g1,g2,g3, ncol = 1, top = grid::textGrob("Distribution of Continuous Values by Status", gp = grid::gpar(fotsize = 12, font = 2)))

# Remaining variables - employment type, graduate, chronic disease
chisq.test(data$TravelInsurance, data$Employment.Type)
chisq.test(data$TravelInsurance, data$GraduateOrNot)
chisq.test(data$TravelInsurance, data$ChronicDiseases)

data_tidy <- data %>% select(2,3,6,9)

a <- data_tidy %>% group_by(Employment.Type) %>%
  summarise(N = n(), Yes = sum(TravelInsurance == "1")) %>%
  mutate(Prop = Yes/N) %>%
  rename(Character = Employment.Type)

b <- data_tidy %>% group_by(GraduateOrNot) %>%
  summarise(N = n(), Yes = sum(TravelInsurance == "1")) %>%
  mutate(Prop = Yes/N) %>%
  rename(Character = GraduateOrNot)

c <- data_tidy %>% group_by(ChronicDiseases) %>%
  summarise(N = n(), Yes = sum(TravelInsurance == "1")) %>%
  mutate(Prop = Yes/N) %>%
  rename(Character = ChronicDiseases)

abc <- rbind(a,b,c)
abc$Character <- c("Government Sector","Private Sector/Self Employed", "Not Graduate", "Graduate", "Dont Have Chronic Diseases", "Have Chronic Diseases")
abc$Group <- c("Employment Type", "Employment Type", "Graduation", "Graduation", "Chronic Diseases", "Chronic Diseases")
abc$Relevant <- c("Yes","Yes","No","No","No","No")
abc <- abc %>% mutate(order = fct_reorder(Character, Prop))

ggplot(abc, aes (x = order, y = Prop, fill = Relevant)) +
  geom_col() +
  scale_y_continuous(limits = c(0,0.65)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size =0.65) +
  coord_flip() +
  scale_fill_manual(
    values = c(No = "#E6E6EA",Yes = "#F56476")) +
 theme(panel.background = element_rect(fill = "white", colour = "white",size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"),
  axis.title.x = element_text(face = "bold", size = 9),
  legend.position = "none") + 
  facet_wrap(vars(Group)) +
  labs(title = "", x= "", y = "Proportion")


gt1 <- abc %>% filter(Group == "Employment Type") %>%
  ggplot(aes(x = order, y = Prop, fill = Relevant)) +
  geom_col() +
  scale_y_continuous(limits = c(0,0.55)) +
  coord_flip() +
  scale_fill_manual(
    values = c(No = "#E6E6EA",Yes = "#F56476")) +
  theme(panel.background = element_rect(fill = "white", colour = "white",size = 2, linetype = "solid"),
        legend.position = "none") + 
  facet_wrap(vars(Group)) +
  labs(title = "", x= "", y = "")

gt2 <- abc %>% filter(Group == "Graduation") %>%
  ggplot(aes(x = order, y = Prop, fill = Relevant)) +
  geom_col() +
  scale_y_continuous(limits = c(0,0.55)) +
  coord_flip() +
  scale_fill_manual(
    values = c(No = "#E6E6EA",Yes = "#F56476")) +
  theme(panel.background = element_rect(fill = "white", colour = "white",size = 2, linetype = "solid"),
        axis.title.x = element_text(face = "bold", size = 9),legend.position = "none") + 
  facet_wrap(vars(Group)) +
  labs(title = "", x= "", y = "Proportion")

gt3 <- abc %>% filter(Group == "Chronic Diseases") %>%
  ggplot(aes(x = order, y = Prop, fill = Relevant)) +
  geom_col() +
  scale_y_continuous(limits = c(0,0.55)) +
  coord_flip() +
  scale_fill_manual(
    values = c(No = "#E6E6EA",Yes = "#F56476")) +
  theme(panel.background = element_rect(fill = "white", colour = "white",size = 2, linetype = "solid"),
        legend.position = "none") + 
  facet_wrap(vars(Group)) +
  labs(title = "", x= "", y = "")

gridExtra::grid.arrange(gt1,gt2,gt3, ncol = 3, top = grid::textGrob("Hallo", gp = grid::gpar(fotsize = 12, font = 2)))


# Other Insights - odds chance
reglog <- glm(TravelInsurance ~ ., data = data, family = "binomial")
summary(reglog)
exp(reglog$coefficients)
