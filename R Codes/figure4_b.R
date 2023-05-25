rm(list=ls(all=TRUE))
library("cjoint")
library("ggplot2")
library("dplyr")
#### For rearranging facet plot in ggplots
library("grid")
library("gtable")
#### For showing layout of ggplots
library("lemon")
#### For bootstrap
library("MASS")

load("/Users/tpliu/Dropbox/Mac/Desktop/Projects/Sexual Harrassement/candidate.rda")

#### Create a attribute list
attribute_list <- list()
attribute_list[["FeatGender"]] <-c("Female", "  Male")
attribute_list[["FeatPartyID"]] <- c("Labour Party", "  Conservative Party", "Liberal Democrats")
attribute_list[["FeatEUIntegration"]] <- c("Oppose EU Integration", "Support EU Integration", "Neutral")
attribute_list[["FeatScandal"]] <- c("No Scandal", "Having an Extramarital Affair", "Plagiarism",
                                     "Sexual Harassment", "Misreporting Funds")

#### Create a constraint list
constraint_list <- list()

#### Creat a baseline list
baselines <- list()
baselines$FeatScandal <- "No Scandal"
baselines$FeatPartyID <- "Conservative Party"

#### Make a conjoint design
candidate_design <- makeDesign(type='constraints', attribute.levels=attribute_list,
                               constraints=constraint_list)

#### Subset the data
candidate_f <- candidate %>% filter(Gender=="Female")
candidate_m <- candidate %>% filter(Gender=="Male")

#### AMCE by respondent's gender
candidate_con_rfg_f <- amce(ChosenCandidate ~ FeatPartyID + FeatEUIntegration +
                              FeatGender:FeatScandal,
                            data=candidate_f, baselines=baselines, cluster=TRUE,
                            respondent.id="CaseID", design=candidate_design)

candidate_con_rfg_m <- amce(ChosenCandidate ~ FeatPartyID + FeatEUIntegration +
                              FeatGender:FeatScandal,
                            data=candidate_m, baselines=baselines, cluster=TRUE,
                            respondent.id="CaseID", design=candidate_design)

#### For calculating the conditional effects by respondent's gender
## Retrieve the estimated coefficients by respondent's gender
beta_f <- vector()
for(i in 1:length(candidate_con_rfg_f[[1]])){
  beta_f <- c(beta_f, candidate_con_rfg_f[[1]][[i]][1,])
}

beta_m <- vector()
for(i in 1:length(candidate_con_rfg_m[[1]])){
  beta_m <- c(beta_m, candidate_con_rfg_m[[1]][[i]][1,])
}

## Retrieve the variance-covariance matrix and delete the first (intercept) row and first column (intercept)
vcov_f <- candidate_con_rfg_f[["vcov.prof"]]; vcov_f <- vcov_f[2:14,2:14]
vcov_m <- candidate_con_rfg_m[["vcov.prof"]]; vcov_m <- vcov_m[2:14,2:14]

## Bootstrap for 100000 samples
candidate_con_rfg_f_draw <- mvrnorm(100000, beta_f, vcov_f)
candidate_con_rfg_m_draw <- mvrnorm(100000, beta_m, vcov_m)

## Retrieve 100000 samples for the estimates of the conditional effects
# First create empty vectors
me_con_f <- vector()
mm_con_f <- vector()
mp_con_f <- vector()
ms_con_f <- vector()
me_con_m <- vector()
mm_con_m <- vector()
mp_con_m <- vector()
ms_con_m <- vector()

# Then sum the unconditional and conditional estimates
for(i in 1:100000){
  me_con_f[i] <- candidate_con_rfg_f_draw[i, 6] + candidate_con_rfg_f_draw[i, 10]
  mm_con_f[i] <- candidate_con_rfg_f_draw[i, 7] + candidate_con_rfg_f_draw[i, 11]
  mp_con_f[i] <- candidate_con_rfg_f_draw[i, 8] + candidate_con_rfg_f_draw[i, 12]
  ms_con_f[i] <- candidate_con_rfg_f_draw[i, 9] + candidate_con_rfg_f_draw[i, 13]
  me_con_m[i] <- candidate_con_rfg_m_draw[i, 6] + candidate_con_rfg_m_draw[i, 10]
  mm_con_m[i] <- candidate_con_rfg_m_draw[i, 7] + candidate_con_rfg_m_draw[i, 11]
  mp_con_m[i] <- candidate_con_rfg_m_draw[i, 8] + candidate_con_rfg_m_draw[i, 12]
  ms_con_m[i] <- candidate_con_rfg_m_draw[i, 9] + candidate_con_rfg_m_draw[i, 13]
}

# Create the matrix of the four conditional effects (each with 100000 samples)
b_con <- rbind(me_con_f, mm_con_f, mp_con_f, ms_con_f, me_con_m, mm_con_m, mp_con_m, ms_con_m)

# Retrieve the .025 quantile, .5 quantile, and .975 quantile for each conditional effect
con_low <- apply(b_con, 1, function(x) quantile(x, probs=c(.025)))
con_high <- apply(b_con, 1, function(x) quantile(x, probs=c(.975)))
con_mean <- apply(b_con, 1, function(x) quantile(x, probs=c(.5)))

## Create a dataframe for conditional effects
cm <- cbind(data.frame(con_mean), data.frame(con_low), data.frame(con_high), data.frame(rep("Male", 8)),
            data.frame(c(rep("Female", 4), rep("Male", 4))))
colnames(cm) <- c("b", "low", "high", "Feature", "Respondent")

#### For retrieving all unconditional effects
## Retrieve standard error through the variance-covariance matrix
se_f <- sqrt(diag(vcov_f)[1:9])
se_m <- sqrt(diag(vcov_m)[1:9])

## Calculate lower and upper bounds of each unconditional effetcs
b_low_f <- beta_f[1:9] - 1.96*se_f
b_high_f <- beta_f[1:9] + 1.96*se_f
b_low_m <- beta_m[1:9] - 1.96*se_m
b_high_m <- beta_m[1:9] + 1.96*se_m

## Create a dataframe for unconditional effects
um_f <- cbind(data.frame(beta_f[1:9]), data.frame(b_low_f), data.frame(b_high_f), "Female", "Female")
um_m <- cbind(data.frame(beta_m[1:9]), data.frame(b_low_m), data.frame(b_high_m), "Female", "Male")
colnames(um_f) <- c("b", "low", "high", "Feature", "Respondent")
colnames(um_m) <- c("b", "low", "high", "Feature", "Respondent")
um <- rbind(um_f, um_m)


#### Combining unconditional and conditional effects into one dataframe
rfg_con_data <- rbind(um, cm)

#### Name and reorder all the labels/levels
## Create the new labels/levels
name <- c("Oppose EU Integration", "Support EU Integration", "Male",
          "Labour Party",  "Liberal Democrats", "Having an Extramarital Affair",
          "Misreporting Funds", "Plagiarism", "Sexual Harassment",
          "Oppose EU Integration", "Support EU Integration", "Male",
          "Labour Party",  "Liberal Democrats", "Having an Extramarital Affair",
          "Misreporting Funds", "Plagiarism", "Sexual Harassment",
          "Having an Extramarital Affair",
          "Misreporting Funds", "Plagiarism", "Sexual Harassment",
          "Having an Extramarital Affair",
          "Misreporting Funds", "Plagiarism", "Sexual Harassment")

## Combine the new labels/levels into the dataframe
rfg_con_data <- rfg_con_data %>% mutate(name)

## Reorder the labels/levels
rfg_con_data$name <- as.factor(rfg_con_data$name)
rfg_con_data$name <- factor(rfg_con_data$name, levels=c("Male", "Labour Party", "Liberal Democrats",
                                                        "Support EU Integration", "Oppose EU Integration",
                                                        "Sexual Harassment", "Misreporting Funds",
                                                        "Plagiarism", "Having an Extramarital Affair"))

## Rename labels/levels again (put 4 empty strings before the label/level) for plotting
levels(rfg_con_data$name) <- plyr::mapvalues(levels(rfg_con_data$name),
                                             c(levels(rfg_con_data$name)),
                                             c(paste0("    ",levels(rfg_con_data$name))))

#### Create another vector of labels/levels for plotting
name2 <- c("Male", "Labour Party", "Liberal Democrats",
           "Support EU Integration", "Oppose EU Integration",
           "Sexual Harassment", "Misreporting Funds",
           "Plagiarism", "Having an Extramarital Affair")

#### Add new labels/levels for plotting
name2 <- paste("   ", name2)
name2 <- append(name2, "Gender", after=0)
name2 <- append(name2, "    (Baseline: Female)", after=1)
name2 <- append(name2, "PartyID", after=3)
name2 <- append(name2, "    (Baseline: Conservative Party)", after=4)
name2 <- append(name2, "EU Integration", after=7)
name2 <- append(name2, "    (Baseline: Neutral)", after=8)
name2 <- append(name2, "Scandal", after=11)
name2 <- append(name2, "    (Baseline: No Scandal)", after=12)

#### Create another name vector
name3 <- name2[14:17]

#### Only plot the effects of scandals

rfg_con_data$name <- trimws(as.character(rfg_con_data$name))
rfg_con_data <- subset(rfg_con_data,rfg_con_data$name %in% c("Sexual Harassment","Misreporting Funds","Plagiarism","Having an Extramarital Affair"))

rfg_con_data$Feature <- ifelse(rfg_con_data$Feature %in% "Male","Male Candidate Feature","Female Candidate Feature")
rfg_con_data$Respondent <- ifelse(rfg_con_data$Respondent %in% "Female","Female Respondents","Male Respondents")

t <- ggplot(rfg_con_data, aes(y=name, x=b, colour = Feature,shape=Feature)) + facet_wrap(~Respondent,ncol=1) + geom_point(position = ggstance::position_dodgev(height = 0.7)) + geom_errorbarh(aes_string(xmin = "low", xmax = "high"),  size = 0.3, height = 0, na.rm = TRUE, position = ggstance::position_dodgev(height = 0.7)) + theme_minimal() + scale_color_manual("Candidate Feature Gender",values=c("#F8766D","#00BFC4")) + scale_shape_manual("Candidate Feature Gender",values=c(16,17)) + labs(y="") + geom_vline(xintercept = 0, colour = gray(1/2), lty = 2)  + scale_x_continuous("Change in Probability (Candidate Preferred) by Candidate & Respondent Gender",breaks=seq(-1,1,0.10)) + theme(legend.position = "bottom")
ggsave(t,file="figure7_a_original.png", width = 7.30, height = 5, units = "in")
