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

ShareParty<- vector()
for(i in 1:nrow(candidate)){
  if(candidate[i,]$FeatPartyID == candidate[i,]$PartyID){
    ShareParty[i]  <- "Same"
  }else{
    ShareParty[i]  <- "Different"
  }  
}

candidate <- candidate %>% mutate(ShareParty)
candidate$ShareParty <- factor(candidate$ShareParty) 

#### Create a attribute list
attribute_list <- list()
attribute_list[["FeatGender"]] <-c("Female", "  Male")
attribute_list[["FeatPartyID"]] <- c("Labour Party", "  Conservative Party", "Liberal Democrats")
attribute_list[["FeatEUIntegration"]] <- c("Oppose EU Integration", "Support EU Integration", "Neutral")
attribute_list[["FeatScandal"]] <- c("No Scandal", "Having an Extramarital Affair", "Plagiarism",
                                     "Sexual Harassment", "Misreporting Funds")
attribute_list[["ShareParty"]] <- c("Same", "Different")

#### Create a constraint list
constraint_list <- list()

#### Creat a baseline list
baselines <- list()
baselines$FeatScandal <- "No Scandal"
baselines$FeatPartyID <- "Conservative Party"

#### Make a conjoint design
candidate_design <- makeDesign(type='constraints', attribute.levels=attribute_list,
                               constraints=constraint_list)


candidate_con_sp <- amce(ChosenCandidate ~ FeatGender + FeatEUIntegration +
                           FeatPartyID + ShareParty:FeatScandal,
                         data=candidate, baselines=baselines, cluster=TRUE,
                         respondent.id="CaseID", design=candidate_design)

#### For calculating the conditional effects
## Retrieve the estimated coefficients
beta <- vector()
for(i in 1:length(candidate_con_sp[[1]])){
  beta <- c(beta, candidate_con_sp[[1]][[i]][1,])
}

## Retrieve the variance-covariance matrix and delete the first (intercept) row and first column (intercept)
vcov <- candidate_con_sp[["vcov.prof"]]; vcov <- vcov[2:15,2:15]

## Bootstrap for 100000 samples
candidate_con_sp_draw <- mvrnorm(100000, beta, vcov)

## Retrieve 100000 samples for the estimates of the conditional effects
# First create empty vectors
se_con <- vector()
sm_con <- vector()
sp_con <- vector()
ss_con <- vector()

# Then sum the unconditional and conditional estimates
for(i in 1:100000){
  se_con[i] <- candidate_con_sp_draw[i, 6] + candidate_con_sp_draw[i, 11]
  sm_con[i] <- candidate_con_sp_draw[i, 7] + candidate_con_sp_draw[i, 12]
  sp_con[i] <- candidate_con_sp_draw[i, 8] + candidate_con_sp_draw[i, 13]
  ss_con[i] <- candidate_con_sp_draw[i, 9] + candidate_con_sp_draw[i, 14]
}

# Create the matrix of the four conditional effects (each with 100000 samples)
b_con <- rbind(se_con, sm_con, sp_con, ss_con)

# Retrieve the .025 quantile, .5 quantile, and .975 quantile for each conditional effect
con_low <- apply(b_con, 1, function(x) quantile(x, probs=c(.025)))
con_high <- apply(b_con, 1, function(x) quantile(x, probs=c(.975)))
con_mean <- apply(b_con, 1, function(x) quantile(x, probs=c(.5)))

## Create a dataframe for conditional effects
cm <- cbind(data.frame(con_mean), data.frame(con_low), data.frame(con_high), "Same")
colnames(cm) <- c("b", "low", "high", "Condition")

#### For retrieving all unconditional effects
## Retrieve standard error through the variance-covariance matrix
se <- sqrt(diag(vcov)[1:9])

## Calculate lower and upper bounds of each unconditional effetcs
b_low <- beta[1:9] - 1.96*se
b_high <- beta[1:9] + 1.96*se

## Create a dataframe for unconditional effects
um <- cbind(data.frame(beta[1:9]), data.frame(b_low), data.frame(b_high), "Different")
colnames(um) <- c("b", "low", "high", "Condition")

#### Combining unconditional and conditional effects into one dataframe
p_con_data <- rbind(um, cm)

#### Name and reorder all the labels/levels
## Create the new labels/levels
name <- c("Oppose EU Integration", "Support EU Integration", "Male",
          "Labour Party",  "Liberal Democrats", "Having an Extramarital Affair",
          "Misreporting Funds", "Plagiarism", "Sexual Harassment",
          "Having an Extramarital Affair",
          "Misreporting Funds", "Plagiarism", "Sexual Harassment")

## Combine the new labels/levels into the dataframe
p_con_data <- p_con_data %>% mutate(name)

## Reorder the labels/levels
p_con_data$name <- as.factor(p_con_data$name)
p_con_data$name <- factor(p_con_data$name, levels=c("Male", "Labour Party", "Liberal Democrats",
                                                    "Support EU Integration", "Oppose EU Integration",
                                                    "Sexual Harassment", "Misreporting Funds",
                                                    "Plagiarism", "Having an Extramarital Affair"))

## Rename labels/levels again (put 4 empty strings before the label/level) for plotting
levels(p_con_data$name) <- plyr::mapvalues(levels(p_con_data$name),
                                           c(levels(p_con_data$name)),
                                           c(paste0("    ",levels(p_con_data$name))))

#### Create another vector of labels/levels for plotting
name2 <- c("Male", "Labour Party", "Liberal Democrats",
           "Support EU Integration", "Oppose EU Integration",
           "Sexual Harassment", "Misreporting Funds",
           "Plagiarism", "Having an Extramarital Affair")


#### Add new labels/levels for plotting
name2 <- paste("   ", name2)
name2 <- append(name2, "FeatGender:", after=0)
name2 <- append(name2, "    (Baseline: Female)", after=1)
name2 <- append(name2, "FeatPartyID:", after=3)
name2 <- append(name2, "    (Baseline: Conservative Party)", after=4)
name2 <- append(name2, "FeatEUIntegration:", after=7)
name2 <- append(name2, "    (Baseline: Neutral)", after=8)
name2 <- append(name2, "FeatScandal:", after=11)
name2 <- append(name2, "    (Baseline: No Scandal)", after=12)

name3 <- name2[14:17]

t <- ggplot(p_con_data, aes(y=name, x=b, group = Condition, colour = Condition,shape=Condition)) +  
  geom_point(position = ggstance::position_dodgev(height = 0.7)) + 
  geom_errorbarh(aes_string(xmin = "low", xmax = "high"),  size = 0.3, height = 0, na.rm = TRUE, position = ggstance::position_dodgev(height = 0.7)) + 
  geom_vline(xintercept = 0, colour = gray(1/2), lty = 2) + 
  scale_y_discrete(limits = rev(name3)) + 
  scale_x_continuous("Change in Probability (Candidate Preferred)",breaks=seq(-1,1,0.10)) + 
  theme_minimal() + theme(legend.position = "bottom") + 
  scale_color_manual("Shared Partisanship",values=c("#F8766D","#00BFC4")) + 
  scale_shape_manual("Shared Partisanship",values=c(16,17)) + labs(y="")

ggsave(t,file="/Users/tzupingliu/Desktop/Projects/Sexual Harrassement/Figure_Full/figure7_b_AMCE.png", width = 6.82, height = 5, units = "in")





