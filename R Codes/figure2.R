rm(list=ls(all=TRUE))
library("cjoint")
library("ggplot2")
library("dplyr")
####for rearranging facet plot in ggplots
library("grid")
library("gtable")
####for showing layout of ggplots
library("lemon")

load("/Users/tpliu/Dropbox/Mac/Desktop/Projects/Sexual Harrassement/candidate.rda")


####cjoint method
attribute_list <- list()
attribute_list[["FeatGender"]] <-c("Female", "Male")
attribute_list[["FeatPartyID"]] <- c("Labour Party", "Conservative Party", "Liberal Democrats")
attribute_list[["FeatEUIntegration"]] <- c("Oppose EU Integration", "Support EU Integration", "Neutral")
attribute_list[["FeatScandal"]] <- c("No Scandal", "Having an Extramarital Affair", "Plagiarism",
                                     "Misreporting Funds", "Sexual Harassment")
#attribute_list[["FeatMPartyID"]] <- c("Labour Party", "Conservative Party", "Liberal Democrats")             
#attribute_list[["FeatMScandal"]] <- c("No Scandal", "Having an Extramarital Affair", "Plagiarism",          
#                                      "Sexual Harassment", "Misreporting Funds")

constraint_list <- list()

baselines <- list()
baselines$FeatScandal <- "No Scandal"
baselines$FeatPartyID <- "Conservative Party"

candidate_design <- makeDesign(type='constraints', attribute.levels=attribute_list,
                               constraints=constraint_list)

candidate_uncon <- amce(ChosenCandidate ~ FeatGender + FeatPartyID +
                        FeatEUIntegration + FeatScandal,
                        data=candidate, baselines=baselines,
                        cluster=TRUE, respondent.id="CaseID", design=candidate_design)


summary(candidate_uncon)

## Retrieve the estimated coefficients
beta <- vector()
for(i in 1:length(candidate_uncon[[1]])){
    beta <- c(beta, candidate_uncon[[1]][[i]][1,])
}

## Retrieve the variance-covariance matrix and delete the first (intercept) row and first column (intercept)
vcov <- candidate_uncon[["vcov.prof"]]; vcov <- vcov[2:10,2:10]

## Retrieve standard error through the variance-covariance matrix
se <- sqrt(diag(vcov))

## Calculate lower and upper bounds of each unconditional effetcs
b_low <- beta - 1.96*se
b_high <- beta + 1.96*se
uncon_data <- cbind(data.frame(beta), data.frame(b_low), data.frame(b_high))

#### Name and reorder all the labels/levels
## Create the new labels/levels
name <- c("Oppose EU Integration", "Support EU Integration", "Male",
          "Labour Party",  "Liberal Democrats", "Having an Extramarital Affair",
          "Misreporting Funds", "Plagiarism", "Sexual Harassment")
name <- paste0("    ", name)
name_new <- c(name[3:5],name[2],name[1],name[9],name[7:8],name[6])
## Combine the new labels/levels into the dataframe
uncon_data <- cbind(uncon_data, name)
uncon_data$name
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

t <- ggplot(uncon_data, aes(y=name, x=beta))
png("candidate_unconditional.png", width=1200, height=1200, res=250)
pdf("/Users/tzupingliu/Desktop/Projects/Sexual Harrassement/figure4.pdf")
t + geom_pointrange(aes(xmin =b_low, xmax = b_high), size = 0.3) +
#geom_point() +
#geom_errorbarh(aes(xmin = "b_low", xmax = "b_high"),  size = 0.3, height = 0, na.rm = TRUE) +
labs(x = "Change in Probability (Candidate Preferred)", y = "") +
geom_vline(xintercept=0.0, linetype="dotted") +
scale_y_discrete(limits = rev(name_new)) + theme_minimal() + 
theme(axis.text.y=element_text(hjust=0), axis.title.x=element_text(size=8, face="bold"))
dev.off()
## Next run: candidate_conditional_rgender.R
