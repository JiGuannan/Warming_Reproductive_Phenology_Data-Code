## All data analyses were conducted with R version 4.3.3
# Code 1: Code for reproducing Figure 1 in R language ---------------------
# START
library(ggplot2)
library(hrbrthemes)
library(gghalves)
library(tidyr)
library(readxl)
df_long4_80 <- read_xlsx(path.expand("~/Desktop/data_Fig1.xlsx"))
ggplot(df_long4_80, aes(x = Group, y = St, fill = Group)) +
  theme(panel.background = element_rect(fill = "white"))+
  theme( axis.line = element_line(colour = "black"))+
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=2.5, color="white")+
  theme(panel.grid = element_blank(), legend.position = "right") +
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  labs(y="Temporal trends (days/year)",x="Phenology events") +
  theme(text = element_text(family = "Arial", size = 20))+
  theme(panel.grid = element_blank()) +
  guides(color = "none", fill = "none")+  
  scale_x_discrete(labels = c("Flowering", "Fruiting")) 
# END

# Code 2: Code for reproducing Figure 2 in R language ---------------------
# START
library(ggplot2)
library(readxl)
frt_hshp <- read_xlsx(path.expand("~/Desktop/data_Fig2.xlsx"))
frt_hshp$Category <- factor(frt_hshp$Category, 
                               levels = c("Significant Increase", "Significant Decrease", "Non-significant Increase", "Non-significant Decrease"))
data <- frt_hshp %>% 
  dplyr::group_by(Category) %>% 
  dplyr::summarise(
    avg = mean(slope_FDP),
    med = median(slope_FDP),
    n   = length(slope_FDP),
    .groups = "drop"
  )

annotations <- data.frame(
  Category = levels(frt_hshp$Category),
  avg = data$avg,
  med = data$med,
  n   = data$n
)

annotations$y_position <- ifelse(
  grepl("Increase", annotations$Category),
  -1.5,
  -0.4
)

p <- ggplot(frt_hshp, aes(x = Category, y = slope_FDP)) +
  geom_boxplot(color="black",width=0.3,size=0.4,outlier.alpha =0) +
  stat_boxplot(geom ="errorbar",color='black',size=0.5,width = 0.3) +
  geom_jitter(width = 0.1, aes(color = Category), size = 2,alpha =0.6) + 
  labs(x = "", y = "Temporal trends of RSL (days/year)", fill = "", color = "") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  theme(axis.text.x = element_text(family = "Arial", size = 11.5), 
        axis.text.y = element_text(family = "Arial", size = 13),
        axis.title = element_text(family = "Arial", size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        axis.line = element_line(color = 'black')) +
  scale_color_manual(values = c("Significant Increase" = "#e77d72", "Significant Decrease" = "#56bcc2", "Non-significant Increase" = "lightpink", "Non-significant Decrease" = "lightblue"))+
  theme(legend.position = "none") +
  theme(text = element_text(family = "Arial", size = 20)) +
  scale_x_discrete(labels = c("Significant\nIncrease",
                              "Significant\nDecrease",
                              "Non-significant\nIncrease",
                              "Non-significant\nDecrease" )) +
  geom_text(data = annotations, aes(label = paste("avg:", round(avg,3), "\nmed:", round(med,3), "\nn:", n), y = y_position), vjust = -1, size = 4) 
p
# END

# Code 3: Code for reproducing Figure 3 in R language ---------------------
# START
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(readxl)
frtem_df <- read_xlsx(path.expand("~/Desktop/data_Fig3.xlsx"))
frtem_df$change <- factor(
  frtem_df$change,
  levels = c("Significant Increase",
             "Significant Decrease",
             "Non-significant Increase",
             "Non-significant Decrease")
)
colors <- c(
  "Significant Increase"      = "#e77d72", 
  "Significant Decrease"      = "#56bcc2", 
  "Non-significant Increase"  = "lightpink", 
  "Non-significant Decrease"  = "lightblue"
)

p1 <- ggplot(frtem_df, aes(x = change, y = slope_FDP_tem_mean_year, fill = change)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar", color = 'black', linewidth = 0.5, width = 0.3) +
  labs(y = "Mean temperature during RS (°C/year)", x = "") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colors) +
  theme_minimal(base_family = "Arial") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        axis.line.y = element_line(color = "black"),
        legend.position = "none",
        text = element_text(size = 17)) +
  annotate("text", x = 0.6, y = max(frtem_df_latin$slope_FDP_tem_mean_year), 
           label = "(a)", size = 5, hjust = 0, vjust = 0)
p1

p2 <- ggplot(frtem_df, aes(x = change, y = slope_FDP_tem10_year, fill = change)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar", color = 'black', linewidth = 0.5, width = 0.3) +
  labs(y = "Forcing during RS (°C/year)", x = "") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colors) +
  theme_minimal(base_family = "Arial") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        axis.line.y = element_line(color = "black"),
        legend.position = "none",
        text = element_text(size = 17)) +
  annotate("text", x = 1, y = max(frtem_df_latin$slope_FDP_tem10_year) + 2, 
           label = "(b)", size = 5, hjust = 0.8, vjust = 0)
p2

p3 <- ggplot(frtem_df, aes(x = change, y = slope_mean_FDP_pre_year, fill = change)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar", color = 'black', linewidth = 0.5, width = 0.3) +
  labs(y = "Precipitation during RS (mm/year)", x = "") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colors) +
  theme_minimal(base_family = "Arial") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        axis.line.y = element_line(color = "black"),
        legend.position = "none",
        text = element_text(size = 17)) +
  ylim(-1.5, 1.5) +
  annotate("text", x = 0.9, y = max(frtem_df_latin$slope_mean_FDP_pre_year) + 0.72, 
           label = "(c)", size = 5, hjust = 0.8, vjust = 0)
p3
p <- grid.arrange(p1, p2, p3, ncol = 3)

legend_data <- data.frame(
  change = factor(c("Significantly Increase", "Significantly Decrease", 
                    "Increase Non-significantly", "Decrease Non-significantly"),
                  levels = c("Significantly Increase", "Significantly Decrease", 
                             "Increase Non-significantly", "Decrease Non-significantly"))
)

legend_plot <- ggplot(legend_data, aes(x = change, y = 1, fill = change)) +
  geom_tile() +
  geom_text(aes(label = change), vjust = 1, size = 4) +
  scale_fill_manual(values = c("Significantly Increase" = "#e77d72", 
                               "Significantly Decrease" = "#56bcc2",
                               "Increase Non-significantly" = "lightpink", 
                               "Decrease Non-significantly" = "lightblue")) +
  theme_void() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(family = "Arial", size = 11.6))
legend_gg <- get_legend(legend_plot)
p_with_legend <- ggarrange(p, legend_gg, ncol = 1, heights = c(10, 1))
grid.draw(p_with_legend)
# END

# Code 4: Code for reproducing Figure 4 in R language ---------------------
# START
library(car)
library(lme4)
library(lmerTest)
library(ggplot2)
library(mgcv)
library(MuMIn)
library(partR2) 
library(dplyr)
require(plyr)
library(readxl)
aa <- read_xlsx(path.expand("~/Desktop/data_Fig4.xlsx"))
aa <- aa[complete.cases(aa), ]
names(aa)[1:3] <- c("Year", "Site", "Species")
func_unique <- function(a)
{
  return(data.frame(mFDP = mean(a$FDP),
                    mFDP_tem10 = mean(a$FDP_tem10), 
                    mFDP_tem_mean = mean(a$FDP_tem_mean), 
                    mmean_FDP_pre = mean(a$mean_FDP_pre),
                    sdFDP = sd(a$FDP),
                    sdFDP_tem10 = sd(a$FDP_tem10), 
                    sdFDP_tem_mean = sd(a$FDP_tem_mean), 
                    sdmean_FDP_pre = sd(a$mean_FDP_pre)
                    
  ))
}
b= ddply(aa, .(Site, Species), func_unique)
d= merge(aa, b, by=c("Site","Species"), all.x = TRUE) # keep all rows in a

#centered values
d$cFDP = d$FDP-d$mFDP
d$cFDP_tem10 = d$FDP_tem10-d$mFDP_tem10
d$cFDP_tem_mean = d$FDP_tem_mean-d$mFDP_tem_mean
d$cmean_FDP_pre = d$mean_FDP_pre-d$mmean_FDP_pre

#scaled values
d$sFDP = (d$FDP-d$mFDP)/d$sdFDP
d$sFDP_tem10 = (d$FDP_tem10-d$mFDP_tem10)/d$sdFDP_tem10
d$sFDP_tem_mean = (d$FDP_tem_mean-d$mFDP_tem_mean)/d$sdFDP_tem_mean
d$smean_FDP_pre = (d$mean_FDP_pre-d$mmean_FDP_pre)/d$sdmean_FDP_pre

ff= lmer(sFDP~ sFDP_tem10 + sFDP_tem_mean + smean_FDP_pre+ (1|Species)+(1|Site), data=d)
summary(ff)
r.squaredGLMM(ff)
aic <- summary(ff)$AIC
bic <- BIC(ff)


f1 = lmer(sFDP~ sFDP_tem10 + sFDP_tem_mean + smean_FDP_pre+ (1|Species)+(1|Site), data=d[d$significance=="Significantly Increase",])
summary(f1)
r.squaredGLMM(f1)
rsqu <- r.squaredGLMM(f1)
aic <- summary(f1)$AIC
bic <- BIC(f1)
random_factors <- rsqu[2]-rsqu[1]
unknown_factors <- 1-rsqu[2]
R1 <- partR2(f1, partvars = c("sFDP_tem10", "sFDP_tem_mean", "smean_FDP_pre"),
             R2_type = "marginal", nboot = 10, CI = 0.95)

R2_df <- data.frame(significance = character(), Variable = character(), Importance = numeric(), Importance_Percent = numeric(), stringsAsFactors = FALSE)
Variable <- c( "FDP_tem","FDP_tem_mean", "mean_FDP_pre","random_factors","unknown_factors")
Importance <- c(R1[["R2"]][["estimate"]][1], R1[["R2"]][["estimate"]][2], R1[["R2"]][["estimate"]][3], random_factors, unknown_factors)
Importance_Percent <- Importance/sum(Importance)

R2_df <- rbind(R2_df, data.frame(significance = "Significantly Increase", Variable = Variable, Importance = Importance, Importance_Percent = Importance_Percent))

f2 = lmer(sFDP~ sFDP_tem10 + sFDP_tem_mean + smean_FDP_pre+ (1|Species)+(1|Site), data=d[d$significance=="Significantly Decrease",])
summary(f2)
r.squaredGLMM(f2)
aic <- summary(f2)$AIC
bic <- BIC(f2)
rsqu <- r.squaredGLMM(f2)
random_factors <- rsqu[2]-rsqu[1]
unknown_factors <- 1-rsqu[2]

R2 <- partR2(f2, partvars = c("sFDP_tem10", "sFDP_tem_mean", "smean_FDP_pre"),
             R2_type = "marginal", nboot = 10, CI = 0.95)
Variable <- c( "FDP_tem","FDP_tem_mean", "mean_FDP_pre","random_factors","unknown_factors")
Importance <- c(R2[["R2"]][["estimate"]][1], R2[["R2"]][["estimate"]][2], R2[["R2"]][["estimate"]][3], random_factors, unknown_factors)
Importance_Percent <- Importance/sum(Importance)

R2_df <- rbind(R2_df, data.frame(significance = "Significantly Decrease", Variable = Variable, Importance = Importance, Importance_Percent = Importance_Percent))

f3 = lmer(sFDP~ sFDP_tem10 + sFDP_tem_mean + smean_FDP_pre+ (1|Species)+(1|Site), data=d[d$significance=="Increase Non-significantly",])
summary(f3)
r.squaredGLMM(f3)
aic <- summary(f3)$AIC
bic <- BIC(f3)
rsqu <- r.squaredGLMM(f3)
random_factors <- rsqu[2]-rsqu[1]
unknown_factors <- 1-rsqu[2]
R3 <- partR2(f3, partvars = c("sFDP_tem10", "sFDP_tem_mean", "smean_FDP_pre"),
             R2_type = "marginal", nboot = 10, CI = 0.95)
Variable <- c( "FDP_tem","FDP_tem_mean", "mean_FDP_pre","random_factors","unknown_factors")
Importance <- c(R3[["R2"]][["estimate"]][1], R3[["R2"]][["estimate"]][2], R3[["R2"]][["estimate"]][3], random_factors, unknown_factors)
Importance_Percent <- Importance/sum(Importance)

R2_df <- rbind(R2_df, data.frame(significance = "Non-significantly Increase", Variable = Variable, Importance = Importance, Importance_Percent = Importance_Percent))

f4 = lmer(sFDP~ sFDP_tem10 + sFDP_tem_mean + smean_FDP_pre+ (1|Species)+(1|Site), data=d[d$significance=="Decrease Non-significantly",])
summary(f4)
r.squaredGLMM(f4)
aic <- summary(f4)$AIC
bic <- BIC(f4)
rsqu <- r.squaredGLMM(f4)
random_factors <- rsqu[2]-rsqu[1]
unknown_factors <- 1-rsqu[2]
R4 <- partR2(f4, partvars = c("sFDP_tem10", "sFDP_tem_mean", "smean_FDP_pre"),
             R2_type = "marginal", nboot = 10, CI = 0.95)
Variable <- c( "FDP_tem","FDP_tem_mean", "mean_FDP_pre","random_factors","unknown_factors")
Importance <- c(R4[["R2"]][["estimate"]][1], R4[["R2"]][["estimate"]][2], R4[["R2"]][["estimate"]][3], random_factors, unknown_factors)
Importance_Percent <- Importance/sum(Importance)

R2_df <- rbind(R2_df, data.frame(significance = "Non-significantly Decrease", Variable = Variable, Importance = Importance, Importance_Percent = Importance_Percent))

R2_df$Variable <- factor(R2_df$Variable, levels = c("unknown_factors","mean_FDP_pre", "FDP_tem_mean", "FDP_tem","random_factors"))
p <- ggplot(R2_df, aes(x = significance, y = Importance_Percent, fill = Variable)) +
  geom_bar(stat = "identity", position = position_stack(), width = 0.7) +
  coord_flip() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "", y = "Relative Importance (%)", fill = "Variable") + 
  theme_minimal() +
  theme(text = element_text(family = "Arial", size = 16)) +
  scale_fill_manual(
    values = c("FDP_tem" = "#014F9C","FDP_tem_mean" = "#78A3CC", "mean_FDP_pre" = "#B3CDE4", "random_factors" = "#FFA07A", "unknown_factors" = "#7f7f7f"),
    labels = c("Forcing", "Random", "Temperature","Unexplained","Precipitation"), 
    breaks = c("FDP_tem", "random_factors", "FDP_tem_mean", "unknown_factors", "mean_FDP_pre") 
  ) +
  scale_x_discrete(labels = c("Significantly Increase" = "Significant\nIncrease",
                              "Significantly Decrease" = "Significant\nDecrease",
                              "Non-significantly Increase" = "Non-significant\nIncrease",
                              "Non-significantly Decrease" = "Non-significant\nDecrease")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(ncol = 3),reverse = TRUE) +
  theme(legend.justification = "right", legend.box.just = "right") +
  theme(legend.position = "top", legend.direction = "horizontal")  
p
# END

