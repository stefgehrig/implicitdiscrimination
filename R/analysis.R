# load libraries
library(tidyverse)    # CRAN v1.3.1    
library(janitor)      # CRAN v2.1.0      
library(estimatr)     # CRAN v0.30.6     
library(binom)        # CRAN v1.1-1
library(openxlsx)     # CRAN v4.2.5     
library(modelsummary) # CRAN v0.10.0 
library(kableExtra)   # CRAN v1.3.4   
library(ggsankey)     # [github::davidsjoberg/ggsankey] v0.0.99999       
library(patchwork)    # CRAN v1.1.1    
library(extrafont)    # CRAN v0.18    
loadfonts()

# read tables
df            <- read.xlsx("data/hiring.xlsx")
df_candidates <- read.xlsx("data/candidates.xlsx")

# load custom function for bar graphs
source("R/plot_function.R")

#########################
#### data processing ####
#########################

# recode gender and treatment variable
df_candidates$gender <- ifelse(df_candidates$gen == 1, "F", "M")
df$gender            <- ifelse(df$gender == 1, "F", "M")
df <- df %>% rename(treatment = treatapp1_choice) %>% mutate(treatment = as.character(treatment))

# create columns for willingness to pay
df <- df %>%
  select(participant, contains("offer")) %>% 
  pivot_longer(cols = contains("offer"), names_to = "offer", values_to = "response") %>%
  mutate(offer = paste0(str_sub(offer, 1,4), "_", str_sub(offer, 10,11))) %>% 
  separate(offer, into = c("cert", "offer"), sep = "_") %>% 
  mutate(offer = as.numeric(offer)) %>% 
  group_by(participant, cert) %>% 
  summarise(price = ifelse(max(response)==0, 0, max(offer[response==1])*10),
            switching = ifelse(max(response)==0, FALSE, 
                               sum(offer[response==max(response)]) != sum(seq(1,max(offer[response==max(response)]),1))),
            .groups = "drop") %>% 
  pivot_wider(names_from = cert, values_from = c(price, switching)) %>% 
  mutate(price_know = ifelse(switching_know == TRUE, NA, price_know),
         price_word = ifelse(switching_word == TRUE, NA, price_word)) %>% 
  select(participant, contains("price")) %>% 
  left_join(df, ., by = "participant")

# run conditional mean imputation of willingness to pay
imp_model_know <- lm(price_know ~ scaleknow, data = df, na.action = "na.exclude")
imp_model_word <- lm(price_word ~ scaleword, data = df, na.action = "na.exclude")
df <- df %>%
  mutate(price_know_with_imput = ifelse(is.na(price_know), round(predict(imp_model_know, newdata = df), -1), price_know),
         price_word_with_imput = ifelse(is.na(price_word), round(predict(imp_model_word, newdata = df), -1), price_word))

# create variable for discrimination type classification
df <- df %>% 
  mutate(type = case_when(
    indif2 & indif3 ~ "non",
    dec2 & dec3  ~ "against_women",
    !dec2 & !dec3 ~ "against_men",
    TRUE ~ "mixed"
  ))

# create variable for consistent certificate preferences in decisions 1, 4, 5
df <- df %>% 
  mutate(consistent = case_when(
    treatment==1 ~ (dec1 == 1 & dec4 == 1 & dec5 == 1 & indif1 == 0 & indif4 == 0 & indif5 == 0) | # K > W
                   (dec1 == 0 & dec4 == 0 & dec5 == 0 & indif1 == 0 & indif4 == 0 & indif5 == 0) | # W > K
                   (indif1 == 1 & indif4 == 1 & indif5 == 1),                                      # ~
    treatment==2 ~ (dec1 == 1 & dec4 == 0 & dec5 == 0 & indif1 == 0 & indif4 == 0 & indif5 == 0) | # W > K
                   (dec1 == 0 & dec4 == 1 & dec5 == 1 & indif1 == 0 & indif4 == 0 & indif5 == 0) | # K > W
                   (indif1 == 1 & indif4 == 1 & indif5 == 1)                                       # ~
  ))

# create variable for consistent certificate preferences in decisions 1, 4, 5, 6-9
# (i.e., choosing consistently between qualifications K, W and -)
df <- df %>% 
  mutate(consistent_all = case_when(
    
    treatment==1 ~ (dec1 & dec4 & dec5 & !dec6 & dec8 & !dec7 & dec9 &                     # K > W > -
                    !indif1 & !indif4 & !indif5 & !indif6 & !indif8 & !indif7 & !indif9) |
      
                   (!dec1 & !dec4 & !dec5 & !dec7 & dec9 & !dec6 & dec8 &                  # W > K > -
                    !indif1 & !indif4 & !indif5 & !indif6 & !indif8 & !indif7 & !indif9) |
     
                   (indif1 & indif4 & indif5 & !dec6 & dec8 & !dec7 & dec9 &               # W ~ K > - 
                    !indif6 & !indif8 & !indif7 & !indif9)                               | 
      
                   (dec1 & dec4 & dec5 & !dec6 & dec8 &                                    # K > W ~ -
                    !indif1 & !indif4 & !indif5 & !indif6 & !indif8 & indif7 & indif9)   |
                   
                   (!dec1 & !dec4 & !dec5 & !dec7 & dec9 &                                 # W > K ~ -
                    !indif1 & !indif4 & !indif5 & indif6 & indif8 & !indif7 & !indif9)   |
      
                   (!dec1 & !dec4 & !dec5 & !dec7 & dec9 & dec6 & dec8 &                   # W > - > K
                    !indif1 & !indif4 & !indif5 & !indif6 & !indif8 & !indif7 & !indif9) |
      
                   (dec1 & dec4 & dec5 & !dec6 & dec8 & dec7 & !dec9 &                     # K > - > W
                   !indif1 & !indif4 & !indif5 & !indif6 & !indif8 & !indif7 & !indif9)  |
      
                   (dec1 & dec4 & dec5 & dec6 & !dec8 & dec7 & !dec9 &                     # - > K > W
                    !indif1 & !indif4 & !indif5 & !indif6 & !indif8 & !indif7 & !indif9) |
                   
                   (!dec1 & !dec4 & !dec5 & dec7 & !dec9 & dec6 & !dec8 &                  # - > W > K
                    !indif1 & !indif4 & !indif5 & !indif6 & !indif8 & !indif7 & !indif9) |
      
                   (indif1 & indif4 & indif5 & dec6 & !dec8 & dec7 & !dec9 &               # - > W ~ K
                    !indif6 & !indif8 & !indif7 & !indif9)                               | 

                   (indif1 & indif4 & indif5 & indif6 & indif7 & indif8 & indif9)          # ~
      ,         
    
    treatment==2 ~ (dec1 & !dec4 & !dec5 & !dec7 & dec9 & !dec6 & dec8 &                   # W > K > -
                    !indif1 & !indif4 & !indif5 & !indif6 & !indif8 & !indif7 & !indif9) |
      
                   (!dec1 & dec4 & dec5 & !dec7 & dec9 & !dec6 & dec8 &                    # K > W > -
                    !indif1 & !indif4 & !indif5 & !indif6 & !indif8 & !indif7 & !indif9) |  
      
                   (indif1 & indif4 & indif5 & !dec6 & dec8 & !dec7 & dec9 &               # W ~ K > - 
                    !indif6 & !indif8 & !indif7 & !indif9)                               | 
      
                   (dec1 & !dec4 & !dec5 & !dec7 & dec9 &                                  # W > K ~ -
                    !indif1 & !indif4 & !indif5 & indif6 & indif8 & !indif7 & !indif9)   |
                  
                   (!dec1 & dec4 & dec5 & !dec6 & dec8 &                                   # K > W ~ -
                    !indif1 & !indif4 & !indif5 & !indif6 & !indif8 & indif7 & indif9)   |  
      
                   (dec1 & !dec4 & !dec5 & !dec7 & dec9 & dec6 & !dec8 &                   # W > - > K
                    !indif1 & !indif4 & !indif5 & !indif6 & !indif8 & !indif7 & !indif9) |
                   
                   (!dec1 & dec4 & dec5 & dec7 & !dec9 & !dec6 & dec8 &                    # K > - > W
                    !indif1 & !indif4 & !indif5 & !indif6 & !indif8 & !indif7 & !indif9) |  
      
                   (dec1 & !dec4 & !dec5 & dec7 & !dec9 & dec6 & !dec8 &                   # - > W > K
                    !indif1 & !indif4 & !indif5 & !indif6 & !indif8 & !indif7 & !indif9) |
                   
                   (!dec1 & dec4 & dec5 & dec7 & !dec9 & dec6 & !dec8 &                    # - > K > W
                    !indif1 & !indif4 & !indif5 & !indif6 & !indif8 & !indif7 & !indif9) |  
      
                   (indif1 & indif4 & indif5 & dec6 & !dec8 & dec7 & !dec9 &               # - > W ~ K
                    !indif6 & !indif8 & !indif7 & !indif9)                               | 

                   (indif1 & indif4 & indif5 & indif6 & indif7 & indif8 & indif9)          # ~
      
  ))

# create a long table with one decision per row and additional variables
df_long <- df %>% 
  rename_with(.fn = function(x){paste0(x, "_dec")},
              .cols = contains("dec")) %>% 
  rename_with(.fn = function(x){paste0("dec", str_sub(x,6,6), "_indif")},
              .cols = contains("indif")) %>% 
  pivot_longer(cols = contains("dec"), 
               names_to = c("decision_nr", ".value"),
               names_sep = "_") %>% 
  mutate(decision_nr = as.numeric(str_remove(decision_nr, "dec"))) %>% 
  mutate(chooses_more_qualified = ifelse(decision_nr %in% c(6,7) & dec == 0, 1,
                                         ifelse(decision_nr %in% c(8,9) & dec == 1, 1, 0)),
         male_is_more_qualified = ifelse(decision_nr %in% c(8,9), 1, 0))

# create a long table with one willingness to pay per row
df_wtp <- df %>% 
  pivot_longer(cols = contains("price"), names_to = "cert", values_to = "price") %>% 
  mutate(imp = ifelse(grepl("with_imput", cert), TRUE, FALSE),
         cert = str_remove(cert, "_with_imput"),
         cert = str_remove(cert, "price_"))

#############################
#### results section 3.1 ####
#############################

# gender bias in simple decisions 6 and 8
df_long %>% filter(decision_nr %in% c(6,8)) %>% tabyl(dec) # pr(male) = 50.8%
df_long %>% filter(decision_nr %in% c(6,8)) %>% 
  {mantelhaen.test(x = .$chooses_more_qualified,
                   y = .$male_is_more_qualified,
                   z = .$participant, 
                   correct = FALSE)} # p-value = 0.56 (cmh)

# gender bias in simple decisions 7 and 9
df_long %>% filter(decision_nr %in% c(7,9)) %>% tabyl(dec) # pr(male) = 50.6%
df_long %>% filter(decision_nr %in% c(7,9)) %>% 
  {mantelhaen.test(x = .$chooses_more_qualified,
                   y = .$male_is_more_qualified,
                   z = .$participant, 
                   correct = FALSE)} # p-value = 0.62 (cmh)

# gender bias in complex decision by treatment
df %>% 
  tabyl(treatment, dec1) %>% 
  adorn_percentages("row") %>%
  adorn_ns() # pr(male)[t1] = 48.7%
             # pr(male)[t2] = 63.6%

chisq.test(table(df$dec1[df$treatment==1]), correct = FALSE) # p-value = 0.78
chisq.test(table(df$dec1[df$treatment==2]), correct = FALSE) # p-value = 0.0027

# gender bias in complex decision across treatments
w_mean_dec1 <- df %>% 
  mutate(dec1_weight = ifelse(treatment == 1, 
                              1 / sum(treatment==1), 
                              1 / sum(treatment==2))) %>% 
  summarise(weighted.mean(dec1, w = dec1_weight)) %>% pull
w_mean_dec1 # pr(male) = 56.2%

pchisq(((w_mean_dec1-0.5) / sqrt((0.5^2/sum(df$treatment==1) + 0.5^2/sum(df$treatment==2))/4))^2, 
       df = 1, lower.tail = FALSE) # p-value = 0.055

(w_mean_dec1-0.5)*2 # gender bias = 12.4%

# figure: choice proportions simple decisions
png("figures/barplot_simple.png", width = 1800, height = 1500, res = 375)
bargraph_choices(df_long %>% filter(decision_nr %in% 6:9), 
                 "chooses_more_qualified", "Propensity to hire more qualified", FALSE)
dev.off()

# figure: choice proportions complex and gender decisions
png("figures/barplot_complex_gender.png", width = 2000, height = 1500, res = 375)
bargraph_choices(df_long %>% filter(decision_nr == 1), "dec", "Propensity to hire male", TRUE) + 
  bargraph_choices(df_long %>% filter(decision_nr %in% 2:3), "dec", "Propensity to hire male", FALSE) + 
  plot_annotation(tag_levels = "A")
dev.off()

#############################
#### results section 3.2 ####
#############################

# gender bias in gender decisions
df %>% tabyl(dec2) # pr(male) = 56.7%
chisq.test(table(df$dec2), correct = FALSE) # p-value = 0.039

df %>% tabyl(dec3) # pr(male) = 58.3%
chisq.test(table(df$dec3), correct = FALSE) # p-value = 0.0098

(mean(c(df$dec2, df$dec3))-0.5)*2 # gender bias = 15%

#############################
#### results section 3.4 ####
#############################

# willingness to pay treatment effects
m1 <- lm_robust(price ~ treatment*cert, data = df_wtp %>% filter(imp == FALSE), 
                clusters = participant, se_type = "stata")
tidy(m1) # interaction coefficient = 7.3, p-value = 0.14

m2 <- lm_robust(price ~ treatment*cert, data = df_wtp %>% filter(imp == TRUE), 
                clusters = participant, se_type = "stata")
tidy(m2) # interaction coefficient = 8.1, p-value = 0.078

# figure: willingness to pay by treatment and certificate
png("figures/wtp_treatments.png", width = 1600, height = 1400, res = 375)
df_wtp %>% 
  mutate(cert = ifelse(grepl("know", cert), "Knowledge", "Word")) %>% 
  group_by(cert, treatment) %>% 
  summarise(n = sum(!is.na(price)),
            mean = mean(price, na.rm = TRUE),
            sd = sd(price, na.rm = TRUE), .groups = "drop") %>% 
  mutate(sd_lower = mean-sd,
         sd_upper = mean+sd) %>% 
  ggplot() +
  geom_errorbar(aes(x = treatment, ymin = sd_lower, ymax = sd_upper, group = cert), 
                width = 0, position = position_dodge(0.2)) +
  geom_point(aes(x = treatment, shape = cert, y = mean), 
             size = 3, position = position_dodge(0.2)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
  theme_bw(14) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(family = "Segoe UI Semilight"),
        legend.position = "bottom") +
  labs(x = "Treatment",
       y = "WTP (€ cents)",
       shape = "Certificate")
dev.off()

#############################
#### results section 4.1 ####
#############################

# table: discrimination types and complex decision by type (initial and ultimate)
tab_types1 <- df %>% 
  group_by(treatment, type) %>% 
  summarise(frequency        = n(),
            pr_male_dec1     = mean(dec1),
            pr_male_dec1_ult = mean(dec1 & indif1==0) + mean(indif1==1)/2,
            .groups = "drop_last") %>% 
  mutate(frequency = frequency/sum(frequency)) %>% 
  ungroup %>% 
  pivot_wider(names_from = treatment, values_from = c(frequency, pr_male_dec1, pr_male_dec1_ult),
              names_prefix = "t")

tab_types2 <- df %>% 
  mutate(type = ifelse(type!="non", "explicit", "non")) %>% 
  group_by(treatment, type) %>% 
  summarise(frequency        = n(),
            pr_male_dec1     = mean(dec1),
            pr_male_dec1_ult = mean(dec1 & indif1==0) + mean(indif1==1)/2,
            .groups = "drop_last") %>% 
  mutate(frequency = frequency/sum(frequency)) %>% 
  ungroup %>% 
  pivot_wider(names_from = treatment, values_from = c(frequency, pr_male_dec1, pr_male_dec1_ult),
              names_prefix = "t") %>% 
  filter(type == "explicit")

tab_types <- bind_rows(tab_types2, tab_types1)

write(tab_types %>% 
        mutate(across(where(is.numeric), ~.x*100)) %>% 
        kable(format = "latex", digits = 1), file = "tables/tab_types.tex")

# bounds on gender bias in complex decision by type
tab_types %>% 
  rowwise %>% 
  mutate(mean_dec1             = mean(c(pr_male_dec1_t1, pr_male_dec1_t2)), 
         mean_dec1_ult         = mean(c(pr_male_dec1_ult_t1, pr_male_dec1_ult_t2)), 
         gender_bias_lower     = (mean_dec1 - 0.5) * 2,
         gender_bias_lower_ult = (mean_dec1_ult - 0.5) * 2,
         gender_bias_upper     = min(pr_male_dec1_t1, pr_male_dec1_t2),
         gender_bias_upper_ult = min(pr_male_dec1_ult_t1, pr_male_dec1_ult_t2)) %>% 
  select(type, contains("gender")) %>% 
  ungroup # explicit discriminators, lower bounds = 29.2%, 29.0%
          # explicit discriminators, upper bounds = 62.5% (ult: 59.4%), 61.3% (ult: 62.9%)
          # explicit non-discriminators, lower bound initial = -7.4%
          # explicit non-discriminators, lower bound ultimate = 2.9%
  
# figure: sankey plot for flow within-subject choices
png("figures/choices_flow.png", width = 2200, height = 1400, res = 290)
df %>%
    mutate(
      "D[2]~D[3]" := case_when(
        type == "non" ~ "Explicit:\nnon",
        type == "mixed" ~ "Explicit:\nmixed",
        type == "against_men" ~ "Female",
        TRUE ~ "Male"),
      "D[1]~(initial)"  := ifelse(dec1 == 1, "Male", "Female"),
      "D[1]~(ultimate)" = ifelse(
        indif1 == 0 & dec1 == 1,
        "Male",
        ifelse(indif1 == 0 & dec1 == 0, "Female", "Sell"))) %>%
    make_long(`D[2]~D[3]`, `D[1]~(initial)`, `D[1]~(ultimate)`) %>%
    mutate(label = ifelse(
      node == "Male" & x == "D[2]~D[3]", "Explicit:\nagainst women",
      ifelse(node == "Female" &x == "D[2]~D[3]", "Explicit:\nagainst men", node))) %>% 
    mutate(node = factor(node, 
                         ordered = TRUE, 
                         levels = rev(c("Sell", "Male", "Female", "Explicit:\nmixed", "Explicit:\nnon")))) %>% 
  ggplot(aes(x = x, 
             next_x = next_x, 
             node = node, 
             next_node = next_node,
             fill = factor(node),
             label = label)) +
    geom_alluvial(flow.alpha = 0.6) + 
    theme_alluvial(base_size = 14) + 
    geom_alluvial_label(size = 4, color = "black", fill = "white",
                        family = "Segoe UI Semilight") +
    theme(legend.position = "none",
          text = element_text(family = "Segoe UI Semilight")) +
    labs(x = "", y= "N") +
    scale_fill_manual(values = c("grey", "grey", "#fff2ae", "#cbd5e8", "grey" )) +
    annotate("text", x = 1, y = 280, label = "Gender decisions", family = "Segoe UI Semilight", size = 5) +
    annotate("text", x = 2, y = 280, label = "Complex decision", family = "Segoe UI Semilight", size = 5) + 
    geom_segment(aes(x = 1, xend = 1, y = 240, yend = 270), col = "grey20") +
    geom_segment(aes(x = 2, xend = 2, y = 240, yend = 270), col = "grey20") +
    geom_segment(aes(x = 3, xend = 2, y = 240, yend = 270), col = "grey20") +
    scale_x_discrete(labels = function(x) parse(text=x))
dev.off()

#############################
#### results section 4.2 ####
#############################

# consistency among non-discriminators
df %>% 
    filter(type == "non") %>% 
    group_by(treatment) %>% 
    summarise(mean(consistent)) # pr(consistent|non-discrimination)[t1] = 55.3%, 
                                # pr(consistent|non-discrimination)[t2] = 54.5%
df %>% 
  tabyl(consistent, type) %>% 
  adorn_percentages("all") # pr(consistent & non-discrimination) = 16%

# simple decisions among explicit discriminators
df_long %>% 
  filter(grepl("against", type),
         decision_nr %in% 6:9) %>% 
  group_by(type) %>% 
  summarise(mean(dec)) # pr(male|against men)   = 46.8%
                       # pr(male|against women) = 57.3%
df_long %>% 
  filter(grepl("against", type),
         decision_nr %in% 6:9) %>% 
  {mantelhaen.test(x = .$chooses_more_qualified,
                   y = .$male_is_more_qualified,
                   z = .$participant, 
                   correct = FALSE)} # p-value = 0.029 (cmh)

# complex decision among explicit discriminators
df_long %>% 
  filter(grepl("against", type),
         decision_nr %in% 1) %>% 
  group_by(type) %>% 
  summarise(mean(dec)) # pr(male|against men)   = 64.5%
                       # pr(male|against women) = 64.5%

# table: regressions willingness to pay treatment effects
models_type <- map(as.list(unique(df$type)), function(x){
  lm_robust(price ~ treatment*cert, data = df_wtp %>% filter(type == x, imp == TRUE), 
            clusters = participant, se_type = "stata")
})

names(models_type) <- unique(df$type)

tab_models <- modelsummary(
  list(
    "Multiple switchers excluded" = m1,
    "Multiple switchers imputed"  = m2,
    "Explicit: against men"       = models_type$against_men,
    "Explicit: against women"     = models_type$against_women,
    "Explicit: mixed"             = models_type$mixed,
    "Explicit: none"              = models_type$non),
  fmt = 1,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = c("treatment2" = "T2",
                  "certword" = "Word",
                  "treatment2:certword" = "T2:Word"),
  gof_omit = "dj", output = "latex") %>% 
  add_header_above(c(" " = 1,
                     "Full sample" = 2,
                     "By discrimination types (Multiple switchers imputed)" = 4))

write(tab_models, file = "tables/tab_models.tex")

###########################
#### results section 5 ####
###########################

# consistency across all 9 decisions
df %>% 
  tabyl(consistent_all, type) %>% 
  adorn_percentages("all") # pr(consistent all & non) = 13%

df %>% 
  tabyl(consistent_all, type, treatment) %>% 
  adorn_percentages("all") # pr(consistent all & non)[t1] = 14%
                           # pr(consistent all & non)[t2] = 11%

#############################
#### results section 5.1 ####
#############################

# test for average gender difference in performance
t.test(df_candidates$matrices ~ df_candidates$gender, var.equal = TRUE) # p-value = 0.82

# figure: density plot of performance by gender
png("figures/candidates_performace_gender.png", width = 1600, height = 1400, res = 375)
ggplot(df_candidates) +
  geom_density(aes(x = matrices, linetype = gender), key_glyph = draw_key_path) +
  theme_bw(14) +
  theme(panel.grid = element_blank(),
        text = element_text(family = "Segoe UI Semilight"),
        legend.position = "bottom") +
  guides(linetype = guide_legend(keywidth = 2)) +
  labs(y = "Density",
       x = "Job task performance",
       linetype = "Gender")
dev.off()

# performance stereotypes among candidates
mean(df_candidates$estimate_matrices) # mean (own belief) = 55.4
sd(df_candidates$estimate_matrices) # sd (own belief) = 16.2
t.test(df_candidates$estimate_matrices, mu = 50) # p-value = 0.0037

mean(df_candidates$belief_matrices)# mean (others' belief) = 56.3
sd(df_candidates$belief_matrices) # sd (others' belief) = 14.1
t.test(df_candidates$belief_matrices, mu = 50) # p-value < 0.001

#############################
#### results section 5.2 ####
#############################

# table: conditional pairwise comparisons between candidates
pairedcomp <- function(x,y){
  comb.grid <- expand.grid(x,y)
  a  <- sum(comb.grid[,1] > comb.grid[,2]) / nrow(comb.grid)
  b  <- sum(comb.grid[,1] < comb.grid[,2]) / nrow(comb.grid)
  tie <- sum(comb.grid[,1] == comb.grid[,2]) / nrow(comb.grid)
  
  return(data.frame(
    proportion = c(a, b, tie),
    outcome = c("a", "b", "tie")
  ))}

p70_w <- quantile(df_candidates$wordpuzzles, 0.7)
p70_k <- quantile(df_candidates$generalknowledge, 0.7)

n_certified <- nrow(df_candidates) * 0.3
n_tied_w    <- sum(df_candidates$wordpuzzles == p70_w)
n_nontied_w <- sum(df_candidates$wordpuzzles > p70_w)
n_tied_k    <- sum(df_candidates$generalknowledge == p70_k)
n_nontied_k <- sum(df_candidates$generalknowledge > p70_k)

comb_w <- combn(1:n_tied_w, n_certified-n_nontied_w, simplify = FALSE)
comb_k <- combn(1:n_tied_k, n_certified-n_nontied_k, simplify = FALSE)

possible_datasets_w <- map(comb_w, function(x){
  nontied_df <- df_candidates %>% 
    filter(wordpuzzles > p70_w)
  tied_df <- df_candidates %>% 
    filter(wordpuzzles == p70_w) %>% 
    {.[x,]}
  rbind(nontied_df, tied_df) %>% as_tibble
})

possible_datasets_k <- map(comb_k, function(x){
  nontied_df <- df_candidates %>% 
    filter(generalknowledge > p70_k)
  tied_df <- df_candidates %>% 
    filter(generalknowledge == p70_k) %>% 
    {.[x,]}
  rbind(nontied_df, tied_df) %>% as_tibble
})

comb_w_k <- expand_grid(nr_dataset_w = 1:length(comb_w), nr_dataset_k = 1:length(comb_k))

results_all_datasets <- map_dfr(1:nrow(comb_w_k), function(x){
  
  dataset_nrs <- comb_w_k[x,]
  dataset_w <- possible_datasets_w[[as.numeric(dataset_nrs[1])]]
  dataset_k <- possible_datasets_k[[as.numeric(dataset_nrs[2])]]
  
  performances <- list(
    `fW` = dataset_w$matrices[dataset_w$gender=="F"],
    `fK` = dataset_k$matrices[dataset_k$gender=="F"],
    `mW` = dataset_w$matrices[dataset_w$gender=="M"],
    `mK` = dataset_k$matrices[dataset_k$gender=="M"],
    `f_` = df_candidates$matrices[df_candidates$gender=="F"],
    `m_` = df_candidates$matrices[df_candidates$gender=="M"],
    `K_` = dataset_k$matrices,
    `W_` = dataset_w$matrices,
    `no` = df_candidates$matrices
  )
  
  truth <- data.frame(
    decision   = c("D1 (T1)", "D1 (T2)", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", rep("n.a.", 4)),
    candidate_a = c("fW", "fK", "fK", "fW", "fW", "mW", "fK", "fW", "f_", "f_","K_","K_", "W_", "f_"),
    candidate_b = c("mK", "mW", "mK", "mW", "fK", "mK", "m_", "m_", "mK", "mW", "W_", "no", "no", "m_")
  )
  
  truth$tie <-  truth$pr_b_better <- truth$pr_a_better <- numeric(nrow(truth))
  for (i in 1:nrow(truth)){
    
    prop_comp <- pairedcomp(with(performances,eval(parse(text=truth$candidate_a[i]))),
                            with(performances,eval(parse(text=truth$candidate_b[i]))))
    
    truth$pr_a_better[i] <- prop_comp[1,1]
    truth$pr_b_better[i] <- prop_comp[2,1]
    truth$tie[i] <- prop_comp[3,1]
  }
  
  truth$pr_a_better <- truth$pr_a_better + truth$tie/2
  truth$pr_b_better <- truth$pr_b_better + truth$tie/2
  return(truth)
  
})

tab_actual <- results_all_datasets %>% 
  group_by(decision, candidate_a, candidate_b) %>% 
  summarise(across(where(is.numeric), mean), .groups = "drop") 

write(tab_actual %>% 
      mutate(across(where(is.numeric), ~round(.x, 3))) %>% 
      kable(format = "latex"), file = "tables/tab_actual.tex")

# number of expected earnings maximizers
should_choose <- tab_actual %>% 
  filter(grepl("D", decision)) %>% 
  mutate(should_choose =
           case_when(
             (pr_b_better > pr_a_better) & (pr_b_better > 31/60) ~ 1, # choose male (or knowledge in decisions 4, 5)
             (pr_a_better > pr_b_better) & (pr_a_better > 31/60) ~ 0, # choose female (or word in decisions 4, 5)
             TRUE ~ NA_real_                                          # choose sell
           )) %>% 
  pull(should_choose)

df_long %>% 
  rowwise %>% 
  mutate(maximizes = case_when(
    decision_nr == 1 & treatment == 1 ~ ifelse(!is.na(should_choose[1]), dec == should_choose[1], indif==1),
    decision_nr == 1 & treatment == 2 ~ ifelse(!is.na(should_choose[2]), dec == should_choose[2], indif==1),
    
    decision_nr == 2 ~ ifelse(!is.na(should_choose[3]),  dec == should_choose[3]  & indif==0, indif==1),
    decision_nr == 3 ~ ifelse(!is.na(should_choose[4]),  dec == should_choose[4]  & indif==0, indif==1),
    decision_nr == 4 ~ ifelse(!is.na(should_choose[5]),  dec == should_choose[5]  & indif==0, indif==1),
    decision_nr == 5 ~ ifelse(!is.na(should_choose[6]),  dec == should_choose[6]  & indif==0, indif==1),
    decision_nr == 6 ~ ifelse(!is.na(should_choose[7]),  dec == should_choose[7]  & indif==0, indif==1),
    decision_nr == 7 ~ ifelse(!is.na(should_choose[8]),  dec == should_choose[8]  & indif==0, indif==1),
    decision_nr == 8 ~ ifelse(!is.na(should_choose[9]),  dec == should_choose[9]  & indif==0, indif==1),
    decision_nr == 9 ~ ifelse(!is.na(should_choose[10]), dec == should_choose[10] & indif==0, indif==1)
  )) %>% 
  group_by(participant) %>% 
  summarise(maximizing_choices = sum(maximizes), .groups = "drop") %>% 
  arrange(desc(maximizing_choices)) # no participant has more than 7 out of 9 maximizing choices

# certificate preferences among non-discriminators
df_long %>% 
  filter(type == "non" & consistent,
         decision_nr %in% c(4,5)) %>% 
  summarise(mean(dec==0)) # pr(word|non-discrimination) = 64%

df_long %>% 
  filter(type == "non" & consistent,
         decision_nr %in% c(4,5)) %>% 
  group_by(treatment) %>% 
  summarise(mean(dec==0)) # initial pr(word|non-discrimination)[t1] = 64.3%
                          # initial pr(word|non-discrimination)[t2] = 63.9%

df_long %>% 
  filter(type == "non" & consistent,
         decision_nr %in% c(4,5)) %>% 
  group_by(treatment) %>% 
  summarise(mean(dec==0 & indif==0) + mean(indif==1)/2) # ultimate pr(word|non-discrimination)[t1] = 61.9%
                                                        # ultimate pr(word|non-discrimination)[t2] = 63.9%

##########################
#### results appendix ####
##########################

# payoffs in the word task
mean(df_candidates$wordpuzzles * 0.4) # mean payoff = 5.03 euro
sd(df_candidates$wordpuzzles * 0.4) # sd payoff = 1.80 euro

# payoffs in the knowledge task
mean(df_candidates$generalknowledge * 0.6) # mean payoff = 5.08 euro
sd(df_candidates$generalknowledge * 0.6) # sd payoff = 2.14 euro

# payoffs in the logic task
mean(df_candidates$matrices * 1.3) # mean payoff = 5.22 euro
sd(df_candidates$matrices * 1.3) # sd payoff = 1.87 euro

# table: demographic information
tab_demogr <- df %>% 
  group_by(treatment) %>% 
  summarise(age = paste0(format(round(mean(age),1),nsmall=1), 
                         " (", format(round(sd(age),1),nsmall=1), ")"),
            gender_female = paste0(sum(gender=="F"), 
                                   " (", format(round(mean(gender=="F")*100,1),nsmall=1), ")"),
            study_stem = paste0(sum(study_coded=="stem",na.rm=TRUE), 
                                " (", format(round(sum(study_coded=="stem",na.rm=TRUE)/n()*100,1),nsmall=1), ")"),
            study_econ = paste0(sum(study_coded=="econ",na.rm=TRUE), 
                                " (", format(round(sum(study_coded=="econ",na.rm=TRUE)/n()*100,1),nsmall=1), ")")) %>% 
  kable(format = "latex")

write(tab_demogr, file = "tables/tab_demogr.tex")

# figure: correlation of belief elicitation measures
png("figures/correlation_wtp_rating.png", width = 1600, height = 1400, res = 375)
df_wtp %>% 
  drop_na(price) %>% 
  mutate(likert = ifelse(cert == "word", scaleword, scaleknow)) %>% 
  group_by(price, likert) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  ggplot() +
  geom_point(aes(y = price, x = likert, size = n), shape = 1, show.legend = FALSE) +
  geom_smooth(aes(y = price, x = likert, weight = n), 
              method = "lm", formula = y ~ x, se = FALSE, col = "black") +
  theme_bw(14) +
  theme(panel.grid = element_blank(),
        text = element_text(family = "Segoe UI Semilight")) +
  labs(y = "WTP (€ cents)",
       x = "Informativeness rating")
dev.off()

# table: aggregate results from all decisions
tab_aggregate <- df_long %>% 
  mutate(
    decision_nr = case_when(
      decision_nr == 1 & treatment == 1 ~ "1 (T1)",
      decision_nr == 1 & treatment == 2 ~ "1 (T2)",
      TRUE ~ as.character(decision_nr)),
    initial_dec_A = case_when(
      dec == 0 ~ 1, TRUE ~ 0),
    ultimate_dec_A= case_when(
      dec == 0 & indif == 0 ~ 1, TRUE ~ 0)) %>% 
  group_by(treatment, decision_nr) %>% 
  summarise(across(.cols = c(initial_dec_A, indif, ultimate_dec_A),
                   ~ paste0(format(round(mean(.x, na.rm = TRUE),3), nsmall=3), 
                            " (", sum(.x, na.rm = TRUE),
                            "/", sum(!is.na(.x)), ")")),
            .groups = "drop") %>% 
  pivot_wider(names_from = treatment,
              values_from = c(initial_dec_A, indif, ultimate_dec_A)) %>% 
  arrange(decision_nr) %>% 
  mutate(A = c("fW", "fK", "fK", "fW", "fW", "mW", "fK", "fW", "f-", "f-"),
         B = c("mK", "mW", "mK", "mW", "fK", "mW", "m-", "m-", "mK", "mW")) %>% 
  relocate(decision_nr, A, B, contains("_1"), contains("_2")) %>% 
  mutate(across(.cols = everything(), ~replace_na(.x, "-"))) %>% 
  kable(
      booktabs = TRUE,
      linesep = "",
      caption = "Aggregate results from all hiring decisions by treatment.",
      col.names = c("Decision", "A", "B", rep(c("Pr. hire A init.", "Pr. sell", "Pr. keep A"),2)),
      format= "latex") %>% 
  add_header_above(c(" " = 3, "Treatment 1" = 3, "Treatment 2" = 3)) %>% 
  kable_styling(full_width = TRUE)

write(tab_aggregate, file = "tables/tab_aggregate.tex")

# figure: gender stereotypes job task
png("figures/distribution_stereotype.png", width = 1600, height = 1400, res = 375)
set.seed(12345)
df_candidates %>% 
  pivot_longer(cols = c(estimate_matrices, belief_matrices),
               names_to = "var", values_to = "val") %>% 
  mutate(var = ifelse(grepl("est", var), "Own belief", "Others' belief")) %>% 
  mutate(var = factor(var, ordered = TRUE, levels = c("Own belief",
                                                      "Others' belief"))) %>% 
  ggplot(aes(x = var, y = val)) +
  geom_boxplot(width = 0.4, outlier.shape = NA, fill = "grey90") +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_jitter(height = 0, width = 0.05, alpha = 0.2) +
  geom_hline(yintercept = 50, linetype = 2) +
  theme_bw(14) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(family = "Segoe UI Semilight")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
  labs(x = "",
       y = "Random pairings in which\nmale outperforms female")
dev.off()

# table: performance distribution by gender
tab_distr <- df_candidates %>% 
  group_by(gender) %>% 
  summarise(.n = n(),
            across(.cols = matrices,
                   .fns = list(mean = mean, 
                               sd   = sd,
                               min  = min,
                               p25  = ~quantile(.x, 0.25),
                               p50  = median, 
                               p75  = ~quantile(.x, 0.75),
                               max  = max),
                   .names = ".{.fn}")) %>% 
  pivot_longer(cols = contains("."), names_to = "statistic") %>% 
  pivot_wider(names_from = gender) %>% 
  mutate(across(where(is.numeric), ~round(.x, 2))) %>% 
  kable(output = "latex")

write(tab_distr, file = "tables/tab_distr.tex")

############################
#### save sessionInfo() ####
############################
write(pander::pander_return(sessionInfo()), file = "R/session_info.md")

