<style>
td {
  font-size: 12px
}
</style>

# implicitdiscrimination
This repository contains raw data and all analysis code for the article *Explicit and Implicit Belief-Based Gender Discrimination: A Hiring Experiment*.

## R scripts

R scripts to reproduce all analyses that are presented in the article can be found in the `R` folder. The script `plot_function.R` is a helper script imported into the main script `analysis.R`, which exports all empirical tables (unformatted) and figures into the respective folders. The order of analyses follows the structure of the article. The script contains comments which highlight some results in order to ease orientation. The folder also contains the R session info in the file `session_info.md`.

## Data

In the `data` folder, the file `candidates.xlsx` contains data from the **Job Candidate Assessment**, while the file `hiring.xlsx` contains data from the **Hiring Experiment**. The tables below explain all variables. 
\
\

#### Data dictionary `candidates.xlsx`


| Variable name               | Description                                            
|-----------------------------|-----------------------------------------------------------
| sessionid                   | Session number   
| participant                 | Unique participant identifier   
| participant.time_started    | Starting date and time of experiment for participant
| generalknowledge            | Total score in the knowledge task
| wordpuzzles                 | Total score in the word task                           
| matrices                    | Total score in the logic task                           
| age                         | Participant's age (years)  
| gen                         | Participant's gender (1: female, 2: male)
| stud                        | Participant's study program
| estimate_matrices           | Belief about performance of men in logic task (% won by man vs. woman)
| belief_matrices             | Belief about others' belief about performance of men in logic task (average % won by man vs. woman)
| payofftotal                 | Participant's total realized payoff including show-up fee (Euro)
\

#### Data dictionary `hiring.xlsx`

| Variable name               | Description                                            
|-----------------------------|-----------------------------------------------------------
participant                   | Unique participant identifier   
date                          | Date of experiment
sessionid                     | Session number
treatapp1_choice              | Randomized treatment with respect to decision 1
treatapp1_frame               | Randomized treatment with respect to candidate order on screen
dec1                          | Initial choice in decision 1 (1: male, 0: female)
indif1                        | Choice whether to sell decision 1 (1: yes, 0: no)
scaleknow                     | Informativeness rating of knowledge certificate for job task (1-5)
scaleword                     | Informativeness rating of word certificate for job task (1-5)
knowoffer1                    | Price list offer for the knowledge certificate 0.1 Euro (1: accepted, 0: declined)
knowoffer2                    | Price list offer for the knowledge certificate 0.2 Euro (1: accepted, 0: declined)
knowoffer3                    | Price list offer for the knowledge certificate 0.3 Euro (1: accepted, 0: declined)
knowoffer4                    | Price list offer for the knowledge certificate 0.4 Euro (1: accepted, 0: declined)
knowoffer5                    | Price list offer for the knowledge certificate 0.5 Euro (1: accepted, 0: declined)
knowoffer6                    | Price list offer for the knowledge certificate 0.6 Euro (1: accepted, 0: declined)
knowoffer7                    | Price list offer for the knowledge certificate 0.7 Euro (1: accepted, 0: declined)
knowoffer8                    | Price list offer for the knowledge certificate 0.8 Euro (1: accepted, 0: declined)
knowoffer9                    | Price list offer for the knowledge certificate 0.9 Euro (1: accepted, 0: declined)
knowoffer10                   | Price list offer for the knowledge certificate 1.0 Euro (1: accepted, 0: declined)
wordoffer1                    | Price list offer for the word certificate 0.1 Euro (1: accepted, 0: declined)
wordoffer2                    | Price list offer for the word certificate 0.2 Euro (1: accepted, 0: declined)
wordoffer3                    | Price list offer for the word certificate 0.3 Euro (1: accepted, 0: declined)
wordoffer4                    | Price list offer for the word certificate 0.4 Euro (1: accepted, 0: declined)
wordoffer5                    | Price list offer for the word certificate 0.5 Euro (1: accepted, 0: declined)
wordoffer6                    | Price list offer for the word certificate 0.6 Euro (1: accepted, 0: declined)
wordoffer7                    | Price list offer for the word certificate 0.7 Euro (1: accepted, 0: declined)
wordoffer8                    | Price list offer for the word certificate 0.8 Euro (1: accepted, 0: declined)
wordoffer9                    | Price list offer for the word certificate 0.9 Euro (1: accepted, 0: declined)
wordoffer10                   | Price list offer for the word certificate 1.0 Euro (1: accepted, 0: declined)
dec2                          | Initial choice in decision 2 (1: male, 0: female)
dec3                          | Initial choice in decision 3 (1: male, 0: female)
dec4                          | Initial choice in decision 4 (1: knowledge, 0: word)
dec5                          | Initial choice in decision 5 (1: knowledge, 0: word)
indif2                        | Choice whether to sell decision 2 (1: yes, 0: no)
indif3                        | Choice whether to sell decision 3 (1: yes, 0: no)
indif4                        | Choice whether to sell decision 4 (1: yes, 0: no)
indif5                        | Choice whether to sell decision 5 (1: yes, 0: no)
dec6                          | Initial choice in decision 6 (1: male, 0: female)
dec7                          | Initial choice in decision 7 (1: male, 0: female)
dec8                          | Initial choice in decision 8 (1: male, 0: female)
dec8                          | Initial choice in decision 9 (1: male, 0: female)
indif6                        | Choice whether to sell decision 6 (1: yes, 0: no)
indif7                        | Choice whether to sell decision 7 (1: yes, 0: no)
indif8                        | Choice whether to sell decision 8 (1: yes, 0: no)
indif9                        | Choice whether to sell decision 9 (1: yes, 0: no)
age                           | Participant's age (years)  
gender                        | Participant's gender (1: female, 2: male)
stud                          | Participant's study program
study_coded                   | Participant's study program recoded (econ: Economics/Business, stem: STEM, ss: Social Sciences, NA: no response or no student)
risk                          | Participant's self-reported inclination to take risks (1-10)<sup>1</sup>
selfknow                      | Participant's hypothetical expectation of own performance percentile in knowledge task
selfword                      | Participant's hypothetical expectation of own performance percentile in word task
selfmatrices                  | Participant's hypothetical expectation of own performance percentile in logic task
totalpayoff_inclfee           | Participant's total realized payoff including show-up fee (Euro)
time1                         | Decision time for initial decicion 1 (seconds)
time2                         | Decision time for initial decicion 2 (seconds)
time3                         | Decision time for initial decicion 3 (seconds)
time4                         | Decision time for initial decicion 4 (seconds)
time5                         | Decision time for initial decicion 5 (seconds)
time6                         | Decision time for initial decicion 6 (seconds)
time7                         | Decision time for initial decicion 7 (seconds)
time8                         | Decision time for initial decicion 8 (seconds)
time9                         | Decision time for initial decicion 9 (seconds)
\

<sup>1</sup><sub>Falk, A., Becker, A., Dohmen, T. J., Huffman, D., & Sunde, U. (2016). *The Preference Survey Module: A Validated Instrument for Measuring Risk, Time, and Social Preferences* (SSRN Scholarly Paper No. 2725874). Social Science Research Network. https://doi.org/10.2139/ssrn.2725874