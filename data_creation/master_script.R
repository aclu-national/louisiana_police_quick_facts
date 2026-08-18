# Running data creation scripts for killings, misconduct, and personnel
source(here::here("scripts/killing_df_creation.R"))
source(here::here("scripts/misconduct_df_creation.R"))
source(here::here("scripts/personnel_df_creation.R"))

# Binding dataframes
data <- rbind(police_misconduct_df, 
              police_killing_df, 
              police_personnel_df)

# Saving binded data as `df.csv`
write.csv(data,"app_creation/df.csv")
