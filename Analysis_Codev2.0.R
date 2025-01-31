#Data Paths (change these)
EngPath<-"~/Documents/chatGPTSurvey/Data/Global Trends in AI-Assisted Language Teaching - English_December 10, 2024_14.35.csv"
SpaPath<-"~/Documents/chatGPTSurvey/Data/Spanish_Global Trends in AI-Assisted Language Teaching_December 10, 2024_11.12.csv"
GerPath<-"~/Documents/chatGPTSurvey/Data/German_Global Trends in AI-Assisted Language Teaching_December 10, 2024_11.05.csv"
CanPath<-"~/Documents/chatGPTSurvey/Data/Cantonese_Global Trends in AI-Assisted Language Teaching_December 10, 2024_11.13.csv"
QuestionPath<-"~/Documents/chatGPTSurvey/SurveyQuestionDecodertxt"

#Load Libraries
library(ggdist)
library(ggplot2)
library(ggpubr)
library(factoextra)
library(FSA)
library(rstatix)
library(viridis)
library(MASS)
library(tidyverse)
library(brant)
library(VGAM)
library(vegan)
library(glmnet)
library(gghalves)

#load in the data, and assmeble the columns we need since there are extra columns based on write ins
EngData <-read.csv(EngPath) 
SpaData <-read.csv(SpaPath) 
GerData <-read.csv(GerPath) 
CanData <-read.csv(CanPath) 

Eng_combined <- combine_with_origin(EngData, "EngData")
Spa_combined <- combine_with_origin(SpaData, "SpaData")
Ger_combined <- combine_with_origin(GerData, "GerData")
Can_combined <- combine_with_origin(CanData, "CanData")

# Combine all datasets into a single tibble
combined_data <- bind_rows(Eng_combined, Spa_combined, Ger_combined, Can_combined)

#Dataset conversions 
# Transform Q7 using the response_map
combined_data <- combined_data %>%
  mutate(
    Q7_numeric = Q7response_map[Q7], # Map responses to numeric
    Q7_numeric = as.numeric(Q7_numeric) # Convert to numeric (NA for unmatched responses)
  )
  
#Convert Q6 to numeric years
combined_data$Q6_cleaned <- combined_data$Q6 %>%
  str_replace_all("(?i)[^0-9.]+", "") %>%  # Remove non-numeric characters (case-insensitive)
  str_replace("^$", NA_character_) %>%    # Replace empty strings with NA
  as.numeric()                            # Convert to numeric
# Handle special cases for text
combined_data$Q6_cleaned[combined_data$Q6 == "未滿一年"] <- 0.5  # Less than one year
combined_data$Q6_cleaned[combined_data$Q6 == "Mit Vorbereitungsdienst ca. Dreieinhalb Jahre"] <- 3.5  # Special German case

#Q3 levels into the same language
combined_data <- combined_data %>%
  mutate(
    Q3_standardized = sapply(Q3, function(x) {
      # Split entries with mixed levels (e.g., "Middle School, Secondary school/High School")
      parts <- unlist(strsplit(as.character(x), ","))
      # Standardize each part using the standardize_levels function
      standardized_parts <- unique(sapply(parts, standardize_levels))
      # If multiple standardized levels exist, join them with commas
      if (length(standardized_parts) == 1) {
        standardized_parts
      } else if (length(standardized_parts) > 1) {
        paste(standardized_parts, collapse = ", ")
      } else {
        NA  # If no match, return NA
      }
    }),
    Q3_standardized = as.character(Q3_standardized)  # Ensure the column remains as a character type
  )
  # Collapse multiple categories into "Multiple"
combined_data <- combined_data %>%
  mutate(
    Q3_collapsed = sapply(Q3_standardized, function(x) {
      if (grepl(",", x)) {
        "Multiple"  # Assign "Multiple" for entries with multiple categories
      } else {
        x  # Retain original category for single-category entries
      }
    })
  )
# Check the results
#combined_data$Q3_standardized
 #combined_data$Q3_collapsed
 
 #standardize Q11
 combined_data <- combined_data %>%
  mutate(
    Q11_standardized = sapply(Q11, standardize_Q11)
  )
  
#convertQ13 to numeric scale
# Apply the conversion to all Q.13.20 columns
combined_data <- combined_data %>%
  mutate(across(starts_with("Q.13.20"), ~ sapply(., Q13convert_to_numeric)))  
  #Q22
combined_data$Frequency_Standardized <- sapply(combined_data$Q22, standardize_frequency)

#convertQ23 to numeric scale
# Apply the conversion to all Q.13.20 columns
combined_data <- combined_data %>%
  mutate(across(starts_with("Q.23.38"), ~ sapply(., Q23convert_to_numeric)))  


##########################
#Familiarity and Country
##########################

#lets just visulaize the raw data to start, we can try a bar plot and the heatmap later. 
relative_data <- combined_data %>%
  mutate(
    Q7_numeric = as.character(Q7_numeric),   # Convert to character to include NA as a value
    Q7_numeric = ifelse(is.na(Q7_numeric), "NA", Q7_numeric) # Replace NA with "NA" as a string
  ) %>%
  group_by(dataOrigin, Q7_numeric) %>%
  summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each value
  group_by(dataOrigin) %>%  # Group again by dataOrigin for relative frequency
  mutate(
    relative_frequency = count / sum(count) * 100  # Calculate relative frequency (%)
  )

# Ensure Q7_numeric is an ordered factor for proper plotting
relative_data$Q7_numeric <- factor(relative_data$Q7_numeric, levels = c("1", "2", "3", "NA"))

# Create the grouped bar plot with relative frequencies
ggplot(relative_data, aes(x = dataOrigin, y = relative_frequency, fill = Q7_numeric)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create grouped bars
  labs(
    title = "Relative Frequency of Q7_numeric Values by Data Origin",
    x = "Data Origin",
    y = "Relative Frequency (%)",
    fill = "Q7_numeric"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

#Test for difference in familiarity between countries
# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(Q7_numeric ~ dataOrigin, data = combined_data)

# Display the Kruskal-Wallis test result
kruskal_result

# Perform Dunn's test with Benjamini-Hochberg correction
dunn_result <- dunnTest(Q7_numeric ~ dataOrigin, data = combined_data, method = "bh")

# Display Dunn's test result
dunn_result$res

#visualize
# Remove any rows with NA in Q7_numeric or dataOrigin
heatcombined_data <- combined_data %>%
  filter(!is.na(Q7_numeric), !is.na(dataOrigin))

# Ensure dataOrigin is a factor
heatmap_data <- heatcombined_data %>%
  group_by(dataOrigin, Q7_numeric) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(dataOrigin) %>%
  mutate(relative_frequency = count / sum(count))

 heatmap_plot <- ggplot(heatmap_data, aes(x = as.factor(Q7_numeric), y = dataOrigin, fill = relative_frequency)) +
  geom_tile(color = "white") +  # Add borders to tiles
  scale_fill_gradient(low = "white", high = "blue", name = "Relative Frequency") +  # Heatmap colors
  #scale_fill_viridis(option = "E", name = "Relative Frequency") +
  geom_text(aes(label = scales::percent(relative_frequency, accuracy = 0.1)), color = "black") +  # Add percentages
  labs(
    title = "Relative Frequency of Familiarity Responses by Country",
    x = "Familiarity Level (1 = Never heard, 3 = Used)",
    y = "Country (Data Origin)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the heatmap
heatmap_plot

#the Macau data is odd, lets bootstrap to look at those NA frequencies
set.seed(42) # For reproducibility

# Define variables
n_sim <- 100000  # Number of simulations
n_macau <- 50  # Number of responses in Macau

# Calculate the overall probability of NAs in the dataset
na_proportion <- mean(is.na(combined_data$Q7_numeric))  # Proportion of NAs
non_na_proportion <- 1 - na_proportion  # Proportion of non-NAs

# Perform bootstrap simulation
simulated_na_counts <- replicate(n_sim, {
  # Simulate 50 responses based on overall probabilities
  sampled_responses <- sample(
    c(NA, "Non-NA"), 
    size = n_macau, 
    replace = TRUE, 
    prob = c(na_proportion, non_na_proportion)
  )
  # Count the number of NAs in the simulated sample
  sum(is.na(sampled_responses))
})

# Observed number of NAs in Macau
observed_na_count <- sum(is.na(combined_data$Q7_numeric[combined_data$dataOrigin == "CanData"]))

# Calculate the mean of simulated NA counts
sim_mean <- mean(simulated_na_counts)

# Two-tailed p-value
p_value <- mean(simulated_na_counts >= observed_na_count | 
                simulated_na_counts <= (2 * sim_mean - observed_na_count))

# Plot the results
hist(simulated_na_counts, breaks = 20, col = "lightblue", main = "Distribution of Simulated NA Counts",
     xlab = "Number of NAs in Simulated Dataset (n = 50)")
abline(v = observed_na_count, col = "red", lwd = 2, lty = 2)
text(observed_na_count, max(table(simulated_na_counts)) * 0.8, 
     labels = paste0("Observed NAs = ", observed_na_count), pos = 4, col = "red")

# Output results
cat("Observed NA count in Macau:", observed_na_count, "\n")
cat("P-value for observed NA count:", p_value, "\n")

##########################
#Familiarity and Age
##########################

# Fit ordinal logistic regression for the entire dataset
model_all <- polr(as.factor(Q7_numeric) ~ Q5, data = combined_data, Hess = TRUE)

# Display model summary
summary(model_all)

# Compute p-values for the coefficients
coeff_all <- coef(summary(model_all))
p_values_all <- pnorm(abs(coeff_all[, "t value"]), lower.tail = FALSE) * 2
coeff_all <- cbind(coeff_all, "p value" = p_values_all)

# Display coefficients with p-values
coeff_all

# Apply the function to each group
results_by_group <- combined_data %>%
  group_by(dataOrigin) %>%
  group_split() %>%
  setNames(unique(combined_data$dataOrigin)) %>%
  lapply(fit_model_by_group)

# Display results for each group
for (group in names(results_by_group)) {
  cat("Results for", group, ":\n")
  print(results_by_group[[group]]$coefficients)
  cat("\n")
}

ggplot(combined_data, aes(x = Q5, y = Q7_numeric)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +
  labs(
    title = "Age vs. Familiarity with GenAI",
    x = "Age (Q5)",
    y = "Familiarity Level (Q7_numeric)"
  ) +
  scale_y_continuous(breaks = c(1, 2, 3), labels = c("Never heard", "Heard but don’t use", "Used")) +
  theme_minimal()

##########################
#Familiarity and Years Teaching
##########################

#fix the one value where a respondent put their start date
combined_data$Q6_cleaned[combined_data$Q6_cleaned==2012]<-12

# Fit ordinal logistic regression for the entire dataset
model_all <- polr(as.factor(Q7_numeric) ~ Q6_cleaned, data = combined_data, Hess = TRUE)

# Display model summary
summary(model_all)

# Compute p-values for the coefficients
coeff_all <- coef(summary(model_all))
p_values_all <- pnorm(abs(coeff_all[, "t value"]), lower.tail = FALSE) * 2
coeff_all <- cbind(coeff_all, "p value" = p_values_all)

# Display coefficients with p-values
coeff_all

# Apply the function to each group
results_by_group <- combined_data %>%
  group_by(dataOrigin) %>%
  group_split() %>%
  setNames(unique(combined_data$dataOrigin)) %>%
  lapply(fit_model_by_group,QNumber=combined_data$Q7)

# Display results for each group
for (group in names(results_by_group)) {
  cat("Results for", group, ":\n")
  print(results_by_group[[group]]$coefficients)
  cat("\n")
}

ggplot(combined_data, aes(x = Q6_cleaned , y = Q7_numeric)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +
  labs(
    title = "Years Teaching vs. Familiarity with GenAI",
    x = "Years Teaching (Q6)",
    y = "Familiarity Level (Q7_numeric)"
  ) +
  scale_y_continuous(breaks = c(1, 2, 3), labels = c("Never heard", "Heard but don’t use", "Used")) +
  theme_minimal()

##########################
#Familiarity and level Teaching
##########################
combined_data2<-combined_data%>%filter(Q3_collapsed!="Preschool")
# Fit ordinal logistic regression, however the proportional odds assumption is violated
model <- polr(as.factor(Q7_numeric) ~ Q3_collapsed, data = combined_data2, Hess = TRUE)
# Display the model summary
summary(model)
# Compute p-values for the coefficients
coeff <- coef(summary(model))
p_values <- pnorm(abs(coeff[, "t value"]), lower.tail = FALSE) * 2
coeff <- cbind(coeff, "p value" = p_values)
# Display coefficients with p-values
print(coeff)
# Check proportional odds assumption
brant_test <- brant(model)
# Display test results
print(brant_test)
#Since we don't meet the assumptions we can generalized ordinal logistic regression that allows coefficients to vary across thresholds

# Fit a generalized ordinal logistic regression model
combined_data2$Q7_numeric <- ordered(combined_data2$Q7_numeric, levels = c(1, 2, 3))
prop_model <- vglm(Q7_numeric ~ Q3_collapsed, family = propodds(), data = combined_data2)

# Display model summary
summary(prop_model)

# Extract coefficients
coefficients <- coef(prop_model)

# Calculate odds ratios (exponentiate the coefficients)
odds_ratios <- exp(coefficients)

# Display odds ratios
print(odds_ratios)

odds_ratios <- data.frame(
  Predictor = c("Primary/Elementary School", 
                "Secondary school/High School", "Post high school/University Level", "Multiple"),
  OddsRatio = c(2.59, 4.79, 3.64, 3.37)  # Example odds ratios
)

# Define the order of educational levels (excluding the reference category)
odds_ratios$Predictor <- factor(odds_ratios$Predictor, levels = c(
  "Primary/Elementary School", 
  "Secondary school/High School", "Post high school/University Level", "Multiple"
))

# Plot the odds ratios
library(ggplot2)
ggplot(odds_ratios, aes(x = OddsRatio, y = Predictor, fill = OddsRatio > 1)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Odds Ratios for Familiarity with GenAI by Education Level",
    x = "Odds Ratio",
    y = "Education Level"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"), guide = "none") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##########################
#Accessibility and country
##########################  
  
 # Convert Q11_standardized to numeric values
combined_data <- combined_data %>%
  mutate(
    Q11_numeric = case_when(
      Q11_standardized == "Very accessible" ~ 3,
      Q11_standardized == "Somewhat accessible" ~ 2,
      Q11_standardized == "Not accessible" ~ 1,
      TRUE ~ NA_real_  # Handle missing or unclassified values
    )
  ) 
  
  # Perform Kruskal-Wallis test
kw_test <- kruskal.test(Q11_numeric ~ dataOrigin, data = combined_data)

# Display the test result
kw_test

#pairwise comparisons  
dunn_result <- dunnTest(Q11_numeric ~ dataOrigin, data = combined_data, method = "bonferroni")
#Display pairwise comparisons
print(dunn_result$res)

# Filter out NA values in Q11_standardized
heatmap_data <- combined_data %>%
  filter(!is.na(Q11_standardized)) %>%  # Exclude NA values
  group_by(dataOrigin, Q11_standardized) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(dataOrigin) %>%
  mutate(relative_frequency = count / sum(count))

# Create the heatmap plot
heatmap_plot <- ggplot(heatmap_data, aes(x = Q11_standardized, y = dataOrigin, fill = relative_frequency)) +
  geom_tile(color = "white") +  # Add borders to tiles
  scale_fill_gradient(low = "white", high = "blue", name = "Relative Frequency") +  # Heatmap colors
  geom_text(aes(label = scales::percent(relative_frequency, accuracy = 0.1)), color = "black") +  # Add percentages
  labs(
    title = "Relative Accessibility by Country",
    x = "Accessibility Level",
    y = "Country (Data Origin)"
  ) +
  theme_minimal() +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the heatmap
heatmap_plot  

########################################################################################################
#Question #2
#
##########################
#Knowledge of teachers
#
########################## ##############################################################################
#Part 1, country variance
##########################


# Select relevant columns for NMDS (Q.13.20_1 to Q.13.20_8)
nmds_data <- combined_data %>%
  dplyr::select(starts_with("Q.13.20"))

# Keep only complete cases (rows without NA) in nmds_data
nmds_data <- na.omit(nmds_data)

# Filter combined_data to match rows in nmds_data
filtered_data <- combined_data[rownames(nmds_data), ]

# Calculate the dissimilarity matrix
dissimilarity_matrix <- vegdist(nmds_data, method = "bray")

# Run NMDS
nmds_result <- metaMDS(dissimilarity_matrix, k = 2, trymax = 100)

# Extract NMDS scores
nmds_scores <- as.data.frame(vegan::scores(nmds_result))

# Add Country (dataOrigin) from filtered_data
nmds_scores$Country <- filtered_data$dataOrigin

# Run PERMANOVA
adonis_result <- adonis2(dissimilarity_matrix ~ dataOrigin, data = filtered_data, method = "bray")

# Display the results
print(adonis_result)

# NMDS scatter plot
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Country)) +
  geom_point(linewidth = 3) +
  stat_ellipse(type = "norm", level = 0.95, size = 1) +
  labs(
    title = "NMDS of Knowledge Scores by Country",
    x = "NMDS Dimension 1",
    y = "NMDS Dimension 2",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
  
  pairwise.adonis2(dissimilarity_matrix, fact = nmds_scores$Country)
 
 # Summarize data by country with updated across()
total_scores <- combined_data %>%
  group_by(dataOrigin) %>%
  summarise(across(starts_with("Q.13.20"), \(x) sum(x, na.rm = TRUE)))
  
max_possible_scores <- combined_data %>%
  group_by(dataOrigin) %>%
  summarise(across(starts_with("Q.13.20"), \(x) 5 * sum(!is.na(x))))
  
  # Normalize scores to percentage of maximum possible
normalized_scores <- total_scores %>%
  mutate(across(starts_with("Q.13.20"), ~ .x / max_possible_scores[[cur_column()]] * 100))

# Reshape data to long format
long_data <- normalized_scores %>%
  pivot_longer(
    cols = starts_with("Q.13.20"),
    names_to = "Question",
    values_to = "ScorePercentage"
  )  
  
  # Reorder Question levels based on the highest ScorePercentage within each group
long_data <- long_data %>%
  mutate(Question = fct_reorder(Question, ScorePercentage, .desc = TRUE))

# Sideways grouped barplot with cooler colors
scientific_palette <- c(
  "EngData" = "#4575b4",  # Muted blue
  "SpaData" = "#91bfdb",  # Soft teal
  "GerData" = "#e0f3f8",  # Light cyan
  "CanData" = "#f6d18a"   # Muted yellow-orange
)

# Plot
ggplot(long_data, aes(x = ScorePercentage, y = Question, fill = dataOrigin)) +
  geom_bar(stat = "identity", position = "dodge") +  # Dodge for grouped bars
  scale_fill_manual(name = "Country", values = scientific_palette) +
  labs(
    title = "Question Scores as Percentage of Maximum Possible by Country",
    x = "Percentage of Maximum Score (%)",
    y = "Questions"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom"
  )
 
 ######## 
 #Part 2: Education Level
 #######
 # Add Education Level (Q3_collapsed) from filtered_data
nmds_scores$EducationLevel <- filtered_data$Q3_collapsed

# Run PERMANOVA
adonis_result <- adonis2(dissimilarity_matrix ~ EducationLevel, data = nmds_scores, method = "bray")

# Display the results
print(adonis_result)

# NMDS scatter plot
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = EducationLevel)) +
  geom_point(size = 3) +
  stat_ellipse(type = "norm", level = 0.95, size = 1) +
  labs(
    title = "NMDS of Knowledge Scores by Education Level",
    x = "NMDS Dimension 1",
    y = "NMDS Dimension 2",
    color = "Education Level"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Pairwise PERMANOVA
pairwise.adonis2(dissimilarity_matrix, fact = nmds_scores$EducationLevel)

# Summarize data by education level
total_scores <- combined_data %>%
  group_by(Q3_collapsed) %>%
  summarise(across(starts_with("Q.13.20"), \(x) sum(x, na.rm = TRUE)))

# Calculate maximum possible scores
max_possible_scores <- combined_data %>%
  group_by(Q3_collapsed) %>%
  summarise(across(starts_with("Q.13.20"), \(x) 5 * sum(!is.na(x))))

# Normalize scores to percentage of maximum possible
normalized_scores <- total_scores %>%
  mutate(across(starts_with("Q.13.20"), ~ .x / max_possible_scores[[cur_column()]] * 100))

# Remove rows with NA in Q3_collapsed before reshaping or plotting
long_data <- normalized_scores %>%
  pivot_longer(
    cols = starts_with("Q.13.20"),
    names_to = "Question",
    values_to = "ScorePercentage"
  ) %>%
  filter(!is.na(Q3_collapsed))  # Exclude rows with NA in Q3_collapsed


# Reorder Q3_collapsed within each Question based on ScorePercentage
long_data <- long_data %>%
  group_by(Question) %>%
  mutate(Q3_collapsed = fct_reorder(Q3_collapsed, ScorePercentage, .desc = TRUE)) %>%
  ungroup()
  
  # Filter out "Preschool" from the data since only 2 people responded
long_data_filtered <- long_data %>%
  filter(Q3_collapsed != "Preschool")


# Define a scientific color palette
scientific_palette2 <- c(
  "Primary/Elementary School" = "#AEC6CF",  # Pastel blue
  "Middle School" = "#FFB6C1",              # Soft pink
  "Secondary school/High School" = "#FFDAB9", # Peach puff
  "Post high school/University Level" = "#C6E2FF", # Light sky blue
  "Multiple" = "#D8BFD8"                    # Thistle (soft lavender)
)

# Create a grouped barplot
ggplot(long_data, aes(x = ScorePercentage, y = Question, fill = Q3_collapsed)) +
  geom_bar(stat = "identity", position = "dodge") +  # Dodge for grouped bars
  scale_fill_manual(name = "Education Level", values = scientific_palette2) +
  labs(
    title = "Question Scores as Percentage of Maximum Possible by Education Level",
    x = "Percentage of Maximum Score (%)",
    y = "Questions"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom"
  )
 
########################################################################################################
#Question #3
#
##########################
#How are teachers using AI
#
########################## ##############################################################################
#Part 1, country usage
##########################  

# Tabulate frequencies by country
frequency_table <- combined_data %>%
  filter(!is.na(Frequency_Standardized), !is.na(dataOrigin)) %>%
  group_by(dataOrigin, Frequency_Standardized) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(dataOrigin) %>%
  mutate(
    proportion = count / sum(count),
    sample_size = sum(count)  # Calculate total sample size per group
  )
  
#make a plot to look at overall usage relative to everyone, hard coding for speed since this was an afterthought, sorry!
useNonUse<-data.frame(use=c(5,55,14,44), nonUse=c(45,33,36,30), country=c("Macau","USA","Germany", "Colombia"))

# Calculate proportions
useNonUse <- useNonUse %>%
  mutate(
    Total = use + nonUse,
    Proportion_Use = use / Total,
    Proportion_NonUse = nonUse / Total
  ) %>%
  pivot_longer(cols = c(Proportion_Use, Proportion_NonUse),
               names_to = "UserType",
               values_to = "Proportion")

# Create the barplot
ggplot(useNonUse, aes(x = Proportion, y = country, fill = UserType)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(
    values = c("Proportion_Use" = "#0072B2", "Proportion_NonUse" = "#D55E00"),
    labels = c("Non-Users","Users")
  ) +
  labs(
    title = "Proportion of GenAI Users and Non-Users by Country",
    x = "Proportion",
    y = "Country",
    fill = "User Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom"
  )

#second barplot to look at frequency in the groups that actually use AI
# Define a color palette
scientific_palette <- c(
  "Daily" = "#0072B2",  # Muted blue
  "Weekly" = "#009E73", # Muted green
  "Monthly" = "#D55E00" # Muted orange
)

# Create the grouped bar plot with sample sizes
ggplot(frequency_table, aes(x = dataOrigin, y = proportion, fill = Frequency_Standardized)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Frequency of Use", values = scientific_palette) +
  geom_text(
    aes(
      label = paste0("n=", sample_size),
      y = 1.05  # Position above the bar group
    ),
    position = position_dodge(width = 0.9),
    color = "black",
    size = 3.5,
    vjust = 0
  ) +
  labs(
    title = "Frequency of Use by Country (Proportional to Sample Size)",
    x = "Country",
    y = "Proportion of Responses"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
  
  relevant_cols <- c("Q.23.38_1", "Q.23.38_2", "Q.23.38_3", "Q.23.38_4", "Q.23.38_5", "Q.23.38_6", "Q.23.38_7", "Q.23.38_8", "Q.23.38_9", "Q.23.38_10", "Q.23.38_11", "Q.23.38_12", "Q.23.38_13", "Q.23.38_14", "Q.23.38_15", "Q.23.38_16", "dataOrigin", "Q7_numeric")
 
#Part 2, usage heatmap
 # Select and pivot the data, including 'dataOrigin' and 'Q7'
 long_data <- combined_data %>%
   dplyr::select(all_of(relevant_cols)) %>%
   pivot_longer(
     cols = starts_with("Q.23.38_"),
     names_to = "question",
     values_to = "response"
   )
   
   # Define groups
groups <- list(
  "D&P" = c("Q.23.38_6", "Q.23.38_11"),
  "A&F" = c("Q.23.38_2", "Q.23.38_3", "Q.23.38_12"),
  "LMC" = c("Q.23.38_1", "Q.23.38_4", "Q.23.38_5", "Q.23.38_7", "Q.23.38_8", "Q.23.38_9", "Q.23.38_10", "Q.23.38_13", "Q.23.38_14", "Q.23.38_15")
)

long_data <- long_data %>%
  mutate(
    group = case_when(
      question %in% groups$`D&P` ~ "D&P",
      question %in% groups$`A&F` ~ "A&F",
      question %in% groups$LMC ~ "LMC"
    )
  )
  
#Filter data for Q7_numeric == 3
#Filter data for Q7_numeric == 3
filtered_data <- long_data %>%
  filter(Q7_numeric == 3)
filtered_data <-na.omit(filtered_data)

filtered_data <-filtered_data %>% dplyr::filter(dataOrigin!="CanData")

#Visualization
ggplot(filtered_data, aes(x = dataOrigin, y = response, fill = group)) +
  # Kernel density estimation for the cloud (vertical orientation)
  see::geom_violinhalf()  +
  geom_boxplot(
     width = 0.15,  # Narrow width for clarity
     outlier.shape = NA,  # Hide outliers
     alpha = 0.8,  # Slight transparency for visibility
     position = position_nudge(x = 0)  # Keep aligned
   ) +
   #geom_jitter(
   ##  size = 0.5,  # Point size
    # alpha = 0.4,  # Transparency for overlap clarity
    ## width = 0.1,  # Horizontal jitter
     #color = "black"
   #) +

  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2
  )+
  facet_wrap(~ group, scales = "free_y") +  # Facet by group
# Color palette
  labs(
    title = "Raincloud Plot with Kernel Density: Response by DataOrigin and Group (Q7_numeric = 3)",
    x = "Response",
    y = "Density",
    fill = "Data Origin"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Legend at the bottom
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_flip() 
  
# Summarize data: calculate mean response for each dataOrigin-question combination
heatmap_data <- filtered_data %>%
  group_by(dataOrigin, question) %>%
  summarise(mean_response = mean(response, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

# Reorder rows (dataOrigin) and columns (question) for better visualization
heatmap_data <- heatmap_data %>%
  mutate(
    question = fct_reorder(question, mean_response, .desc = TRUE),
    dataOrigin = fct_reorder(dataOrigin, mean_response, .desc = TRUE)
  )

# Create the heatmap
heatmap_plot <- ggplot(heatmap_data, aes(x = question, y = dataOrigin, fill = mean_response)) +
  geom_tile(color = "white") +  # Add borders to tiles
  scale_fill_viridis_c(
    option = "A", 
    name = "Mean Response", 
    limits = c(1, 4),  # Set limits for the color scale
    breaks = seq(1, 4, 1),  # Optional: Set tick marks
  ) +
  labs(
    title = "Heatmap of Mean Responses by Question and Data Origin",
    x = "Question",
    y = "Data Origin"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "bottom"  # Place legend at the bottom
  )

# Display it
heatmap_plot
   
  #Stats  
  # Kruskal-Wallis test for each group
kruskal_results <- filtered_data %>%
  group_by(group) %>%
  kruskal_test(response ~ dataOrigin)

# Post-hoc pairwise Dunn’s test
dunn_results <- filtered_data %>%
  group_by(group) %>%
  dunn_test(response ~ dataOrigin, p.adjust.method = "BH")

# Print the results
print(kruskal_results)
print(dunn_results)

#NMDS for fun
nmds_data <- combined_data %>%
   dplyr::select(all_of(relevant_cols))
nmds_data <- nmds_data %>%
  filter(Q7_numeric == 3)
nmds_data <-na.omit(nmds_data)
nmds_data <-nmds_data %>% dplyr::filter(dataOrigin!="CanData")

# Extract response matrix and group variable
response_matrix <- nmds_data %>%
  dplyr::select(-dataOrigin, -Q7_numeric)  # Exclude dataOrigin for NMDS
group_variable <- nmds_data$dataOrigin  # Store dataOrigin as grouping variable

# Calculate Bray-Curtis dissimilarity matrix
dissimilarity_matrix <- vegdist(response_matrix, method = "bray")

# Perform NMDS
set.seed(42)  # Ensure reproducibility
nmds_result <- metaMDS(dissimilarity_matrix, k = 2, trymax = 100)

# Extract NMDS scores
nmds_scores <- as.data.frame(scores(nmds_result))
nmds_scores$dataOrigin <- group_variable  # Add grouping variable

# Print NMDS result summary
print(nmds_result)
#PERMANOVA
permanova_result <- adonis2(dissimilarity_matrix ~ dataOrigin, data = nmds_data, method = "bray")
permanova_result

#ANOSIM
anosim_result <- anosim(dissimilarity_matrix, grouping = nmds_data$dataOrigin)
anosim_result

###############################################
####Repeat by education level
###############################################
#slightly lazy and doing the same block copy paste, sorry to anyone checking this!
  relevant_cols2 <- c("Q.23.38_1", "Q.23.38_2", "Q.23.38_3", "Q.23.38_4", "Q.23.38_5", "Q.23.38_6", "Q.23.38_7", "Q.23.38_8", "Q.23.38_9", "Q.23.38_10", "Q.23.38_11", "Q.23.38_12", "Q.23.38_13", "Q.23.38_14", "Q.23.38_15", "Q.23.38_16", "Q3_collapsed", "Q7_numeric")
  
#Part 2, usage heatmap
 # Select and pivot the data, including 'dataOrigin' and 'Q7'
 long_data2 <- combined_data %>%
   dplyr::select(all_of(relevant_cols2)) %>%
   pivot_longer(
     cols = starts_with("Q.23.38_"),
     names_to = "question",
     values_to = "response"
   )
   
long_data2 <- long_data2 %>%
  mutate(
    group = case_when(
      question %in% groups$`D&P` ~ "D&P",
      question %in% groups$`A&F` ~ "A&F",
      question %in% groups$LMC ~ "LMC"
    )
  )
  
#Filter data for Q7_numeric == 3
#Filter data for Q7_numeric == 3
filtered_data2 <- long_data2 %>%
  filter(Q7_numeric == 3)
filtered_data2 <-na.omit(filtered_data2)

filtered_data2 <-filtered_data2 %>% dplyr::filter(Q3_collapsed!="Preschool")

#Visualization
ggplot(filtered_data2, aes(x = Q3_collapsed, y = response, fill = group)) +
  # Kernel density estimation for the cloud (vertical orientation)
  see::geom_violinhalf()  +
  geom_boxplot(
     width = 0.15,  # Narrow width for clarity
     outlier.shape = NA,  # Hide outliers
     alpha = 0.8,  # Slight transparency for visibility
     position = position_nudge(x = 0)  # Keep aligned
   ) +

  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2
  ) +  #geom_jitter(
   ##  size = 0.5,  # Point size
    # alpha = 0.4,  # Transparency for overlap clarity
    ## width = 0.1,  # Horizontal jitter
     #color = "black"
   #) +
 
  facet_wrap(~ group, scales = "free_y") +  # Facet by group
# Color palette
  labs(
    title = "Raincloud Plot with Kernel Density: Response by Teaching level and Group (Q7_numeric = 3)",
    x = "Response",
    y = "Density",
    fill = "Teaching Level"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Legend at the bottom
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_flip() 
  
# Summarize data: calculate mean response for each dataOrigin-question combination
heatmap_data <- filtered_data2 %>%
  group_by(Q3_collapsed, question) %>%
  summarise(mean_response = mean(response, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

# Reorder rows (dataOrigin) and columns (question) for better visualization
heatmap_data <- heatmap_data %>%
  mutate(
    question = fct_reorder(question, mean_response, .desc = TRUE),
    Q3_collapsed = fct_reorder(Q3_collapsed, mean_response, .desc = TRUE)
  )

# Create the heatmap
heatmap_plot <- ggplot(heatmap_data, aes(x = question, y = Q3_collapsed, fill = mean_response)) +
  geom_tile(color = "white") +  # Add borders to tiles
  scale_fill_viridis_c(
    option = "A", 
    name = "Mean Response", 
    limits = c(1, 4),  # Set limits for the color scale
    breaks = seq(1, 4, 1),  # Optional: Set tick marks
  ) +
  labs(
    title = "Heatmap of Mean Responses by Question and Educational Level",
    x = "Question",
    y = "Data Origin"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "bottom"  # Place legend at the bottom
  )

# Display it
heatmap_plot

# Tabulate frequencies by educational level, excluding CanData
frequency_table <- combined_data %>%
  filter(dataOrigin != "CanData") %>%
    filter(Q3_collapsed!="Preschool") %>%
  group_by(Q3_collapsed, Frequency_Standardized) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Q3_collapsed) %>%
  mutate(
    proportion = count / sum(count),  # Calculate the proportion
    sample_size = sum(count)  # Total responses for each educational level
  )
  
  # Define color palette
scientific_palette <- c(
  "Daily" = "#0072B2",  # Muted blue
  "Weekly" = "#009E73", # Muted green
  "Monthly" = "#D55E00" # Muted orange
   #can't remember what color I used, will adjust with adobe
)

ggplot(frequency_table, aes(x = Q3_collapsed, y = proportion, fill = Frequency_Standardized)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Frequency of Use", values = scientific_palette) +
  geom_text(
    aes(
      label = paste0("n=", sample_size),
      y = 1.05  # Position above the bar group
    ),
    position = position_dodge(width = 0.9),
    color = "black",
    size = 3.5,
    vjust = 0
  ) +
  labs(
    title = "Frequency of Use by Educational Level",
    x = "Educational Level",
    y = "Proportion of Responses"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
   
  #Stats  
  # Kruskal-Wallis test for each group
kruskal_results2 <- filtered_data2 %>%
  group_by(group) %>%
  kruskal_test(response ~ Q3_collapsed)

# Post-hoc pairwise Dunn’s test
dunn_results2 <- filtered_data2 %>%
  group_by(group) %>%
  dunn_test(response ~ Q3_collapsed, p.adjust.method = "BH")

# Print the results
print(kruskal_results2)
print(dunn_results2, n=30)

#NMDS for fun
nmds_data <- combined_data %>%
   dplyr::select(all_of(relevant_cols2))
nmds_data <- nmds_data %>%
  dplyr::filter(Q7_numeric == 3)
nmds_data <-na.omit(nmds_data)
nmds_data <-nmds_data %>% dplyr::filter(Q3_collapsed!="Preschool")

# Extract response matrix and group variable
response_matrix <- nmds_data %>%
  dplyr::select(-Q3_collapsed, -Q7_numeric, -dataOrigin)  # Exclude dataOrigin for NMDS
group_variable <- nmds_data %>% dplyr::filter(Q3_collapsed!="Preschool") %>% dplyr::select(Q3_collapsed) 

# Calculate Bray-Curtis dissimilarity matrix
dissimilarity_matrix <- vegdist(response_matrix, method = "bray")

# Perform NMDS
set.seed(42)  # Ensure reproducibility
nmds_result <- metaMDS(dissimilarity_matrix, k = 2, trymax = 100)

# Extract NMDS scores
nmds_scores <- as.data.frame(scores(nmds_result))
nmds_scores$Q3_collapsed <- group_variable  # Add grouping variable

# Print NMDS result summary
print(nmds_result)
#PERMANOVA
permanova_result <- adonis2(dissimilarity_matrix ~ Q3_collapsed, data = nmds_data, method = "bray")
permanova_result

#ANOSIM
anosim_result <- anosim(dissimilarity_matrix, grouping = nmds_data$Q3_collapsed)
anosim_result

#################################################
########Part 4 Teacher Preparation
#################################################

#darn german commas
combined_data$Q39 <- combined_data$Q39 %>%
  str_replace(
    "Ich habe noch nicht gelernt, wie man generative KI in meinem Unterricht einsetzt\\.",
    "I have not yet learned about how to use generative AI  in my classroom."
  )

combined_data <- combined_data %>%
  mutate(Q39_translated = sapply(Q39, Q39translate_response))
   
# Fix Redundant Spaces and Quotes in Translated Data
combined_data$Q39_translated <- combined_data$Q39_translated %>%
  str_replace_all('"', '') %>%  # Remove extra quotes
  str_trim()  # Remove leading and trailing whitespace

tallied_data <- combined_data %>%
  filter(!is.na(Q39_translated)) %>%  # Remove NA responses
  mutate(Q39_translated = str_split(Q39_translated, ",(?!(?:[^()]*\\)))")) %>%  # Safely split responses
  unnest(Q39_translated) %>%  # Expand into separate rows
  mutate(Q39_translated = str_trim(Q39_translated)) %>%  # Remove whitespace again
  count(dataOrigin, Q39_translated, name = "Count") %>%  # Count occurrences
  pivot_wider(names_from = dataOrigin, values_from = Count, values_fill = 0)  # Create a summary table

# Display Results
print(tallied_data)

# Respondent counts per country
respondent_counts <- c(Can = 50, Eng = 88, Ger = 50, Spa = 74)

# Standardize counts by respondent numbers
tallied_data_standardized <- tallied_data %>%
  mutate(
    CanData = CanData / respondent_counts["Can"] * 100,
    EngData = EngData / respondent_counts["Eng"] * 100,
    GerData = GerData / respondent_counts["Ger"] * 100,
    SpaData = SpaData / respondent_counts["Spa"] * 100
  )
  
  # Convert to long format for visualization
long_data <- tallied_data_standardized %>%
  pivot_longer(cols = c(CanData, EngData, GerData, SpaData),
               names_to = "Country",
               values_to = "RelativeFrequency")
  
  ggplot(long_data, aes(x = fct_reorder(Q39_translated, -RelativeFrequency), 
                      y = RelativeFrequency, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Set3") +  # Scientific palette
  labs(
    title = "Standardized AI Learning Sources by Country",
    x = "AI Learning Source",
    y = "Relative Frequency (%)",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
  
  
  sdata<-tallied_data_standardized %>%
  dplyr::select(-Q39_translated) %>%
  t()  # Transpose to have countries as rows and categories as columns
  
distance_matrix <- vegdist(sdata, method = "bray")

#dendrogram and heatmap
res <- hcut(distance_matrix, k = 3, stand = TRUE)
fviz_dend(res, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB", "#E7B800", "#2E9FDF"))
          
colors <- viridis(256, option = "A")
fviz_dist(distance_matrix, order = FALSE, show_labels = TRUE,
          gradient = list(low = colors[1], mid = colors[128], high = colors[256]))          
          
#Repeat to look at teaching level for the above, again going to be lazy and just replicate all the code for the sake of time saving here: 
table(combined_data$Q3_collapsed)
EdLeveltallied_data <- combined_data %>%
  filter(!is.na(Q39_translated)) %>%  # Remove NA responses
   filter(Q3_collapsed !="Preschool") %>%  # Remove two preschool responses
  mutate(Q39_translated = str_split(Q39_translated, ",(?!(?:[^()]*\\)))")) %>%  # Safely split responses
  unnest(Q39_translated) %>%  # Expand into separate rows
  mutate(Q39_translated = str_trim(Q39_translated)) %>%  # Remove whitespace again
  count(Q3_collapsed, Q39_translated, name = "Count") %>%  # Count occurrences
  pivot_wider(names_from = Q3_collapsed, values_from = Count, values_fill = 0)  # Create a summary table

# Display Results
print(EdLeveltallied_data)

# Respondent counts per country
Ed_respondent_counts <- c(`Middle School` = 18, Multiple = 39, `Post high school/University Level` = 40, `Primary/Elementary School` = 36, `Secondary school/High School`=77)

# Standardize counts by respondent numbers
Edtallied_data_standardized <- EdLeveltallied_data %>%
  mutate(
    `Middle School` = `Middle School` / Ed_respondent_counts[1] * 100,
    Multiple = Multiple / Ed_respondent_counts[2] * 100,
    `Post high school/University Level` = `Post high school/University Level` / Ed_respondent_counts[3] * 100,
    `Primary/Elementary School` = `Primary/Elementary School` / Ed_respondent_counts[4] * 100,
    `Secondary school/High School` = `Secondary school/High School` / Ed_respondent_counts[5] * 100
  )
  
  sdata2<-Edtallied_data_standardized %>%
  dplyr::select(-Q39_translated) %>%
  t() 
  
  distance_matrix2 <- vegdist(sdata2, method = "bray")

#dendrogram and heatmap
res2 <- hcut(distance_matrix2, k = 3, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB", "#E7B800", "#2E9FDF"))
          
colors <- viridis(256, option = "A")
fviz_dist(distance_matrix2, order = FALSE, show_labels = TRUE,
          gradient = list(low = colors[1], mid = colors[128], high = colors[256]))  

Edlong_data <- Edtallied_data_standardized %>%
  pivot_longer(cols = colnames(Edtallied_data_standardized)[2:6],
               names_to = "Level",
               values_to = "RelativeFrequency")
  
  ggplot(Edlong_data, aes(x = fct_reorder(Q39_translated, -RelativeFrequency), 
                      y = RelativeFrequency, fill = Level)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Set3") +  # Scientific palette
  labs(
    title = "Standardized AI Learning Sources by Level Taught",
    x = "AI Learning Source",
    y = "Relative Frequency (%)",
    fill = "Level"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
 
 #################################################
########Part 4.2 PD tasks by country
#################################################

  relevant_cols2 <- c(
  "Q.41.56_1", "Q.41.56_2", "Q.41.56_3", "Q.41.56_4", "Q.41.56_5", 
  "Q.41.56_6", "Q.41.56_7", "Q.41.56_8", "Q.41.56_9", "Q.41.56_10", 
  "Q.41.56_11", "Q.41.56_12", "Q.41.56_13", "Q.41.56_14", "Q.41.56_15", 
  "Q.41.56_16", "dataOrigin", "Q7_numeric", "Q3_collapsed"
)
 
#Part 2, usage heatmap
 # Select and pivot the data, including 'dataOrigin' and 'Q7'
 long_data2 <- combined_data %>%
   dplyr::select(all_of(relevant_cols2)) %>%
   pivot_longer(
     cols = starts_with("Q.41.56_"),
     names_to = "question",
     values_to = "response"
   )


   # Define groups
PDgroups <- list(
  "D&P" = c("Q.41.56_6", "Q.41.56_11"),
  "A&F" = c("Q.41.56_2", "Q.41.56_3", "Q.41.56_12"),
  "LMC" = c("Q.41.56_1", "Q.41.56_4", "Q.41.56_5", "Q.41.56_7", "Q.41.56_8", "Q.41.56_9", "Q.41.56_10", "Q.41.56_13", "Q.41.56_14", "Q.41.56_15"),
  "ETHICS" ="Q.41.56_16"
)
 
 long_data2 <- long_data2 %>%
  mutate(
    group = case_when(
      question %in% PDgroups $`D&P` ~ "D&P",
      question %in% PDgroups $`A&F` ~ "A&F",
      question %in% PDgroups $LMC ~ "LMC",
      question %in% PDgroups $ETHICS ~ "ETHICS",
    )
  )
  
 # Replace empty strings with "empty_string" in your data
long_data2$response[long_data2$response == ""] <- "empty_string"

# Map values using the translation
long_data2 <- long_data2 %>%
  mutate(
    numeric_response = sapply(response, function(x) {
      if (is.na(x)) {
        NA  # Preserve NA values
      } else {
        PDresponse_map[[x]] %||% NA  # Use the map, default to NA if no match
      }
    })
  )
  
#Filter data for Q7_numeric == 3
filtered_data2 <- long_data2 %>%
  filter(Q7_numeric == 3)
filtered_data2 <-na.omit(filtered_data2)
filtered_data2 <-filtered_data2 %>% dplyr::filter(Q3_collapsed!="Preschool")
filtered_data2 <-filtered_data2 %>% dplyr::filter(dataOrigin!="CanData")

#Visualization
ggplot(filtered_data2, aes(x = dataOrigin, y = numeric_response, fill = group)) +
  # Kernel density estimation for the cloud (vertical orientation)
  see::geom_violinhalf()  +
  geom_boxplot(
     width = 0.15,  # Narrow width for clarity
     outlier.shape = NA,  # Hide outliers
     alpha = 0.8,  # Slight transparency for visibility
     position = position_nudge(x = 0)  # Keep aligned
   ) +
   gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2
  ) +
   #geom_jitter( 
   ##  size = 0.5,  # Point size
    # alpha = 0.4,  # Transparency for overlap clarity
    ## width = 0.1,  # Horizontal jitter
     #color = "black"
   #) +
 
  facet_wrap(~ group, scales = "free_y") +  # Facet by group
# Color palette
  labs(
    title = "Raincloud Plot with Kernel Density: Response by DataOrigin and Group (Q7_numeric = 3)",
    x = "Response",
    y = "Density",
    fill = "Data Origin"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Legend at the bottom
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_flip() 
  
# Summarize data: calculate mean response for each dataOrigin-question combination
heatmap_data2 <- filtered_data2 %>%
  group_by(dataOrigin, question) %>%
  summarise(mean_response = mean(numeric_response, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

# Reorder rows (dataOrigin) and columns (question) for better visualization
heatmap_data2 <- heatmap_data2 %>%
  mutate(
    question = fct_reorder(question, mean_response, .desc = TRUE),
    dataOrigin = fct_reorder(dataOrigin, mean_response, .desc = TRUE)
  )

# Create the heatmap
heatmap_plot <- ggplot(heatmap_data2, aes(x = question, y = dataOrigin, fill = mean_response)) +
  geom_tile(color = "white") +  # Add borders to tiles
  scale_fill_viridis_c(
    option = "A", 
    name = "Mean Response", 
    limits = c(1, 4),  # Set limits for the color scale
    breaks = seq(1, 4, 1),  # Optional: Set tick marks
  ) +
  labs(
    title = "Heatmap of Mean Responses by Question and Data Origin",
    x = "Question",
    y = "Data Origin"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "bottom"  # Place legend at the bottom
  )

# Display it
heatmap_plot
   
  #Stats  
  # Kruskal-Wallis test for each group
kruskal_results <- filtered_data2 %>%
  group_by(group) %>%
  kruskal_test(numeric_response ~ dataOrigin)

# Post-hoc pairwise Dunn’s test
dunn_results <- filtered_data2 %>%
  group_by(group) %>%
  dunn_test(numeric_response ~ dataOrigin, p.adjust.method = "BH")

# Print the results
print(kruskal_results)
print(dunn_results)


###############################################
####Repeat by education level
###############################################
#Visualization
ggplot(filtered_data2, aes(x = Q3_collapsed, y = numeric_response, fill = group)) +
  # Kernel density estimation for the cloud (vertical orientation)
  see::geom_violinhalf()  +
  geom_boxplot(
     width = 0.15,  # Narrow width for clarity
     outlier.shape = NA,  # Hide outliers
     alpha = 0.8,  # Slight transparency for visibility
     position = position_nudge(x = 0)  # Keep aligned
   ) +
   gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2
  ) +
   #geom_jitter(
   ##  size = 0.5,  # Point size
    # alpha = 0.4,  # Transparency for overlap clarity
    ## width = 0.1,  # Horizontal jitter
     #color = "black"
   #) +
 
  facet_wrap(~ group, scales = "free_y") +  # Facet by group
# Color palette
  labs(
    title = "Raincloud Plot with Kernel Density: Response by Teaching level and Group (Q7_numeric = 3)",
    x = "Response",
    y = "Density",
    fill = "Teaching Level"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Legend at the bottom
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_flip() 
  
# Summarize data: calculate mean response for each dataOrigin-question combination
heatmap_data <- filtered_data2 %>%
  group_by(Q3_collapsed, question) %>%
  summarise(mean_response = mean(numeric_response, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

# Reorder rows (dataOrigin) and columns (question) for better visualization
heatmap_data <- heatmap_data %>%
  mutate(
    question = fct_reorder(question, mean_response, .desc = TRUE),
    Q3_collapsed = fct_reorder(Q3_collapsed, mean_response, .desc = TRUE)
  )

# Create the heatmap
heatmap_plot <- ggplot(heatmap_data, aes(x = question, y = Q3_collapsed, fill = mean_response)) +
  geom_tile(color = "white") +  # Add borders to tiles
  scale_fill_viridis_c(
    option = "A", 
    name = "Mean Response", 
    limits = c(1, 4),  # Set limits for the color scale
    breaks = seq(1, 4, 1),  # Optional: Set tick marks
  ) +
  labs(
    title = "Heatmap of Mean Responses by Question and Level of Instruction",
    x = "Question",
    y = "Data Origin"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "bottom"  # Place legend at the bottom
  )

# Display it
heatmap_plot
   
  #Stats  
  # Kruskal-Wallis test for each group
kruskal_results2 <- filtered_data2 %>%
  group_by(group) %>%
  kruskal_test(numeric_response ~ Q3_collapsed)

# Post-hoc pairwise Dunn’s test
dunn_results2 <- filtered_data2 %>%
  group_by(group) %>%
  dunn_test(numeric_response ~ Q3_collapsed, p.adjust.method = "BH")

# Print the results
print(kruskal_results2)
print(dunn_results2, n=30)

###############################################
####Curious, what about PD between task types, this is not significant but will add to the supplement
###############################################
#Visualization
ggplot(filtered_data2, aes(x = group, y = numeric_response, fill = group)) +
  # Kernel density estimation for the cloud (vertical orientation)
  see::geom_violinhalf()  +
  geom_boxplot(
     width = 0.15,  # Narrow width for clarity
     outlier.shape = NA,  # Hide outliers
     alpha = 0.8,  # Slight transparency for visibility
     position = position_nudge(x = 0)  # Keep aligned
   ) +
   gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2
  ) +
   #geom_jitter(
   ##  size = 0.5,  # Point size
    # alpha = 0.4,  # Transparency for overlap clarity
    ## width = 0.1,  # Horizontal jitter
     #color = "black"
   #) +
 
  facet_wrap(~ group, scales = "free_y") +  # Facet by group
# Color palette
  labs(
    title = "Raincloud Plot with Kernel Density: Response by Teaching level and Group (Q7_numeric = 3)",
    x = "Response",
    y = "Density",
    fill = "Teaching Level"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Legend at the bottom
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_flip() 

  #Stats  
  # Kruskal-Wallis test for each group
kruskal_results2 <- filtered_data2 %>%
  kruskal_test(numeric_response ~ group)

# Post-hoc pairwise Dunn’s test
dunn_results2 <- filtered_data2 %>%
  dunn_test(numeric_response ~ group, p.adjust.method = "BH")

# Print the results
print(kruskal_results2)
print(dunn_results2, n=30)


###############################################
######Part 5 Relationship between PD and Usage 
###############################################

# Create a new dataset with average scores per group
group_averages <- combined_data %>%
  rowwise() %>%  # Process each row independently
  mutate(
    D_P_Avg = mean(c_across(all_of(groups$`D&P`)), na.rm = TRUE),  # Average for D&P group
    A_F_Avg = mean(c_across(all_of(groups$`A&F`)), na.rm = TRUE),  # Average for A&F group
    LMC_Avg = mean(c_across(all_of(groups$LMC)), na.rm = TRUE)     # Average for LMC group
  ) %>%
  ungroup()  # Reset rowwise grouping
# Select only the columns with averages if needed
averages_only <- group_averages %>%
  dplyr::select(dataOrigin, Q7_numeric, D_P_Avg, A_F_Avg, LMC_Avg)

# View averages
print(averages_only)

# Translate responses to numeric
numeric_data <- combined_data %>%
  mutate(across(
    starts_with("Q.41.56_"), 
    ~ PDresponse_map[.x] %||% NA_real_
  ))

# Calculate average scores per group
PD_averages <- numeric_data %>%
  rowwise() %>%
  mutate(
    PD_D_P_Avg = mean(c_across(all_of(PDgroups$`D&P`)), na.rm = TRUE),
    PD_A_F_Avg = mean(c_across(all_of(PDgroups$`A&F`)), na.rm = TRUE),
    PD_LMC_Avg = mean(c_across(all_of(PDgroups$LMC)), na.rm = TRUE),
    PD_ETHICS_Avg = mean(c_across(all_of(PDgroups$`ETHICS`)), na.rm = TRUE)
  ) %>%
  ungroup()

# View resulting dataset
print(PD_averages)

# Select relevant columns to display
PDaverages_only <- PD_averages %>%
  dplyr::select(dataOrigin, Q7_numeric, Q3_collapsed, PD_D_P_Avg, PD_A_F_Avg, PD_LMC_Avg, PD_ETHICS_Avg)

# View averages
print(PDaverages_only)

# Replace all NA values with 0
# Replace NA values with 0 only in the last 4 columns
PDaverages_only <- PDaverages_only %>%
  mutate(across(c(PD_D_P_Avg, PD_A_F_Avg, PD_LMC_Avg, PD_ETHICS_Avg), ~ replace_na(.x, 0)))
#testData<-cbind(averages_only, PDaverages_only) %>% na.omit()

#Use Penalized Regression:
#Use LASSO (least absolute shrinkage and selection operator) to identify the most important predictors and reduce multicollinearity's effect:

x <- model.matrix(D_P_Avg ~ PD_D_P_Avg + PD_A_F_Avg + PD_LMC_Avg + PD_ETHICS_Avg, data = testData)[, -1]
y <- testData$D_P_Avg
lasso_model <- glmnet(x, y, alpha = 1)
cv_lasso <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_lasso$lambda.min
lasso_best <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(lasso_best)

x <- model.matrix(A_F_Avg ~ PD_D_P_Avg + PD_A_F_Avg + PD_LMC_Avg + PD_ETHICS_Avg, data = testData)[, -1]
y <- testData$A_F_Avg
lasso_model <- glmnet(x, y, alpha = 1)
cv_lasso <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_lasso$lambda.min
lasso_best <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(lasso_best)

x <- model.matrix(LMC_Avg ~ PD_D_P_Avg + PD_A_F_Avg + PD_LMC_Avg + PD_ETHICS_Avg, data = testData)[, -1]
y <- testData$LMC_Avg
lasso_model <- glmnet(x, y, alpha = 1)
cv_lasso <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_lasso$lambda.min
lasso_best <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(lasso_best)


# Fit linear models for each AI usage pattern
lm_D_P <- lm(D_P_Avg ~ PD_D_P_Avg, data = testData)
lm_A_F <- lm(A_F_Avg ~ PD_A_F_Avg + PD_LMC_Avg, data = testData)
lm_LMC <- lm(LMC_Avg ~ PD_A_F_Avg + PD_LMC_Avg, data = testData)

# Summarize the results
summary(lm_D_P)
summary(lm_A_F)
summary(lm_LMC)

# Combine results into a summary table
results <- list(
  D_P_Model = broom::tidy(lm_D_P),
  A_F_Model = broom::tidy(lm_A_F),
  LMC_Model = broom::tidy(lm_LMC)
)

# Print the summary table for each model
results$D_P_Model
results$A_F_Model
results$LMC_Model

# Extract R-squared for each model
rsquared <- data.frame(
  Model = c("D_P", "A_F", "LMC"),
  R_Squared = c(summary(lm_D_P)$r.squared, summary(lm_A_F)$r.squared, summary(lm_LMC)$r.squared)
)
print(rsquared)

# Create a combined dataset with estimates for each model
plot_data <- bind_rows(
  results$D_P_Model %>% mutate(Model = "D_P"),
  results$A_F_Model %>% mutate(Model = "A_F"),
  results$LMC_Model %>% mutate(Model = "LMC")
)

# Add R-squared values to the dataset
rsquared <- data.frame(
  Model = c("D_P", "A_F", "LMC"),
  R_Squared = c(summary(lm_D_P)$r.squared, summary(lm_A_F)$r.squared, summary(lm_LMC)$r.squared)
)

# Merge R-squared values into plot_data
plot_data <- plot_data %>%
  left_join(rsquared, by = "Model")

# Create the facet plot
ggplot(plot_data, aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  facet_wrap(~ Model, scales = "free_y") +
  labs(
    title = "Effect Sizes by Model",
    subtitle = "R-squared values displayed in facet titles",
    x = "Terms",
    y = "Effect Estimate"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("") +
  ggtitle(expression(paste("Effect Estimates by Model", "\n", "R-squared Values: ", r^2))) +
  facet_wrap(~ Model, labeller = labeller(Model = function(x) paste0(x, " (R² = ", round(rsquared$R_Squared[rsquared$Model == x], 2), ")")))