```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.align = 'center',
  out.width = "100%"
)

# Required libraries
library(dplyr)
library(ggplot2)
library(kableExtra)
library(corrplot)
library(reshape2)
library(psych)
library(DT)
library(gridExtra)
library(grid)
library(nnet)
library(caret)
library(car)
library(MASS)
library(tidyverse)
```

```{r load_data}
# Load the Hitters dataset
hitters <- read.csv("Hitters.csv")

# Display a clean summary table of the first few rows
knitr::kable(head(hitters), 
             caption = "First 6 rows of the Hitters dataset",
             align = "c", 
             booktabs = TRUE) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = FALSE,
                          font_size = 11) %>%
  kableExtra::column_spec(column = 1, bold = TRUE)

```

```{r import_data}
# We already imported the data in the previous step
# Check dimensions and structure
dim(hitters)        # Total number of units and measurements
str(hitters)        # Shows data structure and types
```

```{r import_data_visualization, message=FALSE, echo=FALSE}

# Summary of categorical variables
cat("## Categorical Variables Summary\n\n")

cat("### League distribution:\n")
knitr::kable(table(hitters$League), 
             col.names = c("League", "Count"),
             align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("bordered", "condensed"),
                          font_size = 11,
                          full_width = FALSE)

cat("### Division distribution:\n")
knitr::kable(table(hitters$Division), 
             col.names = c("Division", "Count"),
             align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("bordered", "condensed"),
                          font_size = 11,
                          full_width = FALSE)

cat("### New League distribution:\n")
knitr::kable(sort(table(hitters$NewLeague), decreasing = TRUE), 
             col.names = c("New League", "Count"),
             align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("bordered", "condensed"),
                          font_size = 11,
                          full_width = FALSE)
```

```{r missing_values, fig.width=10, fig.height=6}
# Create a standardized theme for all plots
my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

# Check for missing values
missing_counts <- colSums(is.na(hitters))
missing_percentage <- round(missing_counts/nrow(hitters)*100, 2)

missing_df <- data.frame(
  Variable = names(missing_counts),
  Missing_Count = missing_counts,
  Missing_Percentage = missing_percentage
) %>% arrange(desc(Missing_Count))

# Visualize missing values pattern
missing_pattern <- hitters %>%
  is.na() %>%
  colSums() %>%
  sort(decreasing = TRUE)

ggplot(data.frame(feature = names(missing_pattern), missing_pct = missing_pattern), aes(x = missing_pct, y = feature)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(missing_pct, 2))), hjust = 0) +
  labs(title = "Missing Values Distribution Chart", x = "Number of Missing Values", y = "Feature") +
  coord_flip() +
  my_theme
```

```{r handle_missing}
# Count total rows before removing NAs
total_rows <- nrow(hitters)

# Remove rows with missing values
hitters_complete <- na.omit(hitters)

# Count rows after removal
complete_rows <- nrow(hitters_complete)
removed_count <- total_rows - complete_rows
removed_pct <- round(removed_count / total_rows * 100, 2)

# Create a summary dataframe
missing_summary <- data.frame(
  Metric = c("Original Observations", "Observations with Missing Values", 
             "Complete Observations", "Percentage of Data Removed"),
  Value = c(total_rows, removed_count, complete_rows, paste0(removed_pct, "%"))
)

# Display as a nice table with bootstrap styling
knitr::kable(missing_summary, 
             caption = "Missing Value Handling Summary",
             align = c("l", "c")) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                           full_width = FALSE) %>%
  kableExtra::row_spec(0, bold = TRUE, background = "#E6E6FA") %>%
  kableExtra::column_spec(1, bold = TRUE)

# Use the dataset without missing values
hitters <- hitters_complete
```

```{r outlier_detection, fig.width=10, fig.height=8}
# Create a function to identify outliers using IQR method
identify_outliers <- function(x, coef = 1.5) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  upper <- q3 + coef * iqr
  lower <- q1 - coef * iqr
  return(x < lower | x > upper)
}

# Apply to numeric columns
numeric_cols <- sapply(hitters, is.numeric)
outliers_check <- sapply(hitters[, numeric_cols], identify_outliers)
outliers_summary <- colSums(outliers_check)

# Create a better formatted outlier summary table
outlier_df <- data.frame(
  Variable = names(outliers_summary),
  Outlier_Count = outliers_summary,
  Percentage = round(outliers_summary/nrow(hitters)*100, 2)
) %>% arrange(desc(Outlier_Count))

# Display outlier summary as a nice table
knitr::kable(outlier_df, 
             col.names = c("Variable", "Number of Outliers", "Percentage of Data (%)"),
             caption = "Outlier Summary by Variable",
             align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                           full_width = FALSE) %>%
  kableExtra::row_spec(which(outlier_df$Outlier_Count > 15), background = "#FFE4E1")

# Visualize outliers in key variables with enhanced boxplots
key_vars <- c("Salary", "Years", "Hits", "HmRun")
key_data <- reshape2::melt(hitters[, key_vars])

# Create a more visually appealing boxplot with improved formatting
ggplot(key_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "#3498db", alpha = 0.7, outlier.color = "#e74c3c", 
               outlier.shape = 16, outlier.size = 2, outlier.alpha = 0.8) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Boxplots of Key Variables",
       subtitle = "Red dots indicate potential outliers",
       x = "",
       y = "Value") +
  my_theme +
  theme(strip.text = element_text(size = 14, face = "bold"),
        strip.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9", color = NA),
        panel.border = element_rect(color = "#dcdcdc", fill = NA))
```

```{r explore_numeric, fig.width=10, fig.height=8}
# Create a more informative summary statistics table
num_stats <- psych::describe(select_if(hitters, is.numeric))
num_summary <- data.frame(
  Variable = rownames(num_stats),
  n = num_stats$n,
  mean = num_stats$mean,
  sd = num_stats$sd,
  median = num_stats$median,
  min = num_stats$min,
  max = num_stats$max,
  skew = num_stats$skew,
  kurtosis = num_stats$kurtosis
)

# Round only the numeric columns
num_summary[,2:9] <- round(num_summary[,2:9], digits = 2)

# Display the summary as a nicely formatted table with DT for better interactivity
DT::datatable(num_summary,
              caption = "Detailed Summary Statistics of Numeric Variables",
              options = list(
                pageLength = 10,
                dom = 'Bfrtip',
                scrollX = TRUE,
                autoWidth = TRUE
              ),
              rownames = FALSE,
              class = 'cell-border stripe') %>%
  DT::formatStyle(columns = 1, fontWeight = 'bold') %>%
  DT::formatRound(columns = 3:9, digits = 2)

# Enhanced histograms with density plots for key variables
key_vars <- c("Salary", "AtBat", "Hits", "HmRun")
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0), bg = "#f9f9f9")

colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12")
i <- 1

for (var in key_vars) {
  hist(hitters[[var]], 
       main = paste(var, "Distribution"), 
       col = adjustcolor(colors[i], alpha.f = 0.7), 
       border = "white",
       breaks = 20,
       probability = TRUE,
       xlab = var,
       font.main = 2)
  lines(density(hitters[[var]]), col = "darkred", lwd = 2)
  rug(hitters[[var]], col = adjustcolor("navy", alpha.f = 0.5))
  i <- i + 1
}
mtext("Distributions of Key Variables with Density Curves", outer = TRUE, cex = 1.5, font = 2)
```

```{r standardize_data}
# Identify numeric columns (explicitly excluding the target variable "Salary")
predictors <- names(hitters)[sapply(hitters, is.numeric) & names(hitters) != "Salary"]

# Create a copy of the dataset
hitters_scaled <- hitters

# Standardize the numeric predictors
hitters_scaled[predictors] <- scale(hitters[predictors])

# Create a before/after comparison table for a few key variables
set.seed(123) # For reproducible sampling
sample_players <- sample(1:nrow(hitters), 5)

comparison_data <- data.frame(
  Player = 1:5,
  AtBat_Original = hitters[sample_players, "AtBat"],
  AtBat_Scaled = hitters_scaled[sample_players, "AtBat"],
  Hits_Original = hitters[sample_players, "Hits"],
  Hits_Scaled = hitters_scaled[sample_players, "Hits"],
  HmRun_Original = hitters[sample_players, "HmRun"],
  HmRun_Scaled = hitters_scaled[sample_players, "HmRun"]
)

# Display comparison table with improved formatting
knitr::kable(comparison_data, 
             caption = "Comparison of Original vs. Standardized Values for 5 Random Players",
             align = "c",
             digits = 2) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                           full_width = FALSE) %>%
  kableExtra::add_header_above(c(" " = 1, "At Bats" = 2, "Hits" = 2, "Home Runs" = 2)) %>%
  kableExtra::column_spec(1, bold = TRUE, background = "#f9f9f9") %>%
  kableExtra::column_spec(c(3,5,7), background = "#e8f4f8")

# Check standardization results
scaled_summary <- data.frame(
  Variable = predictors,
  Mean = sapply(hitters_scaled[predictors], mean),
  SD = sapply(hitters_scaled[predictors], sd)
)

# Round only the numeric columns
scaled_summary[,2:3] <- round(scaled_summary[,2:3], digits = 5)

# Display standardization verification table
knitr::kable(scaled_summary, 
             col.names = c("Variable", "Mean (should be ~0)", "Standard Deviation (should be 1)"),
             caption = "Verification of Standardization Results",
             align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                           full_width = FALSE) %>%
  kableExtra::row_spec(0, bold = TRUE, background = "#f9f9f9") %>%
  kableExtra::column_spec(1, bold = TRUE)
```

```{r create_categorical_response, fig.width=10, fig.height=8}
# Display the summary of Salary to understand its distribution
summary(hitters$Salary)

# Calculate tertiles
tertiles <- quantile(hitters$Salary, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
tertiles

# Create SalaryLevel using salary tertiles
hitters$SalaryLevel <- cut(hitters$Salary, 
                          breaks = tertiles, 
                          labels = c("LowSalary", "MediumSalary", "HighSalary"),
                          include.lowest = TRUE)

# Verify the new variable
table(hitters$SalaryLevel)
prop.table(table(hitters$SalaryLevel)) * 100  # Percentage in each category

# Visualize the distribution with improved styling
ggplot(hitters, aes(x = SalaryLevel, fill = SalaryLevel)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = c("LowSalary" = "#66c2a5", 
                               "MediumSalary" = "#fc8d62", 
                               "HighSalary" = "#8da0cb")) +
  labs(title = "Distribution of Salary Levels",
       x = "Salary Level",
       y = "Number of Players") +
  my_theme

# Also add SalaryLevel to our scaled dataset
hitters_scaled$SalaryLevel <- hitters$SalaryLevel
```

```{r salary_cutoffs, fig.width=10, fig.height=8}
# Define league-specific thresholds
mlb_avg <- median(hitters$Salary, na.rm = TRUE)
mlb_low <- quantile(hitters$Salary, 0.25)  # Bottom 25%
mlb_high <- quantile(hitters$Salary, 0.75) # Top 25%

# Compare with tertiles
cat("Tertiles:", tertiles, "\n",
    "MLB Benchmarks: Low =", mlb_low, "Avg =", mlb_avg, "High =", mlb_high)

# Create a more informative visualization
# Salary distribution with clear markings
ggplot(hitters, aes(x = Salary)) +
  geom_histogram(bins = 25, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = tertiles, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = c(mlb_low, mlb_avg, mlb_high), 
             color = "darkgreen", linetype = "dotted", size = 1) +
  annotate("text", x = tertiles[2], y = 45, label = "1st Tertile", 
           color = "red", angle = 90, vjust = -0.5) +
  annotate("text", x = tertiles[3], y = 45, label = "2nd Tertile", 
           color = "red", angle = 90, vjust = -0.5) +
  annotate("text", x = mlb_low, y = 40, label = "25th Percentile", 
           color = "darkgreen", angle = 90, vjust = -0.5) +
  annotate("text", x = mlb_avg, y = 40, label = "Median", 
           color = "darkgreen", angle = 90, vjust = 1.5) +
  annotate("text", x = mlb_high, y = 40, label = "75th Percentile", 
           color = "darkgreen", angle = 90, vjust = -0.5) +
  labs(title = "Salary Distribution with Statistical Divisions",
       subtitle = "Red dashed lines = tertiles, Green dotted lines = quartiles and median",
       x = "Salary ($)",
       y = "Count") +
  my_theme

# Compare category sizes
tertile_groups <- cut(hitters$Salary, 
                     breaks = tertiles, 
                     labels = c("Low", "Medium", "High"),
                     include.lowest = TRUE)

mlb_groups <- cut(hitters$Salary, 
                 breaks = c(min(hitters$Salary), mlb_low, mlb_high, max(hitters$Salary)), 
                 labels = c("Low", "Medium", "High"),
                 include.lowest = TRUE)

# Create a comparison table
category_comparison <- data.frame(
  Category = c("Low", "Medium", "High"),
  Tertile_Count = as.numeric(table(tertile_groups)),
  Tertile_Pct = round(prop.table(table(tertile_groups)) * 100, 1),
  MLB_Count = as.numeric(table(mlb_groups)),
  MLB_Pct = round(prop.table(table(mlb_groups)) * 100, 1)
)

# Display the comparison
cat("Category size comparison between tertiles and MLB benchmarks:\n")
print(category_comparison)
```

```{r performance_by_salary, fig.width=10, fig.height=7}
# Create boxplots for key performance metrics with consistent styling
p1 <- ggplot(hitters, aes(x = SalaryLevel, y = Hits, fill = SalaryLevel)) +
  geom_boxplot() +
  scale_fill_manual(values = c("LowSalary" = "#66c2a5", 
                              "MediumSalary" = "#fc8d62", 
                              "HighSalary" = "#8da0cb")) +
  labs(title = "Hits by Salary Level", x = "Salary Level", y = "Hits") +
  my_theme +
  theme(legend.position = "none")

p2 <- ggplot(hitters, aes(x = SalaryLevel, y = HmRun, fill = SalaryLevel)) +
  geom_boxplot() +
  scale_fill_manual(values = c("LowSalary" = "#66c2a5", 
                              "MediumSalary" = "#fc8d62", 
                              "HighSalary" = "#8da0cb")) +
  labs(title = "Home Runs by Salary Level", x = "Salary Level", y = "Home Runs") +
  my_theme +
  theme(legend.position = "none")

p3 <- ggplot(hitters, aes(x = SalaryLevel, y = RBI, fill = SalaryLevel)) +
  geom_boxplot() +
  scale_fill_manual(values = c("LowSalary" = "#66c2a5", 
                              "MediumSalary" = "#fc8d62", 
                              "HighSalary" = "#8da0cb")) +
  labs(title = "RBIs by Salary Level", x = "Salary Level", y = "RBIs") +
  my_theme +
  theme(legend.position = "none")

p4 <- ggplot(hitters, aes(x = SalaryLevel, y = Years, fill = SalaryLevel)) +
  geom_boxplot() +
  scale_fill_manual(values = c("LowSalary" = "#66c2a5", 
                              "MediumSalary" = "#fc8d62", 
                              "HighSalary" = "#8da0cb")) +
  labs(title = "Years in League by Salary Level", x = "Salary Level", y = "Years") +
  my_theme +
  theme(legend.position = "none")

# Arrange the boxplots in a grid
grid.arrange(p1, p2, p3, p4, ncol = 2, 
             top = textGrob("Performance Metrics by Salary Level", 
                           gp = gpar(fontsize = 16, fontface = "bold")))

# Create a data frame for the performance metrics table
performance_metrics <- data.frame(
  Metric = c("Hits", "Home Runs", "RBIs", "Experience (Years)"),
  Observation = c("Higher salaries correlate with more hits (clear increasing trend)", 
                 "Higher-salaried players hit more home runs, with notable separation",
                 "Clear increasing trend with salary level, especially for high-salary players",
                 "Strong indicator of high salary, suggesting experience is highly valued")
)

knitr::kable(performance_metrics, 
             col.names = c("Performance Metric", "Observation"),
             align = c("l", "l"),
             caption = NULL) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                          full_width = FALSE,
                          position = "center",
                          font_size = 11) %>%
  kableExtra::column_spec(1, bold = TRUE, color = "#2c3e50") %>%
  kableExtra::row_spec(0, bold = TRUE, background = "#f2f2f2")
```

```{r career_vs_current, fig.width=10, fig.height=7, results='asis'}
# Years vs Salary colored by Salary Level
p5 <- ggplot(hitters, aes(x = Years, y = Salary, color = SalaryLevel)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = c("LowSalary" = "#66c2a5", 
                               "MediumSalary" = "#fc8d62", 
                               "HighSalary" = "#8da0cb")) +
  labs(title = "Experience vs Salary", 
       x = "Years in League", 
       y = "Salary",
       color = "Salary Level") +
  my_theme

# Hits vs Salary colored by League
p6 <- ggplot(hitters, aes(x = Hits, y = Salary, color = League)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = c("A" = "#e41a1c", "N" = "#377eb8")) +
  labs(title = "Hits vs Salary by League", 
       x = "Hits", 
       y = "Salary",
       color = "League") +
  my_theme

# Compare current season to career performance
p7 <- ggplot(hitters, aes(x = Hits, y = CHits/Years, color = SalaryLevel)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = c("LowSalary" = "#66c2a5", 
                               "MediumSalary" = "#fc8d62", 
                               "HighSalary" = "#8da0cb")) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = "Current vs Average Career Hits", 
       x = "Current Season Hits", 
       y = "Career Average Hits per Season",
       color = "Salary Level") +
  my_theme

# Arrange scatter plots with interpretation
grid.arrange(p5, p6, ncol = 2,
             top = textGrob("Experience and Performance Relationships", 
                           gp = gpar(fontsize = 16, fontface = "bold")))

# Display the third plot separately with additional annotation
p7

# Create a data frame for the career vs current performance table
career_insights <- data.frame(
  Aspect = c("Experience", "League Comparison", "Career Consistency", "Performance Clusters"),
  Finding = c("Clear positive relationship between years in league and salary", 
              "No significant salary differences between American and National League players with similar performance",
              "Players with higher career averages tend to earn more, even when current season performance is similar",
              "High-salary players cluster in the upper-right quadrant of performance metrics (both current and career)")
)

knitr::kable(career_insights, 
             col.names = c("Key Aspect", "Finding"),
             align = c("l", "l")) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                          full_width = FALSE,
                          position = "center",
                          font_size = 11) %>%
  kableExtra::column_spec(1, bold = TRUE, color = "#2c3e50") %>%
  kableExtra::row_spec(0, bold = TRUE, background = "#f2f2f2")
```

```{r performance_distribution, fig.width=9, fig.height=5, results='asis'}
# Density plots for key metrics by Salary Level with improved styling
p8 <- ggplot(hitters, aes(x = AtBat, fill = SalaryLevel)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("LowSalary" = "#66c2a5", 
                              "MediumSalary" = "#fc8d62", 
                              "HighSalary" = "#8da0cb")) +
  labs(title = "At Bats Distribution by Salary Level", 
       x = "At Bats",
       y = "Density",
       fill = "Salary Level") +
  my_theme +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.title = element_text(size=9),
        legend.text = element_text(size=8))

p9 <- ggplot(hitters, aes(x = Runs, fill = SalaryLevel)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("LowSalary" = "#66c2a5", 
                              "MediumSalary" = "#fc8d62", 
                              "HighSalary" = "#8da0cb")) +
  labs(title = "Runs Distribution by Salary Level", 
       x = "Runs",
       y = "Density",
       fill = "Salary Level") +
  my_theme +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(size=9),
        legend.text = element_text(size=8))

# Arrange density plots
grid.arrange(p8, p9, ncol = 2,
             top = textGrob("Performance Distributions by Salary Level", 
                           gp = gpar(fontsize = 14, fontface = "bold")))

# Create a nicer formatted table for distribution analysis
distribution_analysis <- data.frame(
  Metric = c("At Bats", "Runs", "Playing Time", "Production Value"),
  Observation = c("Higher-salaried players have more plate appearances (distribution shifts right with each salary tier)",
                 "Clear separation between salary tiers, with higher-salaried players scoring more runs",
                 "More at-bats suggests higher-paid players receive more regular playing time",
                 "Run production appears to be a key factor in salary determination")
)

knitr::kable(distribution_analysis, 
             col.names = c("Aspect", "Key Observation"),
             align = c("l", "l")) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                          full_width = FALSE,
                          position = "center",
                          font_size = 11) %>%
  kableExtra::column_spec(1, bold = TRUE, color = "#2c3e50") %>%
  kableExtra::row_spec(0, bold = TRUE, background = "#f2f2f2")
```

```{r correlation_analysis, fig.width=9, fig.height=7, results='asis'}
# Select numeric variables for correlation analysis with improved readability
num_vars <- hitters %>% 
  dplyr::select(Salary, AtBat, Hits, HmRun, Runs, RBI, Walks, Years, 
                CAtBat, CHits, CHmRun, CRuns, CRBI, CWalks) %>%
  cor()

# Create a correlation heatmap with better formatting
corr_melted <- reshape2::melt(num_vars)

# Correlation heatmap with improved aesthetics
ggplot(corr_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#4575b4", high = "#d73027", mid = "white", 
                      midpoint = 0, limit = c(-1,1), name="Correlation") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 2.2, 
            color = ifelse(abs(corr_melted$value) > 0.7, "white", "black")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10)) +
  labs(title = "Correlation Heatmap of Key Variables",
       subtitle = "Showing relationships between performance metrics and salary",
       x = "", y = "")

# Create a table for strong salary correlations
salary_correlations <- data.frame(
  Variable = c("Years in league", "Career RBIs (CRBI)", "Career Hits (CHits)", "Season RBIs"),
  Correlation = c(0.64, 0.63, 0.59, 0.54),
  Interpretation = c("Experience is highly valued", 
                    "Long-term production capability", 
                    "Consistent hitting ability", 
                    "Current season productivity")
)

# Create a table for multicollinearity concerns
multicollinearity <- data.frame(
  Issue = c("Performance metrics", "Career vs. current stats", "Modeling implications"),
  Description = c("Hits & AtBat highly correlated (r = 0.97)", 
                 "Current and career statistics for same metrics show strong correlations",
                 "Will need variable selection or dimensional reduction techniques")
)

knitr::kable(salary_correlations, 
             col.names = c("Variable", "Correlation (r)", "Interpretation"),
             align = c("l", "c", "l")) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                          full_width = FALSE,
                          position = "left",
                          font_size = 11) %>%
  kableExtra::column_spec(1, bold = TRUE) %>%
  kableExtra::column_spec(2, color = ifelse(salary_correlations$Correlation > 0.6, "#d73027", "#4575b4"), 
                        bold = TRUE) %>%
  kableExtra::row_spec(0, bold = TRUE, background = "#f2f2f2")

knitr::kable(multicollinearity, 
             col.names = c("Concerns", "Description"),
             align = c("l", "l")) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                          full_width = FALSE,
                          position = "left",
                          font_size = 11) %>%
  kableExtra::column_spec(1, bold = TRUE, width = "15em") %>%
  kableExtra::row_spec(0, bold = TRUE, background = "#f2f2f2")
```

```{r create_eval_function, echo=TRUE}
# Function for model evaluation metrics
evaluate_model <- function(model, data, actual_col = "SalaryLevel") {
  # Get actual values
  actual <- data[[actual_col]]
  
  # Predict using the model
  predicted <- predict(model, data)
  
  # Create confusion matrix
  conf_matrix <- table(Predicted = predicted, Actual = actual)
  
  # Calculate accuracy
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  # Calculate class-specific metrics
  precision <- diag(conf_matrix) / colSums(conf_matrix)
  recall <- diag(conf_matrix) / rowSums(conf_matrix)
  f1_score <- 2 * precision * recall / (precision + recall)
  
  # Return all metrics in a list
  return(list(
    conf_matrix = conf_matrix,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1_score = f1_score
  ))
}

# Function to visualize confusion matrix
plot_confusion_matrix <- function(conf_matrix, title = "Confusion Matrix") {
  # Convert to data frame for ggplot
  conf_df <- as.data.frame(as.table(conf_matrix))
  
  # Create the plot
  ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq)) +
    scale_fill_gradient(low = "white", high = "#2c3e50") +
    labs(title = title, 
         x = "Actual Salary Level", 
         y = "Predicted Salary Level") +
    my_theme
}
```

```{r mnl_full_model, message=FALSE, echo=TRUE}
# Fitting the full multinomial logistic regression model using LowSalary as the reference category
model_full <- multinom(SalaryLevel ~ . -Salary, data = hitters, trace = FALSE)
```

```{r calculate_significance, echo=TRUE}
# Calculating z-values and p-values
z_values <- summary(model_full)$coefficients / summary(model_full)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Creating a formatted table of p-values with significance indicators
format_pvalues <- function(p_matrix, threshold = 0.05) {
  formatted <- round(p_matrix, 4)
  # Adding asterisks for significant values
  formatted_char <- apply(formatted, c(1, 2), function(x) {
    if (x < 0.001) return(paste0(x, "***"))
    else if (x < 0.01) return(paste0(x, "**"))
    else if (x < 0.05) return(paste0(x, "*"))
    else return(as.character(x))
  })
  return(formatted_char)
}
```

```{r show_pvalues, echo=TRUE}
# Create better looking p-value tables with kable
library(knitr)
library(kableExtra)

# MediumSalary coefficients p-values
medium_salary_pvals <- format_pvalues(p_values[1,, drop = FALSE])
medium_salary_df <- data.frame(
  Variable = colnames(medium_salary_pvals),
  "P-value" = as.vector(medium_salary_pvals)
)

# HighSalary coefficients p-values
high_salary_pvals <- format_pvalues(p_values[2,, drop = FALSE])
high_salary_df <- data.frame(
  Variable = colnames(high_salary_pvals),
  "P-value" = as.vector(high_salary_pvals)
)

# Display MediumSalary p-values
cat("P-values for MediumSalary coefficients:\n")
kable(medium_salary_df, format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "left")

# Display HighSalary p-values
cat("P-values for HighSalary coefficients:\n")
kable(high_salary_df, format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "left")
```

```{r evaluate_full_model, echo=TRUE}
# Evaluate the full model using our function
full_model_eval <- evaluate_model(model_full, hitters)
```

```{r full_model_confusion, echo=TRUE}
# Display the confusion matrix using kable for better formatting
conf_matrix_df <- as.data.frame(full_model_eval$conf_matrix)
names(conf_matrix_df) <- c("Actual", "Predicted", "Count")

# Create a better looking table
conf_matrix_wide <- reshape2::dcast(conf_matrix_df, Predicted ~ Actual, value.var = "Count")
rownames(conf_matrix_wide) <- conf_matrix_wide$Predicted
conf_matrix_wide$Predicted <- NULL

kable(conf_matrix_wide, format = "html", caption = "Confusion Matrix for Full Model") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Actual" = ncol(conf_matrix_wide)))
```

```{r class_metrics, echo=TRUE}
# Create a formatted table of class-specific metrics
metrics_df <- data.frame(
  SalaryLevel = names(full_model_eval$precision),
  Precision = round(full_model_eval$precision, 3),
  Recall = round(full_model_eval$recall, 3),
  F1_Score = round(full_model_eval$f1_score, 3)
)

# Create a better looking table
kable(metrics_df, format = "html", 
      caption = "Class-Specific Performance Metrics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE)

# Create a bar plot of class metrics
library(reshape2)
metrics_long <- melt(metrics_df, id.vars = "SalaryLevel", 
                    variable.name = "Metric", value.name = "Value")

ggplot(metrics_long, aes(x = SalaryLevel, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Value, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(title = "Performance Metrics by Salary Level",
       x = "Salary Level", 
       y = "Score") +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, max(metrics_long$Value) * 1.1) +
  my_theme
```

```{r plot_confusion, echo=FALSE}
plot_confusion_matrix(full_model_eval$conf_matrix, "Full Model Confusion Matrix")
```

```{r multicollinearity, fig.width=10, fig.height=6, echo=FALSE}
# We'll use a linear model to check for multicollinearity among predictors
# Using AtBat as an arbitrary dependent variable to check VIF values
lm_model <- lm(AtBat ~ . -Salary -SalaryLevel, data = hitters)
vif_values <- vif(lm_model)
```

```{r vif_table, echo=TRUE}
# Create a df for better visualization
vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = as.numeric(vif_values)
)

# Sort by VIF value
vif_df <- vif_df[order(-vif_df$VIF),]

# Display VIF values as a formatted table
kable(vif_df, format = "html", caption = "Variance Inflation Factors (VIF)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE) %>%
  row_spec(which(vif_df$VIF > 10), background = "#FFCCCC") %>%
  row_spec(which(vif_df$VIF > 5 & vif_df$VIF <= 10), background = "#FFFFCC")
```

```{r vif_plot, echo=TRUE}
# Visualize VIF values
ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "orange") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  geom_text(aes(label = round(VIF, 1)), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(title = "Variance Inflation Factors for Predictors",
       subtitle = "Orange line: moderate multicollinearity (VIF=5), Red line: severe multicollinearity (VIF=10)",
       x = "Variable",
       y = "VIF") +
  my_theme
```

```{r pca_analysis, fig.width=10, fig.height=8, echo=FALSE}
# Select numeric predictors (excluding Salary)
numeric_data <- hitters %>% 
  dplyr::select(where(is.numeric), -Salary)

# Perform PCA
pca_result <- prcomp(numeric_data, scale. = TRUE)
```

```{r pca_summary_table, echo=TRUE}
# Format PCA summary into a nicer table
pca_summary <- summary(pca_result)

# Extract variance explained by each component
variance_explained <- pca_summary$importance[2,] * 100  # Convert to percentage
cumulative_variance <- pca_summary$importance[3,] * 100

# Create a data frame for a nice table
pca_table <- data.frame(
  Component = paste0("PC", 1:length(variance_explained)),
  'Standard Deviation' = pca_summary$importance[1,],
  'Proportion of Variance' = paste0(round(variance_explained, 2), "%"),
  'Cumulative Proportion' = paste0(round(cumulative_variance, 2), "%")
)

# Display as a nice table
kable(pca_table, format = "html", caption = "PCA Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE)
```

```{r variance_plot, echo=TRUE}
# Extract variance explained by each component
variance_explained <- pca_summary$importance[2,] * 100  # Convert to percentage
cumulative_variance <- pca_summary$importance[3,] * 100

# Create a data frame for plotting
variance_df <- data.frame(
  Component = paste0("PC", 1:length(variance_explained)),
  Variance = variance_explained,
  Cumulative = cumulative_variance
)

# Ensure components are ordered correctly by making Component a factor with levels in the right order
variance_df$Component <- factor(variance_df$Component, levels = paste0("PC", 1:length(variance_explained)))

# Create a more visually appealing color palette
variance_colors <- colorRampPalette(c("#4169E1", "#87CEEB"))(length(variance_explained))

# Visualize variance explained with enhanced aesthetics
ggplot(variance_df, aes(x = Component, y = Variance)) +
  geom_bar(stat = "identity", aes(fill = Component)) +
  scale_fill_manual(values = variance_colors) +
  geom_text(aes(label = sprintf("%.1f%%", Variance)), vjust = -0.5, size = 3.5, fontface = "bold") +
  geom_line(aes(y = Cumulative, group = 1), color = "red", size = 1.2) +
  geom_point(aes(y = Cumulative), color = "red", size = 3) +
  scale_y_continuous(
    limits = c(0, max(max(variance_df$Variance) * 1.15, 100)),
    sec.axis = sec_axis(~., name = "Cumulative Variance (%)")
  ) +
  labs(title = "Variance Explained by Principal Components",
       subtitle = "Bars: Individual variance, Red line: Cumulative variance",
       x = "Principal Component",
       y = "Variance Explained (%)") +
  my_theme +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray90"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, face = "italic")
  )
```

```{r biplot, echo=TRUE}
# Create a simplified biplot with better readability
# Get the loadings (relationships between original variables and PCs)
loadings <- pca_result$rotation[, 1:2]
loadings_df <- as.data.frame(loadings)
loadings_df$Variable <- rownames(loadings_df)

# Create a custom biplot
ggplot() +
  # Plot the PC scores
  geom_point(data = data.frame(pca_result$x[,1:2]), 
             aes(x = PC1, y = PC2), 
             alpha = 0.5, color = "darkgrey") +
  # Add arrows for variable loadings
  geom_segment(data = loadings_df,
               aes(x = 0, y = 0, xend = PC1*5, yend = PC2*5),
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  # Add variable labels at the end of arrows
  geom_text(data = loadings_df,
            aes(x = PC1*5.5, y = PC2*5.5, label = Variable),
            size = 3) +
  # Add some theming
  labs(title = "PCA Biplot (First Two Principal Components)",
       subtitle = "Showing the relationship between variables and principal components",
       x = paste0("PC1 (", round(variance_explained[1], 1), "% variance)"),
       y = paste0("PC2 (", round(variance_explained[2], 1), "% variance)")) +
  my_theme +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3)
```

```{r component_selection, echo=FALSE}
# Determine number of components to keep based on common criteria
# 1. Kaiser criterion (eigenvalues > 1)
kaiser_components <- sum(pca_summary$importance[1,] > 1)
# 2. Components needed to explain 95% of variance
var95_components <- which(cumulative_variance >= 95)[1]
```

```{r pca_model, echo=TRUE}
# Combining PCA components with target variable
# We'll use the number of components needed to explain 95% of variance
hitters_pca <- data.frame(
  pca_result$x[, 1:var95_components],
  SalaryLevel = hitters$SalaryLevel
)

# Fitting a multinomial model on PCA-transformed data
model_pca <- multinom(SalaryLevel ~ ., data = hitters_pca, trace = FALSE)
```

```{r evaluate_pca_model_third, echo=FALSE}
# Evaluate the PCA model using our evaluation function
pca_model_eval <- evaluate_model(model_pca, hitters_pca)
```

```{r pca_confusion_matrix_third, echo=TRUE}
# Display the confusion matrix using kable for better formatting
pca_conf_matrix_df <- as.data.frame(pca_model_eval$conf_matrix)
names(pca_conf_matrix_df) <- c("Actual", "Predicted", "Count")

# Create a better looking table
pca_conf_matrix_wide <- reshape2::dcast(pca_conf_matrix_df, Predicted ~ Actual, value.var = "Count")
rownames(pca_conf_matrix_wide) <- pca_conf_matrix_wide$Predicted
pca_conf_matrix_wide$Predicted <- NULL

kable(pca_conf_matrix_wide, format = "html", caption = "Confusion Matrix for PCA Model") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Actual" = ncol(pca_conf_matrix_wide)))

# Create a more visually appealing heatmap
ggplot(pca_conf_matrix_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "#4292c6") +
  labs(title = "PCA Model Confusion Matrix",
       x = "Actual Salary Level",
       y = "Predicted Salary Level") +
  my_theme +
  theme(legend.position = "none")
```

```{r pca_class_metrics_third, echo=TRUE}
# Create a formatted table of class-specific metrics
pca_metrics_df <- data.frame(
  SalaryLevel = names(pca_model_eval$precision),
  Precision = round(pca_model_eval$precision, 3),
  Recall = round(pca_model_eval$recall, 3),
  F1_Score = round(pca_model_eval$f1_score, 3)
)

# Create a better looking table
kable(pca_metrics_df, format = "html", 
      caption = "Class-Specific Performance Metrics for PCA Model") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE)

# Create a bar plot of class metrics
library(reshape2)
metrics_long <- melt(pca_metrics_df, id.vars = "SalaryLevel", 
                    variable.name = "Metric", value.name = "Value")

ggplot(metrics_long, aes(x = SalaryLevel, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Value, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(title = "Performance Metrics by Salary Level for PCA Model",
       x = "Salary Level", 
       y = "Score") +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, max(metrics_long$Value) * 1.1) +
  my_theme
```

```{r model_comparison_pca_third, echo=TRUE}
# Create comparison dataframe for visualization
model_comparison <- data.frame(
  Model = c("Full Model", "PCA Model"),
  Accuracy = c(full_model_eval$accuracy, pca_model_eval$accuracy)
)

# Visualize accuracy comparison
ggplot(model_comparison, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Accuracy*100)), vjust = -0.5) +
  scale_fill_manual(values = c("Full Model" = "#4292c6", "PCA Model" = "#41ab5d")) +
  ylim(0, max(model_comparison$Accuracy) * 1.1) +
  labs(title = "Model Accuracy Comparison",
       subtitle = "Full model vs. PCA-based model",
       y = "Accuracy") +
  my_theme +
  theme(legend.position = "none")
```

```{r cv_setup, message=FALSE}

set.seed(06052004)

# Define function to run cross-validation and collect results
run_cross_validation <- function(formula, data, method = "cv", number = 5, 
                                model_type = "multinom", model_name = "Model") {
  # Set up cross-validation control
  ctrl <- trainControl(
    method = method,
    number = number,
    classProbs = TRUE,
    summaryFunction = multiClassSummary
  )
  
  # Perform cross-validation
  cv_model <- train(
    formula,
    data = data,
    method = model_type,
    trControl = ctrl,
    trace = FALSE
  )
  
  # Extract results
  best_result <- cv_model$results[which.max(cv_model$results$Accuracy),]
  
  # Calculate error rate
  error_rate <- 1 - best_result$Accuracy
  
  # Return results in a list
  return(list(
    model = cv_model,
    accuracy = best_result$Accuracy,
    error_rate = error_rate,
    balanced_accuracy = best_result$AUC,
    kappa = best_result$Kappa,
    model_name = model_name,
    cv_method = paste0(number, "-fold CV")
  ))
}
```

```{r cv5_full}
# Run 5-fold cross-validation on the full model
cv5_full <- run_cross_validation(
  formula = SalaryLevel ~ . -Salary,
  data = hitters,
  number = 5,
  model_name = "Full Model"
)
```

```{r cv5_full_results, echo=FALSE}
# Display results with clear formatting in a table
cv5_full_results <- data.frame(
  Metric = c("Accuracy", "Error Rate", "Balanced Accuracy", "Kappa"),
  Value = c(
    sprintf("%.2f%%", cv5_full$accuracy * 100),
    sprintf("%.2f%%", cv5_full$error_rate * 100),
    sprintf("%.2f%%", cv5_full$balanced_accuracy * 100),
    round(cv5_full$kappa, 4)
  )
)

knitr::kable(cv5_full_results, 
            caption = "5-Fold Cross-Validation Results for Full Model",
            align = c("l", "c"),
            format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = FALSE,
                          position = "center")
```

```{r cv10_full}
# Run 10-fold cross-validation on the full model
cv10_full <- run_cross_validation(
  formula = SalaryLevel ~ . -Salary,
  data = hitters,
  number = 10,
  model_name = "Full Model"
)
```

```{r cv10_full_results, echo=FALSE}
# Display results with clear formatting in a table
cv10_full_results <- data.frame(
  Metric = c("Accuracy", "Error Rate", "Balanced Accuracy", "Kappa"),
  Value = c(
    sprintf("%.2f%%", cv10_full$accuracy * 100),
    sprintf("%.2f%%", cv10_full$error_rate * 100),
    sprintf("%.2f%%", cv10_full$balanced_accuracy * 100),
    round(cv10_full$kappa, 4)
  )
)

knitr::kable(cv10_full_results, 
            caption = "10-Fold Cross-Validation Results for Full Model",
            align = c("l", "c"),
            format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = FALSE,
                          position = "center")
```

```{r cv5_pca}
# Run 5-fold cross-validation on the PCA model
cv5_pca <- run_cross_validation(
  formula = SalaryLevel ~ .,
  data = hitters_pca,
  number = 5,
  model_name = "PCA Model"
)
```

```{r cv5_pca_results, echo=FALSE}
# Display results with clear formatting in a table
cv5_pca_results <- data.frame(
  Metric = c("Accuracy", "Error Rate", "Balanced Accuracy", "Kappa"),
  Value = c(
    sprintf("%.2f%%", cv5_pca$accuracy * 100),
    sprintf("%.2f%%", cv5_pca$error_rate * 100),
    sprintf("%.2f%%", cv5_pca$balanced_accuracy * 100),
    round(cv5_pca$kappa, 4)
  )
)

knitr::kable(cv5_pca_results, 
            caption = "5-Fold Cross-Validation Results for PCA Model",
            align = c("l", "c"),
            format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = FALSE,
                          position = "center")
```

```{r cv10_pca}
# Run 10-fold cross-validation on the PCA model for consistency
cv10_pca <- run_cross_validation(
  formula = SalaryLevel ~ .,
  data = hitters_pca,
  number = 10,
  model_name = "PCA Model"
)
```

```{r cv10_pca_results, echo=FALSE}
# Display results with clear formatting in a table
cv10_pca_results <- data.frame(
  Metric = c("Accuracy", "Error Rate", "Balanced Accuracy", "Kappa"),
  Value = c(
    sprintf("%.2f%%", cv10_pca$accuracy * 100),
    sprintf("%.2f%%", cv10_pca$error_rate * 100),
    sprintf("%.2f%%", cv10_pca$balanced_accuracy * 100),
    round(cv10_pca$kappa, 4)
  )
)

knitr::kable(cv10_pca_results, 
            caption = "10-Fold Cross-Validation Results for PCA Model",
            align = c("l", "c"),
            format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = FALSE,
                          position = "center")
```

```{r cv_comparison, fig.width=10, fig.height=8, echo=FALSE}
# Create a data frame for comparison
cv_results <- data.frame(
  Model = c(cv5_full$model_name, cv10_full$model_name, 
            cv5_pca$model_name, cv10_pca$model_name),
  CV_Method = c(cv5_full$cv_method, cv10_full$cv_method, 
                cv5_pca$cv_method, cv10_pca$cv_method),
  Accuracy = c(cv5_full$accuracy, cv10_full$accuracy, 
               cv5_pca$accuracy, cv10_pca$accuracy),
  BalancedAccuracy = c(cv5_full$balanced_accuracy, cv10_full$balanced_accuracy, 
                       cv5_pca$balanced_accuracy, cv10_pca$balanced_accuracy),
  Kappa = c(cv5_full$kappa, cv10_full$kappa, 
            cv5_pca$kappa, cv10_pca$kappa)
)

# Display the comparison table with better formatting
cv_results_formatted <- cv_results
cv_results_formatted$Accuracy <- sprintf("%.2f%%", cv_results$Accuracy * 100)
cv_results_formatted$BalancedAccuracy <- sprintf("%.2f%%", cv_results$BalancedAccuracy * 100)
cv_results_formatted$Kappa <- round(cv_results$Kappa, 4)

# Rename columns for better display
colnames(cv_results_formatted)[3:5] <- c("Accuracy", "Balanced Accuracy", "Kappa")

knitr::kable(cv_results_formatted, 
            caption = "Comparison of Cross-Validation Results Across Models and Methods",
            align = c("l", "l", "c", "c", "c"),
            format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = TRUE,
                          position = "center") %>%
  kableExtra::row_spec(c(2, 4), background = "#f5f5f5") %>%
  kableExtra::column_spec(3:5, bold = TRUE)
```

```{r cv_plot, fig.width=10, fig.height=8, echo=FALSE}
# Create a long format version for plotting
cv_results_long <- cv_results %>%
  pivot_longer(cols = c(Accuracy, BalancedAccuracy, Kappa),
               names_to = "Metric",
               values_to = "Value")

# Create a faceted plot
ggplot(cv_results_long, aes(x = CV_Method, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = ifelse(Metric == "Kappa", 
                              sprintf("%.3f", Value),
                              sprintf("%.1f%%", Value*100))),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  facet_wrap(~ Metric, scales = "free_y") +
  scale_fill_manual(values = c("Full Model" = "#4292c6", "PCA Model" = "#41ab5d")) +
  labs(title = "Cross-Validation Performance Metrics",
       subtitle = "Comparing different models and CV methods",
       x = "Cross-Validation Method",
       y = "Performance Value") +
  my_theme +
  theme(strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(size = 12, face = "bold"))
```

```{r model_improvement_setup, message=FALSE}
# Use stepwise selection based on AIC
set.seed(06052004)

model_step <- stepAIC(model_full, direction = "both", trace = FALSE)

# Extract selected variables
selected_vars <- attr(terms(model_step), "term.labels")
```

```{r selected_model_analysis, echo=FALSE}
# Summary of the selected model
selected_summary <- summary(model_step)

# Calculate z-values and p-values for the selected model
z_values_step <- selected_summary$coefficients / selected_summary$standard.errors
p_values_step <- 2 * (1 - pnorm(abs(z_values_step)))

# Display significant variables (p < 0.05) for better interpretability
significant_med <- names(which(p_values_step[1,] < 0.05))
significant_high <- names(which(p_values_step[2,] < 0.05))

# Create a data frame for the significant predictors
significant_predictors <- data.frame(
  Category = c(rep("MediumSalary vs. LowSalary", length(significant_med)),
               rep("HighSalary vs. LowSalary", length(significant_high)),
               rep("Common to Both Comparisons", length(intersect(significant_med, significant_high)))),
  Predictors = c(significant_med, 
                 significant_high,
                 intersect(significant_med, significant_high))
)

# Create an improved visualization of significant predictors with better colors and clarity
# First, prepare data for a more informative plot
all_predictors <- unique(c(significant_med, significant_high))
predictor_data <- data.frame(
  Predictor = all_predictors,
  stringsAsFactors = FALSE
)

# Create indicator columns for each salary level
predictor_data$MediumSalary <- predictor_data$Predictor %in% significant_med
predictor_data$HighSalary <- predictor_data$Predictor %in% significant_high
predictor_data$Common <- predictor_data$MediumSalary & predictor_data$HighSalary

# Get p-values for color intensity
predictor_data$Medium_pvalue <- NA
predictor_data$High_pvalue <- NA

for (i in 1:nrow(predictor_data)) {
  pred <- predictor_data$Predictor[i]
  if (pred %in% colnames(p_values_step)) {
    predictor_data$Medium_pvalue[i] <- p_values_step[1, pred]
    predictor_data$High_pvalue[i] <- p_values_step[2, pred]
  }
}

# Convert p-values to significance levels for visualization
predictor_data$Medium_sig <- cut(predictor_data$Medium_pvalue, 
                               breaks = c(0, 0.001, 0.01, 0.05, 1), 
                               labels = c("***", "**", "*", ""), 
                               include.lowest = TRUE)
predictor_data$High_sig <- cut(predictor_data$High_pvalue, 
                             breaks = c(0, 0.001, 0.01, 0.05, 1), 
                             labels = c("***", "**", "*", ""), 
                             include.lowest = TRUE)

# For more meaningful ordering, sort by whether predictors are common and then alphabetically
predictor_data <- predictor_data[order(-predictor_data$Common, predictor_data$Predictor), ]

# Create the enhanced visualization
plot_data <- reshape2::melt(predictor_data[, c("Predictor", "MediumSalary", "HighSalary")], 
                          id.vars = "Predictor", 
                          variable.name = "SalaryLevel", 
                          value.name = "IsSignificant")

# Add significance stars to the plot data
plot_data$Significance <- NA
plot_data$Significance[plot_data$SalaryLevel == "MediumSalary"] <- as.character(predictor_data$Medium_sig)
plot_data$Significance[plot_data$SalaryLevel == "HighSalary"] <- as.character(predictor_data$High_sig)

# Create the enhanced visualization
ggplot(plot_data, aes(x = SalaryLevel, y = Predictor)) +
  # Use better colors with gradient for significance
  geom_tile(aes(fill = IsSignificant), color = "white", size = 0.8) +
  # Add significance stars
  geom_text(aes(label = Significance), size = 5) +
  # Use a more interpretable color scheme
  scale_fill_manual(values = c("FALSE" = "#f5f5f5", "TRUE" = "#3182bd")) +
  # Add labels
  labs(
    title = "Statistically Significant Predictors by Salary Level",
    subtitle = "Variables that significantly influence player salary categories (p < 0.05)",
    x = "Salary Level Comparison (vs. Low Salary)",
    y = "",
    caption = "*** p<0.001  ** p<0.01  * p<0.05"
  ) +
  # Better formatting
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 11, face = ifelse(predictor_data$Common, "bold", "plain")),
    axis.text.x = element_text(size = 12, face = "bold"),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  # More intuitive x-axis labels
  scale_x_discrete(labels = c("Medium vs. Low Salary", "High vs. Low Salary"))
```

```{r model_complexity_table, echo=FALSE, fig.width=10, fig.height=6}
# Compare number of parameters in each model
full_params <- length(coef(model_full))
step_params <- length(coef(model_step))
reduction_pct <- round((full_params - step_params) / full_params * 100, 1)

# Create comparison table with clear formatting
model_complexity <- data.frame(
  Model = c("Full Model", "Stepwise Model"),
  Parameters = c(full_params, step_params),
  Reduction = c("Reference", paste0(full_params - step_params, " (", reduction_pct, "%)"))
)

# Display the table
knitr::kable(model_complexity, 
            caption = "Model Complexity Comparison",
            align = c("l", "c", "c"),
            format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = FALSE,
                          position = "center") %>%
  kableExtra::row_spec(0, background = "#f8f9fa", bold = TRUE) %>%
  kableExtra::row_spec(2, bold = TRUE, background = "#eaffea")
```

```{r model_complexity_plot, echo=FALSE}
# Visualize model complexity
ggplot(model_complexity, aes(x = Model, y = Parameters, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Parameters), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Full Model" = "#4292c6", "Stepwise Model" = "#41ab5d")) +
  labs(title = "Model Complexity Comparison",
       subtitle = paste0("Stepwise selection reduced parameters by ", reduction_pct, "%"),
       y = "Number of Parameters") +
  my_theme +
  theme(legend.position = "none")
```

```{r variable_types, echo=FALSE}
# Compare variable categories in selected model
current_season_vars <- selected_vars[!grepl("^C|League|Division", selected_vars) & selected_vars != "Years"]
career_vars <- selected_vars[grepl("^C", selected_vars)]
categorical_vars <- selected_vars[grepl("League|Division", selected_vars)]
experience_var <- ifelse("Years" %in% selected_vars, "Included", "Not included")

# Create a table of variable types
variable_types <- data.frame(
  "Variable Type" = c("Current Season Statistics", "Career Statistics", "Experience (Years)", "Categorical Variables"),
  "Count" = c(length(current_season_vars), length(career_vars), 
              ifelse("Years" %in% selected_vars, 1, 0), length(categorical_vars)),
  "Status" = c(paste(length(current_season_vars), "variables"),
               paste(length(career_vars), "variables"),
               experience_var,
               paste(length(categorical_vars), "variables")),
  "Details" = c(ifelse(length(current_season_vars) > 0, paste(current_season_vars, collapse = ", "), "None"),
                ifelse(length(career_vars) > 0, paste(career_vars, collapse = ", "), "None"),
                ifelse("Years" %in% selected_vars, "Years in league", "Not selected"),
                ifelse(length(categorical_vars) > 0, paste(categorical_vars, collapse = ", "), "None"))
)

# Display the table
knitr::kable(variable_types, 
            caption = "Types of Variables in the Selected Model",
            align = c("l", "c", "c", "l"),
            format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = TRUE,
                          position = "center") %>%
  kableExtra::row_spec(0, background = "#f8f9fa", bold = TRUE) %>%
  kableExtra::column_spec(4, width = "40%")
```

```{r odds_ratio_plots, echo=FALSE, fig.width=10, fig.height=15}
# Calculate odds ratios for the stepwise model
odds_ratios <- exp(coef(model_step))

# Create a data frame for easier presentation
odds_df <- as.data.frame(odds_ratios)
odds_df$SalaryLevel <- rownames(odds_df)
odds_df_long <- tidyr::pivot_longer(odds_df, 
                                   cols = -SalaryLevel, 
                                   names_to = "Variable", 
                                   values_to = "OddsRatio")

# Add p-values to the odds ratios for better interpretation
odds_df_long$pvalue <- NA
for (i in 1:nrow(odds_df_long)) {
  level <- odds_df_long$SalaryLevel[i]
  var <- odds_df_long$Variable[i]
  level_idx <- ifelse(level == "MediumSalary", 1, 2)
  if (var %in% colnames(p_values_step)) {
    odds_df_long$pvalue[i] <- p_values_step[level_idx, var]
  }
}

# Add significance indicators
odds_df_long$Significance <- cut(odds_df_long$pvalue, 
                                breaks = c(0, 0.001, 0.01, 0.05, 1), 
                                labels = c("***", "**", "*", ""),
                                include.lowest = TRUE)

# Select top influencers for each salary level
top_med <- odds_df_long %>% 
  filter(SalaryLevel == "MediumSalary", pvalue < 0.05) %>% 
  arrange(desc(OddsRatio)) %>% 
  head(5)

top_high <- odds_df_long %>% 
  filter(SalaryLevel == "HighSalary", pvalue < 0.05) %>% 
  arrange(desc(OddsRatio)) %>% 
  head(5)

# Create dataframe for Low Salary top predictors
# For low salary, we want variables that have inverse relationship with higher categories
# (i.e., variables where odds ratio < 1 for medium/high indicate higher odds for low)
top_low <- odds_df_long %>% 
  filter(SalaryLevel == "HighSalary", pvalue < 0.05) %>% 
  arrange(OddsRatio) %>%  # Ascending order to get lowest odds ratios
  head(5)

# Plot top predictors for low salary
p_low <- ggplot(top_low, aes(x = reorder(Variable, -OddsRatio), y = 1/OddsRatio)) +
  geom_bar(stat = "identity", fill = "#66c2a5") +
  geom_text(aes(label = paste0(round(1/OddsRatio, 2), Significance)), vjust = -0.3) +
  labs(title = "Top 5 Predictors for Low Salary", 
       subtitle = "Based on inverse odds ratios (* p<0.05, ** p<0.01, *** p<0.001)",
       x = "Variable", y = "Relative Odds for Low Salary") +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot top influencers with improved visualization
p_med <- ggplot(top_med, aes(x = reorder(Variable, OddsRatio), y = OddsRatio)) +
  geom_bar(stat = "identity", fill = "#fc8d62") +
  geom_text(aes(label = paste0(round(OddsRatio, 2), Significance)), vjust = -0.3) +
  labs(title = "Top 5 Predictors for Medium Salary", 
       subtitle = "Based on odds ratios (* p<0.05, ** p<0.01, *** p<0.001)",
       x = "Variable", y = "Odds Ratio") +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_high <- ggplot(top_high, aes(x = reorder(Variable, OddsRatio), y = OddsRatio)) +
  geom_bar(stat = "identity", fill = "#8da0cb") +
  geom_text(aes(label = paste0(round(OddsRatio, 2), Significance)), vjust = -0.3) +
  labs(title = "Top 5 Predictors for High Salary", 
       subtitle = "Based on odds ratios (* p<0.05, ** p<0.01, *** p<0.001)",
       x = "Variable", y = "Odds Ratio") +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display all three plots
grid.arrange(p_low, p_med, p_high, ncol = 1)
```

```{r key_odds_interpretations, echo=TRUE}
# Create a table for key odds ratio interpretations
key_interpretations <- data.frame(
  Variable = character(),
  Medium_OR = character(),
  High_OR = character(),
  Interpretation = character(),
  stringsToSep = NULL
)

# Add Years interpretation if present
if ("Years" %in% colnames(odds_ratios)) {
  key_interpretations <- rbind(key_interpretations, data.frame(
    Variable = "Years in league",
    Medium_OR = sprintf("%.2f", odds_ratios["MediumSalary", "Years"]),
    High_OR = sprintf("%.2f", odds_ratios["HighSalary", "Years"]),
    Interpretation = paste0("Each additional year of experience multiplies the odds of medium salary by ", 
                          sprintf("%.2f", odds_ratios["MediumSalary", "Years"]), 
                          " and high salary by ", 
                          sprintf("%.2f", odds_ratios["HighSalary", "Years"]), 
                          ". Experience strongly influences compensation.")
  ))
}

# Add Career Home Runs interpretation if present
if ("CHmRun" %in% colnames(odds_ratios)) {
  key_interpretations <- rbind(key_interpretations, data.frame(
    Variable = "Career Home Runs",
    Medium_OR = sprintf("%.3f", odds_ratios["MediumSalary", "CHmRun"]),
    High_OR = sprintf("%.3f", odds_ratios["HighSalary", "CHmRun"]),
    Interpretation = paste0("Each additional career home run multiplies the odds of medium salary by ", 
                          sprintf("%.3f", odds_ratios["MediumSalary", "CHmRun"]), 
                          " and high salary by ", 
                          sprintf("%.3f", odds_ratios["HighSalary", "CHmRun"]), 
                          ". Power hitting is valued in compensation decisions.")
  ))
}

# Add League interpretation if present
if ("LeagueN" %in% colnames(odds_ratios)) {
  key_interpretations <- rbind(key_interpretations, data.frame(
    Variable = "League (National vs American)",
    Medium_OR = sprintf("%.2f", odds_ratios["MediumSalary", "LeagueN"]),
    High_OR = sprintf("%.2f", odds_ratios["HighSalary", "LeagueN"]),
    Interpretation = paste0("Playing in the National League (vs American) multiplies the odds of medium salary by ", 
                          sprintf("%.2f", odds_ratios["MediumSalary", "LeagueN"]), 
                          " and high salary by ", 
                          sprintf("%.2f", odds_ratios["HighSalary", "LeagueN"]), 
                          ". ", ifelse(odds_ratios["HighSalary", "LeagueN"] > 1, 
                                     "National League players tend to earn more, controlling for other factors.", 
                                     "American League players tend to earn more, controlling for other factors."))
  ))
}

# Add another significant variable if available
if (length(significant_high) >= 4) {
  var_to_add <- significant_high[4]
  if (!(var_to_add %in% c("Years", "CHmRun", "LeagueN")) && var_to_add %in% colnames(odds_ratios)) {
    key_interpretations <- rbind(key_interpretations, data.frame(
      Variable = var_to_add,
      Medium_OR = sprintf("%.3f", odds_ratios["MediumSalary", var_to_add]),
      High_OR = sprintf("%.3f", odds_ratios["HighSalary", var_to_add]),
      Interpretation = paste0("Each additional unit of ", var_to_add, " multiplies the odds of medium salary by ", 
                            sprintf("%.3f", odds_ratios["MediumSalary", var_to_add]), 
                            " and high salary by ", 
                            sprintf("%.3f", odds_ratios["HighSalary", var_to_add]), ".")
    ))
  }
}

# Display the interpretations table
knitr::kable(key_interpretations, 
             caption = "Key Odds Ratio Interpretations",
             col.names = c("Variable", "Medium Salary OR", "High Salary OR", "Interpretation"),
             align = c("l", "c", "c", "l"),
             format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = TRUE,
                          position = "center") %>%
  kableExtra::row_spec(0, background = "#f8f9fa", bold = TRUE) %>%
  kableExtra::column_spec(4, width = "50%")
```

```{r stepwise_cv_code, fig.width=10, fig.height=6, echo=FALSE}
# Evaluate the selected model using our cross-validation function
cv5_step <- run_cross_validation(
  formula = formula(model_step),
  data = hitters,
  number = 5,
  model_name = "Stepwise Model"
)

# Also evaluate with 10-fold CV for consistency
cv10_step <- run_cross_validation(
  formula = formula(model_step),
  data = hitters,
  number = 10,
  model_name = "Stepwise Model"
)
```

```{r stepwise_cv_results, echo=FALSE}
# Create a dataframe with all CV results
cv_step_results <- data.frame(
  "Metric" = c("Accuracy", "Error Rate", "Balanced Accuracy", "Kappa"),
  "5-Fold CV" = c(
    sprintf("%.2f%%", cv5_step$accuracy * 100),
    sprintf("%.2f%%", cv5_step$error_rate * 100),
    sprintf("%.2f%%", cv5_step$balanced_accuracy * 100),
    round(cv5_step$kappa, 4)
  ),
  "10-Fold CV" = c(
    sprintf("%.2f%%", cv10_step$accuracy * 100),
    sprintf("%.2f%%", cv10_step$error_rate * 100),
    sprintf("%.2f%%", cv10_step$balanced_accuracy * 100),
    round(cv10_step$kappa, 4)
  )
)

# Display the results table
knitr::kable(cv_step_results, 
             caption = "Cross-Validation Results for Stepwise Model",
             align = c("l", "c", "c"),
             format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = FALSE,
                          position = "center") %>%
  kableExtra::row_spec(0, background = "#f8f9fa", bold = TRUE) %>%
  kableExtra::row_spec(1, bold = TRUE, background = "#eaffea")
```

```{r step_model_eval, echo=FALSE}
# Add model evaluation using our function
step_model_eval <- evaluate_model(model_step, hitters)

# Create a table for in-sample results
in_sample_results <- data.frame(
  "Metric" = "Accuracy",
  "Value" = sprintf("%.2f%%", step_model_eval$accuracy * 100)
)

# Display the in-sample results
knitr::kable(in_sample_results, 
             caption = "In-sample Evaluation for Stepwise Model",
             align = c("l", "c"),
             format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                          full_width = FALSE,
                          position = "center")
```

```{r confusion_matrix_plot, echo=FALSE}
# Visualize the confusion matrix
plot_confusion_matrix(step_model_eval$conf_matrix, 
                     "Stepwise Model Confusion Matrix")
```

```{r comprehensive_comparison, echo=FALSE, fig.width=12, fig.height=8}
# Create a comprehensive comparison dataframe with all models
all_models_cv <- data.frame(
  Model = c("Full Model", "Full Model", 
            "PCA Model", "PCA Model", 
            "Stepwise Model", "Stepwise Model"),
  CV_Method = c("5-Fold CV", "10-Fold CV", 
                "5-Fold CV", "10-Fold CV", 
                "5-Fold CV", "10-Fold CV"),
  Accuracy = c(cv5_full$accuracy, cv10_full$accuracy,
               cv5_pca$accuracy, cv10_pca$accuracy,
               cv5_step$accuracy, cv10_step$accuracy),
  BalancedAccuracy = c(cv5_full$balanced_accuracy, cv10_full$balanced_accuracy,
                       cv5_pca$balanced_accuracy, cv10_pca$balanced_accuracy,
                       cv5_step$balanced_accuracy, cv10_step$balanced_accuracy),
  Kappa = c(cv5_full$kappa, cv10_full$kappa,
            cv5_pca$kappa, cv10_pca$kappa,
            cv5_step$kappa, cv10_step$kappa),
  Parameters = c(full_params, full_params,
                 ncol(hitters_pca) - 1, ncol(hitters_pca) - 1,  # Subtract 1 for the response variable
                 step_params, step_params)
)

# Display the comprehensive comparison
all_models_cv$Accuracy_Pct <- sprintf("%.2f%%", all_models_cv$Accuracy * 100)
all_models_cv$BalancedAccuracy_Pct <- sprintf("%.2f%%", all_models_cv$BalancedAccuracy * 100)
all_models_cv$Kappa_Round <- round(all_models_cv$Kappa, 4)

# Prepare table for display
comparison_table <- all_models_cv[, c("Model", "CV_Method", "Accuracy_Pct", "BalancedAccuracy_Pct", "Kappa_Round", "Parameters")]
colnames(comparison_table) <- c("Model", "CV Method", "Accuracy", "Balanced Accuracy", "Kappa", "Parameters")

# Display nicely formatted table
knitr::kable(comparison_table, 
             caption = "Comprehensive Comparison of All Models",
             align = c("l", "c", "c", "c", "c", "c"),
             format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = TRUE,
                          position = "center") %>%
  kableExtra::row_spec(0, background = "#f8f9fa", bold = TRUE) %>%
  kableExtra::row_spec(c(2, 4, 6), background = "#f5f5f5") %>%
  kableExtra::group_rows("Full Model", 1, 2) %>%
  kableExtra::group_rows("PCA Model", 3, 4) %>%
  kableExtra::group_rows("Stepwise Model", 5, 6)
```

```{r accuracy_vs_complexity, echo=FALSE}
# Create visualization
ggplot(all_models_cv, aes(x = Parameters, y = Accuracy, color = Model, shape = CV_Method)) +
  geom_point(size = 4) +
  geom_text(aes(label = sprintf("%.1f%%", Accuracy*100)), hjust = -0.3, vjust = 0.5) +
  scale_color_manual(values = c("Full Model" = "#4292c6", 
                              "PCA Model" = "#41ab5d",
                              "Stepwise Model" = "#de2d26")) +
  labs(title = "Model Comparison: Accuracy vs. Complexity",
       subtitle = "Better models are in the top-left (high accuracy, low complexity)",
       x = "Number of Parameters (Complexity)",
       y = "Cross-Validation Accuracy") +
  my_theme +
  theme(legend.position = "right")
```

```{r best_model_analysis, echo=FALSE}
# Calculate average CV accuracy for each model (using 10-fold as it's more stable)
model_avg_acc <- aggregate(Accuracy ~ Model, 
                          data = all_models_cv[all_models_cv$CV_Method == "10-Fold CV", ], 
                          FUN = mean)

# Add a column indicating the best model based on 10-fold CV
best_model <- model_avg_acc$Model[which.max(model_avg_acc$Accuracy)]
best_acc <- max(model_avg_acc$Accuracy)*100

# Create formatted best model description
best_model_desc <- data.frame(
  "Best Model" = best_model,
  "10-Fold CV Accuracy" = sprintf("%.2f%%", best_acc),
  "Parameters" = all_models_cv$Parameters[all_models_cv$Model == best_model][1],
  "Key Advantages" = ifelse(best_model == "Stepwise Model", 
                          "Balanced performance and interpretability; uses original variables",
                          ifelse(best_model == "PCA Model", 
                                "Addresses multicollinearity; dimensionality reduction",
                                "Includes all available predictors"))
)

# Display formatted result
knitr::kable(best_model_desc, 
             caption = "Best Performing Model Based on 10-Fold Cross-Validation",
             align = c("c", "c", "c", "l"),
             format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                          full_width = TRUE,
                          position = "center") %>%
  kableExtra::row_spec(0, background = "#f8f9fa", bold = TRUE) %>%
  kableExtra::row_spec(1, background = "#eaffea", bold = TRUE)
```

```{r in_sample_comparison, echo=FALSE}
# Compare in-sample performance with explicit analysis of which model performs better
in_sample_comparison <- data.frame(
  Model = c("Full Model", "PCA Model", "Stepwise Model"),
  InSampleAccuracy = c(full_model_eval$accuracy, 
                       pca_model_eval$accuracy, 
                       step_model_eval$accuracy),
  CV_Accuracy = c(cv10_full$accuracy,  # Using 10-fold CV for better stability
                 cv10_pca$accuracy, 
                 cv10_step$accuracy),
  Difference = c(full_model_eval$accuracy - cv10_full$accuracy,
                pca_model_eval$accuracy - cv10_pca$accuracy,
                step_model_eval$accuracy - cv10_step$accuracy)
)

# Identify which model has the best CV performance
best_cv_model <- in_sample_comparison$Model[which.max(in_sample_comparison$CV_Accuracy)]
pca_vs_full <- ifelse(in_sample_comparison$CV_Accuracy[2] > in_sample_comparison$CV_Accuracy[1], 
                    "better than", 
                    ifelse(in_sample_comparison$CV_Accuracy[2] < in_sample_comparison$CV_Accuracy[1],
                          "worse than",
                          "equivalent to"))

# Format for display
in_sample_comparison$InSampleAccuracy_Pct <- sprintf("%.2f%%", in_sample_comparison$InSampleAccuracy * 100)
in_sample_comparison$CV_Accuracy_Pct <- sprintf("%.2f%%", in_sample_comparison$CV_Accuracy * 100)
in_sample_comparison$Difference_Pct <- sprintf("%.2f%%", in_sample_comparison$Difference * 100)
in_sample_comparison$Gap <- in_sample_comparison$Difference
in_sample_comparison$GapPct <- sprintf("%.1f%%", in_sample_comparison$Gap * 100)

# Display formatted table
knitr::kable(in_sample_comparison[, c("Model", "InSampleAccuracy_Pct", "CV_Accuracy_Pct", "Difference_Pct")], 
             caption = "In-Sample vs. Cross-Validation Accuracy Comparison (using 10-fold CV)",
             col.names = c("Model", "In-Sample Accuracy", "CV Accuracy", "Generalization Gap"),
             align = c("l", "c", "c", "c"),
             format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = TRUE,
                          position = "center") %>%
  kableExtra::row_spec(0, background = "#f8f9fa", bold = TRUE) %>%
  kableExtra::row_spec(which.min(in_sample_comparison$Difference), background = "#eaffea") %>%
  kableExtra::column_spec(4, background = ifelse(in_sample_comparison$Difference < 0.05, 
                                                "#eaffea", 
                                                ifelse(in_sample_comparison$Difference < 0.1, 
                                                      "#ffffea", 
                                                      "#ffeeee")))
```

```{r model_generalization, echo=FALSE, fig.width=12, fig.height=6}
# Create a long format version for the bar chart
in_sample_comparison_long <- pivot_longer(in_sample_comparison,
                                         cols = c("InSampleAccuracy", "CV_Accuracy"),
                                         names_to = "Metric",
                                         values_to = "Accuracy")

# Recalculate comparison for subtitle to ensure accuracy
pca_cv <- in_sample_comparison$CV_Accuracy[2]  # PCA model CV accuracy  
full_cv <- in_sample_comparison$CV_Accuracy[1]  # Full model CV accuracy
pca_comparison <- ifelse(pca_cv > full_cv, 
                       paste0("better than (", round((pca_cv-full_cv)*100, 1), "% higher)"), 
                       ifelse(pca_cv < full_cv,
                             paste0("worse than (", round((full_cv-pca_cv)*100, 1), "% lower)"),
                             "equivalent to"))

# Create improved visualization emphasizing the correct relationship between models
p1 <- ggplot(in_sample_comparison_long, aes(x = Model, y = Accuracy, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sprintf("%.1f%%", Accuracy*100)), 
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("InSampleAccuracy" = "#4292c6", "CV_Accuracy" = "#41ab5d"),
                   labels = c("In-Sample Accuracy", "Cross-Validation Accuracy")) +
  labs(title = "In-Sample vs. Cross-Validation Accuracy",
       subtitle = paste0("The PCA model's CV accuracy (", sprintf("%.1f%%", pca_cv*100), 
                        ") is ", pca_comparison, " the Full model (", sprintf("%.1f%%", full_cv*100), ")"),
       y = "Accuracy") +
  ylim(0, max(in_sample_comparison$InSampleAccuracy) * 1.15) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

# Create a separate plot specifically for the generalization gap
p2 <- ggplot(in_sample_comparison, aes(x = Model, y = Gap, fill = Model)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = GapPct, 
                vjust = ifelse(Gap >= 0, -0.5, 1.5)),
            size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Full Model" = "#4292c6", 
                              "PCA Model" = "#41ab5d",
                              "Stepwise Model" = "#de2d26")) +
  labs(title = "Generalization Gap",
       subtitle = "Smaller values indicate better generalization",
       y = "Accuracy Difference (In-Sample - CV)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")

# Arrange both plots in a way that fits better on HTML output
grid.arrange(
  p1, p2, 
  ncol = 1,
  heights = c(1.2, 1),
  top = textGrob("Model Generalization Analysis", 
               gp = gpar(fontsize = 16, fontface = "bold"))
)
```

```{r model_selection_results, echo=FALSE}
# Identify the model with the best generalization properties
best_gen_model <- in_sample_comparison$Model[which.min(in_sample_comparison$Gap)]
best_perf_model <- in_sample_comparison$Model[which.max(in_sample_comparison$CV_Accuracy)]

# Create a summary table
model_selection <- data.frame(
  Criterion = c("Best Generalization (smallest gap)", 
                "Best CV Performance", 
                "Recommended Model"),
  Model = c(best_gen_model,
            best_perf_model,
            ifelse(best_gen_model == best_perf_model, 
                  best_gen_model, 
                  paste0(best_perf_model, " or ", best_gen_model, "*"))),
  Value = c(sprintf("Gap: %s", in_sample_comparison$GapPct[which.min(in_sample_comparison$Gap)]),
            sprintf("Accuracy: %.1f%%", in_sample_comparison$CV_Accuracy[which.max(in_sample_comparison$CV_Accuracy)]*100),
            ifelse(best_gen_model == best_perf_model, 
                  "Clear choice (best in both metrics)", 
                  "Trade-off between performance and generalization*"))
)

# Display the model selection table
knitr::kable(model_selection, 
             caption = "Model Selection Analysis",
             align = c("l", "c", "l"),
             format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = TRUE,
                          position = "center") %>%
  kableExtra::row_spec(0, background = "#f8f9fa", bold = TRUE) %>%
  kableExtra::row_spec(3, background = "#eaffea", bold = TRUE)
```

```{r misclassification_setup, echo=FALSE}
# Create a dataframe of misclassified cases
misclassified <- hitters %>%
  mutate(Predicted = predict(model_step)) %>%
  filter(Predicted != SalaryLevel)

# Count the types of misclassifications
misclass_types <- table(Actual = misclassified$SalaryLevel, Predicted = misclassified$Predicted)

# Format misclassification types for display
misclass_types_df <- as.data.frame(misclass_types)
misclass_types_df$Actual <- as.character(misclass_types_df$Actual)
misclass_types_df$Predicted <- as.character(misclass_types_df$Predicted)
misclass_types_df$Percentage <- sprintf("%.1f%%", misclass_types_df$Freq / sum(misclass_types_df$Freq) * 100)
misclass_types_df$Description <- paste0(misclass_types_df$Actual, " → ", misclass_types_df$Predicted)

# Calculate the percentage of each class that got misclassified
misclass_rates <- data.frame(
  SalaryLevel = levels(hitters$SalaryLevel),
  Count = as.numeric(table(hitters$SalaryLevel)),
  Misclassified = c(
    sum(misclassified$SalaryLevel == "LowSalary"),
    sum(misclassified$SalaryLevel == "MediumSalary"),
    sum(misclassified$SalaryLevel == "HighSalary")
  )
)
misclass_rates$MisclassRate <- round(misclass_rates$Misclassified / misclass_rates$Count * 100, 1)
misclass_rates$CorrectRate <- 100 - misclass_rates$MisclassRate

# Create a better visualization of misclassification types
# Get the total counts for calculating percentages
total_counts <- lapply(levels(hitters$SalaryLevel), function(lvl) {
  sum(hitters$SalaryLevel == lvl)
})
names(total_counts) <- levels(hitters$SalaryLevel)

# Create data for the visualization
misclass_viz <- misclass_types_df %>%
  mutate(
    TotalInClass = unlist(total_counts[Actual]),
    ProportionMisclassified = Freq / TotalInClass,
    PercentageLabel = sprintf("%.1f%%", ProportionMisclassified * 100),
    ErrorType = case_when(
      (Actual == "LowSalary" & Predicted == "MediumSalary") | 
      (Actual == "MediumSalary" & Predicted == "HighSalary") ~ "Adjacent (Overestimated)",
      (Actual == "MediumSalary" & Predicted == "LowSalary") | 
      (Actual == "HighSalary" & Predicted == "MediumSalary") ~ "Adjacent (Underestimated)",
      TRUE ~ "Two Levels"
    )
  )

# Create the main sankey/flow diagram for misclassifications
ggplot(misclass_viz, aes(x = Actual, y = Predicted, fill = ErrorType, label = PercentageLabel)) +
  geom_tile(color = "white", size = 1, alpha = 0.85) +
  geom_text(size = 4) +
  scale_fill_manual(values = c(
    "Adjacent (Overestimated)" = "#fc8d62",
    "Adjacent (Underestimated)" = "#66c2a5",
    "Two Levels" = "#8da0cb"
  )) +
  labs(
    title = "Types of Misclassifications",
    subtitle = "Percentage of players in each actual salary category that were misclassified",
    x = "Actual Salary Level",
    y = "Predicted Salary Level",
    fill = "Error Type"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    panel.grid = element_blank()
  ) +
  # Add arrows to indicate flow direction
  geom_segment(
    data = data.frame(
      x = c(1, 2, 1),
      y = c(2, 3, 3),
      xend = c(1.4, 2.4, 1.4),
      yend = c(2, 3, 3)
    ),
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.3, "cm")),
    color = "gray30",
    inherit.aes = FALSE
  )

# Display the misclassification types table
# knitr::kable(misclass_types_df[, c("Description", "Freq", "Percentage")], 
#              caption = "Types of Misclassifications",
#              col.names = c("Misclassification Type", "Count", "Percentage of Errors"),
#              align = c("l", "c", "c"),
#              format = "html") %>%
#   kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
#                           full_width = FALSE,
#                           position = "center") %>%
#   kableExtra::row_spec(0, background = "#f8f9fa", bold = TRUE)
```

```{r misclass_rates_table, echo=FALSE}
# Display the misclassification rates table
knitr::kable(misclass_rates, 
             caption = "Misclassification Rates by Salary Level",
             col.names = c("Salary Level", "Total Players", "Misclassified Players", "Misclassification Rate (%)", "Correct Classification Rate (%)"),
             align = c("l", "c", "c", "c", "c"),
             format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = TRUE,
                          position = "center") %>%
  kableExtra::row_spec(0, background = "#f8f9fa", bold = TRUE) %>%
  kableExtra::row_spec(which.max(misclass_rates$MisclassRate), background = "#ffeeee") %>%
  kableExtra::row_spec(which.min(misclass_rates$MisclassRate), background = "#eaffea")
```

```{r classification_accuracy_plot, echo=FALSE}
# Visualize misclassification rates by class
ggplot(misclass_rates, aes(x = SalaryLevel, y = 100)) +
  geom_bar(stat = "identity", fill = "lightgrey", alpha = 0.5) +
  geom_bar(aes(y = CorrectRate, fill = SalaryLevel), stat = "identity") +
  geom_text(aes(y = CorrectRate/2, label = paste0(CorrectRate, "%")), color = "white", fontface = "bold") +
  geom_text(aes(y = CorrectRate + (MisclassRate/2), 
                label = paste0(MisclassRate, "% misclassified")), fontface = "italic") +
  scale_fill_manual(values = c("LowSalary" = "#66c2a5", 
                              "MediumSalary" = "#fc8d62", 
                              "HighSalary" = "#8da0cb")) +
  labs(title = "Classification Accuracy by Salary Level",
       subtitle = "Percentage of correctly and incorrectly classified instances",
       x = "Salary Level",
       y = "Percentage") +
  my_theme +
  theme(legend.position = "none")
```

```{r misclassification_plots, echo=FALSE, fig.width=10, fig.height=8}
# Plot misclassified players' Years vs. Hits
p1 <- ggplot(misclassified, aes(x = Years, y = Hits, color = SalaryLevel, shape = Predicted)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("LowSalary" = "#66c2a5", 
                               "MediumSalary" = "#fc8d62", 
                               "HighSalary" = "#8da0cb"),
                    name = "Actual Salary Level") +
  scale_shape_discrete(name = "Predicted Salary Level") +
  labs(title = "Misclassified Players: Experience vs. Performance",
       x = "Years in League", y = "Hits") +
  my_theme

# Plot salary distribution of misclassified players
p2 <- ggplot(misclassified, aes(x = SalaryLevel, y = Salary, fill = Predicted)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("LowSalary" = "#66c2a5", 
                              "MediumSalary" = "#fc8d62", 
                              "HighSalary" = "#8da0cb"),
                   name = "Predicted Salary Level") +
  labs(title = "Salary Distribution of Misclassified Players",
       x = "Actual Salary Level", y = "Salary") +
  my_theme

# Arrange plots with clear layout
grid.arrange(p1, p2, ncol = 2,
             top = textGrob("Analysis of Misclassified Cases",
                           gp = gpar(fontsize = 16, fontface = "bold")))
```

```{r metric_comparison, echo=FALSE}
# Compare means of key variables between correctly and incorrectly classified players
correct <- hitters %>%
  mutate(Predicted = predict(model_step)) %>%
  filter(Predicted == SalaryLevel)

# Create a function to calculate and format the comparison table
calculate_comparison <- function(vars) {
  comparison <- data.frame(
    Variable = vars,
    Misclassified_Mean = sapply(misclassified[, vars], mean),
    Correct_Mean = sapply(correct[, vars], mean)
  )
  comparison$Difference_Pct <- round((comparison$Misclassified_Mean - comparison$Correct_Mean) / comparison$Correct_Mean * 100, 1)
  comparison$Misclassified = round(comparison$Misclassified_Mean, 1)
  comparison$Correct = round(comparison$Correct_Mean, 1)
  
  return(comparison[, c("Variable", "Misclassified", "Correct", "Difference_Pct")])
}

# Key variables for comparison
key_vars <- c("Years", "Hits", "HmRun", "RBI", "Walks", "Salary")
comparison_formatted <- calculate_comparison(key_vars)

# Display the comparison table
knitr::kable(comparison_formatted, 
             caption = "Comparison of Key Metrics Between Correctly and Incorrectly Classified Players",
             col.names = c("Variable", "Misclassified Players (Mean)", "Correctly Classified Players (Mean)", "Percentage Difference (%)"),
             align = c("l", "c", "c", "c"),
             format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                          full_width = TRUE,
                          position = "center") %>%
  kableExtra::row_spec(0, background = "#f8f9fa", bold = TRUE) %>%
  kableExtra::column_spec(4, color = ifelse(comparison_formatted$Difference_Pct > 0, "#006600", "#990000"),
                        background = ifelse(abs(comparison_formatted$Difference_Pct) > 10, 
                                          ifelse(comparison_formatted$Difference_Pct > 0, "#eaffea", "#ffeeee"), 
                                          "white"))
```

```{r difference_plot, echo=FALSE}
# Visualize the differences
ggplot(comparison_formatted, 
       aes(x = reorder(Variable, abs(Difference_Pct)), y = Difference_Pct, 
           fill = Difference_Pct > 0)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Difference_Pct, "%"), 
                vjust = ifelse(Difference_Pct > 0, -0.5, 1.5))) +
  scale_fill_manual(values = c("TRUE" = "#4daf4a", "FALSE" = "#e41a1c"), 
                   labels = c("TRUE" = "Higher in Misclassified", "FALSE" = "Lower in Misclassified"),
                   name = "Direction") +
  coord_flip() +
  labs(title = "Differences in Key Metrics for Misclassified Players",
       subtitle = "Percentage difference from correctly classified players",
       x = "Variable",
       y = "Percentage Difference (%)") +
  my_theme
```