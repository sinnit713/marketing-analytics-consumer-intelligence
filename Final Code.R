library(dplyr)
library(readxl)
# Data manipulation
library(tidyverse)
library(lubridate)

# Clustering and visualization
library(cluster)
library(factoextra)
library(dendextend)

# Classification
library(MASS)

# Date handling
library(scales)

# STEP 1 — LOAD DATA
# Current customer dataset
current <- read_excel("Current Customers..xlsx")

# Prospect customer dataset
prospects <- read_excel("Prospect Customers.xlsx")


#========================================================
# STEP 2 — INITIAL DATA INSPECTION
#========================================================

# Dataset structure
str(current)

# Summary statistics
summary(current)

# Dimensions
dim(current)

# First few rows
head(current)


#========================================================
# STEP 3 — MISSING VALUES
#========================================================

# Missing values by variable
colSums(is.na(current))


#========================================================
# STEP 4 — DUPLICATE CHECKS
#========================================================

# Duplicate observations
sum(duplicated(current))

# Remove duplicates if necessary
current <- current %>% distinct()


#========================================================
# STEP 5 — DATE CONVERSION
#========================================================

# Convert invoice date
current$InvoiceDate <- as.POSIXct(current$InvoiceDate,
                                  format = "%Y-%m-%dT%H:%M")


#========================================================
# STEP 6 — IDENTIFY CANCELLED TRANSACTIONS
#========================================================

# Create cancellation indicator
current <- current %>%
  mutate(Cancelled = ifelse(substr(InvoiceNo, 1, 1) == "C", 1, 0))

# Frequency of cancellations
table(current$Cancelled)


#========================================================
# STEP 7 — CREATE SALES VARIABLE
#========================================================

current <- current %>%
  mutate(Sales = Quantity * UnitPrice)


#========================================================
# STEP 8 — REMOVE MISSING CUSTOMER IDs
#========================================================

current <- current %>%
  filter(!is.na(CustomerID))


#========================================================
# STEP 9 — VALID TRANSACTIONS
#========================================================

# Keep completed transactions only
valid_transactions <- current %>%
  filter(Cancelled == 0,
         Quantity > 0,
         UnitPrice > 0)


#========================================================
# STEP 10 — BASIC EXPLORATORY ANALYSIS
#========================================================

# Sales distribution
ggplot(valid_transactions,
       aes(x = Sales)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Sales")


# Unit price distribution
ggplot(valid_transactions,
       aes(x = UnitPrice)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of Unit Prices")

#========================================================
# STEP 11 — CREATE RFM VARIABLES
#========================================================
# Convert to Date format
valid_transactions$InvoiceDate <-
  as.Date(valid_transactions$InvoiceDate)

# Snapshot date
snapshot_date <- max(valid_transactions$InvoiceDate) + 1

# Recency
recency_df <- valid_transactions %>%
  group_by(CustomerID) %>%
  summarise(
    Recency =
      as.numeric(snapshot_date - max(InvoiceDate))
  )


#----------------------------
# Frequency
#----------------------------

frequency_df <- valid_transactions %>%
  group_by(CustomerID) %>%
  summarise(Frequency =
              n_distinct(InvoiceNo))

#Frequency Distribution
ggplot(frequency_df,
       aes(x = Frequency)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Purchase Frequency")

#----------------------------
# Monetary
#----------------------------

monetary_df <- valid_transactions %>%
  group_by(CustomerID) %>%
  summarise(Monetary =
              sum(Sales, na.rm = TRUE))

#Monetary Distribution
ggplot(monetary_df,
       aes(x = Monetary)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Monetary Value")


#========================================================
# STEP 12 — ADDITIONAL BEHAVIOURAL VARIABLES
#========================================================

# Crossbuy behaviour
crossbuy_df <- valid_transactions %>%
  group_by(CustomerID) %>%
  summarise(Crossbuy =
              n_distinct(StockCode))


# Return behaviour
return_df <- current %>%
  group_by(CustomerID) %>%
  summarise(Return_Rate =
              mean(Cancelled, na.rm = TRUE))


#========================================================
# STEP 13 — CREATE CUSTOMER-LEVEL DATASET
#========================================================

customer_features <- recency_df %>%
  left_join(frequency_df,
            by = "CustomerID") %>%
  left_join(monetary_df,
            by = "CustomerID") %>%
  left_join(crossbuy_df,
            by = "CustomerID") %>%
  left_join(return_df,
            by = "CustomerID")

#========================================================
# STEP 14 — MERGE DEMOGRAPHIC VARIABLES
#========================================================

demo_data <- current %>%
  group_by(CustomerID) %>%
  summarise(
    married = first(married),
    household_size = first(household_size),
    income = first(income),
    age = first(age),
    Zip_Code = first(Zip_Code),
    Work = first(Work),
    Education = first(Education)
  )

customer_data <- customer_features %>%
  left_join(demo_data,
            by = "CustomerID")

customer_data %>%
  dplyr::select(Recency,
                Frequency,
                Monetary,
                Crossbuy) %>%
  summary()

#========================================================
# STEP 15 — FINAL DATA INSPECTION
#========================================================

str(customer_data)

summary(customer_data)

head(customer_data)


write.csv(customer_data,
          "customer_level_dataset.csv",
          row.names = FALSE)

#========================================================

#STEP 16 — CREATE RFM TABLE

#========================================================

rfm_data <- customer_data %>%
  dplyr::select(CustomerID,
                Recency,
                Frequency,
                Monetary)

head(rfm_data)

#STEP 2B — INDEPENDENT RFM SCORING
rfm_scores <- rfm_data %>%
  mutate(
    
    R_score = ntile(-Recency, 5),
    
    F_score = ntile(Frequency, 5),
    
    M_score = ntile(Monetary, 5)
    
  )

#STEP 2C — CREATE COMBINED RFM SCORE
rfm_scores <- rfm_scores %>%
  mutate(
    
    RFM_Score = paste0(R_score,
                       F_score,
                       M_score)
    
  )

head(rfm_scores)

#STEP 2D — CREATE RFM SEGMENTS
rfm_scores <- rfm_scores %>%
  mutate(
    
    Segment =
      case_when(
        
        R_score >= 4 &
          F_score >= 4 &
          M_score >= 4
        ~ "Champions",
        
        
        R_score >= 3 &
          F_score >= 3 &
          M_score >= 3
        ~ "Loyal Customers",
        
        
        R_score >= 4 &
          F_score <= 2
        ~ "Potential Loyalists",
        
        
        R_score <= 2 &
          F_score >= 3
        ~ "At Risk",
        
        
        TRUE
        ~ "Others"
        
      )
    
  )

rfm_scores %>%
  group_by(Segment) %>%
  summarise(
    
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary)
    
  )

#STEP 2E — SEGMENT DISTRIBUTION
table(rfm_scores$Segment)

#Segment Distribution
ggplot(rfm_scores,
       aes(x = Segment,
           fill = Segment)) +
  
  geom_bar() +
  
  labs(title = "Customer Segments Based on RFM Analysis",
       x = "Segment",
       y = "Number of Customers") +
  
  theme_minimal()

#Monetary Value by Segment
ggplot(rfm_scores,
       aes(x = Segment,
           y = Monetary,
           fill = Segment)) +
  
  geom_boxplot() +
  
  labs(title = "Monetary Value Across RFM Segments",
       x = "Segment",
       y = "Monetary Value") +
  
  theme_minimal()

#SEQUENTIAL RFM SORTING
rfm_seq <- rfm_data %>%
  
  mutate(
    
    R_rank = ntile(-Recency, 5),
    
    F_rank = ntile(Frequency, 5),
    
    M_rank = ntile(Monetary, 5)
    
  ) %>%
  
  arrange(desc(R_rank),
          desc(F_rank),
          desc(M_rank))

#CREATE SEQUENTIAL SCORE
rfm_seq <- rfm_seq %>%
  
  mutate(
    
    Sequential_RFM =
      paste0(R_rank,
             F_rank,
             M_rank)
    
  )
write.csv(rfm_scores,
          "rfm_scores.csv",
          row.names = FALSE)


#========================================================
# STEP 3A — CREATE CLUSTER DATASET
#========================================================

cluster_data <- customer_data %>%
  
  dplyr::select(Recency,
                Frequency,
                Monetary,
                Crossbuy)


#========================================================
# REMOVE EXTREME OUTLIERS USING IQR
#========================================================

Q1 <- quantile(cluster_data$Monetary, 0.25)

Q3 <- quantile(cluster_data$Monetary, 0.75)

IQR_value <- Q3 - Q1

upper_limit <- Q3 + 1.5 * IQR_value


# Clean clustering dataset

cluster_data_clean <- cluster_data %>%
  
  filter(Monetary <= upper_limit)


# Keep matching customer records

customer_data_clean <- customer_data %>%
  
  filter(Monetary <= upper_limit)


#========================================================
# STEP 3B — STANDARDIZE VARIABLES
#========================================================

cluster_scaled <- scale(cluster_data_clean)


#========================================================
# STEP 3C — HIERARCHICAL CLUSTERING
#========================================================

# Distance matrix

distance_matrix <- dist(cluster_scaled,
                        method = "euclidean")


# Hierarchical clustering

hc_complete <- hclust(distance_matrix,
                      method = "complete")


# Dendrogram

plot(hc_complete,
     labels = FALSE,
     hang = -1,
     main = "Hierarchical Clustering Dendrogram")


#========================================================
# STEP 3D — ELBOW METHOD
#========================================================

fviz_nbclust(cluster_scaled,
             kmeans,
             method = "wss") +
  
  labs(title = "Elbow Method for Optimal Clusters")


#========================================================
# STEP 3E — FINAL K-MEANS CLUSTERING
#========================================================

set.seed(123)

kmeans_4 <- kmeans(cluster_scaled,
                   centers = 4,
                   nstart = 25)


#========================================================
# STEP 3F — CLUSTER ASSIGNMENT
#========================================================

customer_data_clean$Cluster <-
  as.factor(kmeans_4$cluster)


#========================================================
# STEP 3G — CLUSTER SIZE
#========================================================

table(customer_data_clean$Cluster)


#========================================================
# STEP 3H — CLUSTER CENTROIDS
#========================================================

aggregate(cluster_data_clean,
          by = list(Cluster =
                      customer_data_clean$Cluster),
          mean)


#========================================================
# STEP 3I — CLUSTER VISUALIZATION
#========================================================

fviz_cluster(kmeans_4,
             data = cluster_scaled,
             geom = "point")


#========================================================
# STEP 3J — CLUSTER SUMMARY
#========================================================

cluster_summary <- customer_data_clean %>%
  
  group_by(Cluster) %>%
  
  summarise(
    
    Avg_Recency = mean(Recency),
    
    Avg_Frequency = mean(Frequency),
    
    Avg_Monetary = mean(Monetary),
    
    Avg_Crossbuy = mean(Crossbuy)
    
  )

cluster_summary

#STEP 4A — DEMOGRAPHIC PROFILING TABLE
profile_summary <- customer_data_clean %>%
  
  group_by(Cluster) %>%
  
  summarise(
    
    Avg_Age = mean(age),
    
    Avg_Income = mean(income),
    
    Avg_Household = mean(household_size),
    
    Married_Rate = mean(married),
    
    Avg_Recency = mean(Recency),
    
    Avg_Frequency = mean(Frequency),
    
    Avg_Monetary = mean(Monetary),
    
    Avg_Crossbuy = mean(Crossbuy)
    
  )

profile_summary

#4B Segment wise visualization
#Monetary Value Across Customer Clusters
ggplot(customer_data_clean,
       aes(x = Cluster,
           y = Monetary,
           fill = Cluster)) +
  
  geom_boxplot() +
  
  labs(title = "Monetary Value Across Customer Clusters",
       x = "Cluster",
       y = "Monetary Value") +
  
  theme_minimal()

#Purchase Frequency Across Customer Clusters
ggplot(customer_data_clean,
       aes(x = Cluster,
           y = Frequency,
           fill = Cluster)) +
  
  geom_boxplot() +
  
  labs(title = "Purchase Frequency Across Customer Clusters",
       x = "Cluster",
       y = "Frequency") +
  
  theme_minimal()

#STEP 5A — ANOVA TESTING
summary(aov(Recency ~ Cluster,
            data = customer_data_clean))

summary(aov(Frequency ~ Cluster,
            data = customer_data_clean))

summary(aov(Monetary ~ Cluster,
            data = customer_data_clean))

summary(aov(Crossbuy ~ Cluster,
            data = customer_data_clean))

#STEP 5B — PREPARE LDA DATASET
lda_data <- customer_data_clean %>%
  
  dplyr::select(Cluster,
                age,
                income,
                household_size,
                married,
                Work,
                Education)

#STEP 5C — TRAIN/TEST SPLIT
set.seed(123)

train_index <- sample(
  1:nrow(lda_data),
  0.7 * nrow(lda_data)
)

train_data <- lda_data[train_index, ]

test_data <- lda_data[-train_index, ]

#STEP 5D — TRAIN LDA MODEL
lda_model <- lda(Cluster ~ age +
                   income +
                   household_size +
                   married +
                   Work +
                   Education,
                 
                 data = train_data)

#STEP 5E — PREDICT TEST DATA
lda_predictions <- predict(lda_model,
                           test_data)

#STEP 5F — CONFUSION MATRIX
table(
  Actual = test_data$Cluster,
  Predicted = lda_predictions$class
)

mean(lda_predictions$class ==
       test_data$Cluster)

#STEP 5G — PREPARE PROSPECT DATA
prospect_data <- prospects %>%
  
  dplyr::select(age,
                income,
                household_size,
                married,
                Work,
                Education)

#STEP 5H — PREDICT PROSPECT SEGMENTS
prospect_predictions <- predict(lda_model,
                                prospect_data)

#STEP 5I — ASSIGN TARGET SEGMENTS
prospects$Predicted_Cluster <-
  prospect_predictions$class

#STEP 5J — TARGETING SUMMARY
table(prospects$Predicted_Cluster)



#______________________________________________________________________________
#Part B
#______________________________________________________________________________

#load datasets
profiles <- read_excel("Product Profiles-A2..xlsx")
preferences <- read_excel("Conjoint Preferences-A2..xlsx")

#inspect data
head(profiles)
head(preferences)

#convert variables to factors:
profiles$Price <- as.factor(profiles$Price)
profiles$Brand <- as.factor(profiles$Brand)
profiles$Horsepower <- as.factor(profiles$Horsepower)
profiles$Color <- as.factor(profiles$Color)
profiles$Sunroof <- as.factor(profiles$Sunroof)

#Create numeric price variable
profiles$PriceNumeric <-
  as.numeric(as.character(profiles$Price))

#CREATE EMPTY STORAGE
partworth_results <- list()

#ESTIMATE UTILITIES FOR EACH RESPONDENT
for(i in 2:16){
  
  conjoint_data <- profiles
  
  conjoint_data$Rating <- preferences[[i]]
  
  
  model <- lm(Rating ~ PriceNumeric +
                Brand +
                Horsepower +
                Color +
                Sunroof,
              
              data = conjoint_data)
  
  
  partworth_results[[paste0("Respondent_", i - 1)]] <-
    coef(model)
  
}

partworth_results

#CREATE PART-WORTH TABLE
partworth_table <-
  do.call(rbind,
          partworth_results)

partworth_table

#CALCULATE AVERAGE UTILITIES
average_partworths <-
  colMeans(partworth_table)

average_partworths

#WTP CALCULATION
price_coef <-
  average_partworths["PriceNumeric"]

wtp_values <-
  -average_partworths / price_coef

wtp_values

#VISUALIZE PART-WORTHS
partworth_df <- data.frame(
  
  Attribute = names(average_partworths),
  
  Utility = average_partworths
  
)


ggplot(partworth_df,
       aes(x = reorder(Attribute,
                       Utility),
           y = Utility,
           fill = Utility)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip() +
  
  labs(title = "Average Part-Worth Utilities",
       x = "Attribute Levels",
       y = "Utility") +
  
  theme_minimal()

#VISUALIZE WTP
wtp_df <- data.frame(
  
  Attribute = names(wtp_values),
  
  WTP = wtp_values
  
)


ggplot(wtp_df[-1, ],
       aes(x = reorder(Attribute,
                       WTP),
           y = WTP,
           fill = WTP)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip() +
  
  labs(title = "Willingness to Pay by Attribute Level",
       x = "Attribute Levels",
       y = "WTP") +
  
  theme_minimal()

#STEP 3A — CREATE ATTRIBUTE RANGES
attribute_ranges <- c(
  
  Price =
    max(average_partworths["PriceNumeric"]) -
    min(average_partworths["PriceNumeric"]),
  
  
  Brand =
    max(c(0,
          average_partworths[
            grep("Brand",
                 names(average_partworths))
          ])) -
    
    min(c(0,
          average_partworths[
            grep("Brand",
                 names(average_partworths))
          ])),
  
  
  Horsepower =
    max(c(0,
          average_partworths[
            grep("Horsepower",
                 names(average_partworths))
          ])) -
    
    min(c(0,
          average_partworths[
            grep("Horsepower",
                 names(average_partworths))
          ])),
  
  
  Color =
    max(c(0,
          average_partworths[
            grep("Color",
                 names(average_partworths))
          ])) -
    
    min(c(0,
          average_partworths[
            grep("Color",
                 names(average_partworths))
          ])),
  
  
  Sunroof =
    max(c(0,
          average_partworths[
            grep("Sunroof",
                 names(average_partworths))
          ])) -
    
    min(c(0,
          average_partworths[
            grep("Sunroof",
                 names(average_partworths))
          ]))
  
)

#STEP 3B — CALCULATE IMPORTANCE
importance_scores <-
  
  (attribute_ranges /
     
     sum(attribute_ranges)) * 100

importance_scores #view result

#STEP 3D — CREATE IMPORTANCE TABLE
importance_df <- data.frame(
  
  Attribute = names(importance_scores),
  
  Importance = importance_scores
  
)

importance_df

#STEP 3E — VISUALIZE ATTRIBUTE IMPORTANCE
ggplot(importance_df,
       aes(x = reorder(Attribute,
                       Importance),
           y = Importance,
           fill = Importance)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip() +
  
  labs(title = "Attribute Importance in Consumer Preferences",
       x = "Attributes",
       y = "Importance (%)") +
  
  theme_minimal()

#STEP 4A — CREATE COMPETITOR PROFILES
market_profiles <- data.frame(
  
  PriceNumeric = c(27000,
                   27000,
                   29000,
                   23000),
  
  Brand = c("Volkswagen",
            "Toyota",
            "Saturn",
            "Kia"),
  
  Horsepower = c(220,
                 250,
                 280,
                 220),
  
  Color = c("Blue",
            "Red",
            "Green",
            "Blue"),
  
  Sunroof = c("No",
              "No",
              "No",
              "No")
  
)

#STEP 4B — CONVERT TO FACTORS
market_profiles$Brand <-
  factor(market_profiles$Brand,
         levels = levels(profiles$Brand))

market_profiles$Horsepower <-
  factor(market_profiles$Horsepower,
         levels = levels(profiles$Horsepower))

market_profiles$Color <-
  factor(market_profiles$Color,
         levels = levels(profiles$Color))

market_profiles$Sunroof <-
  factor(market_profiles$Sunroof,
         levels = levels(profiles$Sunroof))

#STEP 4C — CREATE UTILITY STORAGE
utility_matrix <- matrix(0, nrow = 15, ncol = 4)

#STEP 4D — PREDICT UTILITIES
for(i in 2:16){
  
  conjoint_data <- profiles
  
  conjoint_data$Rating <- preferences[[i]]
  
  
  model <- lm(Rating ~ PriceNumeric +
                Brand +
                Horsepower +
                Color +
                Sunroof,
              
              data = conjoint_data)
  
  
  predicted_utilities <-
    predict(model,
            newdata = market_profiles)
  
  
  utility_matrix[i - 1, ] <-
    predicted_utilities
  
}

#STEP 4E — VIEW UTILITIES
utility_matrix

#STEP 4F — FIRST CHOICE SIMULATION
choices <-
  apply(utility_matrix, 1, which.max)

#STEP 4G — MARKET SHARE ESTIMATION
#STEP 4G — MARKET SHARE ESTIMATION

market_share <- c(
  
  Volkswagen_27000 =
    sum(choices == 1) / length(choices) * 100,
  
  Toyota_27000 =
    sum(choices == 2) / length(choices) * 100,
  
  Saturn_29000 =
    sum(choices == 3) / length(choices) * 100,
  
  Kia_23000 =
    sum(choices == 4) / length(choices) * 100
  
)

market_share

#STEP 4I — VISUALIZE MARKET SHARE
market_share_df <- data.frame(
  
  Product = names(market_share),
  
  Share = as.numeric(market_share)
  
)


ggplot(market_share_df,
       aes(x = reorder(Product,
                       Share),
           y = Share,
           fill = Share)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip() +
  
  labs(title = "Predicted Market Share Simulation",
       x = "Product Profiles",
       y = "Market Share (%)") +
  
  theme_minimal()

#----------------------------------------------------------------
#PCA

cars <- read_excel("Cars PCA-A2.xlsx")

head(cars)

str(cars)

car_features <- cars[, -1] #REMOVE MODEL COLUMN
car_scaled <- scale(car_features) #STANDARDIZE VARIABLES
#RUN PCA
pca_model <- prcomp(car_scaled,
                    
                    center = TRUE,
                    
                    scale. = TRUE)
summary(pca_model)

singular_values <- pca_model$sdev^2 #SINGULAR VALUES

singular_values

#LOADING FACTORS
loading_factors <-
  
  pca_model$rotation

loading_factors

#PROPORTION OF VARIANCE EXPLAINED (PVE)
pve <-
  
  (pca_model$sdev^2) /
  
  sum(pca_model$sdev^2)

pve

#CREATE PVE TABLE
pve_table <- data.frame(
  
  Principal_Component =
    paste0("PC", 1:length(pve)),
  
  PVE = pve,
  
  Cumulative_PVE = cumsum(pve)
  
)

pve_table

#SCREE PLOT
plot(pca_model,
     type = "l",
     main = "Scree Plot")

#Biplot
biplot(pca_model,
       
       scale = 0)

#PERCEPTUAL MAP
pca_scores <-
  
  as.data.frame(pca_model$x)

pca_scores$Model <- cars$Model

#VISUALIZATION
ggplot(pca_scores,
       
       aes(x = PC1,
           y = PC2,
           label = Model)) +
  
  geom_point(size = 3) +
  
  geom_text(vjust = -0.5,
            size = 3) +
  
  labs(title = "Perceptual Map of Cars",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  
  theme_minimal()
