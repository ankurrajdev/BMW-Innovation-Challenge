# BMW-Innovation-Challenge
BMW Innovation Challenge

Problem statement : To increase the liklihood of the customer to make a lead on BMW Website
Strategy : To identify the important features and work on that to increase the liklihood
Data Preperation : The data was about 1Mn per day with 550 features. The data was very raw and had a lot of missing values and was unbalanced.
Steps Involved : Missing Value Imputation
                 Changing the categorical variables to dummy variables
                 Removing the unwanted variables on the basis of Business Logic
                 Making the data balanced using SMOTE technique
 
Model : Since the aim of the project was to identify important features, we decided to use Random Forest which will provide us with feature importance list. We could have used GBM but didn't due to the time constraint

Result: We achieved a model with an accuracy of 93% and F - Score of around 0.65

PS: We cannot share data because of BMW agreement
