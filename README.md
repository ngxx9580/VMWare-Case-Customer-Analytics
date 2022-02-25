# VMWare-Case-Customer-Analytics

## Model Development Portion of the Case:
1. Perform some EDA (exploratory data analysis) and data manipulation/preparation tasks. You should start with understanding your target variable and its relationship to
various features (hint: you may have “leakage” - i.e. inputs/features that are co-linear with your output/labels. This will accidentally lead to a “perfect model” if not removed)
2. Use the data provided to train a multinomial Random Forest model with a few different values for the various hyper-parameters (# of trees, # of variables sampled, tree depth, etc.). Comment on the model development process, potential shortcomings, and accuracy.
3. Now train a classifier using a binary target variable to help alleviate the class imbalance by treating all sessions that ended with any of the five customer
engagement events as 1 and all other sessions as 0. Similar to above, try tuning a few different hyper-parameters (as a bonus, you can try grid-searching to find optimal
parameters). Comment on the model development process, potential shortcomings, and accuracy.
4. Compare the two models and discuss the pros/cons of turning the multivariate problem into a simpler binary classification problem (both from a modeling perspective as well as business implications)

## Data
As the dataset is too large to upload to Github, the dataset used can be found [here](http://hrm.iimb.ernet.in/iimb/download/IMB_623.htm)

## Languages & Modelling Techniques Used:
- R
- Multinomial Random Forest
- Binary Target Variable Classifier
- Hyper Paramter Tuning
