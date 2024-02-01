# Random Forest Regression on Discrimination of Home Loan Applications
The home loan machine learning interactive web application is built using R (Shiny). The supervised regression model predicts loan approvals based on user input with an algorithmic discrimination analysis to check for bias against races, genders, and ethnicities in the loan process. The website features categorical selections and numeric slider labels to allow site visitors to test the model and seek a personalized experience to the home loan application prediction. The website is built on automated data-feed processing, derived by the training data and machine learning model. This facilitates new data sets for the same model and web framework. 

Visit the interactive web application here: [https://juliensimons.shinyapps.io/home-loan/](https://juliensimons.shinyapps.io/home-loan/).

Race/Gender Discrimination Analysis
===
It is crucial to consider ethical implications, especially when vectors for races, genders, and ethnicities are included. Algorithmic discrimination is the unfair treatment of people by automated systems based on their protected characteristics. The following dataset can be used to analyze if there is any evidence of discrimination in the loan approval process by comparing the acceptance rates and the loan terms across different groups of applicants. By analyzing the weight of the feature vectors in the Random Forest model, the dataset shows notable evidence of discrimination. To be denied a home loan because of one's race and ethnicity is a serious reason why data scientists must carefully consider feature vectors and audit the model's performance to protect against unjust bias and discriminatory behavior.

### Future Experiments
Feature vectors considering the protected characteristics of an applicant such as their race, gender, and ethnicity should be manually trained to be negligible local factors, otherwise removed from the model. A new study should train local factors on discriminatory features within a broad data set. New feature vectors should also be considered to randomly test and identify a discriminatory relationship between existing vectors.

### v1.0.0 Alpha Release
The alpha release for the home-loan machine learning dataset is a preliminary version of that contains information about the applicants, the loans, and the outcomes of the loan applications. The dataset is used to train and test a Random Forest machine learning model that can predict if an applicant will get the home loan accepted or not, based on various features such as income, loan amount, property type, and more.
