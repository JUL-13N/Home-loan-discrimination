# Random Forest Regression on Discrimination of Home Loan Applications
The [home loan machine learning interactive web application](https://juliensimons.shinyapps.io/home-loan/) is built using R (Shiny). The supervised regression model predicts loan approvals based on user input with an algorithmic discrimination analysis to check for bias against races, genders, and ethnicities in the loan process. The website features categorical selections and numeric slider labels to allow site visitors to test the model and seek a personalized experience to the home loan application prediction.

Race/Gender Discrimination Analysis
===
Algorithmic discrimination is the unfair treatment of people by automated systems based on their protected characteristics, such as race or gender. The following dataset can be used to analyze if there is any evidence of race or gender discrimination in the loan approval process by comparing the acceptance rates and the loan terms across different groups of applicants. When removing these features from the initial dataset before the model is trained, the prediction results should see no change if discrimination is not a factor for home loan approval/denial. In this dataset, there is zero discrimination.

### v1.0.0 Alpha Release
The alpha release for the home-loan machine learning dataset is a preliminary version of that contains information about the applicants, the loans, and the outcomes of the loan applications. The dataset is used to train and test a Random Forest machine learning model that can predict if an applicant will get the home loan accepted or not, based on various features such as income, loan amount, property type, etc.
