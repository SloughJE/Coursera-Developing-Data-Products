
library(shiny)

shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("Subject's Data"),
    sidebarPanel(
      numericInput('times_pregnant', 'Number of times pregnant', 0, min = 0, max = 17, step = 1),
      numericInput('plasma_glucose', 'Plasma glucose concentration (2 hours in an oral glucose tolerance test)', 120, min = 50, max = 200, step = 5),
      numericInput('diastolic_BP', 'Diastolic blood pressure (mm Hg)', 70, min = 20, max = 120, step = 1),
      numericInput('tri_skin_fold', 'Triceps skin fold thickness (mm)', 30, min = 5, max = 65, step = 1),
      numericInput('insulin', '2-Hour serum insulin (mu U/ml)', 125, min = 10, max = 400, step = 5),
      numericInput('BMI', 'Body mass index (kg/(height in m)^2)', 30, min = 15, max = 65, step = 0.5),
      numericInput('age', 'Age (years)', 30, min = 15, max = 85, step = 1),
      numericInput('d_ped', 'Diabetes Pedigree Function (DPF)*', 0.5, min = 0.05, max = 2.5, step = 0.05),
      submitButton('Submit'),br(),
#       h3("Varible vs. Diabetes Diagnosis"),
#     
#       helpText("Plot a single variable's relationship to diabetes diagnosis. Hit submit again after you change selection."),
#       
#       selectInput("var_plot", 
#                   label = "Choose a variable to display",
#                   choices = c("Plasma glucose"="1", "Body Mass Index"="2" ,"Diabetes pedigree"="3", "Pregnancies"="4",
#                               "Age"="5","Serum insulin"="6","Triceps skin-fold"="7","Blood pressure"="8"),
#                   selected = "4"),
#       br(),
      p("* The DPF provides a measure of the expected genetic influence of affected and unaffected relatives on the subject's eventual diabetes risk. The value of the DPF increases, as the age at which those relatives developed DM decreases, and as a percentages of genes that they share with the subject increases. The value of the DPF decreases as the number of relatives who never developed DM increases, as their ages at their last examination increase, and as the percentage of genes that they share with the subject increases. Range from about 0.05 to 2.5")
      
    ),
    
    mainPanel(
      tabsetPanel
      (
      tabPanel("Prediction",h1("Prediction of Diabetes Mellitus"),
               p("By entering some biometric data of a subject, you are provided with a predicted probability of a positive diabetes mellitus (DM) diagnosis. Please note that this algorithm was developed from the Pima Indian dataset from the UCI Machine Learning Repository (https://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes) and may not be generalizable to the general population. It is based on a generalized linear model (logistic regression) and achieved an accuracy of approximately 80% on the testing dataset. The prediction does not in any way take the place of seeing a doctor and is for educational/informational purposes only."),
               br(),
               h3("Subject's predicted probability of a positive Diabetes diagnosis "),
               verbatimTextOutput("prediction")),
      tabPanel("Importance Plot",h4("This plot shows the importance of each variable in the prediction algorithm"),
               
               plotOutput('VarImpPlot')),
               
      
       tabPanel("A Comment on the Dataset",br(),
      p("This dataset comes from the UCI machine repository. The original research paper from Johns Hopkins that used this dataset is available here:", a("Using the ADAP Learning Algorithm to Forecast the Onset of Diabetes Mellitus.", href="http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2245318/",target="_blank"),
       " It is interesting to note that the paper states that '768 examinations were selected. From those 576 were selected randomly to be used in the training or learning set and the remaining 192 cases came the forecasting set.' If the paper used the exact same dataset that UCI has then the authors made a simple but critical mistake. There are many values in the dataset which are biologically impossible. There are individuals in the dateset who have plasma glucose of 0, a BMI of 0, and/or and age of 0. Obviously none of these are possible and they probably were missing values. However, the original paper, and at least one other published many years later (http://www.svms.org/training/CaCr.pdf) make the same mistake in including these observations in their prediction algorithms, thus assuming the dataset was complete. I could not find any indication that the authors were aware that there were cases in the dataset which should have been treated as missing. For my prediction algorithm, I removed the cases which were biologically impossible. This resulted in a final dataset of 392 cases, rather than the original 768 cases. Not a small difference.",
br(), "Upon further investigation, I found an article pointing to this dataset as an example of ", a("disguised missing values.", href="http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.443.6794&rep=rep1&type=pdf",target="_blank"),
     " Basically the authors of that paper came to the same conclusion as I did and stated that better results were obtained by omitting the disguised missing values 'even though this complete case analysis reduced the effective sample size from 768 to 392 patients.'",  
      
      br(),br(),
      "Well, the moral is, explore the data and understand what it means before making analyses! A simple histogram would have displayed the problems.")

    
    )))
  ))
