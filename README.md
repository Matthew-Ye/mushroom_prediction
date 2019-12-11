# README

## Goal
The project aims to predict whether the mushrooms are poisonous based on their features. Following the CRISP-DM methodology, the project have six parts: 
1. Business understanding
2. Data understanding and analysis
3. Data preparation
4. Modeling
5. Evaluation
6. Deployment

## structure
.
├── README.md
├── RandomForest.rda
├── mush.R
├── mushdeploy.R
├── mushrooms.csv
├── mushroomsfinal.pdf
└── test_data.csv


- mush.R is the code of data analysis and modeling.
- mushdeploy.R is the code of shiny program.
- mushrooms.csv is the input dataset.
- RandomForest.rda is the Rdata including random forest training data. Because it takes long time, we save it in order to use for the next time quickly.
- test_data.csv is the input test data by users when they are using our shiny application, the format is the same with mushrooms.csv.
- mushroomsfinal.pdf is our final report. Please check the details in this document.



