# Animal-Shelter-Outcomes
##### Help Improve Outcomes for Shelter Animals
##### Competition Link : https://www.kaggle.com/c/shelter-animal-outcomes/overview

![shelter](https://user-images.githubusercontent.com/20414758/57697444-dbf21180-7670-11e9-9d5a-543e69fa27f2.jpg)

### Business Problem/Objective

Every year, approximately 7.6 million companion animals end up in US shelters. Many animals are given up as unwanted by their owners, while others are picked up after getting lost or taken out of cruelty situations. Many of these animals find forever families to take them home, but just as many are not so lucky. 2.7 million dogs and cats are euthanized in the US every year.
Using a dataset of intake information including breed, color, sex, and age from the Austin Animal Center, we're asking you to predict the outcome for each animal.
We also believe this dataset can help us understand trends in animal outcomes. These insights could help shelters focus their energy on specific animals who need a little extra help finding a new home.
The data comes from Austin Animal Center from October 1st, 2013 to March, 2016. Outcomes represent the status of animals as they leave the Animal Center. All animals receive a unique Animal ID during intake. In this competition, you are going to predict the outcome of the animal as they leave the Animal Center. 
These outcomes include: 
Adoption, Died, Euthanasia, Return to owner, and Transfer. 

### Evaluation Metric

Submissions are evaluated using the multi-class logarithmic loss. Each incident has been labeled with one true class. For each animal, you must submit a set of predicted probabilities (one for every class). The formula is then,

![shelteroutcomes_evalmetric](https://user-images.githubusercontent.com/20414758/57697389-b82ecb80-7670-11e9-90ad-82308db66e3a.PNG)

where N is the number of animals in the test set, M is the number of outcomes, log is the natural logarithm, yij is 1 if observation i is in outcome j and 0 otherwise, and pij is the predicted probability that observation i belongs to outcome j.

The submitted probabilities for a given animal are not required to sum to one because they are rescaled prior to being scored (each row is divided by the row sum). In order to avoid the extremes of the log function, predicted probabilities are replaced with max(min(p,1−10−15),10−15).

### Final Score
The final model scored 0.73615 on leaderboard with the standing of 242/1604 participants.

### Comments
The approach and data exploration steps along with visualizations are presented in the pdf as a case study.

## Things I could have done:
- Extraction and exploration of more features
- Implementation of bagging, boosting, stacking might have improved the score
