# Proposal

### Motivation and Purpose

- Our role: Data scientist research team
- Target audience: Newly graduated data science students

The knowledge gap over salaries in the field of data science has been one of the challenges for graduates with different education levels around the world. Understanding the salary levels can help keep appropriate expectations for people who are considering getting into the field or seeking related jobs. To achieve this purpose, we plan to build an application which provides reliable information and visualizations of data science salary levels around the world for those who need it.


### Description of the Data

To populate our dashboard, we will be using the [Kaggle Data Scientists Salaries Around the World](https://www.kaggle.com/ikleiman/data-scientists-salaries-around-the-world) dataset by [Iair Kleiman](https://www.kaggle.com/ikleiman). Specifically, the files `conversionRates.csv` and `multipleChoiceResponses.csv` are used in this project. The data is processed using `src/data_cleaning.R` to mainly remove rows that do not have salary data, and to convert all salaries to USD from the respective countries' currencies. After data wrangling, the tidy dataset contains information about data scientist salaries in USD in 2018 for 43 different countries with the following features of interest:

- **Age** - The age of data scientist (numerical).

- **Country** - The country where the data scientist works (categorical with 43 levels).

- **Gender** - The gender of the data scientists (categorical with 4 levels: (1) Male, (2) Female, (3) Non-binary, genderqueer, or gender non-conforming, and (4) A different identity).

- **FormalEducation** - The level of formal education attained (ordinal with 7 levels: (1) Doctoral degree, (2) Master's degree, (3) Bachelor's degree, (4) Professional degree, (5) Some college/university study without earning a bachelor's degree, (6) I did not complete any formal education past high school, and (7) I prefer not to answer).

- **RemoteWork** - How often the data scientist works remotely (ordinal with 6 levels: (1) Always, (2) Most of the time, (3) Sometimes, (4) Rarely, (5) Never, (6) Don't know)

To see the other features in the dataset, please see `data/schema.csv`.

### Research Questions and Usage Scenarios

Our app will provide answers to two research questions:

- How is the level of formal education (e.g. bachelor's, master's degree etc.) associated with a data scientist's salary?
- Data Science professionals in which countries are paid the highest?

#### Usage Scenario

Heroku is a UBC Master of Data Science student who will be graduating in July and will be starting his job hunt next month. He is overwhelmed by the options that he has in front of him and wants to [compare] salaries in his home country to Canada to decide whether he should rather be focusing his energy on finding job in Canada or The States. He is mostly concerned with the range of salary that he should be expecting in a certain country and wants to [explore] salary expectations for new graduates. His priority is to work remotely, but he is afraid he might need to be asking for less salary if he is working remotely. He needs to know how remote Data Science jobs [compare] to in-person jobs. Using the Data Science Salaries dashboard, he will be able to [explore] salary ranges per age and gender. If not happy with the salary ranges, he has the option to [explore] if doing a PHD can increase his salary.
