# Data Science Salaries Dashboard

## About

This project aims to build a dashboard to visualize data science salaries across the world. The link to the dashboard is [here](https://datasci-salaries-r.herokuapp.com/).

Further details can be found in our proposal [here](https://github.com/UBC-MDS/datasci_salaries-R/blob/main/doc/proposal.md).

## Run the app locally

To run this app locally, clone this repository, then open the `app.R` file in RStudio. Next, click `Source` and by default, the app will run at the URL 0.0.0.0:8050.

## Problem

The knowledge gap over salaries in the field of data science has been one of the challenges for graduates with different education levels around the world. Understanding the salary levels can help keep appropriate expectations for people who are considering getting into the field or seeking related jobs. To achieve this purpose, we plan to build an application which provides reliable information and visualizations of data science salary levels around the world for those who need it.

## Description

The dashboard shows a world map with the median data science salary of a country encoded in the colour channel. By selecting a country to focus on using a dropdown menu, a histogram showing the distribution of salaries, a boxplot showing the distribution of salaries per gender, and a heatmap showing the distribution of salaries by age are updated for the specified country. A salary range slider is also provided so that users can zoom in on regions of the plot that they are interested in. Furthermore, a scatter plot of the salaries for each country is shown on the right.

![app](https://github.com/UBC-MDS/datasci_salaries-R/blob/main/media/r-dashboard.png)

## Data

To populate our dashboard, the [Kaggle Data Scientists Salaries Around the World](https://www.kaggle.com/ikleiman/data-scientists-salaries-around-the-world) dataset by Iair Kleiman is used. Specifically, the files `conversionRates.csv` and `multipleChoiceResponses.csv` are used in this project. For details about data wrangling, please see [this repository](https://github.com/UBC-MDS/datasci_salaries_py).

## Contributing

Contributors: Artan Zandian, Joshua Sia, Siqi Tao, Wenjia Zhu (DSCI_532_GROUP19).

Interested in contributing? Check out the contributing guidelines.

Please note that this project is released with a Code of Conduct. By contributing to this project, you agree to abide by its terms.

## License

This dashboard was created by Artan Zandian, Joshua Sia, Siqi Tao, Wenjia Zhu (DSCI_532_GROUP19). It is licensed under the terms of the MIT license.