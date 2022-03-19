# Reflection

## What has been implemented

The python dashboard features 7 plots which describe the distribution of data science salaries for the features country, gender, education level, age, tenure, and a scatter plot of all the data science salaries. The dashboard is split into two parts: the left side shows country-specific salary distributions while the right side (marked by a darker background) shows the global salary trends. The global salary trends are colored based on the data scientist's tenure, and interactivity is provided to filter based on tenure in the python dashboard. However, this feature was removed in the R dashboard due to incompatibility of the two plots in plotly. Both functions are fully functional and have been implemented in Heroku.

The user is able to select a specific country by using a dropdown menu (top left) and country-specific plots will be updated to reflect salaries corresponding to the chosen country. A slider is also provided to shift the focus of the country-specific plots depending on what salary range the user is interested in exploring. For instance, the user may be very new to data science, and wants to visualize the lower end of the salary distribution to see what proportion of data scientist's have a bachelor's degree.

## Improvements over previous versions

The main theme in the feedbacks recieved from project reviews was about finetuning the details and the design of the dashboard. The relevant ones incorporated in the current iteration for both python and R dashboards are:  

- Sorted highest paying countries based on median to provide an overall sense of higher paying countries.  
- Replaced `education_level` with `remote_work` in the histogram.
- Made salary range vertical for heatmap to increase legibility for the plot and separate it visually from the rest of the plots.  
- Added top 5 highest paying countries based on median as a text box  .
- Made the dropdown title of sidebar more intuitive.  
- Moved salary range bar above the map to show that it is a global parameter controlling the whole dashboard.

## What has not been implemented

For the R dashboard, due to technology limitations the window selection interactivity between two plots in the side bar were not implemented. Later on, due to technical issues with Plotly the count bar under the scatter plot in the side bar was removed. Another design choice in regards to the side bar was to change the background color from black to white as the black theme in ggplot was not legible. A darker banner at the top of this plot shows indicates the location of side bar in the R dashboard.

In the review it was suggested to breakdown the dashboard into two tabs for increased legibility. Instead a collective decision was made to instead play around with the design and spacing of plots and bars to fit everything in one page.

To visualize country-specific trends, a dropdown menu is used. However, we believe that the dashboard will be more engaging if the user is able to click on a country (see discussion [1](https://github.com/UBC-MDS/datasci_salaries_py/issues/29), [2](https://github.com/UBC-MDS/datasci_salaries_py/pull/30)) on the world map to update the country-specific plots. This will also allow the dashboard to appear more seamless and well-connected.

## Effective components of current dashboard

The world map is effective in giving a high level overview of how data science salaries are distributed across the world. In addition, the country-specific plots describe the salary distributions clearly for many different features. The global trends are always shown on the right side of the dashboard to provide a comparison of country-specific trends to the broader context.

The dropdown menu allows users to explore country-specific trends, and the slider also provides users with more flexibility in exploring the data set.

## Limitations of current dashboard

One limitation with the current dashboard is that not enough feedback has been gathered about its effectiveness and ease of use. For instance, some users may feel like there are too many plots shown in the dashboard which can be overwhelming at first glance. Another limitation is that the dashboard's layout is not consistent for different screen sizes which could cause the dashboard to appear very messy.

## Future improvements

To summarize, improvements that can be made to the dashboard are: (1) Click on world map to select country, (2) Gather feedback about UI/UX, (3) Improve dashboard layout to be consistent across multiple devices.
