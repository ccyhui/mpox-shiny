# Monkeypox in Europe: A Real-Time View of the Latest Trends

This Shiny app provides a real-time view of monkeypox trends in Europe. The dashboard allows users to explore the latest data on monkeypox cases in different countries, using interactive visualizations that update dynamically as the user changes the input parameters.

## Getting started

To run the app, you will need to have R and the following packages installed:

- tidyverse
- lubridate
- shiny
- shinythemes
- plotly
- zoo

You can install these packages by running the following code in R:

```
install.packages(c("tidyverse", "lubridate", "shiny", "shinythemes", "plotly", "zoo"))
```

The monkeypox dataset is updated by the `mpox.R` script. It downloads the latest monkeypox data from the European Centre for Disease Prevention and Control (ECDC) and saves it as a CSV file in the data directory. The `app.R` script reads this data file and generates the visualizations in the app.

Run the app by opening the `app.R` file in RStudio and clicking on the "Run App" button.

You can also access the app [here](https://ccyhui.shinyapps.io/mpox-shiny).

## Using the app

The app has two tabs: "Line Chart" and "Heatmap". In both tabs, you can use the sidebar to select the following input parameters:

- End Date: The date up to which you want to see the monkeypox data. You can slide the date slider to choose the end date.
- Countries: The countries for which you want to see the monkeypox data. You can select multiple countries using the dropdown menu.
- Metric: The metric you want to use to visualize the monkeypox data. You can choose from "New", "Cumulative", and "Smooth".

The "Line Chart" tab displays a line chart that shows the monkeypox cases for the selected countries over time, based on the selected metric. The "Heatmap" tab displays a heatmap that shows the monkeypox cases for the selected countries and dates, based on the selected metric.

## Data sources

The monkeypox data used in this app was obtained from the European Centre for Disease Prevention and Control (ECDC). The `mpox.R` script downloads the data automatically from the ECDC website and saves it as a CSV file in the `data` directory.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

This app was created as a personal project. Please contact me iIf you have any feedback or suggestions for improvement.
