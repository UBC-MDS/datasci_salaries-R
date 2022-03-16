library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

library(readr)
library(here)
library(purrr)
library(dplyr)

# import salaries data

df_salary <- readr::read_csv(here::here('data/processed', 'cleaned_salaries.csv'))

# import world map data

df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv") %>%
        select(COUNTRY, CODE)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

# Define section styles

SIDEBAR_STYLE = list(
    "position" = "fixed",
    "top" = "0rem",
    "right" = 0,
    "bottom" = 0,
    "width" = 380,
    "padding" = "2rem 0rem",
    "background-color" = "#2C2C2C"
)

TOPBAR_STYLE = list(
    "top" = 0,
    "right" = 0,
    "left" = 0,
    "height" = "2rem",
    "margin-right" = "10rem",
    "padding" = "0rem 0rem",
    "background-color" = "#808080"
    # "background-color": "#fffff",
)

CONTENT_STYLE = list(
    "margin-left" = "0rem",
    "margin-right" = "3rem",
    "padding" = "0rem 0rem"
)

# define fection contents

topbar = htmlDiv(
    list(
        dbcCol(
            list(
                htmlDiv(
                    list(
                        htmlH2(
                            "Data Science Salaries Dashboard",
                            style = list(
                                "color" = "white",
                                "font-size" = "20px",
                                "text-align" = "center"
                            )
                        )
                    )
                )
            )
        )
    ),
    style=TOPBAR_STYLE
)

sidebar = htmlDiv(
    list(
        dbcCol(
            list(
                htmlDiv(
                    list(
                        htmlH2(
                            "Are you a Data Scientist?",
                            style=list("color" = "white", "font-size" = "14px")
                        )
                        ,
                        dccDropdown(
                            id="data_scientist",
                            options=list(
                                list("label" = "Yes", "value" = "Yes"),
                                list("label" = "No", "value" = "No"),
                                list("label" = "Sort of", "value" = "Sort of (Explain more)")
                            ),
                            value=list("Yes", "No", "Sort of (Explain more)"),
                            style=list("font-size" = "12px", "height" = "3vh"),
                            multi=TRUE
                        )
                    )
                ),
                dccGraph(
                    id="scatter",
                    # srcDoc=plot_13(DS_identity=['Yes', 'No', 'Sort of (Explain more)']),
                    style=list("border-width" = "0", "width" = "100%", "height" = "100vh")
                )
            )
        )
    ),
    style=SIDEBAR_STYLE
)

content = dbcRow(
    list(
        dbcCol(
            list(
                dbcRow(
                    dbcCol(
                        list(
                            dccDropdown(
                                id="select-country",
                                placeholder='Please select a country',
                                value=NULL,
                                options=df_salary %>%
                                        arrange(Country) %>%
                                        pull(Country) %>%
                                        unique %>%
                                        purrr::map(function(country) list(label = country,
                                                                            value = country))
                            )
                        ),
                        width=6,
                        style=list("height" = "3vh")
                    ),
                    style=list("height" = "3vh")
                ),
                dbcRow(
                    list(
                        dbcCol(
                            list(
                                htmlIframe(
                                    style=list(
                                        "border-width" = "0",
                                        "width" = "100%",
                                        "height" = "10vh"
                                    )
                                ),
                                dccGraph(
                                    id="world_map",
                                    figure=plot_map(NULL),
                                    style=list(
                                        "border-width" = "0",
                                        "width" = "100%",
                                        "height" = "45vh"
                                    )
                                ),

                                # htmlDiv(
                                #     "Country:",   
                                #     id="country",                      
                                #     style=list("color" = "black", "font-size" = "12px")
                                #         ),
                                # htmlDiv(
                                #     "Salary:",
                                #     id="salary",                        
                                #     style=list("color" = "black", "font-size" = "12px")
                                #         ),

                                htmlIframe(
                                    style=list(
                                        "border-width" = "0",
                                        "width" = "100%",
                                        "height" = "10vh"
                                    )
                                )
          
                            ),
                            width=6
                        ),
                        

                        dbcCol(
                            list(
                                htmlH2("Select a salary range:",                             
                                        style=list("color" = "black", "font-size" = "12px")
                                        ),
                                dccRangeSlider(
                                    id="xslider_1",
                                    min=0,
                                    max=500000,
                                    value=list(0, 500000),
                                    marks=list(
                                            "0"="0K", "50000"="50K", "100000"="100K",
                                            "150000"="150K", "200000"="200K", "250000"="250K",
                                            "300000"="300K", "350000"="350K", "400000"="400K",
                                            "450000"="450K", "500000"="500K", "550000"="550K"
                                        )
                                ),
                                
                                dccGraph(
                                    id="salary_heatmap",
                                    figure=plot_salary_heatmap(list(0,550000), NULL),
                                    style=list(
                                        "border-width" = "0",
                                        "width" = "100%",
                                        "height" = "52vh"
                                    )
                                )
                            ),
                            width=5
                        )
                    )
                ),
                dbcRow(
                    list(
                        dbcCol(
                            list(
                                dccGraph(
                                    id="gender-boxplot",
                                    style=list(
                                        "border-width" = "0",
                                        "width" = "100%",
                                        "height" = "32vh",
                                        "display" = "block"
                                    )
                                )
                            ),
                            width=6
                        ),
                        dbcCol(
                            list(
                                dccGraph(
                                    id="edu_histogram",
                                    style=list(
                                        "border-width" = "0",
                                        "width" = "100%",
                                        "height" = "32vh"
                                    )
                                )
                            ),
                            width=5
                        )
                    )
                )
            )
        ),
        
        dbcCol(
            list(sidebar),
            width=3
        )
    ),
    style=CONTENT_STYLE
)
    
# Define dashborad layout

app$layout(
    htmlDiv(
        # c(
            list(
            # dccLocation(id="url", refresh=False),
            topbar,
            content
          #   ),
          # sidebar
         )
    )
)
    
# Plot functions
    
plot_sidebar <- function(DS_identity) {
    # Clean data
    data <- df_salary %>%
      drop_na() %>%
      dplyr::filter(Salary_USD < 400000) %>%
      dplyr::filter(Tenure != "I don't write code to analyze data")
    
    data <- data %>%
      dplyr::filter(DataScienceIdentitySelect %in% DS_identity)
    
    # Plot order
    order_tenure <- c('More than 10 years', '6 to 10 years', '3 to 5 years', '1 to 2 years', 'Less than a year')
    
    # Create Plot
    points <- data %>% ggplot(aes(
      x = Salary_USD,
      y = Country,
      color = Tenure
    )) + geom_point() +
      labs(
        title = "Salary distribution per country",
        x = "Salary in USD",
        y = "Country",
        color = "Coding Experience"
      ) +
      scale_x_continuous(labels = scales::label_number_si())
      # theme_dark()
    
    # bars <- data %>% ggplot(aes(
    #   y = Tenure,
    #   fill = Tenure
    # )) + geom_bar() +
    #   labs(
    #     x = "Counts",
    #     y = "Coding Experience"
    #   ) +
    #   theme(legend.position="none")
    
    
    
    ggplotly(points, tooltip = "EmployerIndustry")
    
    
    # subplot(ggplotly(points, tooltip = "EmployerIndustry"),
    #         bars,
    #         nrows = 2,
    #         heights = c(0.85, 0.15))
  }
    

plot_salary_heatmap <- function(xmax, xcon){
  
    source <- as.data.frame(df_salary) %>%
            filter(Age > 0 & Salary_USD <= xmax[2])

    if(is.null(xcon[[1]])){
        xcon  <- "the World"
    } else {
        source  <- source %>%
            filter(Country == xcon)
    }

    x_bin_num = max(as.integer((nrow(source)/6)^(0.65)), 6)
    y_bin_num = max(as.integer((nrow(source)/6)^(0.65) / 2), 6)

    fig1 <- plot_ly(x = source$Age, y = source$Salary_USD) %>% 
        add_histogram2d(colorscale="YlGnBu", nbinsx=x_bin_num, nbinsy=y_bin_num)


    p <- ggplot(source,aes(x=Age)) +
        geom_histogram(aes(y = ..density..), color="blue", fill = "blue", alpha = 0.2) +
        geom_density(fill="blue", alpha = 0.2) +
        theme_bw()
        
    fig2  <- ggplotly(p) 



    fig <- subplot(
        fig1,
        fig2,
        nrows = 2, 
        margin = 0.04,
        shareY = TRUE, 
        titleX = TRUE
    ) %>% layout(
      height=350, 
      width=450,
      title = list(text=paste("Heatmap of ", xcon), font=list(size = 16)),
      yaxis = list(title = 'Salary', font=list(size = 8)), 
      legend = list(title=list(text='Counts'), font=list(size = 8))
    )
    
    fig
}

    
plot_map <- function(xcon){

    print(xcon)

    df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv") %>%
        select(COUNTRY, CODE)

    source <- as.data.frame(df_salary) %>%
        select(Country, Salary_USD)

    source <- merge(source, df, by.x="Country", by.y="COUNTRY", all.y = TRUE) %>%
        mutate(Salary_USD = coalesce(Salary_USD, 0)) %>%
        group_by(Country, CODE) %>%
        summarize(Salary_USD = round(median(Salary_USD, na.rm=TRUE), 2))  %>%
        filter(Country !='antarctica')

    l <- list(color = toRGB("grey"), width = 0.5)

    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator'),
      clickmode = 'event+select'
    )

    fig <- plot_geo(source)
    fig <- fig %>% add_trace(
        z = ~Salary_USD, color = ~Salary_USD, colors = 'Blues', height=1200, width=450, scope="north america",
        text = ~Country, locations = ~CODE, marker = list(line = l)
      ) %>%
      layout(
      title = 'Median Salary of the World<br>Source:<a href="https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv">Kaggle Dataset</a>',
      geo = g#,
      # dragmode = 'select'
    )

    fig
}
    
plot_edu_histo <- function(country) {

    if (!is.null(country[[1]])) {
      p <- df_salary %>%
        filter(Country == country)
    }
    else {
      p <- df_salary
    }
    p <- p %>%
      drop_na(Salary_USD, Tenure, FormalEducation) %>%
      filter(Tenure != "I don't write code to analyze data") %>%
      mutate(
        FormalEducation = case_when(
          !(FormalEducation %in% education_order) ~ "Less than bachelor's degree",
          TRUE ~ FormalEducation)) %>%
      mutate(
        FormalEducation = factor(
          FormalEducation, levels = c(
          "Less than bachelor's degree", education_order)
          ),
        Tenure = factor(Tenure, levels = tenure_order)
        ) %>%
      ggplot(aes(x = Salary_USD, fill = !!sym(stack))) +
      geom_histogram(bins = 20, color = "white") +
      scale_x_continuous(labels = scales::label_number_si()) +
      labs(x = "Salary in USD", y = "Counts") +
      theme_bw()


    p <- p +
      labs(fill = "Formal education level")


    ggplotly(p)
  }
    

gender_box <- function(con) {
        p <- df_salary %>%
            mutate(GenderSelect = factor(GenderSelect)) %>%
            filter(Country == con) %>%
            ggplot(aes(y = Salary_USD,
                       x = GenderSelect,
                       fill = GenderSelect,
                       text = GenderSelect)) +
            geom_boxplot() +
            coord_flip() +
            theme(legend.position="none") +
            ggthemes::scale_color_tableau() 
        ggplotly(p)
    }

# Callback
    
app$callback(
    list(
        output("scatter", "figure")
    ),
    list(
        input("data_scientist", "value")
    ),
    plot_sidebar
    )

app$callback(
    list(
        output("salary_heatmap", "figure")
    ),
    list(
        input("xslider_1", "value"), 
        input("select-country", "value")
    ),
    plot_salary_heatmap

)


app$callback(
    list(
        output('world_map', 'figure'),
        output('edu_histogram', 'figure'),
        output('gender-boxplot', 'figure')
    ),
    list(
        input('select-country', 'value')
    ),
    list(
        plot_map,
        plot_edu_histo,
        gender_box
    )
)


app$run_server(host = '0.0.0.0')
