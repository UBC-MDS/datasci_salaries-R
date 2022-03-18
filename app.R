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
library(tidyr)

# import salaries data

df_salary <- read_csv(here("data", "processed", "cleaned_salaries.csv"))


education_order <- c(
  "Less than bachelor's degree",
  "Bachelor's degree",
  "Master's degree",
  "Doctoral degree"
)

tenure_order <- c(
  "Less than a year",
  "1 to 2 years",
  "3 to 5 years",
  "6 to 10 years",
  "More than 10 years"
)

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

# Define widgets

con_dropdown <- dccDropdown(
  id="select-country",
  placeholder='Please select a country',
  value=NULL,
  options = df_salary %>% 
    arrange(Country) %>% 
    pull(Country) %>%
    unique() %>%
    purrr::map(function(con) list(label = con, value = con))
)

slider <- dccRangeSlider(
  id="xslider_1",
  min=0,
  max=500000,
  value=list(0, 500000),
  marks=list(
    "0"="0K", "50000"="50K", "100000"="100K",
    "150000"="150K", "200000"="200K", "250000"="250K",
    "300000"="300K", "350000"="350K", "400000"="400K",
    "450000"="450K", "500000"="500K", "550000"="550K"
  ),
  allowCross= FALSE
)

scientist <- dccDropdown(
  id="data_scientist",
  options=list(
    list("label" = "Data Scientist", "value" = "Yes"),
    list("label" = "Not Data Scientist", "value" = "No"),
    list("label" = "Mixed", "value" = "Sort of (Explain more)")
  ),
  value=list("Yes", "No", "Sort of (Explain more)"),
  style=list("font-size" = "12px", "height" = "3vh"),
  multi=TRUE
)

edu_dropdown <- dccDropdown(
  id="stack-select",
  options = list(list(label = "Formal Education",
                      value = "FormalEducation"),
                 list(label = "Coding Experience", 
                      value = "Tenure")),
  value="FormalEducation"
)

# Define sections

topbar = htmlDiv(
  list(
    dbcCol(
      list(
        htmlDiv(
          list(
            htmlH2(
              "Data Science Salaries Dashboard",
              style = TOPBAR_STYLE
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
              "Type of role",
              style=list("color" = "white", "font-size" = "14px")
            )
            ,
            scientist
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
              htmlH2(" ",                             
                     style=list("height" = "2vh")
              ),
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
              ),
              htmlH2(" ",                             
                     style=list("height" = "1vh")
              ),
              htmlH2("Zoom in on a salary range:",                             
                     style=list("color" = "black", "font-size" = "12px")
              ),
              slider
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
                    "height" = "16vh"
                  )
                ),
                dccGraph(
                  id="world_map",
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
                htmlH2(" ",                             
                       style=list("height" = "2vh")
                ),
                dccGraph(
                  id="salary_heatmap",
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
                htmlH2("Select a feature to stack by:",                             
                       style=list("color" = "black", "font-size" = "12px")
                ),
                edu_dropdown,
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

# Set layout

app$layout(
  htmlDiv(
    list(
      topbar,
      content
    )
  )
)

# Callback

app$callback(
  output("scatter", "figure"),
  list(
    input("data_scientist", "value")
  ),
  function(DS_identity) {
    # Clean data
    data <- df_salary %>%
      tidyr::drop_na() %>%
      dplyr::filter(Tenure != "I don't write code to analyze data")
    
    data <- data %>%
      dplyr::filter(DataScienceIdentitySelect %in% DS_identity)
    
    # Plot order
    order_tenure <- c('More than 10 years', '6 to 10 years', '3 to 5 years', '1 to 2 years', 'Less than a year')
    
    # median order
    med_order <- data |> group_by(Country) |> summarize(med = median(Salary_USD)) |> 
      arrange(desc(med)) |> select(Country) |> pull()
    data$Country <- factor(data$Country,  # Change ordering manually
                           levels = med_order)
    
    # Create Plot
    points <- data %>% ggplot(aes(
      x = Salary_USD,
      y = Country,
      color = Tenure
    )) + geom_point() +
      labs(
        title = "Salary distribution",
        x = "Salary in USD",
        y = "Country",
        color = "Coding Experience"
      ) +
      scale_x_continuous(labels = scales::label_number_si())+
      guides(fill=guide_legend(nrow=3,byrow=TRUE))  + 
      theme_bw() +
      theme(text = element_text(size = 10))
    
    ggplotly(points, tooltip = "EmployerIndustry") %>% layout(legend = list(orientation = "v", x = 0.3, y = 0.9))
    
  }
)

app$callback(
  output("salary_heatmap", "figure"),
  list(
    input("xslider_1", "value"), 
    input("select-country", "value")
  ),
  function(xmax, xcon){
    source <- as.data.frame(df_salary) %>%
      filter(Age > 0, Salary_USD <= xmax[2], Salary_USD >= xmax[1])
    
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
      labs(y = "Density") +
      theme_bw() +
      theme(text = element_text(size = 10))
    
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
      title = list(text=paste0("Heatmap of ", xcon), font=list(size = 16)),
      yaxis = list(title = 'Salary', font=list(size = 8)), 
      legend = list(title=list(text='Counts'), font=list(size = 8))
    )
    
    fig
  }
  
)


app$callback(
  output('world_map', 'figure'),
  list(
    input('select-country', 'value')
  ),
  function(xcon){
    
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
        title = 'Median Salary of the World<br>Source: <a href="https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv">Kaggle Dataset</a>',
        geo = g#,
        # dragmode = 'select'
      )
    
    fig
  }
)


app$callback(
  output('edu_histogram', 'figure'),
  list(
    input('select-country', 'value'),
    input("stack-select", "value"),
    input("xslider_1", "value")
  ),
  function(country, stack, xmax) {
    
    if (!is.null(country[[1]])) {
      p <- df_salary %>%
        filter(Country == country)
    }
    else {
      p <- df_salary
    }
    
    p <- filter(p, Age > 0, Salary_USD <= xmax[2], Salary_USD >= xmax[1])
    
    p <- p %>%
      tidyr::drop_na(Salary_USD, Tenure, FormalEducation) %>%
      filter(Tenure != "I don't write code to analyze data") %>%
      mutate(
        FormalEducation = case_when(
          !(FormalEducation %in% education_order) ~ "Less than bachelor's degree",
          TRUE ~ FormalEducation)) %>%
      mutate(
        FormalEducation = factor(
          FormalEducation, levels = education_order
        ),
        Tenure = factor(Tenure, levels = tenure_order)
      ) %>%
      ggplot(aes(x = Salary_USD, fill = !!sym(stack))) +
      geom_histogram(bins = 20, color = "white") +
      scale_x_continuous(labels = scales::label_number_si()) +
      labs(x = "Salary in USD", y = "Counts", fill = "") +
      theme_bw() +
      theme(text = element_text(size = 10)) +
      guides(fill=guide_legend(title=""))
    
    if (stack == "Tenure") {
      p <- p +
        labs(fill = "Coding experience") 
    }
    else {
      p <- p +
        labs(fill = "Formal education level")
    }
    
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", x = -0.1, y =-0.6))
  }
)

app$callback(
  output('gender-boxplot', 'figure'),
  list(
    input('select-country', 'value'),
    input("xslider_1", "value")
  ),
  function(con, xmax) {
    if (!is.null(con[[1]])) {
      p <- as.data.frame(df_salary) %>%
        filter(Country == con)
    }
    else {
      p <- as.data.frame(df_salary)
    }
    
    p <- p %>% tidyr::drop_na() 
    p <- filter(p, Age > 0, Salary_USD <= xmax[2], Salary_USD >= xmax[1])
    
    p$GenderSelect[(p$GenderSelect != 'Male')&(p$GenderSelect != 'Female') & (p$GenderSelect != 'A different identity')] <- 'Other'
    
    p <- p %>%
      mutate(GenderSelect = factor(GenderSelect)) %>%
      ggplot(aes(y = Salary_USD,
                 x = GenderSelect,
                 fill = GenderSelect,
                 text = GenderSelect)) +
      geom_boxplot() +
      
      scale_y_continuous(labels = scales::label_number_si()) +
      xlab("Gender") +
      ylab("Salary in USD") +
      coord_flip() +
      theme(legend.position="none") + 
      theme_bw() +
      theme(text = element_text(size = 10))
    
    ggplotly(p) %>% hide_legend()
  }
)

app$run_server(host = '0.0.0.0')
