library(shiny)
library(rsconnect)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(plotly)
library(dplyr)
library(maps)
library(stats)
library(cluster)
library(DT)
library(bslib)

# Loading the data
data_url = 'https://raw.githubusercontent.com/ninarsv106/DS501/refs/heads/main/Country-data.csv'
data <- read.csv(data_url, encoding = "UTF-8")

#Clean the data
data <- na.omit(data)
data <- data[!duplicated(data), ]

# Define UI for app 
ui <- navbarPage(
"Country Data",
# Page 1
tabPanel(
  "Data",
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "columns",
        label = "Select variables to display:",
        choices = names(data),
        selected = names(data)  
      ),
      sliderInput(
        inputId = "child_mort_range",
        label = "Filter Child Mortality Rate:",
        min = min(data$child_mort),
        max = max(data$child_mort),
        value = c(min(data$child_mort), max(data$child_mort))
      ),
      sliderInput(
        inputId = "exports_range",
        label = "Filter Exports:",
        min = min(data$exports),
        max = max(data$exports),
        value = c(min(data$exports), max(data$exports))
      ),
      sliderInput(
        inputId = "health_range",
        label = "Filter Health spending:",
        min = min(data$health),
        max = max(data$health),
        value = c(min(data$health), max(data$health))
      ),
      sliderInput(
        inputId = "imports_range",
        label = "Filter Imports:",
        min = min(data$imports),
        max = max(data$imports),
        value = c(min(data$imports), max(data$imports))
      ),
      sliderInput(
        inputId = "income_range",
        label = "Filter Income:",
        min = min(data$income),
        max = max(data$income),
        value = c(min(data$income), max(data$income))
      ),
      sliderInput(
        inputId = "inflation_range",
        label = "Filter Inflation Rate:",
        min = min(data$inflation),
        max = max(data$inflation),
        value = c(min(data$inflation), max(data$inflation))
      ),
      sliderInput(
        inputId = "life_expec_range",
        label = "Filter Life Expectancy:",
        min = min(data$life_expec),
        max = max(data$life_expec),
        value = c(min(data$life_expec), max(data$life_expec))
      ),
      sliderInput(
        inputId = "total_fer_range",
        label = "Filter Fertility Rate:",
        min = min(data$total_fer),
        max = max(data$total_fer),
        value = c(min(data$total_fer), max(data$total_fer))
      ),
      sliderInput(
        inputId = "gdpp_range",
        label = "Filter GDP per capita:",
        min = min(data$gdpp),
        max = max(data$gdpp),
        value = c(min(data$gdpp), max(data$gdpp))
      ),
      selectInput(
        inputId = "sort_by",
        label = "Sort data by:",
        choices = list(
          "Country" = "country",
          "Child Mortality Rate" = "child_mort",
          "Exports" = "exports",
          "Health spending" = "health",
          "Imports" = "imports",
          "Income" = "income",
          "Inflation Rate" = "inflation",
          "Life Expectancy" = "life_expec",
          "Fertility Rate" = "total_fer",
          "GDP per capita" = "gdpp"
        ),
        selected = "country"
      ),
      radioButtons(
        inputId = "sort_order",
        label = "Sorting Order:",
        choices = c("Ascending" = "asc", "Descending" = "desc"),
        selected = "asc"
      )
    )
    ,
    mainPanel(
      h3("Data Table"),
      DTOutput("data_table")
    )
  )
),
# Page 2
tabPanel(
  "Plot Data",
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "hist_var",
          label = "Select variable:",
          choices = names(data)[!names(data) %in% "country"],
          selected = "income"
        ),
        h4("Descriptive Statistics"),
        tableOutput("descriptive_stats_var")
      ),
      mainPanel(
        h3("Frequency Distribution of variables"),
        plotOutput("hist_plot", height = "350px")
      )
    ),
    
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "pair_x",
          label = "Select X:",
          choices = names(data)[!names(data) %in% "country"],
          selected = "income"
        ),
        selectInput(
          inputId = "pair_y",
          label = "Select Y:",
          choices = names(data)[!names(data) %in% "country"],
          selected = "life_expec"
        )
      ),
      mainPanel(
        h3("Relationships between variables"),
        plotOutput("pair_plot", height = "350px")
      )
    ),
    
    br(), 
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "map_var",
          label = "Select variable:",
          choices = names(data)[!names(data) %in% "country"],
          selected = "income"
        )
      ),
      mainPanel(
        h3("Geographical Distribution of variables"),
        plotlyOutput("map_plot", height = "500px")
       )
     )
   )
 ),
# Page 3
tabPanel(
  "Clustering",
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "clust_vars",
        label   = "Select variables:",
        choices = setdiff(names(data), "country"),
        selected = setdiff(names(data), "country")
      ),
      numericInput(
        inputId = "k_clusters",
        label   = "Number of clusters",
        value   = 3, min = 1, max = 10, step = 1
      )
    ),
    mainPanel(
      h3("K-Means algorithm results"),
      plotOutput("cluster_pca_plot", height = 420),
      br(),
      verbatimTextOutput("sil_text"),
      br(),
      h3("Cluster Profiles: Sizes and Mean Values of Variables"),
      DTOutput("cluster_stats"),
      
    )
  ),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "box_var",
        label   = "Select variable:",
        choices = setdiff(names(data), "country"),
        selected = "income"
      )
    ),
    mainPanel(
      h3("Variable Distributions by Cluster"),
      plotOutput("cluster_boxplots", height = 420),
      br(),
      h3("Cluster Assignment Across Countries"),
      plotOutput("cluster_map", height = 420)
    )
    )
  ),
#Page 4
tabPanel(
  "About",
  fluidPage(
    div(
      h2("About This Application", 
         style = "text-align:center; font-weight:bold; margin-top:15px; margin-bottom:20px;")
    ),
    br(),
    
    # Section 1: The Dataset
    h3("1. The Dataset"),
    p("This application uses a global socioeconomic dataset [1] with 10 key indicators for 167 countries. The variables it includes are the following:"),
    tags$ul(
      tags$li(tags$b("country:"), " Name of the country"),
      tags$li(tags$b("child_mort:"), " Death of children under 5 years of age per 1,000 live births"),
      tags$li(tags$b("exports:"), " Exports of goods and services per capita, given as a percentage of the GDP per capita"),
      tags$li(tags$b("health:"), " Total health spending per capita, given as a percentage of GDP per capita"),
      tags$li(tags$b("imports:"), " Imports of goods and services per capita, given as a percentage of the GDP per capita"),
      tags$li(tags$b("income:"), " Net income per person ($)"),
      tags$li(tags$b("inflation:"), " The measurement of the annual growth rate of the total GDP"),
      tags$li(tags$b("life_expec:"), " The average number of years a newborn child would live if current mortality patterns remain the same"),
      tags$li(tags$b("total_fer:"), " The number of children that would be born to each woman if current age-fertility rates remain the same"),
      tags$li(tags$b("gdpp:"), " The GDP per capita, calculated as the total GDP divided by the total population ($)")
    ),
    p("This dataset allows for both quantitative analysis and country-level comparisons."),
    # Section 2: Objective
    h3("2. Objective"),
    p("The objective of this application is to categorize countries based on key socio-economic and health indicators that determine their overall level of development. 
      Specifically, this analysis aims to reveal global development patterns and provide insights that can help humanitarian organizations and policymakers allocate their resources more effectively, in order to support underdeveloped countries in need."),
    # Section 3: Motivation
    h3("3. Motivation"),
    p("I chose this dataset, because I believe understanding global inequality and identifying countries in need of support is crucial for promoting sustainable development and improving quality of life worldwide. 
      Also, this topic is particularly interesting to me, because it demonstrates how data-driven analysis can be used for social good to create a real-world impact."),
    # Section 4: Data Preprocessing
    h3("4. Data Preprocessing"),
    h4("4.1 Data Cleaning"),
    p("The dataset initially underwent a data cleaning process to ensure the accuracy, consistency, and reliability of the dataset before conducting any analysis. Specifically, data cleaning involved:"),
    tags$ul(
      tags$li("Removing missing values to avoid distortions in calculations and clustering results."),
      tags$li("Eliminating duplicate records to ensure that each country was represented only once in the analysis.")
    ),
    p("Outliers were intentionally retained, as they may reflect exceptionally high or low feature values, highlighting countries in particularly critical conditions and in need of support."),
    h4("4.2 Data Transformation"),
    p("After cleaning the dataset, numerical variables were transformed to ensure compatibility with clustering algorithms. Specifically, all continuous features were standardized using the scale() function in R, which centers each variable to have a mean of zero and a standard deviation of one.
      This was necessary as K-Means is a distance-based algorithm. Without standardization, variables with larger numeric ranges (e.g. gdpp) could dominate the distance calculations and bias the clustering results. Standardizing the data ensured that all features contributed equally to the analysis, regardless of their original units or scales."),
    h4("4.3 Feature Selection"),
    p("Feature selection was performed to remove redundant variables. This step aimed to simplify the feature space, reduce multicollinearity, and ensure that only meaningful attributes contributed to the clustering.
      A correlation heatmap of all numerical variables is displayed below:"),plotOutput("corr_heatmap", height = 450),
    p("Using a magnitude threshold of 0.8 to identify strong correlations, the above heatmap reveals high multicollinearity among the pairs
    ('gdpp', 'income'), ('total_fer', 'child_mort'), and ('life_expec', 'child_mort'). To avoid multicollinearity the correlated variables 'income' and 'child_mort' were removed from the final dataset."),
    h4("4.4 Data Reduction"),
    p("To visualize the clustering results, Principal Component Analysis (PCA) was applied to the standardized dataset. PCA reduced the multidimensional feature space into two principal components, enabling the representation of countries in a two-dimensional plot."),
    # Section 5: Exploratory Data Analysis (EDA)
    h3("5. Exploratory Data Analysis (EDA)"),
    p("A comprehensive exploratory data analysis was performed to understand the distribution of each feature, and examine the relationships between the features. The purpose was to uncover insights about the features that may play a significant role in differentiating countries during the subsequent clustering phase.
       Presented below are the histograms and choropleth maps illustrating the distributions and geographic patterns of all 9 numerical variables in the dataset:"),
    uiOutput("eda_plots"),
    p("Based on the visualizations above, the main insights are:"),
    tags$ul(
      tags$li("'child_mort' has a highly right-skewed distribution, as most countries have relatively low child mortality, but several countries exhibit extremely high values, particularly countries in Sub-Saharan Africa, highlighting severe health challenges in the region"),
      tags$li("'exports' has a fairly symmetric distribution centered around moderate export percentages (41%), with higher export rates seen in parts of Asia and Europe, while many African countries show lower export levels"),
      tags$li("'health' has a right-skewed distribution, as a small number of countries (in North America and Western Europe) invest heavily in health, while most have modest healthcare spending (average is 6.82% of GDP per capita)"),
      tags$li("'imports' has a right-skewed distribution and higher import levels appear in Europe and East Asia"),
      tags$li("'income' has a strongly right-skewed distribution, as wealth is concentrated in North America, Western Europe, Australia, and parts of East Asia and lower income values dominate Africa, South America, and South Asia."),
      tags$li("'inflation' is centered around a moderate level (7.78%) with a few extreme outliers in Africa and South America, indicating economic instability in these regions"),
      tags$li("'life_expec' has a left-skewed distribution, with most countries having moderately high life expectancy (average is 70.56), while countries in Sub-Saharan Africa show significantly low values"),
      tags$li("'total_fer' has a right-skewed distribution, as high fertility rates appear in Central and Eastern Africa, while Europe, East Asia, and North America show low fertility "),
      tags$li("'gdpp' has an extremely right-skewed distribution, as only a few countries (in Western Europe, North America, and Australia) have very high GDP per capita, while the majority remain far lower (average is around $12,964)")
      ),
    p("To examine the relationships between variabes, we plot the following scatterplots:"),
    uiOutput("scatter_grid"),
    p("These plots reveal the following key patterns:"),
    tags$ul(
      tags$li("Countries with higher income per person tend to have higher life expectancy"),
      tags$li("Countries with higher GDP per capita tend to have lower child mortality rates"),
      tags$li("There is a very weak positive relationship between 'income' and 'health', implying that higher national income does not necessarily translate into proportionally higher health expenditure"),
      tags$li("Higher GDP per capita is associated with a higher percentage of exports"), 
      tags$li("Higher GDP per capita doesn't directly correlate with a higher percentage of imports"),
      tags$li("Higher GDP per capita is linked with lower fertility rates"),
      tags$li("There is a very weak negative relationship between GDP per capita and inflation."),
      tags$li("Higher child mortality rates are linked with lower life expectancy")
      ),
    # Section 6: Machine Learning (ML) Algorithms
    h3("6. Machine Learning (ML) Algorithms"),
    p("The task of categorizing countries based on socioeconomic indicators does not come with predefined class labels.
      The goal is to discover the underlying structure of the data and identify groups of countries that share similar characteristics.
      Therefore, the task falls under unsupervised machine learning, particularly clustering. Clustering algorithms are specifically designed to uncover hidden patterns, and
      form meaningful groups without prior assumptions about class membership, thus they are well suited for this type of country-level socioeconomic analysis."),
    h4("6.1 Clustering"),
    p("The clustering algorithm we will use in this analysis is the K-Means algorithm. K-Means is particularly appropriate for our task because:"),
    tags$ul(
      tags$li("It handles continuous numerical variables effectively, and all variables in the dataset that will be used for clustering are numeric rather than categorical."),
      tags$li("It works well with moderate-sized datasets like ours."),
      tags$li("It forms compact, spherical clusters, which aligns with the expectation that countries with similar socioeconomic profiles naturally group together into dense, cohesive clusters."),
      tags$li("It is computationally efficient and interpretable, which is consistent with the needs of exploratory country-level analysis.")
      ),
    p("We will now outline the mathematical and statistical details of the K-Means algorithm:"),
    p("K-Means partitions the dataset into K clusters (where K is a given parameter) by minimizing within-cluster variance. Formally, it solves:"),
    withMathJax(
      HTML("$$\\min_{C_1,\\dots,C_K} \\sum_{k=1}^{K} \\sum_{x_i \\in C_k} \\lVert x_i - \\mu_k \\rVert^2$$")
    ),
    withMathJax(
      HTML("
    <p>where:</p>
    <ul>
      <li>\\( C_k \\): cluster \\(k\\)</li>
      <li>\\( x_i \\): data point</li>
      <li>\\( \\mu_k \\): centroid of cluster \\(k\\)</li>
      <li>\\( \\lVert \\cdot \\rVert \\): Euclidean distance</li>
    </ul>
  ")
    ),
    p("K-Means begins by initializing K cluster centroids. These initial centroids are chosen randomly from the dataset. After initialization, the algorithm follows an iterative process made up of two main steps:"),
    withMathJax(
      HTML("
    <h5>1. Assignment Step</h5>
    <p>
      Each data point is assigned to the nearest centroid (measured using Euclidean distance):
    </p>

    $$ 
      C_k = \\{ x_i : \\lVert x_i - \\mu_k \\rVert^2 \\; \\text{is minimal} \\}
    $$

    <h5>2. Update Step</h5>
    <p>
      Each centroid is recalculated as the mean of the points currently assigned to it:
    </p>

    $$
      \\mu_k = \\frac{1}{|C_k|} \\sum_{x_i \\in C_k} x_i
    $$
  ")
    ),
    p("The algorithm alternates between these two steps until it converges—either when the cluster assignments stop changing or when 
      the reduction in within-cluster variance becomes negligible. The final solution represents a partition of the data points into
    K groups where the data points inside each group are as similar as possible, while the groups themselves are as distinct as possible from one another."),
    h4(" 6.2 Principal Component Analysis (PCA)"),
    p("Although PCA was not used for clustering itself, it was used for visualization, because it preserves as much variance as possible, ensuring that cluster separation in the 2D plot reflects the real structure in the data."),
    p("We summarize below the mathematical and statistical details of PCA:"),
    withMathJax(
      HTML("
<p>PCA finds a set of orthogonal directions (principal components) that capture the maximum variance in the data.</p>

<p>Given a standardized data matrix \\( X \\), PCA:</p>

<p>1. Computes the covariance matrix:</p>

$$
S = \\frac{1}{n - 1} X^T X
$$

<p>2. Solves the eigenvalue problem:</p>

$$
S v = \\lambda v
$$

<p>where:</p>

<ul>
  <li>\\( v \\) = eigenvector (principal component direction)</li>
  <li>\\( \\lambda \\) = eigenvalue (variance the component explains)</li>
</ul>

<p>3. Orders eigenvectors by decreasing eigenvalues.</p>

<p>4. Projects data onto the top k components (where k is a given parameter):</p>

$$
Z = X V_k
$$

<p>where \\( V_k \\) contains the first \\( k \\) eigenvectors.</p>
  ")
    ),
    p("Thus, PC1 captures the direction of greatest variance in the data, PC2 captures the next most informative axis (that is orthogonal to PC1), and so on up to the k-th principal component."),
    # Section 7: Evaluation Metrics
    h3("7. Evaluation Metrics"),
    withMathJax(HTML("
<p>The quality of clustering is evaluated using two primary metrics:</p>

<ol>
  <li>
    Silhouette score, which measures how well each data point fits within its assigned cluster 
    compared to neighboring clusters. For a point <i>i</i>, the silhouette value is:
    $$ s(i) = \\frac{b(i) - a(i)}{\\max(a(i), \\, b(i))} $$
    where:
    <ul>
      <li><i>a(i)</i> = average distance from point <i>i</i> to all other points in its own cluster</li>
      <li><i>b(i)</i> = lowest average distance from point <i>i</i> to points in any other cluster</li>
    </ul>
    The silhouette score is the mean of all <i>s(i)</i> values.
  </li>

  <li>
    Within-Cluster Sum of Squares (WCSS), also called within-cluster variance, measures how compact each cluster is. We defined this earlier in Section 6.1.  
  </li>
</ol>

<p>
Overall, a higher silhouette score and a lower WCSS indicate better clustering performance.
</p>
"))
    ,
    
    # Section 8: Modeling
    h3("8. Modeling"),
    h4("8.1 Clustering Methodology"),
    p("The final dataset that will be used for clustering will have the following features as previously mentioned in Section 4.3: 'exports', 'health', 'imports', 'inflation', 'life_expec', 'total_fer' and 'gdpp'."),
    p("To determine the optimal number of clusters (K) for the K-Means algorithm we will use the elbow method and silhouette analysis, as illustrated in the plots below:"),
    plotOutput("k_diagnostics", height = "450px"),
    br(),
    p("Based on the plots, we select the optimal number of clusters to be K=3."),
    h4("8.2 Results"),
    p("After fitting K-Means with K=3 on the selected features, we obtain the following 2D visualization of the resulting clusters:"),
    plotOutput("static_pca_k3"),
    br(),
    p("The 3 clusters appear to be moderately well-seperated with minimal overlap. The silhouette score is 0.264 and the WCSS value is 715.61."),
    p("To examine each cluster in more detail, we compute per-cluster summary statistics—including cluster size and variable means—and visualize the distributions of all numerical variables by cluster:"),
    DTOutput("cluster_summary"),
    br(),
    uiOutput("cluster_boxplots_all"),
    br(),
    p("The 3 clusters have moderately balanced sizes, with Cluster 2 being the largest, containing 88 countries. We summarize the clusters based on their characteristics as follows:"),
    p("Cluster 1:"),
    tags$ul(
      tags$li("Very low child mortality (≈4)"),
      tags$li("High life expectancy (≈80 years)"),
      tags$li("High GDP per capita (≈44,000) and income"),
      tags$li("High exports and imports"),
      tags$li("High health spending"),
      tags$li("Low inflation"),
      tags$li("Low fertility rate")
    ),
    p("Cluster 1 represents fully developed countries with strong economies, robust healthcare systems, high standards of living, strong global trade participation, and stable macroeconomic conditions."),
    p("Cluster 2:"),
    tags$ul(
      tags$li("Moderate child mortality (≈22)"),
      tags$li("Moderate life expectancy (≈73 years)"),
      tags$li("Mid-range GDP per capita (≈7,800) and income"),
      tags$li("Moderate exports and imports"),
      tags$li("Moderate health spending"),
      tags$li("Moderate inflation"),
      tags$li("Moderate fertility rate")
    ),
    p("Cluster 2 represents developing countries that are transitioning economically, show moderate health outcomes, and a growing participation in trade. Their indicators suggest improving development but with room for advancement."),
    p("Cluster 3:"),
    tags$ul(
      tags$li("Extremely high child mortality (≈90)"),
      tags$li("Low life expectancy (≈59 years)"),
      tags$li("Very low GDP per capita (≈1,900) and income"),
      tags$li("Low exports and imports"),
      tags$li("Low health spending"),
      tags$li("High inflation"),
      tags$li("High fertility rate")
    ),
    p("Cluster 3 represents underdeveloped countries characterized by weak health systems, high population growth, limited economic capacity, and in general, deep structural challenges."),
    p("Next, we visualize the cluster assignments across countries using the following choropleth map:"),
    plotOutput("cluster_map_3", height = 420),
    p("We notice that:"),
    tags$ul(
      tags$li("Cluster 1 is concentrated in North America, Western Europe, Australia, New Zealand, parts of East Asia (e.g. Japan) and some Middle Eastern countries (e.g. UAE)."),
      tags$li("Cluster 2 covers most of South America, Eastern Europe, Middle East, North Africa, and large portions of Asia, including Russia, and Central Asian countries."),
      tags$li("Cluster 3 is highly concentrated in Sub-Saharan Africa, with some countries in South Asia (e.g. Pakistan) and a few countries in the Middle East (e.g. Yemen).")
    ),
    # Section 9: Conclusion
    h3("9. Conclusion"),
    p("We conclude that countries in Cluster 3 are underdeveloped countries in need of aid, and thus, humanitarian organizations and policymakers should prioritize allocating their resources towards those countries.
      Additionally, we observe that the developing countries in Cluster 2 may be better suited for targeted development initiatives rather than immediate humanitarian assistance, while the 
      fully developed countries in Cluster 1 show little need for external assistance."),
    #References
    h3("References"),
    tags$p(
      "[1] ",
      "Unsupervised Learning on Country Data. ",
      tags$a(
        href = "https://www.kaggle.com/datasets/rohan0301/unsupervised-learning-on-country-data",
        "https://www.kaggle.com/datasets/rohan0301/unsupervised-learning-on-country-data",
        target = "_blank"
      ),
      ". Accessed: November 13, 2025."
    ),
    p("AI Usage Disclosure: I used ChatGPT to review and debug code. No content was copied directly. Final submission reflects my original thinking.")
  )
)
)


  





# Define server logic required
server <- function(input, output) {
  #Page 1 logic
  output$data_table <- renderDT({
    filter_data <- data[
      data$child_mort >= input$child_mort_range[1] & data$child_mort <= input$child_mort_range[2] &
        data$exports    >= input$exports_range[1]    & data$exports    <= input$exports_range[2] &
        data$health     >= input$health_range[1]     & data$health     <= input$health_range[2] &
        data$imports    >= input$imports_range[1]    & data$imports    <= input$imports_range[2] &
        data$income     >= input$income_range[1]     & data$income     <= input$income_range[2] &
        data$inflation  >= input$inflation_range[1]  & data$inflation  <= input$inflation_range[2] &
        data$life_expec >= input$life_expec_range[1] & data$life_expec <= input$life_expec_range[2] &
        data$total_fer  >= input$total_fer_range[1]  & data$total_fer  <= input$total_fer_range[2] &
        data$gdpp       >= input$gdpp_range[1]       & data$gdpp       <= input$gdpp_range[2],
    ]
    # handle empty results
    if (!nrow(filter_data)) {
      return(datatable(data.frame(Message = "No matching records found")))
    }
    sort_data <- filter_data %>%
      arrange(
        if (input$sort_order == "asc")  !!sym(input$sort_by)
        else desc(!!sym(input$sort_by))
      )
    if (!is.null(input$columns)) {
      sort_data <- sort_data[, input$columns, drop = FALSE]
    # display datatable
     datatable(sort_data, options = list(pageLength = 25))
    }
  })
  
  #Page 2 logic
  output$hist_plot <- renderPlot({
    ggplot(data, aes_string(x = input$hist_var)) +
      geom_histogram(fill = "steelblue", color = "black", bins = 30) +
      scale_x_continuous(labels = scales::comma,
                         breaks = c(25000, 50000, 75000, 100000)) +
      labs(
        title = paste("Histogram of", input$hist_var),
        x = input$hist_var,
        y = "Count"
      ) + 
      theme_minimal(base_family = "Open Sans")+ theme(
        plot.title = element_text(
          hjust = 0.5,
          size = 18          
        )
      )
  })
  
  output$pair_plot <- renderPlot({
    ggplot(data, aes_string(x = input$pair_x, y = input$pair_y)) +
      geom_point(color = "tomato", alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") +
      labs(
        title = paste("Scatterplot of", input$pair_x, "vs", input$pair_y),
        x = input$pair_x,
        y = input$pair_y
      ) + theme_minimal(base_family = "Open Sans") +theme(
        plot.title = element_text(
          hjust = 0.5,
          size = 18          
        )
      )
  })
  output$map_plot <- renderPlotly({
    all_countries <- data.frame(country = unique(map_data("world")$region))
    map_data_complete <- all_countries %>%
      left_join(data, by = "country")
    map_data_complete$z_value <- as.numeric(map_data_complete[[input$map_var]])
    map_data_complete$z_value[is.na(map_data_complete$z_value)] <- 0
    
    fig <- plot_geo(map_data_complete) %>%
      add_trace(
        locations = ~country,
        locationmode = "country names",
        z = ~z_value,
        text = ~country,
        colorscale = list(
          c(0, "lightgrey"),   # grey for missing (NA) values
          c(0.0001,  "rgb(230,245,255)"),  
          c(0.1,  "rgb(200,230,255)"),
          c(0.2,  "rgb(170,210,255)"),
          c(0.3,  "rgb(140,190,255)"),
          c(0.4,  "rgb(110,170,255)"),
          c(0.5,  "rgb(80,150,255)"),
          c(0.6,  "rgb(60,130,230)"),
          c(0.7,  "rgb(40,110,210)"),
          c(0.8,  "rgb(25,90,190)"),
          c(0.9,  "rgb(15,70,160)"),
          c(1.0,  "rgb(0,50,130)")   
        ),
        colorbar = list(title = input$map_var),
        showscale = TRUE,
        name = input$map_var
      ) %>%
      layout(
        title = paste("Choropleth Map of", input$map_var), 
        geo = list(
          showframe = FALSE,
          showcoastlines = FALSE,
          projection = list(type = 'miller')
        )
      )
    
    fig
  })
  output$descriptive_stats_var <- renderTable({
    var_name <- input$hist_var
    x <- data[[var_name]]
    stats <- data.frame(
      Statistic = c("Mean", "Standard Deviation", "Median", "Minimum", "Maximum"),
      Value = c(
        round(mean(x), 2),
        round(sd(x), 2),
        round(median(x), 2),
        round(min(x), 2),
        round(max(x), 2)
      )
    )
    stats
  })
  #Page 3 logic 
  selected_vars <- reactive({
    req(input$clust_vars)
    validate(need(length(input$clust_vars) >= 2,
                  "Please select at least two variables"))
    input$clust_vars
  })
  scaled_matrix <- reactive({
    X <- as.data.frame(data[, selected_vars(), drop = FALSE])
    M <- scale(X)
    colnames(M) <- colnames(X)
    M
  })
  km_fit <- reactive({
    k <- input$k_clusters
    set.seed(123)
    kmeans(scaled_matrix(), centers = k, nstart = 25)
  })
  pca_2d <- reactive({
    pr <- prcomp(scaled_matrix(), center = FALSE, scale. = FALSE)
    as.data.frame(pr$x[, 1:2, drop = FALSE]) |>
      transform(
        PC1 = `PC1`,
        PC2 = `PC2`,
        cluster = factor(km_fit()$cluster)
      )
  })
  output$cluster_pca_plot <- renderPlot({
    df <- pca_2d()
    ggplot(df, aes(PC1, PC2, color = cluster)) +
      geom_point(size = 2.5, alpha = 0.9) +
      scale_color_brewer(palette = "Set2") +  
      labs(
        title = paste0(
          "PCA (2D) Projection of K-Means Clusters (k = ", input$k_clusters, ")"
        ),
        color = "Cluster",
        x = "Principal Component 1",
        y = "Principal Component 2"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        plot.title = element_text(
          hjust = 0.5,      
          size = 18
        ),
        legend.position = "right"
      )
  })
  output$sil_text <- renderText({
    k <- input$k_clusters
    km <- km_fit()  
    wcss <- sum(km$withinss)  
    if (k < 2) return(sprintf(
      "The silhouette score is not defined for k = 1.\nThe total within-cluster sum of squares (WCSS) is: %.2f",
      wcss
    ))
    sil <- silhouette(km_fit()$cluster,
                               dist(scaled_matrix()))
    sil_mean <- mean(sil[, 3], na.rm = TRUE)
    sprintf(
      "The silhouette score is: %.3f\nThe total within-cluster sum of squares (WCSS) is: %.2f",
      sil_mean, wcss
    )
  })
  output$cluster_stats <- renderDT({
    dfc <- mutate(
      data,
      cluster = factor(km_fit()$cluster)
    )
    numeric_vars <- names(dfc)[sapply(dfc, is.numeric)]
    stats_tbl <- dfc |>
      group_by(cluster) |>
      summarise(
        `Size` = n(),
          across(
          all_of(numeric_vars),
          ~round(mean(.x), 3)
        ),
        .groups = "drop"
      ) |>
      rename(Cluster = cluster)
    datatable(stats_tbl,
              options = list(
                dom = 't',        
                paging = FALSE,   
                searching = FALSE))
             
  })
  output$cluster_boxplots <- renderPlot({
    dfc <- mutate(
      data,
      cluster = factor(km_fit()$cluster)
    )
    ggplot(dfc, aes(x = cluster, y = .data[[input$box_var]], fill = cluster)) +
      geom_boxplot(outlier.alpha = 0.4) +
      scale_fill_brewer(palette = "Set2") +  
      labs(
        title = paste("Distribution of", input$box_var, "by Cluster"),
        x = "Cluster",
        y = input$box_var
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none"
      )
  })
  output$cluster_map <- renderPlot({
    world <- map_data("world")
    country_clusters <- data.frame(
      country = data$country,
      cluster = factor(km_fit()$cluster)
    )
    map_df <- world %>%
      left_join(country_clusters, by = c("region" = "country"))
    ggplot(map_df, aes(x = long, y = lat, group = group, fill = cluster)) +
      geom_polygon(color = "gray50", size = 0.2) +
      scale_fill_brewer(palette = "Set2", na.value = "lightgray") +
      coord_quickmap() +
      labs(
        title = "Choropleth Map of Clusters",
        fill = "Cluster"
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5)
      )
  })
  #Page 4 logic 
  output$corr_heatmap <- renderPlot({
    corr_data <- data[sapply(data, is.numeric)]
    corr_matrix <- cor(corr_data)
    melted_corr <- melt(corr_matrix)
    ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.2f", value)),
                color = "black", size = 3.5) +
      scale_fill_gradient2(
        low = "#7B1FA2",     
        mid = "white",
        high = "#d73027",    
        midpoint = 0,
        limits = c(-1, 1), name = NULL
      ) +
      
      labs(
        title = "Correlation Heatmap of Variables",
        x = "",
        y = ""
      ) +
      
      coord_fixed() +
      theme_minimal(base_family = "Open Sans") +
      theme(
        axis.text.x = element_text(
          angle = 45, vjust = 1, hjust = 1, size = 11
        ),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        panel.grid = element_blank()
      )
  })
  output$eda_plots <- renderUI({
    
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    
    plot_list <- lapply(numeric_vars, function(varname) {
      hist_plot <- ggplot(data, aes_string(x = varname)) +
        geom_histogram(fill = "steelblue", color = "black", bins = 30) +
        labs(
          title = paste("Histogram of", varname),
          x = varname,
          y = "Count"
        ) +
        theme_minimal(base_family = "Open Sans") +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18)
        )

      all_countries <- data.frame(country = unique(map_data("world")$region))
      map_data_complete <- all_countries %>%
        left_join(data, by = "country")
      
      map_data_complete$z_value <- as.numeric(map_data_complete[[varname]])
      map_data_complete$z_value[is.na(map_data_complete$z_value)] <- 0
      
      map_plot <- plot_geo(map_data_complete) %>%
        add_trace(
          locations = ~country,
          locationmode = "country names",
          z = ~z_value,
          text = ~country,
          hoverinfo = "none",
          colorscale = list(
            c(0,     "lightgrey"),
            c(0.0001,"rgb(230,245,255)"),
            c(0.1,   "rgb(200,230,255)"),
            c(0.2,   "rgb(170,210,255)"),
            c(0.3,   "rgb(140,190,255)"),
            c(0.4,   "rgb(110,170,255)"),
            c(0.5,   "rgb(80,150,255)"),
            c(0.6,   "rgb(60,130,230)"),
            c(0.7,   "rgb(40,110,210)"),
            c(0.8,   "rgb(25,90,190)"),
            c(0.9,   "rgb(15,70,160)"),
            c(1.0,   "rgb(0,50,130)")
          ),
          showscale = TRUE,
          colorbar = list(title = varname)
        ) %>%
        layout(
          title = paste("Choropleth Map of", varname),
          geo = list(
            showframe = FALSE,
            showcoastlines = FALSE,
            projection = list(type = 'miller')
          )
        )
      
      fluidRow(
        column(6, plotOutput(outputId = paste0("hist_", varname))),
        column(6, plotlyOutput(outputId = paste0("map_", varname)))
      )
    })
    tagList(plot_list)
  })
  
  observe({
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    
    for (varname in numeric_vars) {
      
      local({
        
        v <- varname
        output[[paste0("hist_", v)]] <- renderPlot({
          ggplot(data, aes_string(x = v)) +
            geom_histogram(fill = "steelblue", color = "black", bins = 30) +
            labs(
              title = paste("Histogram of", v),
              x = v,
              y = "Count"
            ) +
            theme_minimal(base_family = "Open Sans") +
            theme(plot.title = element_text(hjust = 0.5, size = 18))
        })
        
        output[[paste0("map_", v)]] <- renderPlotly({
          
          all_countries <- data.frame(country = unique(map_data("world")$region))
          
          map_data_complete <- all_countries %>%
            left_join(data, by = "country")
          
          map_data_complete$z_value <- as.numeric(map_data_complete[[v]])
          map_data_complete$z_value[is.na(map_data_complete$z_value)] <- 0
          
          plot_geo(map_data_complete) %>%
            add_trace(
              locations = ~country,
              locationmode = "country names",
              z = ~z_value,
              hoverinfo = "none",   
              colorscale = list(
                c(0,     "lightgrey"),
                c(0.0001,"rgb(230,245,255)"),
                c(0.1,   "rgb(200,230,255)"),
                c(0.2,   "rgb(170,210,255)"),
                c(0.3,   "rgb(140,190,255)"),
                c(0.4,   "rgb(110,170,255)"),
                c(0.5,   "rgb(80,150,255)"),
                c(0.6,   "rgb(60,130,230)"),
                c(0.7,   "rgb(40,110,210)"),
                c(0.8,   "rgb(25,90,190)"),
                c(0.9,   "rgb(15,70,160)"),
                c(1.0,   "rgb(0,50,130)")
              ),
              showscale = TRUE,
              colorbar = list(title = v)
            ) %>%
            layout(
              title = paste("Choropleth Map of", v),
              geo = list(
                showframe = FALSE,
                showcoastlines = FALSE,
                projection = list(type = 'miller')
              )
            )
        })
      })
    }
  })
  output$scatter_grid <- renderUI({
    
    pairs <- list(
      c("income", "life_expec"),
      c("gdpp", "child_mort"),
      c("income", "health"),
      c("gdpp", "exports"),
      c("gdpp", "imports"),
      c("gdpp", "total_fer"),
      c("gdpp", "inflation"),
      c("child_mort", "life_expec")
    )
    
    plot_rows <- lapply(seq(1, length(pairs), by = 2), function(i) {
      fluidRow(
        column(6, plotOutput(paste0("scatter_", pairs[[i]][1], "_", pairs[[i]][2]))),
        column(6, if (i + 1 <= length(pairs))
          plotOutput(paste0("scatter_", pairs[[i+1]][1], "_", pairs[[i+1]][2])))
      )
    })
    
    tagList(plot_rows)
  })
  observe({
    
    pairs <- list(
      c("income", "life_expec"),
      c("gdpp", "child_mort"),
      c("income", "health"),
      c("gdpp", "exports"),
      c("gdpp", "imports"),
      c("gdpp", "total_fer"),
      c("gdpp", "inflation"),
      c("child_mort", "life_expec")
    )
    
    for (p in pairs) {
      local({
        x <- p[1]
        y <- p[2]
        id <- paste0("scatter_", x, "_", y)
        
        output[[id]] <- renderPlot({
          ggplot(data, aes_string(x = x, y = y)) +
            geom_point(color = "tomato", alpha = 0.7) +
            geom_smooth(method = "lm", se = FALSE,
                        color = "darkblue", linetype = "dashed") +
            labs(
              title = paste("Scatterplot of", x, "vs", y),
              x = x,
              y = y
            ) +
            theme_minimal(base_family = "Open Sans") +
            theme(
              plot.title = element_text(hjust = 0.5, size = 18)
            )
        })
      })
    }
  })
  
  select_vars <- c("exports", "health", "imports", "inflation",
                   "life_expec", "total_fer", "gdpp")
  
  scaled_matrix_fixed <- reactive({
    X <- as.data.frame(data[, select_vars, drop = FALSE])
    scale(X)
  })
  km3 <- reactive({
    set.seed(123)             
    kmeans(scaled_matrix_fixed(), centers = 3, nstart = 25)
  })
  output$k_diagnostics <- renderPlot({
    X <- scale(data[, select_vars, drop = FALSE]) 
    k_values <- 1:10
    wcss <- numeric(length(k_values))
    for (i in k_values) {
      km <- kmeans(X, centers = i, nstart = 25)
      wcss[i] <- sum(km$withinss)
    }
    sil_values <- rep(NA, length(k_values))
    for (i in 2:10) {
      km <- kmeans(X, centers = i, nstart = 25)
      sil <- cluster::silhouette(km$cluster, dist(X))
      sil_values[i] <- mean(sil[, 3])
    }
    df_wcss <- data.frame(k = k_values, wcss = wcss)
    df_sil  <- data.frame(k = k_values, sil = sil_values)
    p1 <- ggplot(df_wcss, aes(x = k, y = wcss)) +
      geom_line(color = "#2c7fb8", size = 1) +
      geom_point(color = "#2c7fb8", size = 2) +
      labs(title = "Elbow Method",
           x = "Number of clusters (K)",
           y = "WCSS") +
      scale_x_continuous(breaks = 1:10, limits = c(0, 10)) +
      expand_limits(y = 0) +
      theme_minimal(base_family = "Open Sans") +
      theme(plot.title = element_text(size = 16, hjust = 0.5))
    
    p2 <- ggplot(df_sil, aes(x = k, y = sil)) +
      geom_line(color = "#2c7fb8", size = 1) +
      geom_point(color = "#2c7fb8", size = 2) +
      labs(title = "Silhouette Analysis",
           x = "Number of clusters (K)",
           y = "Silhouette Score") +
      scale_x_continuous(breaks = 2:10) +
      scale_y_continuous(
        limits = c(0.20, 0.30),
        breaks = seq(0.20, 0.30, by = 0.01)
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(plot.title = element_text(size = 16, hjust = 0.5))
    grid.arrange(p1, p2, ncol = 2)
  })
  output$static_pca_k3 <- renderPlot({
    X <- scale(data[, select_vars, drop = FALSE])
    pca <- prcomp(X, center = FALSE, scale. = FALSE)
    pca_df <- data.frame(
      PC1 = pca$x[, 1],
      PC2 = pca$x[, 2],
      cluster = factor(km3()$cluster)
    )
    ggplot(pca_df, aes(PC1, PC2, color = cluster)) +
      geom_point(size = 2.8, alpha = 0.9) +
      scale_color_brewer(palette = "Set2") +
      labs(
        title = "PCA (2D) Projection of K-Means Clusters (k = 3)",
        x = "Principal Component 1",
        y = "Principal Component 2",
        color = "Cluster"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "right"
      )
  })

  output$cluster_summary <- renderDT({
    
    dfc <- data |>
      mutate(cluster = factor(km3()$cluster))
    all_numeric <- names(dfc)[sapply(dfc, is.numeric)]
    
    stats_tbl <- dfc |>
      group_by(cluster) |>
      summarise(
        Size = n(),
        across(all_of(all_numeric), ~ round(mean(.x), 3)),
        .groups = "drop"
      ) |>
      rename(Cluster = cluster)
    
    datatable(
      stats_tbl,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE
      )
    )
  })
  output$cluster_boxplots_all <- renderUI({
    
    dfc <- data |>
      mutate(cluster = factor(km3()$cluster))
    
    numeric_vars <- names(dfc)[sapply(dfc, is.numeric)]

    plot_list <- lapply(numeric_vars, function(v) {
      
      plotname <- paste0("box_", v)
      
      output[[plotname]] <- renderPlot({
        
        ggplot(dfc, aes(x = cluster, y = .data[[v]], fill = cluster)) +
          geom_boxplot(outlier.alpha = 0.4) +
          scale_fill_brewer(palette = "Set2") +
          labs(
            title = paste("Distribution of", v, "by Cluster"),
            x = "Cluster",
            y = v
          ) +
          theme_minimal(base_family = "Open Sans") +
          theme(
            plot.title = element_text(hjust = 0.5, size = 16),
            legend.position = "none"
          )
      })
      column(
        width = 6,
        plotOutput(plotname)
      )
    })
    fluidRow(
      tagList(plot_list)
    )
  })
  output$cluster_map_3 <- renderPlot({
    world <- map_data("world")
    clusters_df <- data.frame(
      country = data$country,
      cluster = factor(km3()$cluster)   
    )
    map_df <- world %>%
      left_join(clusters_df, by = c("region" = "country"))
    ggplot(map_df, aes(long, lat, group = group, fill = cluster)) +
      geom_polygon(color = "gray50", size = 0.2) +
      scale_fill_brewer(palette = "Set2", na.value = "lightgray") +
      coord_quickmap() +
      labs(
        title = "Choropleth Map of Clusters",
        fill = "Cluster"
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5)
      )
  })
  
  
  
  
}
shinyApp(ui = ui, server = server)