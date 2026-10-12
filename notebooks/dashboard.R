library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(naniar)
library(GGally)
library(ggcorrplot)
library(knitr)
library(broom)
library(dplyr)

# Read data directly using readr::read_csv
df <- read_csv("https://drive.google.com/uc?id=1C-jmWeo2xl6zwqqrIMMgk0VXd1GbnBJK")

num_vars <- c('nongkrong', 'belajar', 'durasi_mainhp', 'jarak', 'durasi_kampus', 'durasi_tidur', 'frekuensi_makan',
              'frekuensi_or', 'durasi_or', 'durasi_makan', 'durasi_nugas', 'umur')
cat_vars <- c('organisasi', 'bermainhp', 'kafein', 'obat', 'gender')


df2 <- df
replace_outliers_with_median <- function(data, vars) {
  for (var in vars) {
    Q1 <- quantile(data[[var]], 0.25)
    Q3 <- quantile(data[[var]], 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Replace outliers with median
    data[[var]][data[[var]] < lower_bound] <- median(data[[var]], na.rm = TRUE)
    data[[var]][data[[var]] > upper_bound] <- median(data[[var]], na.rm = TRUE)
  }
  return(data)
}

df2_clean <- replace_outliers_with_median(df2, num_vars)

#Tranformasi
df_normal <- df2_clean

var_positiveskew<-c('jarak','durasi_mainhp', 'durasi_kampus')

for (var in var_positiveskew) {
  df_normal[[var]] <- sqrt(df_normal[[var]])}

var_multimodal <- c('frekuensi_makan', 'durasi_or', 'durasi_makan', 'durasi_nugas', 'umur')
for (var in var_multimodal) {
  df_normal[[var]] <- sqrt(df_normal[[var]])}

#encoding

for (var in cat_vars) {
  df_normal[[var]] <- as.integer(factor(df_normal[[var]], levels = unique(df_normal[[var]])))
}

# Normalisasi z score
scaled_vars <- scale(df_normal[, num_vars])

# Gabungkan hasil normalisasi dengan data asli
df_normal_scaled <- cbind(df_normal[, cat_vars, drop = FALSE], scaled_vars)

#PCA
df_pca <-subset(df_normal_scaled, select = -durasi_tidur)
pca_result <- prcomp(df_pca, center = TRUE, scale. = TRUE)
eigenvalues <- pca_result$sdev^2

# REGRESI
pca_data <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2], PC3 = pca_result$x[, 3],
                       PC4 = pca_result$x[, 4], PC5 = pca_result$x[, 5], PC6 = pca_result$x[, 6], PC7 = pca_result$x[, 7])

# Menambahkan variabel response ke dataset PCA
pca_data$durasi_tidur <- df_normal_scaled$durasi_tidur

# Regresi Linear Berganda
reg_model <- lm(durasi_tidur ~ ., data = pca_data)


#---------------------------------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Analisis Pengaruh Pola Hidup terhadap Durasi Tidur pada Mahasiswa TSD Angkatan 2022"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("user")),
      menuItem("Data", tabName = "Data", icon = icon("database"),
               menuSubItem("Dataset", tabName = "Dataset"),
               menuSubItem("Preprocessing", tabName = "Preprocessing")
      ),
      menuItem("Visualisasi & Analisa", tabName = "Visualisasi & Analisa", icon = icon("bar-chart"),
               menuSubItem("Correlation", tabName = "Correlation"),
               menuSubItem("Scatter Plot", tabName = "ScatterPlot"),
               menuSubItem("Box Plot", tabName = "BoxPlot"),
               menuSubItem("Barplot", tabName = "Histogram"),
               menuSubItem("Regresi", tabName = "Regresi")
      ),
      menuItem("Kesimpulan", tabName = "Kesimpulan", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      # Home Tab
      tabItem(tabName = "Home",
              fluidRow(
                box(
                  title = div(
                    icon("users", lib = "font-awesome", class = "fa-2x", style = "float: left; margin-right: 10px;"),
                    h2("Anggota Kelompok", style = "color: white; font-weight: bold;")
                  ),
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                  style = "background: #ffffff; padding: 10px; margin-bottom: 10px; border-radius: 5px; position: relative;",
                  h4("Jihan Ashifa Hakim        (164221016)", style = "font-weight: bold;"),
                  h4("Andreas Hendra Herwanto   (164221064)", style = "font-weight: bold;"),
                  h4("Raissa Dinda Maya Sabilla (164221069)", style = "font-weight: bold;"),
                  h4("Rexwald Kamasean Kabanga  (164221073)", style = "font-weight: bold;"),
                  h4("Devina Salsabilla         (164221115)", style = "font-weight: bold;")
                )
              ),
              fluidRow(
                box(
                  title = div(
                    icon("info-circle", lib = "font-awesome", class = "fa-2x", style = "float: left; margin-right: 10px;"),
                    h2("Latar Belakang", style = "color: white; font-weight: bold;")
                  ),
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                  style = "background: #ffffff; padding: 10px; margin-bottom: 10px; border-radius: 5px; position: relative;",
                  p("Kualitas tidur memainkan peran penting dalam menjaga kesehatan dan kinerja mahasiswa, termasuk mereka dari program studi Teknologi Sains Data angkatan 2022. Jadwal kuliah yang padat dan tekanan akademik dapat menyebabkan masalah kualitas tidur yang signifikan. Mahasiswa yang mengalami gangguan tidur seringkali mengalami kesulitan belajar, kurang konsentrasi, dan penurunan kesejahteraan mental. Penelitian ini fokus menganalisis berbagai faktor seperti keikutsertaan dalam organisasi, durasi nongkrong, durasi belajar, dan aktivitas lain yang mungkin memengaruhi durasi tidur mahasiswa. Maka dari itu, kami mengambil data seputar pola hidup dari 90 orang mahasiswa TSD angkatan 2022 yang kemudian kami eksplorasi untuk mengetahui pola antara data - data tersebut. Hasil penelitian diharapkan memberikan wawasan mendalam untuk membantu meningkatkan kualitas tidur dan kesejahteraan secara keseluruhan.", style = " font-size: 16px;")
                )
              )
      ),
      # Data Tab
      tabItem(tabName = "Data",
              # Empty tab, no content
      ),
      tabItem(tabName = "Dataset",
              fluidRow(
                box(
                  icon("icon-simple", lib = "font-awesome", class = "fa-2x", style = "float: left; margin-right: 10px;"),
                  title = h2("Interactive Dataset", style = "color: white; font-weight: bold;" ),
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                  style = "background: #ffffff; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                  DTOutput("data_table", width = "100%")
                  ,  # Use DTOutput here
                  selectInput("desc_var", "Statistika Deskriptif", choices = colnames(df)),
                  tableOutput("desc_stats_table")
                )
              )
      ),
      
      # Preprocessing Tab
      tabItem(tabName = "Preprocessing",
              h2("Missing Value Check and Data Type"),
              dataTableOutput("summaryTable"),
              
              h2('Handling Outlier'),
              p("Pada tahap ini kami mengganti semua nilai outlier dengan nilai median dan berikut adalah boxplot hasil handling outlier."),
              plotOutput("boxplotOutliers"),
              
              h2("Transformasi"),
              h3("UjiSahpiro-Wilk test"),
              dataTableOutput("shapiroTable"),
              
              h3("Transformasi Akar Kuadrat"),
              h4('Sebelum Tranformasi'),
              plotOutput("densityBefore"),
              h4('Sesudah Tranformasi'),
              plotOutput("densityAfter"),
              
              h2("Encoding"),
              tableOutput("encodedDataFrame"),
              
              h2("Normalisasi Z-Score"),
              tableOutput("zScoreDataFrame"),
              
              h2("PCA Analysis"),
              h4('Output PCA'),
              verbatimTextOutput("pcaSummary"),
              h4('Eigen Value'),
              verbatimTextOutput("eigenvaluesOutput"),
              h4('Faktor PCA'),
              verbatimTextOutput("pcaRotation"),

      ),
      
      
      # Visualisasi Tab
      tabItem(tabName = "Visualisasi & Analisa",
              h2("Visualisasi Content Goes Here"),
              # Empty tab for now
              
              # New subtab for Multiple Linear Regression
              tabItem(tabName = "MultipleRegression",
                      fluidRow(
                        box(
                          title = h2("Multiple Linear Regression", style = "color: white; font-weight: bold;"),
                          width = 12,
                          solidHeader = TRUE,
                          status = "info",
                          style = "background: #ffffff; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                          # Output for Multiple Linear Regression
                          tableOutput("multiple_regression_output")
                        )
                      )
              )
      ),
      # Correlation Tab
      tabItem(tabName = "Correlation",
              fluidRow(
                box(
                  title = h2("Correlation Matrix", style = "color: white; font-weight: bold;"),
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                  style = "background: #ffffff; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                  plotlyOutput("correlation_plot"),
                  textOutput("correlation_explanation")
                )
              )
      ),
      tabItem("Histogram",
              fluidRow(
                box(
                  title = h2("Barplot", style = "color: white; font-weight: bold;"),
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                  style = "background: #ffffff; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                  selectInput("hist_cat_var", "Choose Categorical Variable", choices = cat_vars),
                  plotOutput("histogram_plot")
                )
              )
      )
      
      ,
      # Scatter Plot Tab
      tabItem(tabName = "ScatterPlot",
              fluidRow(
                box(
                  title = h2("Scatter Plot", style = "color: white; font-weight: bold;"),
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                  style = "background: #ffffff; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                  selectInput("scatter_x", "X-axis Variable", choices = num_vars),
                  selectInput("scatter_y", "Y-axis Variable", choices = num_vars),
                  plotlyOutput("scatter_plot")
                )
              )
      ),
      # Box Plot Tab
      tabItem(tabName = "BoxPlot",
              fluidRow(
                box(
                  title = h2("Box Plot Comparison", style = "color: white; font-weight: bold;"),
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                  style = "background: #ffffff; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                  selectInput("boxplot_var1", "Variable 1", choices = num_vars),
                  selectInput("boxplot_var2", "Variable 2", choices = num_vars),
                  plotlyOutput("box_plot")
                )
              )
      ),
      # Regresi tab
      tabItem(tabName = "Regresi",
              fluidRow(
                box(
                  title = h2('Regresi Linear Berganda', style = "color: white; font-weight: bold;"),
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                  style = "background: #ffffff; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                  verbatimTextOutput("regressionSummaryOutput")
                
                )
              )
            
      ),
      # Kesimpulan Tab
      tabItem(tabName = "Kesimpulan",
              h1(strong("Kesimpulan & Saran")),
              fluidRow(
                box(
                  title = h2("Kesimpulan", style = "color: white; font-weight: bold;"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  style = "background: #ffffff; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                  p("1) Berdasarkan hasil regresi sebelum dilakukan data prepocessing,  didapatkan p-value 2.2e-16, sehingga didapatkan keputusan Tolak H0. Hal ini berarti variable-variabel independen dalam analisis ini berpengaruh secara signifikan terhadap variabel dependennya yaitu durasi_tidur."),
                  p("2) Berdasarkan hasil analisis menggunakan uji korelasi, yaitu uji pearson dan chi square didapatkan bahwa pada uji korelasi pearson variabel yang cukup memiliki korelasi tinggi dengan variabel dependennya adalah durasi tidur dan durasi bermain hp sebesar 0.85. Sedangkan pada uji chi square nilai korelasi yang cukup tinggi berada di hubungan antara organisasi dan bermain hp, organisasi dan mengonsumsi kafein, organisasi dan gender, bermain hp dan mengkonsumsi obat, bermain hp dan gender, mengonsumsi kafein dan mengkonsumsi obat, mengonsumsi obat dan gender, yaitu semuanya memiliki nilai korelasi sebesar 1. Artinya kedua variabel pada kedua uji memiliki nilai hubungan yang cukup tinggi dan positif. 
"),
                  p("3) Berdasarkan hasil dari analisis regresi linier berganda didapatkan bahwa Y= 3.406e-16 + 1.152e-02(PC1) + 5.147e-02(PC2) - 6.535e-02(PC3) - 6.541e-02(PC4) + 5.615e-02 (PC5) - 1.041e-01(PC6) -  9.751e-02(PC7). Dari persamaan model tersebut yang memiliki nilai positif, yaitu PC1 dengan variabel gender dan frekuensi olahraga, PC2 dengan variabel kafein dan umur, dan PC5 dengan variabel obat dan jarak yang artinya memiliki korelasi positif dengan variabel dependennya. Dan yang memiliki nilai negatif, yaitu PC3 dengan variabel nongkrong dan durasi olahraga, PC4 dengan variabel keikutsertaan berorganisasi dan frekuensi makan, PC6 dengan variabel durasi kampus dan durasi nugas, dan PC7 dengan variabel durasi main hp dan durasi belajar yang artinya memiliki pengaruh negatif terhadap variabel dependennya.
"),
                  p("4) Berdasarkan hasil analisis yang dilakukan, solusi yang dapat kami tawarkan, yaitu meningkatkan frekuensi olahraga, mengurangi durasi nongkrong dan bermain hp, serta melakukan manajemen waktu yang baik.
")
                  
                  
                )
              ),
              fluidRow(
                box(
                  title = h2("Saran", style = "color: white; font-weight: bold;"),
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  style = "background: #ffffff; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                  p("Berdasarkan hasil analisis, kami memiliki beberapa saran dan rekomendasi untuk penelitian lanjutan terkait dengan “Analisis Faktor-Faktor yang Dapat Memengaruhi Kualitas Tidur Mahasiswa Teknologi Sains Data Angkatan 2022”. Salah satu area yang perlu diperhatikan adalah identifikasi faktor-faktor yang mungkin telah berkontribusi terhadap kinerja model yang kurang optimal. Oleh karena itu, disarankan untuk melakukan analisis lebih lanjutan untuk memahami lebih baik faktor-faktor tersebut."),
          
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  output$data_table <- renderDT({
    # Display data in an interactive table with horizontal scrolling
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), extensions = 'Scroller', callback = JS('tableScroller(table)'))
  })
  
  
  
  # Encoding Categorical Variables
  output$encoding_output <- renderDT({
    # Replace categorical variables with encoded ones in the dataset
    df_encoded <- model.matrix(~.+0, data = df[, c(num_vars, cat_vars)], 
                               contrasts.arg = lapply(df[cat_vars], contrasts, contrasts = FALSE))
    
    # Display the encoded dataset
    datatable(df_encoded, options = list(pageLength = 10))
  })
  
  output$correlation_plot <- renderPlotly({
    # Create a correlation matrix plot using ggcorrplot
    corr_plot <- ggcorrplot(cor(df[num_vars]), hc.order = TRUE, ggtheme = ggplot2::theme_minimal())
    corr_plotly <- ggplotly(corr_plot)
    corr_plotly
  })
  
  # Multiple Linear Regression using PCA
  output$multiple_regression_output <- renderTable({
    # Perform PCA on the selected variables
    selected_vars <- c('nongkrong', 'belajar', 'durasi_mainhp', 'jarak', 'durasi_kampus', 'durasi_tidur', 
                       'frekuensi_makan', 'frekuensi_or', 'durasi_or', 'durasi_makan', 'durasi_nugas', 'umur')
    
    pca_data <- df %>% select(selected_vars) %>% drop_na()
    pca_result <- prcomp(pca_data, scale. = TRUE)
    
    # Extract principal components
    pc_data <- as.data.frame(predict(pca_result))
    
    # Combine principal components and target variable
    regression_data <- cbind(pc_data, durasi_tidur = df$durasi_tidur)
    
    # Fit multiple linear regression model
    model <- lm(durasi_tidur ~ ., data = regression_data)
    
    # Display model summary
    summary_table <- broom::tidy(model)
    return(summary_table)
  })
  
  output$correlation_explanation <- renderText({
    # Explanation text for the correlation matrix
    "Pada matrix korelasi ini, kita dapat melihat ada/tidaknya dan seberapa kuatnya korelasi antara satu variabel dan lainnya. Pada matrix korelasi, kotak berwarna merah menandakan korelasi positif sedangkan biru menandakan korelasi negatif. Semakin pekat warna kotak, semakin kuat korelasinya.
     Variabel yang berkorelasi paling kuat dengan jam tidur mahasiswa adalah variabel bermain hape sebelum tidur. Beberapa variabel lain yang saling berkorelasi adalah variabel durasi olahraga dan frekuensi olahraga dengan korelasi positif yang sedang."
  })
  
  # Placeholder output for scatter plot
  output$scatter_plot <- renderPlotly({
    # Add content for scatter plot here
    plot_ly(df, x = df[[input$scatter_x]], y = df[[input$scatter_y]], mode = "markers", color = I("purple")) %>%
      layout(title = paste("Scatter Plot of", input$scatter_y, "vs", input$scatter_x),
             xaxis = list(title = input$scatter_x),
             yaxis = list(title = input$scatter_y))
  })
  
#----------- PREPOCESSING ---------------------
  output$summaryTable <- renderDataTable({
    missing_values <- sapply(df, function(x) sum(is.na(x)))
    unique_values <- sapply(df, function(x) length(unique(x)))
    data_types <- sapply(df, function(x) class(x))
    
    summary_df <- data.frame(
      Missing_Values = missing_values,
      Unique_Values = unique_values,
      Data_Type = data_types)
    
    summary_df
  })

  output$boxplotOutliers <- renderPlot({
    df2 <- df
    replace_outliers_with_median <- function(data, vars) {
      for (var in vars) {
        Q1 <- quantile(data[[var]], 0.25)
        Q3 <- quantile(data[[var]], 0.75)
        IQR <- Q3 - Q1
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        
        # Replace outliers with median
        data[[var]][data[[var]] < lower_bound] <- median(data[[var]], na.rm = TRUE)
        data[[var]][data[[var]] > upper_bound] <- median(data[[var]], na.rm = TRUE)
      }
      return(data)
    }
    
    df2_clean <- replace_outliers_with_median(df2, num_vars)

    
    # Menampilkan boxplot
    par(mfrow=c(2,6))
    for (var in num_vars) {
      boxplot(df2_clean[[var]], main=var, ylab=var, las=2)
    }
  })
  
  # Shapiro
  output$shapiroTable <- renderDataTable({
    df2 <- df
    replace_outliers_with_median <- function(data, vars) {
      for (var in vars) {
        Q1 <- quantile(data[[var]], 0.25)
        Q3 <- quantile(data[[var]], 0.75)
        IQR <- Q3 - Q1
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        
        # Replace outliers with median
        data[[var]][data[[var]] < lower_bound] <- median(data[[var]], na.rm = TRUE)
        data[[var]][data[[var]] > upper_bound] <- median(data[[var]], na.rm = TRUE)
      }
      return(data)
    }
    
    df2_clean <- replace_outliers_with_median(df2, num_vars)
    
    shapiro_wilk_results <- function(data, vars) {
      results <- data.frame()
      
      for (var in vars) {
        test_result <- shapiro.test(data[[var]])
        decision <- ifelse(test_result$p.value < 0.05, "Tolak H0", "Terima H0")
        temp_df <- data.frame(Nama_Variable = var,
                              p_value = test_result$p.value,
                              Kepustakaan = decision)
        
        results <- rbind(results, temp_df)
      }
      return(results)
    }
    
    shapiro_results_df <- shapiro_wilk_results(df2_clean, num_vars)
    shapiro_results_df
  })
  
  # Transformasi sqrt
    output$densityBefore <- renderPlot({
      
      df2 <- df
      replace_outliers_with_median <- function(data, vars) {
        for (var in vars) {
          Q1 <- quantile(data[[var]], 0.25)
          Q3 <- quantile(data[[var]], 0.75)
          IQR <- Q3 - Q1
          lower_bound <- Q1 - 1.5 * IQR
          upper_bound <- Q3 + 1.5 * IQR
          
          # Replace outliers with median
          data[[var]][data[[var]] < lower_bound] <- median(data[[var]], na.rm = TRUE)
          data[[var]][data[[var]] > upper_bound] <- median(data[[var]], na.rm = TRUE)
        }
        return(data)
      }
      
      df2_clean <- replace_outliers_with_median(df2, num_vars)
      
      #Tranformasi
      df_normal <- df2_clean
      
      var_positiveskew<-c('jarak','durasi_mainhp', 'durasi_kampus')
      
      for (var in var_positiveskew) {
        df_normal[[var]] <- sqrt(df_normal[[var]])}
        
        var_multimodal <- c('frekuensi_makan', 'durasi_or', 'durasi_makan', 'durasi_nugas', 'umur')
      for (var in var_multimodal) {
          df_normal[[var]] <- sqrt(df_normal[[var]])}
        
      par(mfrow=c(3, 4))
      for (var in num_vars) {
        plot(density(df2_clean[[var]]), main=paste(var), xlab=var, ylab="Density")
      }
    })
    
    output$densityAfter <- renderPlot({
      
      df2 <- df
      replace_outliers_with_median <- function(data, vars) {
        for (var in vars) {
          Q1 <- quantile(data[[var]], 0.25)
          Q3 <- quantile(data[[var]], 0.75)
          IQR <- Q3 - Q1
          lower_bound <- Q1 - 1.5 * IQR
          upper_bound <- Q3 + 1.5 * IQR
          
          # Replace outliers with median
          data[[var]][data[[var]] < lower_bound] <- median(data[[var]], na.rm = TRUE)
          data[[var]][data[[var]] > upper_bound] <- median(data[[var]], na.rm = TRUE)
        }
        return(data)
      }
      
      df2_clean <- replace_outliers_with_median(df2, num_vars)
      
      #Tranformasi
      df_normal <- df2_clean
      
      var_positiveskew<-c('jarak','durasi_mainhp', 'durasi_kampus')
      
      for (var in var_positiveskew) {
        df_normal[[var]] <- sqrt(df_normal[[var]])}
      
      var_multimodal <- c('frekuensi_makan', 'durasi_or', 'durasi_makan', 'durasi_nugas', 'umur')
      for (var in var_multimodal) {
        df_normal[[var]] <- sqrt(df_normal[[var]])}
      
      par(mfrow=c(3, 4))
      for (var in num_vars) {
        plot(density(df_normal[[var]]), main=paste(var), xlab=var, ylab="Density")
      }
    })
    
    
    #encoding
    output$encodedDataFrame <- renderTable({
      head(df_normal)
    }, rownames = TRUE) 
    
    output$zScoreDataFrame <- renderTable({
      head(df_normal_scaled)
    }, rownames = TRUE)
    
    output$pcaSummary <- renderPrint({
      summary(pca_result)
    })
    
    output$pcaRotation <- renderPrint({
      print(pca_result$rotation)
    })
    
    output$eigenvaluesOutput <- renderPrint({
      print(eigenvalues)
    })

#-----------------------------------------------
  
#-----------REGRESI----------------------------

      
    output$regressionSummaryOutput <- renderPrint({
      summary(reg_model)
    })
    
    
  
#-----------------------------------------------
  
  # Descriptive statistics analysis
  output$desc_stats_table <- renderTable({
    desc_var <- input$desc_var
    if (desc_var %in% num_vars) {
      desc_stats <- df %>% summarise(
        Mean = mean(!!sym(desc_var), na.rm = TRUE),
        Median = median(!!sym(desc_var), na.rm = TRUE),
        SD = sd(!!sym(desc_var), na.rm = TRUE),
        Min = min(!!sym(desc_var), na.rm = TRUE),
        Max = max(!!sym(desc_var), na.rm = TRUE),
        N = n(),
        Missing = sum(is.na(!!sym(desc_var)))
      )
    } else if (desc_var %in% cat_vars) {
      desc_stats <- df %>% group_by(!!sym(desc_var)) %>% summarise(N = n())
    } else {
      desc_stats <- data.frame(Message = "Please select a valid variable.")
    }
    desc_stats
  })
  
  # Histogram plot for categorical variable
  output$histogram_plot <- renderPlot({
    ggplot(df, aes(x = !!sym(input$hist_cat_var), fill = !!sym(input$hist_cat_var))) +
      geom_bar() +
      labs(title = paste("Histogram of", input$hist_cat_var),
           x = input$hist_cat_var,
           y = "Frequency") +
      theme_minimal()
  })
  
  # Multiple Linear Regression analysis
  output$multiple_regression_output <- renderTable({
    # Prepare data for multiple linear regression
    df_pca <- df
    df_pca[num_vars] <- scale(df_pca[num_vars])  # Standardize numeric variables for PCA
    pca_model <- prcomp(df_pca[num_vars], center = TRUE, scale. = TRUE)
    df_pca <- as.data.frame(predict(pca_model))  # Use PCA-transformed data for regression
    
    # Fit multiple linear regression
    lm_model <- lm(durasi_tidur ~ ., data = df_pca)
    
    # Extract coefficients, standard errors, t-values, and p-values
    summary_lm <- summary(lm_model)
    coef_table <- summary_lm$coefficients[, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")]
    
    # Return the result as a data frame
    coef_table
  })
  

  # Updated box plot output
  output$box_plot <- renderPlotly({
    # Create a side-by-side box plot of two variables
    box_plot <- ggplot(df, aes(x = 1, y = df[[input$boxplot_var1]], fill = input$boxplot_var1)) +
      geom_boxplot(position = position_dodge(width = 0.8)) +
      geom_boxplot(aes(x = 2, y = df[[input$boxplot_var2]], fill = input$boxplot_var2), position = position_dodge(width = 0.8)) +
      labs(title = "Side-by-Side Box Plot",
           x = "Variables",
           y = "Values") +
      scale_x_continuous(breaks = c(1, 2), labels = c(input$boxplot_var1, input$boxplot_var2)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 1)) +  # Adjust title position
      guides(fill = guide_legend(title = "Keterangan"))  # Modify legend title
    
    plotly::ggplotly(box_plot)
  })
}

shinyApp(ui = ui, server = server)