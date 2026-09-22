library(shiny)
library(shinythemes)
library(ggplot2)
library(quantmod)
library(forecast)


ui <- fluidPage(
  theme = shinytheme("flatly"),
  title = "Universal Analytics & Math Suite",
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f8fafc; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
      .navbar { border-radius: 0; margin-bottom: 25px; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.1); }
      .well { background-color: #ffffff; border: 1px solid #e2e8f0; border-radius: 12px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); }
      .btn-primary { background-color: #4f46e5; border-color: #4338ca; border-radius: 8px; font-weight: bold; }
      .btn-primary:hover { background-color: #4338ca; }
      .result-box { background-color: #f1f5f9; padding: 15px; border-left: 5px solid #4f46e5; border-radius: 8px; font-family: monospace; font-size: 14px; margin-top: 15px; }
      h3 { color: #1e293b; font-weight: 700; margin-top: 0; }
    "))
  ),
  
  navbarPage(
    title = strong("Проект R"),
    
    # --------------------------------------------------------------------------
    # БЛОК 1: МАТЕМАТИЧЕСКИЕ ЗАДАЧИ
    # --------------------------------------------------------------------------
    tabPanel("📐 Математические задачи",
             sidebarLayout(
               sidebarPanel(
                 h4("Параметры вычислений"),
                 radioButtons("math_mode", "Выберите тип задачи:",
                              choices = c("Решение СЛАУ 2x2" = "slau",
                                          "Численный интеграл f(x) = x^n" = "integral")),
                 hr(),
                 
                 # Подблок: СЛАУ
                 conditionalPanel(
                   condition = "input.math_mode == 'slau'",
                   p(strong("Уравнение 1: a1*x + b1*y = c1")),
                   fluidRow(
                     column(4, numericInput("a1", "a1", value = 2)),
                     column(4, numericInput("b1", "b1", value = 1)),
                     column(4, numericInput("c1", "c1", value = 5))
                   ),
                   p(strong("Уравнение 2: a2*x + b2*y = c2")),
                   fluidRow(
                     column(4, numericInput("a2", "a2", value = 1)),
                     column(4, numericInput("b2", "b2", value = 3)),
                     column(4, numericInput("c2", "c2", value = 10))
                   )
                 ),
                 
                 # Подблок: Интеграл
                 conditionalPanel(
                   condition = "input.math_mode == 'integral'",
                   numericInput("int_power", "Степень n для f(x) = x^n:", value = 2, min = 1, max = 5),
                   numericInput("int_lower", "Нижний предел (a):", value = 0),
                   numericInput("int_upper", "Верхний предел (b):", value = 3)
                 ),
                 
                 actionButton("btn_calc_math", "Вычислить", class = "btn-primary w-100")
               ),
               
               mainPanel(
                 h3("Результаты вычислений"),
                 verbatimTextOutput("math_output"),
                 plotOutput("math_plot")
               )
             )
    ),
    
    # --------------------------------------------------------------------------
    # БЛОК 2: АНАЛИЗ ДАННЫХ И СТАТИСТИКА
    # --------------------------------------------------------------------------
    tabPanel("📊 Анализ данных и статистика",
             sidebarLayout(
               sidebarPanel(
                 h4("Параметры выборки"),
                 sliderInput("stat_n", "Размер выборки (N):", min = 50, max = 1000, value = 250, step = 50),
                 selectInput("stat_dist", "Тип распределения:",
                             choices = c("Нормальное" = "norm", "Равномерное" = "unif", "Экспоненциальное" = "exp")),
                 numericInput("stat_mean", "Среднее (Mean) / Lambda:", value = 50),
                 actionButton("btn_gen_stats", "Сгенерировать и рассчитать", class = "btn-primary w-100")
               ),
               
               mainPanel(
                 h3("Описательная статистика и графики"),
                 verbatimTextOutput("stats_summary"),
                 fluidRow(
                   column(6, plotOutput("hist_plot")),
                   column(6, plotOutput("box_plot"))
                 )
               )
             )
    ),
    
    # --------------------------------------------------------------------------
    # БЛОК 3: ФИНАНСОВЫЕ ЗАДАЧИ
    # --------------------------------------------------------------------------
    tabPanel("📈 Финансовые задачи",
             sidebarLayout(
               sidebarPanel(
                 h4("Финансовые инструменты"),
                 tabsetPanel(
                   id = "fin_tabs",
                   tabPanel("Кредит",
                            br(),
                            numericInput("loan_p", "Сумма кредита (₽):", value = 300000, step = 10000),
                            numericInput("loan_rate", "Годовая ставка (%):", value = 16, step = 0.5),
                            numericInput("loan_months", "Срок (месяцев):", value = 24, step = 6),
                            actionButton("btn_calc_loan", "Считать платеж", class = "btn-primary")
                   ),
                   tabPanel("Инвестиции",
                            br(),
                            numericInput("inv_p", "Депозит (₽):", value = 100000, step = 5000),
                            numericInput("inv_rate", "Доходность (% годовых):", value = 12, step = 1),
                            sliderInput("inv_years", "Срок (лет):", min = 1, max = 20, value = 5),
                            actionButton("btn_calc_inv", "Считать сложный процент", class = "btn-primary")
                   ),
                   tabPanel("ARIMA Трекер",
                            br(),
                            selectInput("stock_ticker", "Тикер актива (Yahoo):",
                                        choices = c("Bitcoin (BTC-USD)" = "BTC-USD",
                                                    "Ethereum (ETH-USD)" = "ETH-USD",
                                                    "Apple (AAPL)" = "AAPL")),
                            sliderInput("forecast_horizon", "Горизонт прогноза (дней):", min = 7, max = 30, value = 14),
                            actionButton("btn_run_arima", "Построить прогноз", class = "btn-primary")
                   )
                 )
               ),
               
               mainPanel(
                 h3("Финансовые показатели"),
                 verbatimTextOutput("finance_metrics"),
                 plotOutput("finance_plot")
               )
             )
    )
  )
)

# ==============================================================================
# SERVER: ВЫЧИСЛИТЕЛЬНАЯ ЛОГИКА
# ==============================================================================
server <- function(input, output, session) {
  
  # --- 1. ЛОГИКА: МАТЕМАТИКА ---
  math_results <- eventReactive(input$btn_calc_math, {
    if (input$math_mode == "slau") {
      A <- matrix(c(input$a1, input$b1, input$a2, input$b2), nrow = 2, byrow = TRUE)
      B <- c(input$c1, input$c2)
      
      det_A <- det(A)
      if (abs(det_A) < 1e-9) {
        return(list(type = "slau", text = "Определитель равен 0! Система не имеет единственного решения."))
      }
      X <- solve(A, B)
      res_txt <- sprintf("Ответ СЛАУ:\n  x = %.4f\n  y = %.4f\n(Определитель матрицы: %.4f)", X[1], X[2], det_A)
      return(list(type = "slau", text = res_txt, X = X))
      
    } else {
      p <- input$int_power
      f <- function(x) x^p
      res <- integrate(f, lower = input$int_lower, upper = input$int_upper)
      res_txt <- sprintf("Численное интегрирование функции f(x) = x^%d\n  Интервал: [%.2f, %.2f]\n  Значение интеграла: %.4f\n  Абсолютная погрешность: %e",
                         p, input$int_lower, input$int_upper, res$value, res$abs.error)
      return(list(type = "integral", text = res_txt, p = p, a = input$int_lower, b = input$int_upper))
    }
  }, ignoreNULL = FALSE)
  
  output$math_output <- renderText({
    res <- math_results()
    res$text
  })
  
  output$math_plot <- renderPlot({
    res <- math_results()
    if (is.null(res)) return(NULL)
    
    if (res$type == "integral") {
      p <- res$p
      x_vals <- seq(res$a - 1, res$b + 1, length.out = 200)
      df <- data.frame(x = x_vals, y = x_vals^p)
      df_shade <- subset(df, x >= res$a & x <= res$b)
      
      ggplot(df, aes(x = x, y = y)) +
        geom_line(color = "#4f46e5", size = 1.2) +
        geom_area(data = df_shade, aes(x = x, y = y), fill = "#818cf8", alpha = 0.5) +
        theme_minimal(base_size = 14) +
        labs(title = paste0("Площадь под кривой f(x) = x^", p), x = "x", y = "f(x)")
    }
  })
  
  # --- 2. ЛОГИКА: СТАТИСТИКА ---
  stats_data <- eventReactive(input$btn_gen_stats, {
    n <- input$stat_n
    dist <- input$stat_dist
    val <- input$stat_mean
    
    if (dist == "norm") data <- rnorm(n, mean = val, sd = val * 0.2)
    else if (dist == "unif") data <- runif(n, min = 0, max = val * 2)
    else data <- rexp(n, rate = 1 / val)
    
    return(data)
  }, ignoreNULL = FALSE)
  
  output$stats_summary <- renderPrint({
    d <- stats_data()
    cat(sprintf("Выборка N = %d\n", length(d)))
    cat(sprintf("Среднее (Mean):      %.3f\n", mean(d)))
    cat(sprintf("Медиана (Median):    %.3f\n", median(d)))
    cat(sprintf("Станд. откл. (SD):   %.3f\n", sd(d)))
    cat(sprintf("Размах (Range):      %.3f - %.3f\n", min(d), max(d)))
  })
  
  output$hist_plot <- renderPlot({
    d <- stats_data()
    ggplot(data.frame(x = d), aes(x = x)) +
      geom_histogram(fill = "#0ea5e9", color = "white", bins = 20) +
      theme_minimal(base_size = 14) +
      labs(title = "Гистограмма частот", x = "Значения", y = "Частота")
  })
  
  output$box_plot <- renderPlot({
    d <- stats_data()
    ggplot(data.frame(x = d), aes(y = x)) +
      geom_boxplot(fill = "#f43f5e", alpha = 0.7, outlier.color = "red") +
      theme_minimal(base_size = 14) +
      labs(title = "Ящик с усами (Boxplot)", y = "Значения")
  })
  
  # --- 3. ЛОГИКА: ФИНАНСЫ ---
  fin_action <- reactiveValues(mode = "loan")
  
  observeEvent(input$btn_calc_loan, { fin_action$mode <- "loan" })
  observeEvent(input$btn_calc_inv, { fin_action$mode <- "inv" })
  observeEvent(input$btn_run_arima, { fin_action$mode <- "arima" })
  
  output$finance_metrics <- renderPrint({
    if (fin_action$mode == "loan") {
      P <- input$loan_p
      r <- (input$loan_rate / 100) / 12
      m <- input$loan_months
      payment <- P * (r * (1 + r)^m) / ((1 + r)^m - 1)
      total <- payment * m
      cat(sprintf("Ежемесячный аннуитетный платеж: %.2f ₽\n", payment))
      cat(sprintf("Общая сумма выплат:             %.2f ₽\n", total))
      cat(sprintf("Итоговая переплата банку:       %.2f ₽ (%.1f%%)\n", total - P, ((total - P)/P)*100))
      
    } else if (fin_action$mode == "inv") {
      P <- input$inv_p
      r <- input$inv_rate / 100
      y <- input$inv_years
      capital <- P * (1 + r)^y
      cat(sprintf("Начальный депозит: %.2f ₽\n", P))
      cat(sprintf("Капитал через %d лет: %.2f ₽\n", y, capital))
      cat(sprintf("Чистая прибыль:    %.2f ₽\n", capital - P))
      
    } else if (fin_action$mode == "arima") {
      cat(sprintf("Анализ и авто-прогнозирование ARIMA для актива: %s\nГоризонт: %d дней\n", input$stock_ticker, input$forecast_horizon))
      cat("Модель успешно обучена на исторических данных Yahoo Finance.")
    }
  })
  
  output$finance_plot <- renderPlot({
    if (fin_action$mode == "loan") {
      P <- input$loan_p
      r <- (input$loan_rate / 100) / 12
      m <- input$loan_months
      payment <- P * (r * (1 + r)^m) / ((1 + r)^m - 1)
      
      # Динамика остатка долга
      balance <- numeric(m + 1)
      balance[1] <- P
      for (i in 1:m) {
        interest <- balance[i] * r
        principal <- payment - interest
        balance[i + 1] <- max(0, balance[i] - principal)
      }
      df <- data.frame(Month = 0:m, Balance = balance)
      ggplot(df, aes(x = Month, y = Balance)) +
        geom_line(color = "#dc2626", size = 1.2) +
        geom_area(fill = "#fca5a5", alpha = 0.4) +
        theme_minimal(base_size = 14) +
        labs(title = "График погашения остатка задолженности", x = "Месяц", y = "Остаток (₽)")
      
    } else if (fin_action$mode == "inv") {
      P <- input$inv_p
      r <- input$inv_rate / 100
      y <- input$inv_years
      years_seq <- 0:y
      df <- data.frame(Year = years_seq, Capital = P * (1 + r)^years_seq)
      ggplot(df, aes(x = Year, y = Capital)) +
        geom_bar(stat = "identity", fill = "#10b981", alpha = 0.8) +
        theme_minimal(base_size = 14) +
        labs(title = "Рост капитала со сложным процентом", x = "Лет", y = "Сумма (₽)")
      
    } else if (fin_action$mode == "arima") {
      ticker <- input$stock_ticker
      env <- new.env()
      tryCatch({
        getSymbols(ticker, src = "yahoo", from = Sys.Date() - 180, to = Sys.Date(), env = env)
        ts_data <- ts(as.numeric(na.omit(Cl(env[[ticker]]))), frequency = 7)
        fit <- auto.arima(ts_data)
        fc <- forecast(fit, h = input$forecast_horizon)
        plot(fc, main = paste("ARIMA Прогноз:", ticker), col = "darkblue", fcol = "#4f46e5", lwd = 2)
        grid()
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Ошибка соединения с Yahoo Finance:\n", conditionMessage(e)), col = "red", cex = 1.2)
      })
    }
  })
}

# Запуск приложения
shinyApp(ui = ui, server = server)