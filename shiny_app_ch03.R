# Shiny App: 回帰直線と誤差項のインタラクティブなデモンストレーション
# 実行方法: shiny::runApp("shiny_app_ch03.R")

library(shiny)
library(tidyverse)

# グローバル環境でデータを準備
set.seed(123)
n <- 15
x <- seq(1, 10, length.out = n)
y <- 3 + 1.5 * x + rnorm(n, mean = 0, sd = 2)
df <- data.frame(x = x, y = y)
my_font <- if (Sys.info()["sysname"] == "Darwin") "HiraKakuProN-W3" else "sans"

# UI定義
ui <- fluidPage(
  titlePanel("回帰直線と誤差項の可視化"),
  
  sidebarLayout(
    sidebarPanel(
      h3("パラメータ調整"),
      sliderInput(
        "intercept", 
        "切片 (Alpha):", 
        min = 0, max = 10, value = 2, step = 0.1
      ),
      sliderInput(
        "slope", 
        "傾き (Beta):", 
        min = 0, max = 3, value = 0.5, step = 0.05
      ),
      hr(),
      h4("誤差統計"),
      verbatimTextOutput("sumError"),
      verbatimTextOutput("rssError")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "誤差の可視化",
          plotOutput("distPlot", height = "600px"),
          h4("説明"),
          p("スライダーで切片と傾きを調整すると、グラフ上の直線が変わります。"),
          p("黒い点がデータ、赤い直線がモデル、青い線がそれぞれのデータポイントから直線までの誤差（残差）を表します。")
        ),
        tabPanel(
          "2種類の傾き比較",
          plotOutput("regressionPlot", height = "600px"),
          h4("説明"),
          p("mtcarsデータセットを使用した例です。スライダーで傾きを変更すると、直線の角度が変わります。")
        )
      )
    )
  )
)

# Server定義
server <- function(input, output) {
  
  # 誤差の可視化
  output$distPlot <- renderPlot({
    # 指定されたパラメータでの予測値
    df$y_pred <- input$intercept + input$slope * df$x
    
    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = "black", size = 3, alpha = 0.8) +
      geom_line(aes(y = y_pred), color = "firebrick", size = 1.2) +
      # 誤差（残差）を垂直線で描画
      geom_segment(aes(xend = x, yend = y_pred), color = "blue", alpha = 0.5, size = 0.8) +
      theme_minimal(base_family = my_font, base_size = 12) +
      labs(
        title = "回帰直線と誤差項の可視化",
        subtitle = paste("モデル: y =", round(input$intercept, 2), "+", round(input$slope, 2), "* x"),
        x = "従業員数 (X)",
        y = "売上高 (Y)"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12)
      )
  })
  
  # 誤差統計の表示
  output$sumError <- renderPrint({
    errors <- df$y - (input$intercept + input$slope * df$x)
    sum_e <- sum(errors)
    cat("誤差項の合計 (Sum of Errors):\n")
    cat(sprintf("%.4f\n\n", sum_e))
  })
  
  output$rssError <- renderPrint({
    errors <- df$y - (input$intercept + input$slope * df$x)
    rss <- sum(errors^2)
    cat("残差平方和 (RSS):\n")
    cat(sprintf("%.4f\n", rss))
  })
  
  # 傾きの変更によるグラフの動的更新
  output$regressionPlot <- renderPlot({
    ggplot(mtcars, aes(x = wt, y = mpg)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_abline(slope = input$slope, intercept = 30, color = "blue", size = 1.2) +
      theme_minimal(base_family = my_font, base_size = 12) +
      labs(
        title = "mtcarsデータでの傾きの比較",
        subtitle = paste("傾き (slope) =", round(input$slope, 2)),
        x = "重量 (wt)",
        y = "燃費 (mpg)"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12)
      )
  })
}

# アプリの実行
shinyApp(ui, server)
