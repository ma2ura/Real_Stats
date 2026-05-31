pacman::p_load(
    tidyverse,
    modelsummary,
    knitr, 
    haven, 
    AER)

opts_chunk$set(echo = TRUE)
options(digits = 3)

# Remove objects from the previous session
rm(list = ls(all = TRUE))

## Load data; data saved as object named "dta"
load(file = "data/Ch3_ChapterExample_Incumbent_vote.RData")
IncVote = lm(dta$Vote100 ~ dta$RDI)
summary(IncVote)


load(file = "data/Ch3_ChapterExample_HeightAndWages.RData") 
HeightWageBivar = lm(dta$wage96 ~ dta$height85)
summary(HeightWageBivar)

results <- list(IncVote, HeightWageBivar)

modelsummary(results)

coeftest(HeightWageBivar, vcov = vcovHC(HeightWageBivar, type = "HC1"))


summary(HeightWageBivar)$sigma^2
## [1] 142
summary(HeightWageBivar)$sigma
## [1] 11.9
summary(HeightWageBivar)$r.squared
## [1] 0.00927

X = seq(-5 * 0.5, 5 * 0.5, length = 100)
NormY = dnorm(X/0.5)
tY  = dt(X/0.5, df=15)

par(mfrow=c(1, 1), mar=c(3.5, 3.0, 1, 1.5), oma=c(0.2, 1, 1.5, 0.3))     # mar(south, west, north, east)
plot(X, tY, type="l", lty=1,xlab="", ylab="", xaxt='n', yaxt='n', lwd = 1.0, col="#3c78c3", xlim = c(-2.5, 2.75))

mtext("Probability\ndensity", las = 1, side = 3, at = -3.1, line = -1.3, cex = 0.9) 

axis(1, at=c(-2, -1, 0, 1, 2),      labels=seq(-2, 2), cex.axis=0.85, padj=-0.1)

mtext(expression(paste(hat(beta))[1]), side = 1, line = 2.5, cex=1.0)

text(1.25, 0.30, expression(paste("Distribution of ", hat(beta)[1], " under the")),     cex=0.85, col="#3c78c3")
    
text(1.25, 0.28, expression(paste("null hypothesis that ", beta[1], "=0")),             cex=0.85, col="#3c78c3")
    
text(1.25, 0.26, "(standard error is 0.55)",                                cex=0.85, col="#3c78c3")

abline(h = 0.00,   lwd=0.7, col= "darkgrey") # add horizontal line at 0    

points(c(2.2, 2.2), c(-0.1, 0.10), type ="l", lwd =2, col="#FF7F24") ##FF7F24
text(2.4, 0.090, "Actual", cex=0.7, col="#FF7F24")
text(2.38, 0.080, "value", cex=0.7, col="#FF7F24")
text(2.38, 0.068, expression(paste("of ", hat(beta)[1])), cex=0.7, col="#FF7F24")
axis(1, at= 2.2,        labels= 2.20, cex.axis=0.65, padj=-1.8, lwd.ticks= 2, col.ticks="#FF7F24", col.axis="#FF7F24")

points(c(-0.3, -0.3), c(-0.1, 0.10), type = "l", lwd = 2, col="darkgreen")
    
text(0.11, 0.105, expression(paste("Example of ", hat(beta)[1])), cex=0.7, col="darkgreen")
    
text(0.10, 0.090, expression(paste("for which we")), cex=0.7, col="darkgreen")
text(0.06, 0.075, expression(paste("would fail to")), cex=0.7, col="darkgreen")
text(0.10, 0.060, expression(paste("reject the null")), cex=0.7, col="darkgreen")
axis(1, at = -0.3, labels= -0.3, cex.axis=0.65, padj=-1.7, lwd.ticks= 2, col.ticks="darkgreen", col.axis="darkgreen")

# Power curve

Beta = seq(0, 10.5, 0.1)
seBeta  = 1
PowerC1 = pnorm(Beta /1.0*seBeta - 2.32)
PowerC2 = pnorm(Beta /2.0*seBeta - 2.32)
pnorm(2/0.9*seBeta - 2.32)
## [1] 0.461
#cbind(PowerC1, PowerC2, Beta)

par(mfrow=c(1, 1), mar=c(2.5, 2.8, 0.5, 1.5), oma=c(0.2, 3.75, 1.,0.3))  # mar(south, west, north, east)
plot(Beta, PowerC1, type="l", lty=1, xlab="", xaxt='n',  yaxt='n', ylim= c(-0.01, 1), ylab="", lwd = 1.0, col="#3c78c3")
abline(v = 2.0, col="grey", lwd=0.4, lty = 2) # add vertical line
text(5.5, 0.96, expression(paste("Power curve for")), cex=0.75, col="#3c78c3")
text(5.3, 0.92, expression(paste("se(", hat(beta)[1], ") = 1.0")), cex=0.75, col="#3c78c3")
lines(Beta, PowerC2, lty=2, lwd=2.0, col="#FF7F24")
text(6.5, 0.60, expression(paste("Power curve for")), cex=0.75, col="#FF7F24")
text(6.2, 0.56, expression(paste("se(", hat(beta)[1], ") = 2.0")), cex=0.75, col="#FF7F24")
points(c(-0.5, 2), c(pnorm(2/1.0*seBeta - 2.32), pnorm(2/1.0*seBeta - 2.32)), type ="l", lty= 2, col="#3c78c3")
points(2, pnorm(2/1.0*seBeta - 2.32), cex=1.2, pch=19, col = "#3c78c3")
axis(2, las = 1, pnorm(2/1.0*seBeta - 2.32), labels =  0.37, tick = T, 
    cex.axis = .70, mgp = c(2,.6,0), col.axis = "#3c78c3")

points(c(-0.5, 2), c(pnorm(2/2.0*seBeta - 2.32), pnorm(2/2.0*seBeta - 2.32)), type ="l", lty= 2, col="#FF7F24")
points(2, pnorm(2/2.0*seBeta - 2.32), cex=1.2, pch=19, col = "#FF7F24")
axis(1, at=0:10,    labels=0:10, cex.axis=0.85, padj=-1.2)
axis(2, las = 1, at = seq(0.0, 1 , by=0.25), labels =  seq(0.0, 1 , by=0.25), tick = T, 
    cex.axis = .75, mgp = c(2,.6,0))
axis(2, las = 1, pnorm(2/2.0*seBeta - 2.32), labels =  0.09, tick = T, 
    cex.axis = .70, mgp = c(2,.6,0), col.axis = "#FF7F24" )

mtext("Probability of",     side = 2, line = 2.1,       at = 0.95, cex=0.8, las = 1)
mtext("rejecting the",      side = 2, line = 2.2,       at = 0.91, cex=0.8, las = 1)
mtext("null hypothesis",    side = 2, line = 1.7,       at = 0.87, cex=0.8, las = 1)
mtext(expression(paste("for ", alpha, " = 0.01")), side = 2, line = 2.3, at = 0.83, cex=0.8, las = 1)
mtext(expression(paste(beta)[1]), side = 1, line = 1.3, cex=1.0)




# gemini修正

# ==============================================================================
# パッケージの読み込みと環境設定
# ==============================================================================
pacman::p_load(
  tidyverse,      # データ操作・ggplot2での描画
  modelsummary,   # 美しい回帰表の出力
  knitr,          # チャンクオプション制御
  haven,          # データインポート
  AER,            # 計量経済学用パッケージ (coeftest, vcovHC)
  patchwork       # 複数のggplotを綺麗に並べる用
)

knitr::opts_chunk$set(echo = TRUE)
options(digits = 3)

# ワークスペースの初期化
rm(list = ls(all = TRUE))

# ==============================================================================
# 1. 回帰分析の実行
# ==============================================================================

# (1) 現職候補の得票率モデル
load(file = "data/Ch3_ChapterExample_Incumbent_vote.RData")
# lm関数内では `data =` 引数を指定するとコードがすっきりします
IncVote <- lm(Vote100 ~ RDI, data = dta)

# (2) 身長と賃金のモデル
load(file = "data/Ch3_ChapterExample_HeightAndWages.RData") 
HeightWageBivar <- lm(wage96 ~ height85, data = dta)

# 推定結果の一覧表示（modelsummaryの標準機能を利用）
results <- list(
  "Incumbent Vote" = IncVote, 
  "Height and Wages" = HeightWageBivar
)
modelsummary(results, stars = TRUE)

# 異分散強健（堅牢）標準誤差の算出
coeftest(HeightWageBivar, vcov = vcovHC(HeightWageBivar, type = "HC1"))

# 適合度の確認
cat("残差分散 (sigma^2):", summary(HeightWageBivar)$sigma^2, "\n")
cat("残差標準誤差 (sigma):", summary(HeightWageBivar)$sigma, "\n")
cat("決定係数 (R^2):", summary(HeightWageBivar)$r.squared, "\n")


# ==============================================================================
# 2. グラフ描画1: 帰無仮説のもとでの推定量 \hat{\beta}_1 の分布
# ==============================================================================

# パラメータの設定
se_beta1 <- 0.55
df_t <- 15

plot_dist <- ggplot(data = data.frame(x = c(-2.5, 2.75)), aes(x = x)) +
  # 帰無仮説 H0: beta1 = 0 のもとでのt分布（または正規分布）
  stat_function(
    fun = function(x) dt(x / se_beta1, df = df_t) / se_beta1,
    geom = "area", fill = "#3c78c3", alpha = 0.1
  ) +
  stat_function(
    fun = function(x) dt(x / se_beta1, df = df_t) / se_beta1,
    color = "#3c78c3", linewidth = 1
  ) +
  # 基準線（x = 0）
  geom_vline(xintercept = 0, color = "darkgrey", linetype = "solid") +
  # 実際の推定値 (2.2) のライン
  geom_vline(xintercept = 2.2, color = "#FF7F24", linewidth = 1.2) +
  # 棄却できない推定値の例 (-0.3) のライン
  geom_vline(xintercept = -0.3, color = "darkgreen", linewidth = 1.2) +
  # テキスト注釈の追加 (annotateを用いることで直感的に位置を指定できます)
  annotate(
    "text", x = 1.2, y = 0.6, color = "#3c78c3", hjust = 0, size = 3.5,
    label = paste0("Distribution of hat(beta)[1] under H[0]: beta[1] == 0\n(se = ", se_beta1, ")"),
    parse = TRUE
  ) +
  annotate(
    "text", x = 2.3, y = 0.2, color = "#FF7F24", hjust = 0, size = 3,
    label = "Actual value\nof hat(beta)[1] (2.20)"
  ) +
  annotate(
    "text", x = -0.4, y = 0.2, color = "darkgreen", hjust = 1, size = 3,
    label = "Example where we\nfail to reject H[0] (-0.3)"
  ) +
  # 軸とラベルの設定
  scale_x_continuous(breaks = c(-2, -1, -0.3, 0, 1, 2, 2.2)) +
  labs(
    x = expression(hat(beta)[1]),
    y = "Probability density"
  ) +
  theme_minimal()

# ==============================================================================
# 3. グラフ描画2: 検出力曲線 (Power Curve)
# ==============================================================================

# 検出力関数のデータフレーム作成
alpha_level <- 0.01
z_crit <- qnorm(1 - alpha_level) # 片側検定（コードの2.32に対応）

power_data <- data.frame(Beta = seq(0, 10.5, 0.1)) %>%
  mutate(
    Power1 = pnorm(Beta / 1.0 - z_crit),
    Power2 = pnorm(Beta / 2.0 - z_crit)
  )

# 特定の対立仮説（beta1 = 2）における検出力の値
p_val1 <- pnorm(2 / 1.0 - z_crit)
p_val2 <- pnorm(2 / 2.0 - z_crit)

plot_power <- ggplot(power_data, aes(x = Beta)) +
  # 各標準誤差における検出力曲線
  geom_line(aes(y = Power1), color = "#3c78c3", linewidth = 1) +
  geom_line(aes(y = Power2), color = "#FF7F24", linewidth = 1, linetype = "dashed") +
  # 基準線 (Beta = 2)
  geom_vline(xintercept = 2.0, color = "grey", linetype = "dotted") +
  # 交点のプロットと補助線
  geom_point(aes(x = 2, y = p_val1), color = "#3c78c3", size = 3) +
  geom_segment(aes(x = 0, xend = 2, y = p_val1, yend = p_val1), color = "#3c78c3", linetype = "dotted") +
  geom_point(aes(x = 2, y = p_val2), color = "#FF7F24", size = 3) +
  geom_segment(aes(x = 0, xend = 2, y = p_val2, yend = p_val2), color = "#FF7F24", linetype = "dotted") +
  # テキスト注釈
  annotate("text", x = 5.5, y = 0.90, label = "se(hat(beta)[1]) == 1.0", color = "#3c78c3", parse = TRUE, size = 3.5) +
  annotate("text", x = 6.5, y = 0.50, label = "se(hat(beta)[1]) == 2.0", color = "#FF7F24", parse = TRUE, size = 3.5) +
  # 軸の設定
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(
    breaks = c(seq(0, 1, 0.25), p_val1, p_val2),
    labels = function(x) sprintf("%.2f", x)
  ) +
  labs(
    x = expression(beta[1]),
    y = paste("Probability of rejecting H[0]\n(for alpha =", alpha_level, ")")
  ) +
  theme_minimal()

# ==============================================================================
# 4. グラフの表示
# ==============================================================================
# patchworkパッケージにより、2つのグラフをスマートに縦に並べます
plot_dist / plot_power
