suppressMessages({library(dplyr); library(readr); library(tidyr)})
d <- read_csv("C:/dev/brownlow/analysis/submission_comparison/detailed_predictions_vs_actual.csv",
              show_col_types = FALSE)
cat("rows:", nrow(d), " actual total votes:", sum(d$Actual), " our total:", round(sum(d$brownlow_votes),2), "\n")
cat("matches:", n_distinct(d$match_id), "\n\n")

ours <- d$brownlow_votes; act <- d$Actual
rivals <- c("qp","consensus","wheelo","brebbles","jt","crow","bg3","betfair")

rmse <- function(p,a) sqrt(mean((p-a)^2, na.rm=TRUE))

cat("=== 1. BASELINES: how much skill is actually in play ===\n")
cat(sprintf("  all-zero            RMSE %.5f\n", rmse(0, act)))
cat(sprintf("  flat 6/46=0.1304    RMSE %.5f\n", rmse(6/46, act)))
cat(sprintf("  ours                RMSE %.5f\n", rmse(ours, act)))
cat(sprintf("  qp (winner)         RMSE %.5f\n", rmse(d$qp, act)))
cat(sprintf("  consensus           RMSE %.5f\n", rmse(d$consensus, act)))
cat(sprintf("  perfect             RMSE 0\n\n"))

cat("=== 2. WHERE THE SQUARED ERROR LIVES (by actual votes) ===\n")
seg <- d %>% group_by(Actual) %>%
  summarise(n=n(),
            our_sse=sum((brownlow_votes-Actual)^2),
            qp_sse=sum((qp-Actual)^2),
            cons_sse=sum((consensus-Actual)^2),
            our_mean_pred=mean(brownlow_votes),
            qp_mean_pred=mean(qp), .groups="drop") %>%
  mutate(our_pct=100*our_sse/sum(our_sse), gap_vs_qp=our_sse-qp_sse)
print(as.data.frame(seg), digits=4, row.names=FALSE)
cat(sprintf("\n  TOTAL SSE  ours %.1f  qp %.1f  gap %.1f\n\n", sum(seg$our_sse), sum(seg$qp_sse), sum(seg$gap_vs_qp)))

cat("=== 3. OPTIMAL GLOBAL RESCALE (is our spread wrong?) ===\n")
# ours*a + b, unconstrained least squares
fit <- lm(act ~ ours)
cat(sprintf("  best linear recal: actual = %.4f + %.4f * ours\n", coef(fit)[1], coef(fit)[2]))
cat(sprintf("  RMSE after recal  %.5f  (vs %.5f raw, gain %.5f)\n", rmse(fitted(fit), act), rmse(ours,act), rmse(ours,act)-rmse(fitted(fit),act)))
# pure scalar shrink toward 0
as_ <- seq(0.7,1.3,0.01); rs <- sapply(as_, function(a) rmse(pmin(3,a*ours), act))
cat(sprintf("  best pure scalar  a=%.2f -> RMSE %.5f\n\n", as_[which.min(rs)], min(rs)))

cat("=== 4. CALIBRATION BY PREDICTION BIN ===\n")
cal <- d %>% mutate(bin=cut(brownlow_votes, c(-.01,.05,.15,.3,.6,1,1.5,2,2.5,3.01))) %>%
  group_by(bin) %>% summarise(n=n(), mean_pred=mean(brownlow_votes), mean_actual=mean(Actual),
                              qp_mean_pred=mean(qp), sse=sum((brownlow_votes-Actual)^2), .groups="drop") %>%
  mutate(bias=mean_pred-mean_actual)
print(as.data.frame(cal), digits=4, row.names=FALSE)

cat("\n=== 5. BLENDING: do we add information beyond the market? ===\n")
for (w in seq(0,1,0.1)) cat(sprintf("  %.1f*ours + %.1f*consensus  RMSE %.5f\n", w, 1-w, rmse(w*ours+(1-w)*d$consensus, act)))

cat("\n=== 6. WORST 15 MATCHES vs qp (SSE gap) ===\n")
m <- d %>% group_by(match_id, round_num, home_team_std, away_team_std) %>%
  summarise(our_sse=sum((brownlow_votes-Actual)^2), qp_sse=sum((qp-Actual)^2),
            .groups="drop") %>% mutate(gap=our_sse-qp_sse) %>% arrange(desc(gap))
print(as.data.frame(head(m,15)), digits=4, row.names=FALSE)
cat(sprintf("\n  top 15 matches = %.1f of our %.1f total SSE gap over 207 matches\n",
            sum(head(m,15)$gap), sum(m$gap)))

cat("\n=== 7. WORST 25 INDIVIDUAL ROWS (our squared error) ===\n")
w <- d %>% mutate(sq=(brownlow_votes-Actual)^2, qp_sq=(qp-Actual)^2) %>%
  arrange(desc(sq)) %>% head(25) %>%
  select(round_num, full_player_name, player_team_std, ours=brownlow_votes, qp, consensus, Actual, sq, qp_sq)
print(as.data.frame(w), digits=3, row.names=FALSE)

cat("\n=== 8. MISSED 3-VOTE GAMES (actual 3, we predicted low) ===\n")
m3 <- d %>% filter(Actual==3) %>% arrange(brownlow_votes) %>% head(20) %>%
  select(round_num, full_player_name, player_team_std, ours=brownlow_votes, qp, consensus, Actual)
print(as.data.frame(m3), digits=3, row.names=FALSE)

cat("\n=== 9. FALSE POSITIVES (we predicted high, actual 0) ===\n")
fp <- d %>% filter(Actual==0) %>% arrange(desc(brownlow_votes)) %>% head(20) %>%
  select(round_num, full_player_name, player_team_std, ours=brownlow_votes, qp, consensus, Actual)
print(as.data.frame(fp), digits=3, row.names=FALSE)
