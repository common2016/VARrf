## code to prepare `regdata` dataset goes here
rm(list = ls())
library(pacman)
p_load(tsDyn,vars,randomForestSRC,bvartools,MyFun,tidyverse,devtools,
       parallel,doParallel,ggplot2)
load_all()

seed <- 1024
# 参数矩阵，第一列截距，第二列趋势项
B1 <- matrix(c(0.2, 0.1, 0.2, 0.3,0.3,0.4,0.2,0.1), 2)
sim_dt <- VAR.sim(B1, n = 200, lag = 2,include = 'none',show.parMat = T) %>%
  as.data.frame()
colnames(sim_dt) <- c('y1','y2')
# ts.plot(sim_dt)

fit <- vars::VAR(sim_dt, p = 2,type = 'none')
# summary(fit)
fit <- VARrf(sim_dt)
fit <- VARrf(sim_dt, p = 4)

# IRF_mt
VARrf_forcast(fit = fit, s = 12, n_eps = 2)
IRFrf_mt(fit, shock_varnm, s = 12, n = 10, ncores = 7)

# 绘图LP
picdata <- IRFrf(data = sim_dt, pmax = 5, s = 12, shockvar = 1)
ggplot(data = picdata, aes(x = s, y = y1)) + geom_line() +
  geom_hline(yintercept = 0) + theme_bw()

IRFrf_gen(data = sim_dt[1:20,], pmax = 3, s = 5, shockvar = 1, ncores = 7)

# 广义脉冲响应的比较
ar2ma(B1)

# panel data
regdata <- openxlsx::read.xlsx('E:\\13_PapersFromFriends\\OuYangZG\\EPUMoneyPolicy\\RCode\\data-raw/2020季度数据.xlsx',1)
ans <- dt_gen_panel(regdata, indx, s = 2, plag = 2, yname = T)

ans <- VARrf(regdata[regdata$num == 39| regdata$num == 49,], indx = c('num','year'), pmax = 15, s = 1)
picdata <- IRFrf(data = regdata[regdata$num %in% c(39.49,63,66,78),],
                 indx = c('num','year'),p = 8, s = 12, shockvar = 1)

# usethis::use_data(regdata, overwrite = TRUE)
