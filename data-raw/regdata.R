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

# 绘图LP
picdata <- IRFrf(data = sim_dt, pmax = 5, s = 12, shockvar = 1)
ggplot(data = picdata, aes(x = s, y = y1)) + geom_line() +
  geom_hline(yintercept = 0) + theme_bw()

IRFrf_gen(data = sim_dt[1:50,], pmax = 5, s = 12, shockvar = 1, ncores = 7)

# 广义脉冲响应的比较
ar2ma(B1)




# usethis::use_data(regdata, overwrite = TRUE)
