library(survival)
library(ggplot2)
library(ggfortify)

# 样本量
n1=1000
n2=1000

# 事件发生率
## prob of event after 1 year
pe1 = 0.1
pe2 = 0.12
## prob of loss-to-followup after 1 year
pd1 = 0.2
pd2 = 0.25

# 研究时间，years
studyduration = 3

# 产生enrollment time，根据均匀分布，0.25 years
enrolldistr = 0.25
enrollran1 = 0.25*runif(n1,0,1)
enrollran2 = 0.25*runif(n2,0,1)

# 产生event time
eventran1 = log(1-runif(n1,0,1))/log(1-pe1)
eventran2 = log(1-runif(n2,0,1))/log(1-pe2)

# 计算从study start开始到event的时间
totaleventran1 = enrollran1+eventran1
totaleventran2 = enrollran2+eventran2

# 产生loss-to-follow-up time
lossran1 = log(1-runif(n1,0,1))/log(1-pd1)
lossran2 = log(1-runif(n2,0,1))/log(1-pd2)

# 计算从study start开始到loss-to-follow-up的时间
totallossran1 = enrollran1+lossran1
totallossran2 = enrollran2+lossran2

# 考虑研究开始后，可能由于研究设置的原因发生删失
censor1 = (totallossran1<studyduration)*totallossran1+studyduration*(totallossran1>=studyduration)
censor2 = (totallossran2<studyduration)*totallossran2+studyduration*(totallossran2>=studyduration)

# 构建最终状态矩阵
matrixinitial1 = matrix(data = NA,nrow=n1,ncol = 3)
matrixinitial2 = matrix(data = NA,nrow=n2,ncol = 3)
colnames(matrixinitial1) = c("time","status","treatment")
colnames(matrixinitial2) = c("time","status","treatment")

# 数据装入最终矩阵中, 0:censor/loss-to-FU, 0:observed
matrixinitial1[,1]=pmin(totaleventran1,censor1)
matrixinitial1[,2]=totaleventran1<=censor1
matrixinitial1[,3]=rep(1,n1)

matrixinitial2[,1]=pmin(totaleventran2,censor2)
matrixinitial2[,2]=totaleventran2<=censor2
matrixinitial2[,3]=rep(2,n2)

finalmatrix = rbind(matrixinitial1,matrixinitial2)


# 分析
df_finalmatrix = as.data.frame(finalmatrix)
km_fit <- survfit(Surv(time, status) ~ treatment, data=df_finalmatrix)
autoplot(km_fit)+theme_bw()

## log-rank test
surv_test <- survdiff(Surv(time, status) ~ treatment, data=df_finalmatrix)
surv_test