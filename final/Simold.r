library(manipulate)
# 帶有「參數」的成本效益函數
manipulate({##############################
  # 用curve指令函數設定m, b, a
  curve(DP(x,m,b,a), 0, 30, lwd=2, ylim=c(0, 0.25),
        main="F( x | m, b, a )", ylab="delta P")
  abline(h=seq(0,0.2,0.05),v=seq(0,30,5),col='lightgrey',lty=2)
},########################################
m = slider(0.05, 0.35,  0.20, step=0.01),
a = slider(  10,   40,    25, step=1),
b = slider(   4,   40,    20, step=1)
) 
# 不同參數，不同行銷工具的改變
# manipulate({##########################
#   X = seq(0,250,5)
#   data.frame(
#     Inst=paste0('Inst',i), Para=X,
#     Gain=DP(X,m,b,a)
#   ) %>% data.frame %>%
#   ggplot(aes(x=Para, y=Gain, col=Inst)) +
#   geom_line(size=1.5,alpha=0.5) + theme_bw() +
#   ggtitle("Prob. Function: f(x|m,b,a)")
#   },#################################
#   i=slider(1,5,1,step=1),
#   m=slider(0.05,0.25, 0.2,step=0.01),
#   b=slider(10,150, 20,step=5),
#   a=slider(0,200, 30,step=5)
# )
# 
# manipulate({####################################
#   do.call(rbind, lapply(seq(5,50,0.5), function(x){
#     dp = DP(x,m,b,a)
#     B %>% mutate(
#       dp = ifelse(Buy+dp>1, 1-Buy, dp),
#       eR = dp*Rev - x
#     ) %>%
#       group_by(grp) %>% summarise(
#         Cost = x,
#         Group.Sz = n(),
#         eR.ALL = sum(eR>0),
#         eR.SEL = sum(eR[eR>0]),
#       ) } ) ) %>% 
#     ggplot(aes(x=Cost, y=eR.SEL, col=factor(grp))) +
#     geom_line(size=1.2) +
#     ggtitle("Cost Effeciency per Segment ")
# },######################################
# m = slider(0.05, 0.25,  0.20, step=0.01),
# a = slider(  10,   30,    25, step=1),
# b = slider(  10,   50,   20, step=5)
# ) 
# 行銷工具
manipulate({####################################
  do.call(rbind, lapply(seq(5,50,0.5), function(x){
    dp = DP(x,m,b,a)
    B %>% mutate(
      #dp = ifelse(Buy+dp>1, 1-Buy, dp),
      eR = dp*Buy*Rev - x # 行銷工具
    ) %>%
      group_by(grp) %>% summarise(
        Cost = x,
        Group.Sz = n(),
        eR.ALL = sum(eR>0),
        eR.SEL = sum(eR[eR>0]),
      ) } ) ) %>% 
    ggplot(aes(x=Cost, y=eR.SEL, col=factor(grp))) +
    geom_line(size=1.2) +
    ggtitle("Cost Effeciency")
},######################################
m = slider(0.05, 0.25,  0.20, step=0.01),
a = slider(  10,   30,    25, step=1),
b = slider(  10,   50,   20, step=5)
) 
# manipulate({##########################
#   X = seq(10, 250, 5) 
#   df = 
#     sapply(X, function(x) {
#       dp = DP(x,m,b,a)
#       dp = ifelse(B$Buy[B$grp==i]+dp>1, 1-B$Buy[B$grp==i], dp)
#       eR = dp*B$Rev[B$grp==i] - x # 改成回購的機率
#       c(i=i, x=x, eR.ALL=sum(eR), N=sum(eR>0), eR.SEL=sum(eR[eR > 0]) )
#     ) %>% t %>% data.frame
#   df %>% gather('key','value',-i,-x) %>% 
#     mutate(Instrument = paste0('I',i)) %>% 
#     ggplot(aes(x=x, y=value, col=Instrument)) + 
#     geom_hline(yintercept=0, linetype='dashed', col='blue') +
#     geom_line(size=1.5,alpha=0.5) + 
#     xlab('工具選項(成本)') + ylab('預期報償') + 
#     ggtitle('行銷工具優化','假設行銷工具的效果是其成本的函數') +
#     facet_wrap(~key,ncol=1,scales='free_y') + theme_bw()
#   })) 
#   },#################################
#   i=slider(1,5,1,step=1),
#   m=slider(0.05,0.25, 0.2,step=0.01),
#   b=slider(10,150, 20,step=5),
#   a=slider(0,200, 30,step=5)
# )