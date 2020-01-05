mm=c(0.25,0.18,0.12,0.08)  ###I1:a24,a29
bb=c(40,50,80,100)        ###I2:a34,a39
aa=c(60,100,150,200)      ###I3:a44,a49
mm2=c(0.2,0.16,0.12,0.08)  ###I1:a24,a29
bb2=c(40,50,80,100)        ###I2:a34,a39
aa2=c(60,100,150,200)      ###I3:a44,a49

gm = 0.15 * 2
X = seq(10, 250, 1) 
df = do.call(rbind, lapply(1:4, function(i) {
  sapply(X, function(x) {
    dp = DP(x,mm[i],bb[i],aa[i])
    dp = ifelse(B$Buy+dp>1, 1-B$Buy, dp)
    dm = DP(x,mm2[i],bb2[i],aa2[i])
    # eR = dp*B$Rev - x*(B$Buy+dp)
    eR = ((B$Buy+dp) * B$Rev*(1+dm) - B$Rev*B$Buy) * gm - x
    
    ###改成折價卷的寫法，x變成x*(B$Buy+dp)
    c(i=i, x=x, eR.ALL=sum(eR), N=sum(eR>0), eR.SEL=sum(eR[eR > 0]) )
  }) %>% t %>% data.frame
})) 

df %>% gather('key','value',-i,-x) %>% 
  mutate(Instrument = paste0('I',i)) %>% 
  ggplot(aes(x=x, y=value, col=Instrument)) + 
  geom_hline(yintercept=0, linetype='dashed', col='blue') +
  geom_line(size=1.5,alpha=0.5) + 
  xlab('工具選項(成本)') + ylab('預期報償') + 
  ggtitle('行銷工具優化','假設行銷工具的效果是其成本的函數') +
  facet_wrap(~key,ncol=1,scales='free_y') + theme_bw()
