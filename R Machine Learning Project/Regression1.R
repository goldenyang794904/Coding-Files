option<-data.frame(call<-c(0,20,40,60,80,100,120,140,160),
                   x<-c(-2,-2,-2,-2,-1,0,1,2,3))  

library(ggplot2)
ggplot(data=option,aes(call,x))+
  geom_hline(yintercept=0)+
  theme_light()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank())+
  geom_line()+
  labs(x='Stock Price at Expiration',y='Payoff', title='Long Call Option')+
  theme(plot.title=element_text(hjust=.5))

options<-data.frame(sell<-c(0,1,2,3,4,5,6,7,8),
                    y<-c(2,2,2,2,1,0,-1,-2,-3))

ggplot(data=options,aes(sell,y))+
  geom_hline(yintercept=0)+
  theme_light()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank())+
  geom_line()+
  labs(x='Stock Price at Expiration',y='Payoff', title='Short Call Option')+
  theme(plot.title=element_text(hjust=.5))

