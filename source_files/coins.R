flip_coin<-function(n, p){
  temp<-rbinom(n,1,p)
  ifelse(temp, "H","T")
}

flip_coin_number<-function(n,p){
    rbinom(n,1,p)
}

mystery_coin<-function(n){
    return(flip_coin_number(n,0.2213))
}

prob_curve<-function(sample){
    n<-length(sample)
    s<-sum(sample==1)
    phat<- s/n
    probs<-seq(0,1,by=.01)
    likelihoods<-sapply(probs, function(x){
        dbinom(s,n,x)
    })
    ggplot(data = data.frame(x = probs,
                             y = likelihoods),
           aes(x=x,y=y))+
        geom_line(color='blue')+
        geom_vline(xintercept=phat,color='red',linetype = "dashed")+
        geom_hline(yintercept = 0)+
        labs(y="Likelihood of\nMy Sample",
             x="Possible Probability of Heads",
             title="Which probability of heads maximizes the probability\nof seeing my sample?")+
        theme_bw()
}


my_flips<-mystery_coin(30)
class_flips<-mystery_coin(30*25)
