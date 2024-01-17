flip_coin_number<-function(n,p){
    rbinom(n,1,p)
}

mystery_coin<-function(n){
    return(flip_coin_number(n,0.2213))
}

my_flips<-mystery_coin(30)
class_flips<-mystery_coin(30*25)


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

HT_regions_binomial<-function(){
    plot_df<-data.frame(
        space = 0:30,
        pmf = dbinom(0:30, 30, p=0.5)
    )
    c<-10
    
    g1<-ggplot(data = plot_df,
           aes(x = space,
               y = pmf))+
        geom_col(color='gray3',fill="dodgerblue",alpha=.7)+
        scale_x_continuous(breaks = seq(0,30,5))+
        theme_bw()+
        geom_col(data = subset(plot_df,space<=c), 
                 aes(x = space, y =pmf), fill = 'red',alpha =.8)+
        labs(x = "Space of Assumed Model",
             y = "Probabilities",
             title = "Left-tailed test",
             subtitle = "Observed 10 heads, P(X<=10)")

    g2<-ggplot(data = plot_df,
               aes(x = space,
                   y = pmf))+
        geom_col(color='gray3',fill="dodgerblue",alpha=.7)+
        scale_x_continuous(breaks = seq(0,30,5))+
        theme_bw()+
        geom_col(data = subset(plot_df,space>=20), 
                 aes(x = space, y =pmf), fill = 'red',alpha =.8)+
        labs(x = "Space of Assumed Model",
             y = "Probabilities",
             title = "Right-tailed test",
             subtitle = "Observed 20 heads, P(X>=20)")
    
    g3<-ggplot(data = plot_df,
               aes(x = space,
                   y = pmf))+
        geom_col(color='gray3',fill="dodgerblue",alpha=.7)+
        scale_x_continuous(breaks = seq(0,30,5))+
        theme_bw()+
        geom_col(data = subset(plot_df,abs(space-15) >= abs(c-15)), 
                 aes(x = space, y =pmf), fill = 'red',alpha =.8)+
        labs(x = "Space of Assumed Model",
             y = "Probabilities",
             title = "Two-tailed test: Observed 10 heads",
             subtitle = "P(|X-15|>|15-10|) = P(|X-15|>=5) = P(X<=10) + P(X>=20)")
    
    layout1<-"
    AABB
    AABB
    CCCC
    CCCC
    "
    g1+g2+g3 +
        plot_layout(design = layout1)
}


