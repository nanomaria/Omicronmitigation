
Rmax = 2.2
Rmax_alpha = Rmax*1.5
Rmax_delta = Rmax_alpha*1.6
Rmax_omicron = Rmax_delta*3.3

Rl = 0.1
Rl_alpha = Rl*1.5
Rl_delta = Rl_alpha*1.6
Rl_omicron = Rl_delta*3.3
Rlv = c(Rl,Rl_alpha,Rl_delta,Rl_omicron)


gamma = 1/7
beta = Rmax*gamma
beta_alpha = Rmax_alpha*gamma
beta_delta = Rmax_delta*gamma
beta_omicron = Rmax_omicron*gamma



t = 2

#Is = exp((beta-gamma)*t)
#Is_alpha = exp((beta_alpha-gamma)*t)
#Is_delta = exp((beta_delta-gamma)*t)
#Is_omicron = exp((beta_omicron-gamma)*t)
#Isv = c(Is,Is_alpha,Is_delta,Is_omicron)
Ib = 50
Isv = c(Ib,Ib,Ib,Ib)
Is = Ib
Is_alpha = Ib
Is_delta = Ib
Is_omicron = Ib

If = 1

Tl = log(If/Is)/(Rl*gamma - gamma)
Tl_alpha = log(If/Is_alpha)/(Rl_alpha*gamma - gamma)
Tl_delta = log(If/Is_delta)/(Rl_delta*gamma - gamma)
Tl_omicron = log(If/Is_omicron)/(Rl_omicron*gamma - gamma)


m = 1/(1/180)
m_alpha = 1/(1/m*1.5)
m_delta = 1/(1/m_alpha*1.6)
m_omicron = 1/(1/m_delta*3.3)



Tlv = c(Tl,Tl_alpha,Tl_delta,Tl_omicron)
mv = c(m,m_alpha,m_delta,m_omicron)
#mv = c(m,m,m,m)

betahat = 1*gamma
betahat_alpha = betahat*1.5
betahat_delta = betahat_alpha*1.6
betahat_omicron = betahat_delta*3.3
betahatv = c(betahat,betahat_alpha,betahat_delta,betahat_omicron)


Free = (mv-Tlv)/mv*100
#Free = 1/(Tlv/(mv-Tlv))
Free[4] = 0


#L_supp = (1-betahatv)^2*mv
#L_el = (1-Rlv*gamma)^2*Tlv
#Cost = L_supp/L_el

H = 0.023 #percent hospitalization for wildtype

H.alpha.0 = H*1.67
H.alpha.1 = H*0.78
#H.alpha.b

H.delta.0 = H*2.39
H.delta.1 = H*0.93
H.delta.2 = H*0.75

H.omicron.0 = H*0.79
H.omicron.1 = H*0.49
H.omicron.2 = H*0.44

#Dates taken from https://nccid.ca/covid-19-variants/
#Percents taken from  https://health-infobase.canada.ca/covid-19/vaccination-coverage/

#Percent of vaccinated individuals in NL when alpha came to NL (February 12, 2021)
pa.1 = 0.0171
pa.0 = 1-pa.1

#Percent of vaccinated individuals in NL when delta came to NL (April 28, 2021)
pd.1 = 0.2646
pd.2 = 0.0186
pd.0 = 1-pd.1-pd.2

# Percent of vaccinated individuals in NL when omicron came to Canada (December 15, 2021)
po.1 = 0.0641
po.2 = 0.8533
po.0 = 1-po.1-po.2


av.H = H
av.H.alpha = H.alpha.0*pa.0+H.alpha.1*pa.1
av.H.delta = H.delta.0*pd.0+H.delta.1*pd.1+H.delta.2*pd.2
av.H.omicron = H.omicron.0*po.0+H.omicron.1*po.1+H.omicron.2*po.2



#expected number of hospitalizations every 1000 cases?
C=1000
num.H = av.H*C
num.H.alpha = av.H.alpha*C
num.H.delta = av.H.delta*C
num.H.omicron = av.H.omicron*C

H.tot = c(num.H,num.H.alpha,num.H.delta, num.H.omicron)

labels_virus = c('wild-type','alpha','delta','omicron')
df = data.frame(Variants = labels_virus, 
                'R_0 during severe lockdown' = Rlv, 
                'Time between outbreaks [days]' = mv, 
                'Number of cases if outbreak is undetected for 3 days'=Isv,
                'length of severe lockdown [days]'=Tlv, 
                'Days of no lockdown per 1day of severe lockdown' = (mv-Tlv)/Tlv, 
                'Expected hospitalization per 1000 cases' = H.tot)


plot(Free,H.tot,
     pch='o',col='red',
     ylab='Average hospitalizations per 1000 cases \n given NL vaccination rates when the variant was established',
     xlab='If elimination is implemented, the percentage of days with no lockdown',
     xlim=c(-3,100),ylim =c(-0.1,50))
#abline(h=1,lty='dotted')
#abline(v=1,lty='dotted')
text(Free,H.tot, labels=c('wild-type','alpha','delta','omicron'),cex=0.9, font=2,pos=3)



