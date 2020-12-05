assentos<- seats_fed(2014)
assentos<- assentos %>% filter(DESCRICAO_CARGO=="Deputado Federal") %>% 
  dplyr::select(SIGLA_UF,QTDE_VAGAS) %>% 
  rename(SG_UF=SIGLA_UF,MD=QTDE_VAGAS)


theta<- DF_gastos_2018 %>% ungroup() %>% 
  group_by(SG_UF) %>% mutate(pvotos=prop.table(QT_VOTOS_NOMINAIS),
                             pgasto=prop.table(VR_DESPESA_CONTRATADA)) %>% 
  dplyr::select(SG_UF,pvotos,pgasto) %>% left_join(assentos) %>% 
  mutate(theta=(pgasto*pvotos)/MD) %>% rename(alpha=pgasto,
                                              beta=pvotos,
                                              gamma=MD)


AC<-alpha(theta,na.rm=T)


a<-theta %>% filter(alpha>0.0125) %>% ggplot(aes(x=alpha)) +
  geom_density()+
  geom_density(color="darkblue", fill="lightblue")+
labs(title="",x="%",y="")

b<-theta %>%  filter(beta>0.02) %>% ggplot(aes(x=beta)) +
  geom_density()+
  geom_density(color="darkgreen", fill="lightgreen")+
  labs(title="",x="%",y="")


save(AC,theta,file="lista04.RData")
