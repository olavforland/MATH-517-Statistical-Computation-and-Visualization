## Multiple Regression


M1 = lm(trip_total ~ ., data = df) 
step.back.aic1 = step(M1, 
                      direction = "backward", 
                      trace = FALSE)
t1 <- round(summary(step.back.aic1)$coef,3)

t2<- step.back.aic1 %>%
  broom::glance() %>% 
  round(2) %>%
  dplyr::select(r.squared, adj.r.squared, p.value)


M2 = lm(avg_pickup_time ~ ., data = df) 
step.back.aic2 = step(M2, 
                      direction = "backward", 
                      trace = FALSE)
t3<-round(summary(step.back.aic2)$coef,3)

t4<-step.back.aic2 %>%
  broom::glance() %>% 
  round(2) %>%
  dplyr::select(r.squared, adj.r.squared, p.value)

