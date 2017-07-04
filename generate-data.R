library(dplyr) 
library(purrr)

set.seed(1)

# generate clients
client_id <- floor(runif(1000, 300000000, 800000000)) 
client_id <- sprintf("%012d", client_id)

# generate products
products <- c('CREDIT CARD', 'OVERDRAFT', 'CURRENT ACCOUNT', 'PERSONAL LOAN', 'MORTGAGE', 'CAR LOAN')

# generate the number of products for each client
df <- data.frame(	n = 1:length(client_id), client_id = client_id, num_products = (floor(1+6*rbeta(length(client_id), .9, 1.4))), stringsAsFactors = FALSE)

# generate random products for each client
cl_prod <- map(df$num_products, ~sample(products, .x)) %>% map(~data.frame(products = .x, stringsAsFactors = F)) %>%
   map2(df$client_id, ~data.frame(client_id = .y, .x, stringsAsFactors=F)) %>%
   map_df(rbind.data.frame)

# generate segment for each client
segment <- data.frame(client_id = sample(client_id, round(length(client_id)/2.54)), stringsAsFactors=F)
segment <- segment %>%
   mutate(segment = c('SILVER', 'GOLD')[round(sample(c(1.1, 1.2, 1.8), nrow(segment), replace=T))])

# add two years of client data
d <- rbind.data.frame(cl_prod %>% mutate(year = 2016),
                      cl_prod %>% mutate(year = 2015)) %>%
   left_join(segment) %>%
   mutate(segment = ifelse(is.na(segment), 'OTHER', segment),
          REVENUE = ifelse(segment=='GOLD', 
                 rnorm(nrow(.), 1000, 70), 
                 ifelse(segment=='SILVER', 
                        rnorm(nrow(.), 750, 70), 
                        rnorm(nrow(.), 400, 70))))


segments <- select(d, client_id, segment) %>% 
   filter(segment != 'OTHER') %>% 
   distinct() %>% 
   write.csv('segments.csv', row.names = F)
clients <- select(d, client_id, year, products, REVENUE) %>% write.csv('clients.csv', row.names = F)


# solution
clients %>%
   filter(year == 2016) %>%
   left_join(segments) %>%
   mutate(segment = ifelse(is.na(segment), 'OTHER', segment)) %>%
   group_by(segment, client_id) %>%
   summarise(tot = sum(REVENUE)) %>%
   group_by(segment) %>%
   summarise(tot = mean(tot))



d %>% 
   filter(year == 2016) %>%
   group_by(segment, client_id) %>%
   summarise(tot = sum(REVENUE)) %>%
   group_by(segment) %>%
   summarise(tot = mean(tot))
