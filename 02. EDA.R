Step_Data_List <- list.files("01.data/steps/", full.names = TRUE)
Detail_Step    <- rbindlist(lapply(Step_Data_List, fread))
Detail_Step$DateTime <- ymd_hms(Detail_Step$DateTime)

Detail_Step <- Detail_Step %>% 
  mutate(Month    = lubridate::month(DateTime, label = TRUE, abbr = FALSE),
         Day      = lubridate::day(DateTime),
         Day_Week = lubridate::wday(DateTime, label = TRUE, abbr = FALSE)) %>% 
  mutate(Weekend  = case_when(Day_Week == "일요일" ~ "일요일",
                              Day_Week == "토요일" ~ "토요일",
                              TRUE ~ "평일"))

Detail_Step %>%
  # filter(between(DateTime, ymd("2017-02-01"), ymd("2017-04-01"))) %>% 
  ggplot() +
  geom_line(aes(x = DateTime,
                y = DataValue)) +
  scale_x_datetime(breaks = "1 months") +
  facet_wrap(Month ~., scale = "free")


Detail_Step %>% 
  group_by(Date) %>% 
  summarise(Month = first(Month),
            Day = first(Day),
            Day_Week = first(Day_Week),
            Weekend = first(Weekend),
            Category = first(Category),
            DataValue = sum(DataValue)) %>% 
  ungroup() %>% 
  filter(DataValue > 1000) %>% 
  ggplot() +
  geom_boxplot(aes(x = Day_Week,
                   y = DataValue,
                   fill = Month),
               outlier.color = "red") +
  scale_fill_brewer(palette = "Blues") + 
  facet_wrap(Day_Week ~., scale = "free")

Detail_Step %>% 
  group_by(Date) %>% 
  summarise(Month = first(Month),
            Day = first(Day),
            Day_Week = first(Day_Week),
            Weekend = first(Weekend),
            Category = first(Category),
            DataValue = sum(DataValue)) %>% 
  ungroup() %>% 
  filter(DataValue > 1000) %>% 
  ggplot() +
  geom_boxplot(aes(x = Day_Week,
                   y = DataValue,
                   fill = Day_Week),
               outlier.color = "red") +
  scale_fill_brewer(palette = "Blues") + 
  facet_wrap(Month ~., scale = "free")
