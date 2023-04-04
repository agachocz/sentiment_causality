# Impulse response function
feir <- irf(model, n.ahead = 12, ortho = F, runs = 1000, ci = 0.9)

plot(feir)

plot(feir$irf$CS[,2])
lines(feir$Lower$CS[,2], col = "red")
lines(feir$Upper$CS[,2], col = "red")

# plots

# Impulse from CS
from_CS_main <- feir$irf$CS %>% as.data.frame() %>% cbind(month = 0:12) %>% 
  pivot_longer(-month, names_to = "to") %>% 
  mutate(line = "main")
from_CS_lower <- feir$Lower$CS %>% as.data.frame() %>% cbind(month = 0:12) %>%
  pivot_longer(-month, 
               names_to = "to") %>% mutate(line = "lower")
from_CS_upper <- feir$Upper$CS %>% as.data.frame() %>% cbind(month = 0:12) %>% 
  pivot_longer(-month, 
               names_to = "to") %>% mutate(line = "upper")

from_CS <- rbind(from_CS_lower, from_CS_main, from_CS_upper)
from_CS$to <- factor(from_CS$to, levels = c("DAX", "WIG", "Vol_GER", "Vol_PL", "STOXX50", "EG", "CS"))

from_CS %>% filter(to != "CS") %>% 
  ggplot(aes(x = month, y = value, col = line)) + geom_line() +
  facet_wrap(~to, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = 0:12) +
  scale_color_discrete(type = c("red", "black", "red")) +
  theme_minimal() + theme(legend.position = "none") +
  labs(title = "Response to the impulse from CS", 
       x = "Months after impulse", y = "Change")


# Impulse from EG
from_EG_main <- feir$irf$EG %>% as.data.frame() %>% cbind(month = 0:12) %>% 
  pivot_longer(-month, names_to = "to") %>% 
  mutate(line = "main")
from_EG_lower <- feir$Lower$EG %>% as.data.frame() %>% cbind(month = 0:12) %>%
  pivot_longer(-month, 
               names_to = "to") %>% mutate(line = "lower")
from_EG_upper <- feir$Upper$EG %>% as.data.frame() %>% cbind(month = 0:12) %>% 
  pivot_longer(-month, 
               names_to = "to") %>% mutate(line = "upper")

from_EG <- rbind(from_EG_lower, from_EG_main, from_EG_upper)
from_EG$to <- factor(from_EG$to, levels = c("DAX", "WIG", "Vol_GER", "Vol_PL", "STOXX50", "EG", "CS"))

from_EG %>% filter(to != "EG") %>% 
  ggplot(aes(x = month, y = value, col = line)) + geom_line() +
  facet_wrap(~to, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = 0:12) +
  scale_color_discrete(type = c("red", "black", "red")) +
  theme_minimal() + theme(legend.position = "none") +
  labs(title = "Response to the impulse from EG", 
       x = "Months after impulse", y = "Change")


# Impulse from DAX
from_DAX_main <- feir$irf$DAX %>% as.data.frame() %>% cbind(month = 0:12) %>% 
  pivot_longer(-month, names_to = "to") %>% 
  mutate(line = "main")
from_DAX_lower <- feir$Lower$DAX %>% as.data.frame() %>% cbind(month = 0:12) %>%
  pivot_longer(-month, 
               names_to = "to") %>% mutate(line = "lower")
from_DAX_upper <- feir$Upper$DAX %>% as.data.frame() %>% cbind(month = 0:12) %>% 
  pivot_longer(-month, 
               names_to = "to") %>% mutate(line = "upper")

from_DAX <- rbind(from_DAX_lower, from_DAX_main, from_DAX_upper)
from_DAX$to <- factor(from_DAX$to, levels = c("DAX", "WIG", "STOXX50", "Vol_GER", "Vol_PL", "EG", "CS"))

from_DAX %>% filter(to != "DAX") %>% 
  ggplot(aes(x = month, y = value, col = line)) + geom_line() +
  facet_wrap(~to, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = 0:12) +
  scale_color_discrete(type = c("red", "black", "red")) +
  theme_minimal() + theme(legend.position = "none") +
  labs(title = "Response to the impulse from DAX", 
       x = "Months after impulse", y = "Change")


# Impulse from WIG
from_WIG_main <- feir$irf$WIG %>% as.data.frame() %>% cbind(month = 0:12) %>% 
  pivot_longer(-month, names_to = "to") %>% 
  mutate(line = "main")
from_WIG_lower <- feir$Lower$WIG %>% as.data.frame() %>% cbind(month = 0:12) %>%
  pivot_longer(-month, 
               names_to = "to") %>% mutate(line = "lower")
from_WIG_upper <- feir$Upper$WIG %>% as.data.frame() %>% cbind(month = 0:12) %>% 
  pivot_longer(-month, 
               names_to = "to") %>% mutate(line = "upper")

from_WIG <- rbind(from_WIG_lower, from_WIG_main, from_WIG_upper)
from_WIG$to <- factor(from_WIG$to, levels = c("DAX", "WIG","STOXX50", "Vol_GER", "Vol_PL", "EG", "CS"))

from_WIG %>% filter(to != "WIG") %>% 
  ggplot(aes(x = month, y = value, col = line)) + geom_line() +
  facet_wrap(~to, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = 0:12) +
  scale_color_discrete(type = c("red", "black", "red")) +
  theme_minimal() + theme(legend.position = "none") +
  labs(title = "Response to the impulse from WIG", 
       x = "Months after impulse", y = "Change")
