library(devtools)
library(tidyverse)
load_all()
data(demographics)
data(georgia)
# Due to a time crunch I will be running all of these examples
# here and moving them to the associated scripts when I have time


# geom_scaledmap
ggplot(georgia, aes(x = year, y = edu)) +
  geom_scaledmap()


# geom_spike
ggplot(georgia, aes(x = year, y = high_school_grads)) +
  geom_spike()
# goem_spiral
ggplot(georgia, aes(x = year, y = high_school_grads)) +
  geom_spiral() +
  coord_polar()
# geom_pathspiral
ggplot(georgia, aes(x = year, y = high_school_grads)) +
  geom_pathspiral() +
  coord_polar()
# geom_wrappedbar
ggplot(georgia, aes(x = year, y = high_school_grads)) +
  geom_wrappedbar()


# geom_wovenbar
ggplot(georgia, aes(x = year, y = value, group = relationship_type, fill = location)) +
  geom_wrappedbar()
# square
ggplot(georgia, aes(x = year, y = percent_population, fill = occupation)) +
  geom_wrappedbar()

# scale_dubois

# theme_dubois
ggplot(georgia, aes(x = year, y = percent_population, fill = occupation)) +
  geom_wrappedbar() +
  theme_dubois()

# dubois_pal
dubois_pal(4)
# but can return a sequential color scale, e.g. with colors on a continuum
dubois_pal(10, type = "sequential")
ggplot(georgia, aes(x = median_income, y = high_school_graduates)) +
  geom_point() +
  scale_color_gradientn(colors = dubois_pal(10))
# the above is equivalent to the slightly more conveient syntax of:
ggplot(georgia, aes(x = median_income, y = high_school_graduates)) +
  geom_point() +
  scale_color_dubois()