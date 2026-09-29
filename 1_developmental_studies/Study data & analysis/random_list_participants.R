# A very simple script to create the randomization list for the website
write_csv(tibble(condition = sample(rep(c(0, 1), each=70)), user = NA),
         file = file.path("..", "Website", "server", "flask-website",
                          "random_list.csv"), na = "")
