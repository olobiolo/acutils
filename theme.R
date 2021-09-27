ibrary(acutils)
show_colors("cyan")
library(magrittr)
"lightskyblue" %>% col2rgb() %>% as.list %>% setNames(c("red", "green", "blue")) %>%
  append(values = list("maxColorValue" = 255)) %>%
  do.call(rgb, .) %>%
  tolower

rstudioapi::addTheme("/usr/lib/rstudio/resources/themes/dracula_nv.rstheme", force = TRUE)
rstudioapi::applyTheme("Dracula Night Vision")

dc <- function(x) {
  color <- if (is.character(x)) {
    paste0("#", x)
  } else if (is.numeric(x)) {
    if (length(x) != 3) stop("x must be a 6-character strign or a numeric vector of length 3")
    if (any(x > 1)) stop("numeric values of x must be in [0,1]")
    x %>% setNames(c("red", "green", "blue")) %>% as.list %>% do.call(rgb, .)
  }
  on.exit(print(color))
  par(mar = rep(1, 4))
  barplot(1, col = color, axes = FALSE, border = NA)
}


"90ee90"
"00eeee"
"00cdcd"
dc(c(.5, .9, .9))
