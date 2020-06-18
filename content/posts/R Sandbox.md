+++
title = "R Sandbox"
author = ["Naskuv"]
date = 2020-05-25
draft = true
weight = 1006
bookComments = true
bookHidden = true
bookToC = true
+++

this post tests the org-babel function of rendering R block. I am mainly seeking three functions: automatic graphic output; persistent R session; present both codes and outputs

```R
setwd('~/itflows/content-org/')
  library(dplyr)
  library(ggplot2)
  ggplot(iris, aes(x =  Sepal.Width, y = Sepal.Length)) +
    geom_point()

square <- function(x)
  {
    x * x
  }
```

{{< figure src="testr.png" >}}

```R
  square(1:10)
```

```text
 [1]   1   4   9  16  25  36  49  64  81 100
```
