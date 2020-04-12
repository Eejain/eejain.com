+++
title = "R Sandbox"
author = ["Eejain Huang"]
tags = ["development"]
categories = ["Meta"]
draft = false
bookHidden = true
+++

Testing org-babel R block

-   automatic graphic output
-   persistent R session
-   present both codes and outputs

<!--listend-->

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

{{< figure src="/ox-hugo/test.png" >}}

```R
  square(1:10)
```

```text
 [1]   1   4   9  16  25  36  49  64  81 100
```
