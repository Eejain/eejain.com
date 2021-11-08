+++
title = "Org R Sandbox"
author = ["Eejain Huang"]
date = 2020-05-25
categories = ["Meta", "Academic"]
draft = false
weight = 1007
bookComments = true
bookHidden = true
bookToC = true
+++

This post presents the workflow of interacting with R in org mode. I am mainly seeking three functions in this workflow: automatic output; persistent R session across multiple source blocks; ability to present both codes and outputs.


## Preparation {#preparation}

-   `org-babel` for executing and tangling source blocks
-   `ESS` for interacting directly with R in emacs through the inferior R process


## Instruction {#instruction}

-   set up proper sub-tree/buffer header arguments, for this blog post I used sub-tree header arguments: `:header-args:R: :session *R* :exports both :eval no-export`
-   run `org-bable-execute-src-block` (or on sub-tree/buffer level) to produce outputs in the text area


## Example {#example}


### Plot output {#plot-output}

-   For the following code block, I used the source block header arguments `#+begin_src R :results output graphics file :file test.svg :output-dir ~/itflows/static`
-   Note on the header arguments:
    -   use `:results graphics file` when producing base R plot. use `:results output graphics file` for ggplot (see <https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-R.html>).
    -   remember to specify `:output-dir` as the static folder in your hugo directory, or else the org-link to the output plot after code evaluation will be directed to the same directory as the .org file by default

<!--listend-->

```R
setwd('~/itflows/static') # output plot stored in the static directory of your hugo blog
library(dplyr)
library(ggplot2)
print(ggplot(iris, aes(x =  Sepal.Width, y = Sepal.Length)) +
      geom_point() +
      theme_bw()
      )
                                        # If the source code block uses grid-based R graphics, e.g., the lattice and ggplot2 packages, then care must be taken either to print() the graphics object, specify :results output, or run the code in a :session", use print() to produce proper .svg output

square <- function(x) # test session persistency
  {
    x * x
  }
```

{{< figure src="/test.svg" >}}

```R
plot(matrix(rnorm(100), ncol=2), type="h")
```

{{< figure src="/test2.svg" >}}


### Plain text output {#plain-text-output}

-   source block header arguments: `#+begin_src R :results output`

<!--listend-->

```R
square(1:10)
```

```text
 [1]   1   4   9  16  25  36  49  64  81 100
```

{{< embed-pdf url="./test2.pdf" >}}
