+++
title = "Hugo Org Sandbox"
author = ["Eejain Huang"]
date = 2020-04-10
tags = ["development"]
categories = ["Meta"]
draft = false
bookHidden = true
+++

## Basic formatting {#basic-formatting}

-   list item
-   [ ] checkbox item
-   numbered item

regular main text, **bold**, _italic_, <span class="underline">underlined</span>, `verbatim`, `code`,
 ~~strike-through~~


## Paragraphs {#paragraphs}

<p class="verse">
Great clouds overhead<br />
Tiny black birds rise and fail<br />
Snow covers Emacs<br />
<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;--- AlexSchroeder<br />
</p>

> Everything should be made as simple as possible,
> but not any simpler ---ALbert Einstein

<style>.org-center { margin-left: auto; margin-right: auto; text-align: center; }</style>

<div class="org-center">
  <div></div>

A fog sinks in, <br />
and it overlays the death.

</div>


## Math symbols {#math-symbols}

X<sup>2</sup>, X<sub>2</sub>

inline katex expression: {{< katex >}}&ang;\alef{{< /katex >}}

{{< katex display >}}

\begin{array}{cc}
a & b \\c & d
\end{array}

{{< /katex >}}

note: ox-hugo sometimes render extra \\


## R block {#r-block}

```R
getwd()
```


## Table {#table}


### simple table {#simple-table}

| h1 | h2 |
|----|----|
| a  | b  |


### three line table {#three-line-table}

| h1 | h2 | h3 |
|----|----|----|
| a  | b  | c  |
| d  | e  | f  |


## Link {#link}

[test link description](http://www.gnu.org)


## References {#references}
