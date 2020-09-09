+++
title = "My Academic Reading Workflow"
author = ["Naskuv"]
date = 2020-09-08
tags = ["productivity"]
categories = ["Academic"]
draft = false
weight = 1013
bookComments = true
bookHidden = true
bookToC = true
+++

## Tools {#tools}


### Skim {#skim}

-   notes are saved in a separate .skim file, cannot be extracted in Zotero
-   resolution not as sharp as acrobat
-   only one highlight color
-   no type machine function
-   small and fast, good for note taking


### Adobe Acrobat {#adobe-acrobat}

-   highlights color coding:

    -   light green: key word
        -   \![](/Users/naskuv/GoogleDrive/MarkdownNotes/MDImage/BD248B97-C189-4FFF-AEF2-EC16E35B96C0.png)

    <!--listend-->

    -   light yellow: key statement
        -   \![](/Users/naskuv/GoogleDrive/MarkdownNotes/MDImage/1F49A5A3-D911-49FE-8C98-4FF7278025A7.png)
    -   light pink: method, operationalisation, key results
        -   \![](/Users/naskuv/GoogleDrive/MarkdownNotes/MDImage/86732702-1B95-4F6A-A7B8-03863444B5C4.png)


### Preview {#preview}

-   fast and simple, have all the necessary annotation functions, and can be extracted to Zotero
-   rich hotkey support


### Zotero {#zotero}

-   create child note while reading
-   create notes for: quotation, summary, comment
-   add or modify tags according to [Article tags]({{< relref "my reference literature management workflow" >}})


### MindNode {#mindnode}

-   Use MindNode to create a map for the classic literatures/authors


### Emacs + Zotero {#emacs-plus-zotero}

-   requires Emacs package: pdf-tools, [org-noter](https://github.com/weirdNox/org-noter#customization-), [zotxt](https://github.com/egh/zotxt-emacs)
-   requires Zotero plugin: zotxt, zotxt-emacs
-   also see [{{< relref "my reference literature management workflow" >}}]({{< relref "my reference literature management workflow" >}})


#### Workflow in Emacs {#workflow-in-emacs}

-   in Zotero, open the attachment for annotation (set default pdf viewer to emacs)
-   start org-noter, save a new note file, or an existing note file will shown
-   `M-i` insert note at precise location (create a new heading in corresponding org notes file)
-   document view → notes view: the note heading will automatically expand when scrolling through document buffer. navigate
-   notes view → document view: `M-p`, `M-.`, `M-n` sync previous/current/next note, meaning the document buffer will scroll to the location associated with the previous/current/next note
-   `org-noter-create-skeleton` imports the PDF outline or annotations as notes


## Template {#template}
