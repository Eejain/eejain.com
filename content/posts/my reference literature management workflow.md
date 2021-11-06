+++
title = "My Reference Management Workflow with Zotero"
author = ["Eejain Huang"]
date = 2020-09-09
tags = ["productivity"]
categories = ["Academic"]
draft = true
weight = 1009
bookComments = true
bookHidden = true
bookToC = true
+++

## Current workflow {#current-workflow}


### 1. Get references {#1-dot-get-references}

-   Search engine like Google scholar, or databases like Scopus, use tools like Publish or Perish with proper
-   Social network like email, journal club, researcher gate, rss of keywords or journals
-   Remember to take note of search keywords and the thinking process
    -   Why am I interested in this article? What leads me here (keyword, or citation from another article, or interested authors)
-   Save new references with proper tags as best as you can in **Inbox** collection for further processing


### 2. Inbox Processing Workflow {#2-dot-inbox-processing-workflow}

If this is too much, just tag and refile to publication project collection. Deal with format checking, and attachment management if preparing for manuscript.


#### Add appropriate tags {#add-appropriate-tags}

-   topic (construct, term, phenomenon)
    -   e.g., noticing, prof\_vision, prof\_knowledge
-   sample (tageted group)
    -   e.g., novice, teacher, learner, global
-   method (statistical procedure, data type)
    -   type of stimulus/material: real, sim, video, text,
    -   type of instrument/measurement: interview, behavioral, em, rating
    -   type of stats procedure: e.g., spatial\_analy
-   format (audiences)
    -   review, empirical, evaluation, pratical
-   For articles that need deeper processing, add "todo" tag and read the article according to [My Academic Reading Workflow]({{< relref "my academic reading workflow" >}})


#### Move articles to collection {#move-articles-to-collection}

-   cmd + drag


#### Check citation format and info {#check-citation-format-and-info}

-   Check whether the item type is correct (book chapter, conference paper, journal article, etc.)
-   Check whether the information is complete and correct: issue, doi, page, name spelling
    -   if not, use google scholar or crossref to fetch/redownload the item, merge the old and new items
    -   make sure the citation key stays the same (extra: citation key, pick from older version)
    -   make sure old pdf file with annotations are not deleted
    -   Tips: conference proceedings (place and date, may need to use other versions from google scholar), dissertation thesis (need link or data base call number) and book chapters may need extra attention
-   Use full sentence case not APA sentence case (right click the title then choose sentence case), and not title case (e.g. Concerns of teachers: A developmental conceptualisation)


#### Check and manage attachment {#check-and-manage-attachment}

-   Tools → Storage Scanner, check entries with the broken\_attachments and duplicate\_attachments and nosource tags
    -   broken\_attachments: likely missing pdf
    -   duplicate\_attachments: likely both linked and stored copy of the same attachment
    -   nosource: likely just for citation, no need to attach the original file
-   check "Duplicate Items", merge both metadata and attachments
-   choose all entries (or newly added entries), right click context menu choose "Manage Attachments" → Rename Attachments (now the renamed attachment should be moved to GoogleDrive/dropbox/Paper from the internal folder)
-   delete internal folders with the stored copy of files in ~/Zotero/storage, close the zotero software at this step, also see


### 3. Use the references {#3-dot-use-the-references}

-   In word document, use the Zotero add-on (cmd + opt + Z)
-   In other documents, see [Citation Syntax](<../../Dropbox/MarkdownNotes/MDNotes/Citation Syntax.md>)


### Appendix {#appendix}


#### Notes about Zotero plugins {#notes-about-zotero-plugins}

-   <https://www.zotero.org/support/plugins>
-   [Zotero Storage Scanner](https://github.com/retorquere/zotero-storage-scanner): check and tag the entries with broken or duplicate attachment link then generate corresponding tags
-   [ZotFile](http://zotfile.com/): change setting in **ZotFile Preferences** in **Tools**
    -   New PDF attachments: attach the most recent modified file from a folder as a new child attachment to the currently selected Zotero item
    -   Rename and move attachments: select any number of Zotero items and automatically rename and move all attachments of these items according to the user defined rules using metadata of the respective zotero item
    -   extract pdf annotations:
        -   annotation extraction only works for highlights and comments, not text entries (directly type on the pdf page)
        -   only works for annotations saved together with the pdf files (therefore Skim notes won't work as they are saved as separate files)
    -   [auto-index](https://github.com/retorquere/zotero-auto-index)
-   [Better BibTex](https://github.com/retorquere/zotero-better-bibtex): create unique and self-defined citation keys for LaTeX
-   [Zotero DOI Manager](https://github.com/bwiernik/zotero-shortdoi): find, shorten, clean DOIs, and tag items with invalid DOIs
-   [Zotero QuickLook](https://github.com/mronkko/ZoteroQuickLook): quick look attachment of an item
-   [Zutilo](https://github.com/willsALMANJ/Zutilo): add many utility functions to Zotero, such as configure keyboard shortcuts, copy/paste tags, relate multiple items through select and context menu
-   [Zotero OCR](https://github.com/UB-Mannheim/zotero-ocr): recognize text when the pdf is image
-   [Propachi](https://github.com/Juris-M/propachi-vanilla): CSL processor, used for proper APA sentence case (choose Uppercase Subtitles)
-   [Zotero Folder Import](https://github.com/retorquere/zotero-folder-import): import a folder of attachment files into a Zotero collection hierarchy
-   [MDnotes for Zotero](https://github.com/argenos/zotero-mdnotes): Export item metadata and notes as Markdown files (unfortunately not the other way around)
-   [Zotxt](https://github.com/egh/zotxt): Cite Zotero items in plain text (such as md, latex)
    -   [zotxt-emacs](https://gitlab.com/egh/zotxt-emacs): manage citation keys for pandoc markdown documents as well as org mode links to Zotero items. currently im using this function just for taking notes
-   [Zotero memento](https://github.com/leonkt/zotero-memento): when clipping new item from browser, attach a permanent link (from Internet achieves) in note and extra field of the item. I don't need permanent url and don't like the note it attached automatically, so no longer using this
-   [zotero-find](https://github.com/dalanicolai/zotero-find): find item in Zotero library from Emacs. Im not using this one rightnow because [ZotHero](https://github.com/deanishe/zothero) for Alfred is more convenient, also because zotero-find doesn't work when zotero is actually in use...
-   [Zotero Scholar Citations](https://github.com/MaxKuehn/zotero-scholar-citations): add citaion counts of an item in the extra field ZSCC, not really working


#### Notes about stored copy vs linked file in Zotero {#notes-about-stored-copy-vs-linked-file-in-zotero}

[zotero manual](https://www.zotero.org/support/attaching%5Ffiles#:~:text=Stored%20files%2C%20which%20are%20the,the%20attachment%20item%20in%20Zotero.&text=With%20linked%20files%2C%20Zotero%20only,original%20file%20on%20your%20computer.)

-   by default, zotero save attachments as stored files in the "Storage" subfolder in "Data Directory Location" (currently ~/Zotero)
    -   if using file syncing with Zotero, attachments have to be stored in internal folders
    -   currently im not using zotero sync (free account only has limited storage, and im not sharing my library with team), so there's no need to save attachments in internal folders
-   linked files are saved in "Linked Attachment Base Directory", and zotero saves the link
    -   linked files are not copied into the default data directory unless use ZotFile (in ZotFile preferences, choose "Attach stored copy of file(s)" then use Manage Attachments in the right click context menu)
    -   linked files are suitable for workflow based on a folder
-   From stored file to linked file: use right click → Manage Attachments → Rename Attachments (function from ZotFile). the stored files will remain in the internal folder.
-   From linked file to stored file: use Tools → Manage Attachments → Convert Linked Files to Stored Files. the linked files will remain in the original folder.


#### Related notes {#related-notes}

[{{< relref "compare zotero and citavi" >}}]({{< relref "compare zotero and citavi" >}})


## Ideal workflow {#ideal-workflow}


### Get and organize references in zotero {#get-and-organize-references-in-zotero}


### read and take note in emacs {#read-and-take-note-in-emacs}

-   pdf-view, org-noter
-   choose emacs as default pdf reader, open the pdf from folder or zotero, then start org-noter
-   turn off centered-window-mode
-   note saved separately, allows for system wide search
-   when finished, kill the noter session
-   but need to add org notes as attachment file in zotero manually (cmd + ctrl + L)
-   useful shortcuts:
    -   pdf add highlight annotation `C-c C-a h`
    -   org-noter insert note at precise location `M-i`
    -   list all occurrences in popup `M-s o`
    -   open outline in popup `o`
    -   in note file, sync the current position `C-M-.`


### feature wishlist {#feature-wishlist}

-   [ ] how to highlight searches across pages
-   [ ] mouse scroll (currently way too fast)
-   [ ] show reading progress (page number)
