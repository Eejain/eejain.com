+++
title = "Notes on Citation Syntax"
author = ["Eejain Huang"]
date = 2020-09-08
tags = ["productivity"]
categories = ["Academic"]
draft = true
weight = 1012
bookComments = true
bookHidden = true
bookToC = true
+++

\## MMD:

-   cite a paper: [locator][#citationkey] (locator is optional)
-   cite multiple paper: [locator1][#citationkey1][locator2][citationkey2]...
-   inline citation: [#citationkey; locator]

\## Pandoc

-   citation: Citations go inside square brackets and are separated by semicolons
-   cite a paper: [@citekey, locator]
-   cite multiple paper [@citekey, locator; @citekey, locator...]
-   inline citation: @citekey [locator]

\## bibtex
BibTeX cheat sheet and entry templates
`~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
by pts@fazekas.hu at Wed Jul 29 15:42:08 CEST 2009


## Based on information form <http://en.wikipedia.org/wiki/BibTeX> {#based-on-information-form-http-en-dot-wikipedia-dot-org-wiki-bibtex}


## See also: <http://amath.colorado.edu/documentation/LaTeX/reference/faq/bibstyles.html> {#see-also-http-amath-dot-colorado-dot-edu-documentation-latex-reference-faq-bibstyles-dot-html}


## Nonstandard entries: url=, annote=, crossref=, {#nonstandard-entries-url-annote-crossref}


## No url= field in standard types, should be put to note={URL \url{...}}. {#no-url-field-in-standard-types-should-be-put-to-note-url-url-dot-dot-dot-dot}


## There are no comments in BibTeX .bib files, not even % {#there-are-no-comments-in-bibtex-dot-bib-files-not-even}


## ?NAME= describes an optional field. {#name-describes-an-optional-field-dot}


## BibTeX silently ignores a field whose name it doesn't know. {#bibtex-silently-ignores-a-field-whose-name-it-doesn-t-know-dot}

.bib file entry templates
"""""""""""""""""""""""""
@article{NAME,
  author={},
  title={},
  journal={},
  year=,
  ?volume={},
  ?number={},
  ?pages={},
  ?month=,
  ?note={},
  ?key={},
}

@book{NAME,
  author/editor={},
  title={},
  publisher={},
  year=,
  ?volume={},
  ?series={},
  ?address={},
  ?edition={},
  ?month=,
  ?note={},
  ?key={},
  ?pages={},
}

@booklet{NAME,
  title={},
  ?author={},
  ?howpublished={},
  ?address={},
  ?month=,
  ?year=,
  ?note={},
  ?key={},
}

@conference{NAME,
  author={},
  title={},
  booktitle={},
  year=,
  ?editor={},
  ?pages={},
  ?organization={},
  ?publisher={},
  ?address={},
  ?month=,
  ?note={},
  ?key={},
}

@inbook{NAME,
  author/editor={},
  title={},
  chapter/pages={},
  publisher={},
  year=,
  ?volume={},
  ?series={},
  ?address={},
  ?edition={},
  ?month=,
  ?note={},
  ?key={},
}

@incollection{NAME,
  author={},
  title={},
  booktitle={},
  year=,
  ?editor={},
  ?pages={},
  ?organization={},
  ?publisher={},
  ?address={},
  ?month=,
  ?note={},
  ?key={},
}

@inproceedings{NAME,
  author={},
  title={},
  booktitle={},
  year=,
  ?editor={},
  ?pages={},
  ?organization={},
  ?publisher={},
  ?address={},
  ?month=,
  ?note={},
  ?key={},
}

@manual{NAME,
  title={},
  ?author={},
  ?organization={},
  ?address={},
  ?edition={},
  ?month=,
  ?year=,
  ?note={},
  ?key={},
}

@mastersthesis{NAME,
  author={},
  title={},
  school={},
  year=,
  ?address={},
  ?month=,
  ?note={},
  ?key={},
}

@misc{NAME,
  ?author={},
  ?title={},
  ?howpublished={},
  ?month=,
  ?year=,
  ?note={},
  ?key={},
}

@phdthesis{NAME,
  author={},
  title={},
  school={},
  year=,
  ?address={},
  ?month=,
  ?note={},
  ?key={},
}

@proceedings{NAME,
  title={},
  year=,
  ?editor={},
  ?publisher={},
  ?organization={},
  ?address={},
  ?month=,
  ?note={},
  ?key={},
}

@techreport{NAME,
  author={},
  title={},
  institution={},
  year=={},
  ?type={},
  ?number={},
  ?address={},
  ?month=,
  ?note={},
  ?key={},
}

@unpublished{NAME,
  author={},
  title={},
  note={},
  ?month=,
  ?year=,
  ?key={},
}

Description of entries and fields
"""""""""""""""""""""""""""""""""


## @article: An article from a journal or magazine. {#article-an-article-from-a-journal-or-magazine-dot}


## @book: A book with an explicit publisher. {#book-a-book-with-an-explicit-publisher-dot}


## @booklet: A work that is printed and bound, but without a named publisher or {#booklet-a-work-that-is-printed-and-bound-but-without-a-named-publisher-or}

sponsoring institution.


## @conference: The same as inproceedings, included for Scribe compatibility. {#conference-the-same-as-inproceedings-included-for-scribe-compatibility-dot}


## @inbook: A part of a book, usually untitled. May be a chapter (or section or {#inbook-a-part-of-a-book-usually-untitled-dot-may-be-a-chapter-or-section-or}

whatever) and/or a range of pages.


## @incollection: A part of a book having its own title. {#incollection-a-part-of-a-book-having-its-own-title-dot}


## @inproceedings: An article in a conference proceedings. {#inproceedings-an-article-in-a-conference-proceedings-dot}


## @manual: Technical documentation. {#manual-technical-documentation-dot}


## @mastersthesis: A Master's thesis. {#mastersthesis-a-master-s-thesis-dot}


## @misc: For use when nothing else fits. {#misc-for-use-when-nothing-else-fits-dot}


## @phdthesis: A Ph.D. thesis. {#phdthesis-a-ph-dot-d-dot-thesis-dot}


## @proceedings: The proceedings of a conference. {#proceedings-the-proceedings-of-a-conference-dot}


## @techreport: A report published by a school or other institution, usually {#techreport-a-report-published-by-a-school-or-other-institution-usually}

numbered within a series.


## @unpublished: A document having an author and title, but not formally {#unpublished-a-document-having-an-author-and-title-but-not-formally}

published.


## address=: Publisher's address (usually just the city, but can be the full {#address-publisher-s-address-usually-just-the-city-but-can-be-the-full}

address for lesser-known publishers)


## annote=: An annotation for annotated bibliography styles (not typical) {#annote-an-annotation-for-annotated-bibliography-styles--not-typical}


## author=: The name(s) of the author(s) (in the case of more than one author, {#author-the-name--s--of-the-author--s--in-the-case-of-more-than-one-author}

separated by and)


## booktitle=: The title of the book, if only part of it is being cited {#booktitle-the-title-of-the-book-if-only-part-of-it-is-being-cited}


## chapter=: The chapter number {#chapter-the-chapter-number}


## crossref=: The key of the cross-referenced entry {#crossref-the-key-of-the-cross-referenced-entry}


## edition=: The edition of a book, long form (such as "first" or "second") {#edition-the-edition-of-a-book-long-form--such-as-first-or-second}


## editor=: The name(s) of the editor(s) {#editor-the-name--s--of-the-editor--s}


## eprint=: A specification of an electronic publication, often a preprint or a {#eprint-a-specification-of-an-electronic-publication-often-a-preprint-or-a}

technical report


## howpublished=: How it was published, if the publishing method is nonstandard {#howpublished-how-it-was-published-if-the-publishing-method-is-nonstandard}


## institution=: The institution that was involved in the publishing, but not {#institution-the-institution-that-was-involved-in-the-publishing-but-not}

necessarily the publisher


## journal=: The journal or magazine the work was published in {#journal-the-journal-or-magazine-the-work-was-published-in}


## key=: A hidden field used for specifying or overriding the alphabetical order {#key-a-hidden-field-used-for-specifying-or-overriding-the-alphabetical-order}

of entries (when the "author" and "editor" fields are missing). Note that
this is very different from the key (mentioned just after this list) that is
used to cite or cross-reference the entry.


## month=: The month of publication (or, if unpublished, the month of creation). {#month-the-month-of-publication--or-if-unpublished-the-month-of-creation--dot}

Example 1=: month=jan. Example 2=: month="17~" # feb.


## note=: Miscellaneous extra information {#note-miscellaneous-extra-information}


## number=: The "number" of a journal, magazine, or tech-report, if applicable. {#number-the-number-of-a-journal-magazine-or-tech-report-if-applicable-dot}

(Most publications have a "volume", but no "number" field.)


## organization=: The conference sponsor {#organization-the-conference-sponsor}


## pages=: Page numbers, separated either by commas or double-hyphens. For books, {#pages-page-numbers-separated-either-by-commas-or-double-hyphens-dot-for-books}

the total number of pages.


## publisher=: The publisher's name {#publisher-the-publisher-s-name}


## school=: The school where the thesis was written {#school-the-school-where-the-thesis-was-written}


## series=: The series of books the book was published in (e.g. "The Hardy Boys" {#series-the-series-of-books-the-book-was-published-in-e-dot-g-dot-the-hardy-boys}

or "Lecture Notes in Computer Science")


## title=: The title of the work {#title-the-title-of-the-work}


## type=: The type of tech-report, for example, "Research Note" {#type-the-type-of-tech-report-for-example-research-note}


## url=: The WWW address {#url-the-www-address}


## volume=: The volume of a journal or multi-volume book {#volume-the-volume-of-a-journal-or-multi-volume-book}


## year=: The year of publication (or, if unpublished, the year of creation) {#year-the-year-of-publication--or-if-unpublished-the-year-of-creation}

<SSI COUNTER="">

`~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`

\## LaTex

\citet{} Textual citation: Jones et al. (1990)
\citet\*{} Same as \citet but if there are several authors, all names are printed: Jones, Baker, and Smith (1990)
\citep{} Parenthetical citation:  (Jones et al. 1990)
\citep\*{} The same as \citep but if there are several authors, all names are printed:   (Jones, Baker, and Smith 1990)
\citeauthor{}Prints only the name of the authors(s):   Jones et al.
\citeauthor\*{key}: Jones, Baker, and Smith
\citeyear{} Prints only the year of the publication.: 1990
\citeapos{key}: Jones et al.'s (1990)
\cite[chapter, p.~215]{citation01}

\citet{jon90}     -->     Jones et al. (1990)
\citet[chap. 2]{jon90}      -->     Jones et al. (1990, chap. 2)
\citep{jon90}     -->     (Jones et al., 1990)
\citep[chap. 2]{jon90}      -->     (Jones et al., 1990, chap. 2)
\citep[see][]{jon90}      -->     (see Jones et al., 1990)
\citep[see][chap. 2]{jon90}     -->     (see Jones et al., 1990, chap. 2)
\citet\*{jon90}      -->     Jones, Baker, and Williams (1990)
\citep\*{jon90}      -->     (Jones, Baker, and Williams, 1990)

\#@academic
\#@codes
