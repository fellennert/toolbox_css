# Toolbox CSS website: handoff

*Last updated 12 October 2026 by Felix Lennert (felix.lennert@uni-leipzig.de)*

The course site is a [Quarto](https://quarto.org) website. You edit `.qmd` text files, Quarto turns them into HTML, and GitHub Pages serves the result.

- Live site: <https://fellennert.github.io/toolbox_css/>
- Repository: <https://github.com/fellennert/toolbox_css>

The repository has two branches that matter:

| Branch | Holds | Updated by |
| --- | --- | --- |
| `main` | The source files you edit | `git push` |
| `gh-pages` | The built site that students see | `quarto publish gh-pages` |

Pushing to `main` does not change the live site. Only the publish command does.

## Setup

1. Get write access to the repository. It lives under Felix's personal GitHub account, so ask him to add you as a collaborator.
2. Install [Quarto](https://quarto.org/docs/get-started/) (the site was last built with 1.8.26), R, and git. RStudio or VS Code both work as editors.
3. Clone the repository: `git clone https://github.com/fellennert/toolbox_css.git`
4. Run `quarto preview` in the project folder. A local copy of the site opens in your browser and reloads as you edit.

You do not need Python to edit text, links, or slides. You only need it if you change the code in a chapter that uses it (see "Chapters with code" below).

## Where things are

| Path | What it is |
| --- | --- |
| `_quarto.yml` | Site title, footer, theme, and the navigation menus |
| `index.qmd` | Homepage, including the syllabus link and embed |
| `lectures-overview.qmd`, `lectures-week_N.qmd` | One page per week: learning goals, slides, videos |
| `1_r_index.qmd` to `19_BERTopic.qmd` | The course script, one chapter per file |
| `slides/` | Slide PDFs, linked from the weekly pages |
| `syllabus/` | Syllabus source (`.Rmd`) and the PDF it produces |
| `files/`, `data/`, `figures/` | Data sets and images used by the chapters |
| `styles.css` | Custom styling on top of the `darkly` theme |
| `_freeze/` | Stored results of code chunks. Commit it, do not edit it |
| `_site/` | The built site. Generated, do not edit by hand |

## Common changes

**Edit an existing page.** Change the `.qmd` file, check it in `quarto preview`, then publish (below).

**Add a week or a chapter.**

1. Copy an existing file, e.g. `lectures-week_10.qmd` to `lectures-week_11.qmd`, and change the `title` at the top.
2. Add a line for it in `_quarto.yml`, under the `Lectures` or `R Script` menu. A page that is not listed there is built but does not appear in the navigation.

**Add slides.** Put the PDF in `slides/` and link it from the weekly page. Existing weekly pages show the pattern: a download link plus an `<iframe>` that embeds the PDF.

**Replace the syllabus.**

1. Edit `syllabus/syllabus_toolboxCSS_winter2627.Rmd` and knit it to PDF in RStudio. The session dates are computed from the two start dates near the top of the file.
2. If you rename the file for a new term, update both references to the PDF in `index.qmd` (the download link and the `<iframe>`). Otherwise the homepage link breaks.

**Change the footer or contact address.** Both are in `_quarto.yml` (`page-footer`) and in the first paragraph of `index.qmd`. They currently name Felix.

## Publishing

```bash
# 1. Save your source changes
git add -A
git commit -m "describe the change"
git push

# 2. Update the live site
quarto publish gh-pages
```

The publish command rebuilds the site and pushes it to the `gh-pages` branch. The live site updates a few minutes later; you may need a hard refresh in the browser to see it.

## Chapters with code

Most pages have `freeze: auto` in their header. Quarto then reuses the stored results in `_freeze/` and only reruns a page's code when that page's source file changes. This is why the site builds in a minute without any special setup.

It also means that editing a chapter that contains code makes Quarto rerun all of that chapter's code on your machine. For that you need the R packages the chapter loads, and for the Python chapters a conda environment with the name the chapter expects:

| Environment | Used by |
| --- | --- |
| `toolbox_env` | `3_reticulate`, `4_python`, `5_pandas`, `8_selenium` |
| `torch_env` | `11_transcription` |
| `bert-env` | `17_BERT` |
| `bertopic_env` | `19_BERTopic` |

The environment name is set in a `use_condaenv(...)` line near the top of each chapter. Some chapters (transcription, BERT, BERTopic) take a long time to run and download large models.

## Things to watch out for

- **GitHub rejects files over 100 MB.** A push that contains one fails entirely, including every other commit in it. Keep large data sets out of the repository and link to them instead.
- **Do not keep the project folder in Dropbox or iCloud.** Sync conflicts create duplicate files such as `index 2.html` that end up in the repository.
- **`6_stringr_regex.qmd` gives four warnings about a stray `:::` when it builds.** The page still renders; a fenced block in it is probably not closed properly.
- **This file starts with an underscore on purpose.** Quarto skips files whose names start with `_`, so it is not published to the live site.
