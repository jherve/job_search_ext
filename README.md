# JobSearch, a Firefox extension to boost your job search

This extension helps you keep track of the job offers you stumble upon on LinkedIn, automagically saving all of them into a human-editable database file.

Here are some of its features :

* Extract data from job offer pages (e.g. job position, company name, link, company domain, location, ...)
* Save it in a plain text database format ([recfile](https://www.gnu.org/software/recutils/))
* Add a colored overlay on the job offers depending on their status (seen/applied to/dismissed/rejected/...)
* Display the offers you're interested in applying in a sidebar

## How it runs

From the settings of the extension, you can choose where the file will be located ; let's say `/home/me/job_search/`. A `jobs.rec` file will be created in this directory.

From then, everytime you visit a page that contains a job offer, `/home/me/job_search/jobs.rec` will be updated with data extracted from the page.

E.g. if you visit https://www.linkedin.com/jobs/view/3765452342/, you will get  a record about the job offer itself :

```
first_seen_date: Mon, 19 Feb 2024 13:31:00 +0100
url: https://www.linkedin.com/jobs/view/3765452342/
title: Data Engineer
origin: linked_in
location: Amérique latine
id: linked_in_3765452342
flexibility: full_remote
company: Mentor Talent Acquisition
application_process: regular
```

... and another with info about the company :

```
url: https://www.linkedin.com/company/mentor-talent-acquisition/life
name: Mentor Talent Acquisition
domain: Recrutement et placement de personnel
```

Because the database is just a plain text file, you can then update those records with other information that is harder to extract automatically (e.g. required experience, skills, ...) or with information about a potential application. You can also version it with `git`. Data integrity can be ensured via `recutils` utilities.

## Installation

0. Install external dependencies :
    * [recutils](https://www.gnu.org/software/recutils/) to read/write the database file
    * [pdm](https://pdm-project.org/) to install the Python environment
    * [npm](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm) to install the Javascript environment
1. Clone this repository
1. Install the native backend : `native/install.sh`
1. Build the extension : `npm install && npm run build`
1. Install the extension as [temporary](https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Your_first_WebExtension#installing) by pointing to the file `extension/manifest.json` (**NOT** the `manifest.json` located at root)
1. Setup the location of the job offers' file

## Tech stack / general tech info

* [Firefox WebExtensions](https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions)
* Frontend code in [PureScript](https://www.purescript.org/), a pure functional language very similar to Haskell
* Native application code is a basic [Python](https://www.python.org/) app
* [Recutils](https://www.gnu.org/software/recutils/), a genious piece of free software that brings database-like capabilities to a human-readable file format

Overall the extension architecture is not too complex, even though web extension standard mandates lots of message passing between parts that run in isolation of each other and can only communicate via JSON messages. E.g. only "content scripts" can read/write a web page's content ; only a native application that is launched by the browser is allowed to interact with the local file system ; only a background script can interact with the native application.

The major hard point was parsing LinkedIn pages to extract meaningful information. The HTML structure is not very semantic (lots of nested `div` and `span` with little identifiable class names), quite hard to retro-engineer in a reliable way, and evolves with UI updates.

For this task especially, PureScript type system proved incredibly useful.

## Caveats

This extension was mostly written to :

* help me with my current job search (come and say [hello](https://www.linkedin.com/in/julien-herve/))
* have an excuse to dive into Purescript
* experiment with methods to properly extract data from unfriendly HTML code

Therefore it has the following caveats :

* Very poor documentation
* Likely not to run on Windows without pain (recutils doesn't work there)
* Works only on Firefox
* Poor packaging
* Minimal UI

But it works on my machine 🤷 !

I will likely improve on this, e.g. by extracting the LinkedIn parsing code into a PureScript/Javascript standalone library, but don't hold your breath !

## Tests

Frontend tests can be run with `npm run test`.

Native application tests can be run with `(cd native && pdm run pytest)`.
