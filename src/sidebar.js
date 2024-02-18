function elWithText (tag, text) {
    const el = document.createElement(tag);
    el.textContent = text;
    return el;
}

function link (text, url) {
    const div = document.createElement("div");
    const el = document.createElement("a");
    el.textContent = text;
    el.href = url;
    div.appendChild(el);
    return div;
}

function createJob (job) {
    const li = document.createElement("li");

    li.appendChild(elWithText("h2", job.title));
    li.appendChild(elWithText("h3", job.company));
    li.appendChild(link("Job offer link", job.url));

    if (job.company_url)
        li.appendChild(link("Company page", job.company_url));

    if (job.alternate_url)
        li.appendChild(link("Job offer on company website", job.alternate_url));

    if (job.comment)
        li.appendChild(elWithText("p", job.comment));

        return li;
}

function createListOfJobs (jobs) {
    const ul = document.createElement("ul");
    for (const j of jobs) {
        ul.appendChild(createJob(j));
    }
    return ul;
}

async function updateContent () {
    const content = document.querySelector("#content");
    const allJobs = Object.entries(await browser.storage.local.get()).map(a => a[1]);
    const stillToApplyJobs = allJobs.filter(j => j.application_date === null && j.application_considered);

    content.innerHTML = createListOfJobs(stillToApplyJobs).innerHTML;
}

(async () => {
    updateContent();
    browser.storage.local.onChanged.addListener(updateContent);
})();
