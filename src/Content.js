const rejected_color = "rgb(255, 108, 108)";
const applied_color = "rgb(236, 253, 207)";
const seen_color = "rgb(204, 241, 255)";

export function colorVisitedJobsLoopImpl () {
    // Add a div on the left side that displays a legend of the color used in the UI
    const el = document.createElement("div");
    el.innerHTML = `
        <div id="jobs-search-legend" style="position: absolute; top: 50%; background: white;">
            <p style="background: ${seen_color}">Seen</p>
            <p style="background: ${applied_color}">Applied</p>
            <p style="background: ${rejected_color}">Rejected</p>
        </div>
    `;
    document.querySelector("body").appendChild(el);

    // We need to poll at regular intervals to catch all the new elements
    // that appear as the page loads. A cleaner solution would involve use
    // of MutationObserver but this simple timer does the trick.
    setInterval(async () => {
        document.querySelectorAll("div[data-job-id]").forEach(async el => {
            const id = `linked_in_${el.dataset.jobId}`;

            const inStorage = (await browser.storage.local.get(id))[id];
            if (inStorage) {
                el.style.backgroundColor = getBackgroundColor(inStorage);
            }
        })
    }, 1000);
}

function getBackgroundColor(job) {
    if (job.application_rejection_date)
        return rejected_color;
    else if (job.application_date)
        return applied_color;
    else
        return seen_color;
}
