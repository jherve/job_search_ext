export function colorVisitedJobsLoopImpl () {
    // We need to poll at regular intervals to catch all the new elements
    // that appear as the page loads. A cleaner solution would involve use
    // of MutationObserver but this simple timer does the trick.
    setInterval(async () => {
        document.querySelectorAll("div[data-job-id]").forEach(async el => {
            const id = `linked_in_${el.dataset.jobId}`;

            const inStorage = (await browser.storage.local.get(id))[id];
            if (inStorage) {
                el.style.backgroundColor = "#ffe4da";
            }
        })
    }, 1000);
}
