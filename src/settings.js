document.addEventListener("DOMContentLoaded", async (e) => {
    const jobsPath = await browser.storage.sync.get("jobsPath");
    document.querySelector("#jobs-path").value = jobsPath.jobsPath || "";
});

document.querySelector("form").addEventListener("submit", (e) => {
    e.preventDefault();
    browser.storage.sync.set({jobsPath: document.querySelector("#jobs-path").value});
});
