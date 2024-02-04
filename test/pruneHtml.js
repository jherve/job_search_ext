#! /usr/bin/env node

// This can be called with e.g. `test/pruneHtml.js path/to/the/file.html`
// The same file can then be prettified using `npx prettier --parser html --print-width 250 -w path/to/the/file.html`

const fs = require("fs");
const url = require("url");
const jsdom = require("jsdom");
const {JSDOM} = jsdom;

const [_1, _2, filePath] = process.argv;
const content = fs.readFileSync(filePath);
const dom = new JSDOM(content);

function removeTrackers(link) {
    const u = new URL(link);
    u.searchParams.delete("courseClaim");
    u.searchParams.delete("eBP");
    u.searchParams.delete("refId");
    u.searchParams.delete("trackingId");
    u.searchParams.delete("destRedirectURL");
    return url.format(u);
}

dom.window.document.querySelectorAll("code").forEach(el => el.remove());
dom.window.document.querySelectorAll("meta").forEach(el => el.remove());
dom.window.document.querySelectorAll("script").forEach(el => el.remove());
dom.window.document.querySelectorAll("a").forEach(el => el.href = removeTrackers(el.href));

fs.writeFileSync(filePath, dom.window.document.documentElement.innerHTML);
