var Content = require("../output/ExampleWebExt.Content");

function main() {
    Content.main();
}

browser.runtime.onMessage.addListener(async message => {
    console.log("[message received]", message);
});

main();
