var Background = require("../output/ExampleWebExt.Background");

function main() {
  browser.browserAction.onClicked.addListener(async (tab) => {
    console.log("clicked !");
  });
  browser.browserAction.onClicked.addListener(async (tab) => {
    console.log("clicked 2 !");
  });

  Background.main();
}

main();
