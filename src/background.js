var Background = require("../output/ExampleWebExt.Background");

function main() {
  Background.main();
}

browser.browserAction.onClicked.addListener(async (tab) => {
  try {
    browser.tabs.sendMessage(tab.id, "Message from JS background");
  } catch (error) {
    error(error);
  }
});

main();
