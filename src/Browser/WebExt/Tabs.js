export function sendMessageImpl (tabId, message) {
  return browser.tabs.sendMessage(tabId, message);
}
