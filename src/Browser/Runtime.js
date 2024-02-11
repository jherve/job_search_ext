export function onClickedAddListenerImpl (fn) {
  return browser.browserAction.onClicked.addListener(fn);
}

export function tabsSendMessageImpl (tabId, message) {
  return browser.tabs.sendMessage(tabId, message);
}
