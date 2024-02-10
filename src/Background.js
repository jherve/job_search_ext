export function onClickedAddListener (fn) {
  return function () {
    return browser.browserAction.onClicked.addListener(fn);
  }
}

export function tabsSendMessage (tabId, message) {
  return browser.tabs.sendMessage(tabId, message);
}
