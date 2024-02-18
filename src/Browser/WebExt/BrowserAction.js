export function onClickedAddListenerImpl (fn) {
  return browser.browserAction.onClicked.addListener(fn);
}

export function setBadgeTextImpl(text, tabId) {
  return browser.browserAction.setBadgeText({ text, tabId });
}

export function setBadgeBackgroundColorImpl(color, tabId) {
  return browser.browserAction.setBadgeBackgroundColor({ color, tabId });
}
