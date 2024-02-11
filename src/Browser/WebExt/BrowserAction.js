export function onClickedAddListenerImpl (fn) {
  return browser.browserAction.onClicked.addListener(fn);
}
