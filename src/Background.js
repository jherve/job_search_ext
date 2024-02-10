export function onClickedAddListener (fn) {
  return function () {
    return browser.browserAction.onClicked.addListener(fn);
  }
}
