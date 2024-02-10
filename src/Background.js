export function onClickedAddListener (fn) {
  return function () {
    return browser.browserAction.onClicked.addListener(fn);
  }
}

export function onClickedAddListener1 (fn) {
  return function () {
    return browser.browserAction.onClicked.addListener(fn);
  }
}

export function mkListenerOne (fn) {
  return function () {
    return function (one) {
      return fn(one)();
    }
  }
};
