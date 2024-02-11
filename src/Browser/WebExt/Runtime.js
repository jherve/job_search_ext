export function onMessageAddListenerImpl(fn) {
  return browser.runtime.onMessage.addListener(fn);
}

export function runtimeSendMessageImpl (message) {
  return browser.runtime.sendMessage(message);
}
