export function postMessageImpl (port, message) {
  return port.postMessage(message);
}

export function onMessageAddListenerImpl (port, fn) {
  return port.onMessage.addListener(fn);
}
