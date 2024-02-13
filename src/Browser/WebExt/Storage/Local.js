export function setImpl (obj) {
  return browser.storage.local.set(obj);
}

export function getImpl (key) {
  return browser.storage.local.get(key);
}

export function clearImpl () {
  return browser.storage.local.clear();
}
