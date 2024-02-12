export function setImpl (obj) {
  return browser.storage.sync.set(obj);
}

export function getImpl (key) {
  return browser.storage.sync.get(key);
}
