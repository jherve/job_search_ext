const wait = ms => new Promise(resolve => setTimeout(resolve, ms));

export const sleepImpl = ms => () => wait(ms);
