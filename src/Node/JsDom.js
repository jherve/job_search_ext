"use strict";
import { JSDOM } from "jsdom";

export const jsDomParse = function (string) {
  // JSDOM does not properly handle :scope CSS selector (https://github.com/jsdom/jsdom/issues/3067)
  // It does not handle innerText property either : https://github.com/jsdom/jsdom/issues/1245
  // This code is used to patch it (inspiration from : https://github.com/asamuzaK/domSelector#monkey-patch-jsdom)

  return function () {
    const jsdom = new JSDOM(string, {
      beforeParse: (window) => {
        const querySelectorAllFunc = window.Element.prototype.querySelectorAll;
        window.Element.prototype.querySelectorAll = function (...args) {
          if (!args.length) {
            throw new window.TypeError(
              "1 argument required, but only 0 present."
            );
          }
          const [selector] = args;

          if (selector.startsWith(":scope")) {
            this.id = "scope";
            const ret = this.parentElement.querySelectorAll(
              selector.replace(":scope", "#scope")
            );
            this.id = undefined;

            return ret;
          } else {
            return querySelectorAllFunc.apply(this, args);
          }
        };

        const querySelectorFunc = window.Element.prototype.querySelector;
        window.Element.prototype.querySelector = function (...args) {
          if (!args.length) {
            throw new window.TypeError(
              "1 argument required, but only 0 present."
            );
          }
          const [selector] = args;

          if (selector.startsWith(":scope")) {
            this.id = "scope";
            const ret = this.parentElement.querySelector(
              selector.replace(":scope", "#scope")
            );
            this.id = undefined;
            return ret;
          } else {
            return querySelectorFunc.apply(this, args);
          }
        };

        Object.defineProperty(window.Element.prototype, "innerText", {
          get() {
            return this.textContent;
          },
        });
      },
    });
    return jsdom.window.document;
  };
};
