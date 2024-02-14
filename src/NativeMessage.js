export function toArrayOfObjects(field) {
    return function (obj) {
        return {
            values: [Object.entries(obj.values[field]).map(a => a[1])],
            tag: obj.tag
        };
    }
}
