export function toArrayOfObjects(field) {
    return function (obj) {
        return {
            [field]: Object.entries(obj[field]).map(a => a[1]),
            tag: obj.tag
        };
    }
}
