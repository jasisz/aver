// Contract-directed transport. Values crossing workers contain ordinary JS
// data (including BigInt), never references into another instance's GC heap.
const utf8 = new TextEncoder();
const text = new TextDecoder();
export const identifier = (name) => "n" + Array.from(utf8.encode(name), b => b.toString(16).padStart(2, "0")).join("");

// A module carries one descriptor if it has either door: job kinds it can be
// asked to run, or a wait a host has to decode. A program that waits on
// sockets without running a job of its own has the second and not the first,
// so `kinds` may legitimately be empty.
export function workManifest(module) {
    const sections = WebAssembly.Module.customSections(module, "aver:work/v1");
    if (sections.length !== 1) throw new Error("work: expected one aver:work/v1 descriptor");
    const manifest = JSON.parse(text.decode(sections[0]));
    if (manifest.version !== 1) throw new Error("work: unsupported ABI version");
    manifest.kinds ??= [];
    return manifest;
}

export class WorkCodec {
    constructor(exports, manifest) {
        this.exports = exports;
        this.types = manifest.types;
    }
    call(type, operation, ...args) {
        const name = `__cap_abi_${identifier(type)}_${operation}`;
        const helper = this.exports[name];
        if (!helper) throw new Error(`work: missing ABI helper ${name}`);
        return helper(...args);
    }
    bytesIn(bytes, helper) {
        const memory = this.exports.memory;
        if (bytes.byteLength > memory.buffer.byteLength) {
            memory.grow(Math.ceil((bytes.byteLength - memory.buffer.byteLength) / 65536));
        }
        new Uint8Array(memory.buffer, 0, bytes.byteLength).set(bytes);
        return this.exports[helper](bytes.byteLength);
    }
    bytesOut(value, helper) {
        const length = this.exports[helper](value);
        return new Uint8Array(this.exports.memory.buffer, 0, Number(length)).slice();
    }
    stringIn(value) { return this.bytesIn(utf8.encode(value), "__rt_string_from_lm"); }
    stringOut(value) { return text.decode(this.bytesOut(value, "__rt_string_to_lm")); }
    descriptor(type) {
        const shape = this.types[type];
        if (!shape) throw new Error(`work: missing type descriptor ${type}`);
        return shape;
    }
    argument(type, value) { return type === "Unit" ? [] : [this.encode(type, value)]; }
    field(type, operation, fieldType, value) {
        return fieldType === "Unit" ? null : this.decode(fieldType, this.call(type, operation, value));
    }
    encode(type, value) {
        const { kind, args = [], fields, variants } = this.descriptor(type);
        switch (kind) {
            case "Unit": return undefined;
            case "Bool": return value ? 1 : 0;
            case "Float": return value;
            case "String": return this.stringIn(value);
            case "Int": {
                if (typeof value !== "bigint") throw new Error("work: Int transport requires BigInt");
                if (value >= -(1n << 63n) && value < (1n << 63n)) return this.call(type, "from_i64", value);
                const parsed = this.call(type, "from_decimal", this.stringIn(value.toString()));
                const result = "Result<Int, String>";
                if (this.call(result, "tag", parsed) !== 1) throw new Error("work: invalid Int transport");
                return this.call(result, "ok_value", parsed);
            }
            case "Bytes": return this.bytesIn(value, "__rt_bytes_from_lm");
            case "Resource": return value;
            case "Result": {
                const ok = Object.hasOwn(value, "ok");
                return this.call(type, ok ? "ok" : "err", ...this.argument(args[ok ? 0 : 1], value[ok ? "ok" : "err"]));
            }
            case "Option": return value === null ? this.call(type, "none") : this.call(type, "some", ...this.argument(args[0], value.some));
            case "List": {
                let list = this.call(type, "nil");
                for (let i = value.length - 1; i >= 0; i--) list = this.call(type, "cons", ...this.argument(args[0], value[i]), list);
                return list;
            }
            case "Vector": {
                const vector = this.call(type, "new", value.length);
                value.forEach((item, i) => this.call(type, "set", vector, i, ...this.argument(args[0], item)));
                return vector;
            }
            case "Map": {
                let map = this.call(type, "empty");
                for (const [key, item] of value) map = this.call(type, "set", map, ...this.argument(args[0], key), ...this.argument(args[1], item));
                return map;
            }
            case "Tuple": return this.call(type, "make", ...args.flatMap((ty, i) => this.argument(ty, value[i])));
            case "Record": return this.call(type, "make", ...fields.flatMap(([name, ty]) => this.argument(ty, value[name])));
            case "Sum": {
                const variant = variants.find(v => v.name === value.variant);
                if (!variant) throw new Error(`work: unknown ${type} variant ${value.variant}`);
                return this.call(type, `variant_${identifier(variant.name)}_make`, ...variant.fields.flatMap((ty, i) => this.argument(ty, value.fields[i])));
            }
            default: throw new Error(`work: unsupported transport ${type}`);
        }
    }
    decode(type, value) {
        const { kind, args = [], fields, variants } = this.descriptor(type);
        switch (kind) {
            case "Unit": return null;
            case "Bool": return value !== 0;
            case "Float": return value;
            case "String": return this.stringOut(value);
            case "Int": return BigInt(this.stringOut(this.call(type, "to_decimal", value)));
            case "Bytes": return this.bytesOut(value, "__rt_bytes_to_lm");
            case "Resource": return value;
            case "Result": {
                const ok = this.call(type, "tag", value) === 1;
                return { [ok ? "ok" : "err"]: this.field(type, ok ? "ok_value" : "err_value", args[ok ? 0 : 1], value) };
            }
            case "Option": return this.call(type, "tag", value) === 0 ? null : { some: this.field(type, "value", args[0], value) };
            case "List": {
                const items = [];
                while (!this.call(type, "is_empty", value)) {
                    items.push(this.field(type, "head", args[0], value));
                    value = this.call(type, "tail", value);
                }
                return items;
            }
            case "Vector": return Array.from({ length: this.call(type, "len", value) }, (_, i) => args[0] === "Unit" ? null : this.decode(args[0], this.call(type, "get", value, i)));
            case "Map": {
                const keys = this.decode(`List<${args[0]}>`, this.call(type, "keys", value));
                return keys.map(key => {
                    const item = this.call(type, "get", value, ...this.argument(args[0], key));
                    return [key, this.decode(`Option<${args[1]}>`, item).some];
                });
            }
            case "Tuple": return args.map((ty, i) => this.field(type, `field_${i}`, ty, value));
            case "Record": return Object.fromEntries(fields.map(([name, ty]) => [name, this.field(type, `field_${identifier(name)}`, ty, value)]));
            case "Sum": {
                const variant = variants[this.call(type, "kind", value)];
                if (!variant) throw new Error(`work: invalid ${type} variant`);
                return { variant: variant.name, fields: variant.fields.map((ty, i) => this.field(type, `variant_${identifier(variant.name)}_field_${i}`, ty, value)) };
            }
            default: throw new Error(`work: unsupported transport ${type}`);
        }
    }
}
