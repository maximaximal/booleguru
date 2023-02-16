class StdoutProcessor {
    stdout(code) {
        if (code === "\n".charCodeAt(0) && this.stdout_buf !== "") {
            postMessage({"type": "out", "data": this.stdout_buf});
            this.stdout_buf = "";
        } else {
            this.stdout_buf += String.fromCharCode(code);
        }
    }

    stderr(code) {
        if (code === "\n".charCodeAt(0) && this.stderr_buf !== "") {
            postMessage({"type": "err", "data": this.stdout_buf});
            this.stderr_buf = "";
        } else {
            this.stderr_buf += String.fromCharCode(code);
        }
    }

    constructor() {
        this.stdout_buf = "";
        this.stderr_buf = "";
        this.stdout_str = "";
        this.stderr_str = "";
    }

    async load(loaderFunc) {
        let self = this;
        let options = {
            preRun: function (mod) {
                mod.FS.init(self.stdout.bind(self),
                            self.stderr.bind(self));
            },
        };
        this.module = await loaderFunc(options);
        return this.module;
    }

    prepareRun(input_str = "") {
        this.stdout_buf = "";
        this.stderr_buf = "";
        this.stdout_str = "";
        this.stderr_str = "";
    }
}

importScripts("./booleguru.js")

let processor = new StdoutProcessor();

async function load_execute() {
    let mod = await processor.load(createBooleguruModule);
    return mod.execute;
}

let loaded = false;
let execute_func = null;

async function execute(query, type) {
    if(!loaded) {
        execute_func = await load_execute();
        loaded = true;
    }
    return execute_func(query, type);
}

onmessage = async (msg) => {
    switch(msg.type) {
    case "task":
        await execute(msg.query, msg.query_type);
        break;
    }
}
