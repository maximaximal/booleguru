let worker_available_promise_resolve = null;
let worker_available_promise = null;

function reset_worker_available_promise() {
    let worker_available_promise = new Promise((resolve, reject) => {
        worker_available_promise_resolve = resolve;
    });
}

reset_worker_available_promise();

class BooleguruWorker {
    constructor() {
        this.worker = new Worker('booleguru-worker.js');
        this.worker.onmessage = this.onmessage.bind(this);
        this.reset();
    }

    reset() {
        this.working = false;
        this.request_cb = null;
        this.stdout_cb = null;
        this.stderr_cb = null;
        this.finish = null;
        this.cancel = null;

        worker_available_promise_resolve(this);
    }

    terminate() {
        this.worker.terminate();
        if(this.cancel) {
            this.cancel();
        }

        this.worker = new Worker('booleguru-worker.js');
        this.worker.onmessage = this.onmessage.bind(this);

        this.reset();
    }

    onmessage(msg) {
        switch(msg.type) {
        case "request":
            this.worker.postMessage({"type": "reply", "code": this.request_cb(msg.name)});
            break;
        case "out":
            this.stdout_cb(msg.data);
            break;
        case "err":
            this.stderr_cb(msg.data);
            break;
        case "done":
            let finish = this.finish;
            this.reset();
            finish();
            break;
        }
    }

    run(promise_finish, promise_cancel, query, query_type, request_cb, stdout_cb, stderr_cb) {
        assert(!this.working, "Worker must not be running in order to post new work.");
        this.request_cb = request_cb;
        this.stdout_cb = stdout_cb;
        this.stderr_cb = stderr_cb;
        this.finish = promise_finish;
        this.cancel = promise_cancel;
        this.worker.postMessage({
            "type": "task",
            "query": query,
            "query_type": query_type
        });
    }
}

let workers = [
    new BooleguruWorker('booleguru-worker.js')
];

async function find_idle_worker() {
    assert(worker_available_promise, "Worker availability promise must always be there!");
    let worker = await worker_available_promise;

    // Now, reset the availability promise so the next time this function is
    // called, it can again wait for a worker to be finished.
    reset_worker_available_promise();

    return worker;
}

function Deferred() {
    var self = this;
    this.promise = new Promise(function(resolve, reject) {
        self.reject = reject
        self.resolve = resolve
    })
}

export function execute(query, request_cb, stdout_cb, stderr_cb) {
    let deferred_terminator = new Deferred();
    let promise = new Promise((resolve, reject) => {
        find_idle_worker().then(w => {
            w.run(resolve, reject, query, request_cb, stdout_cb, stderr_cb);
            deferred_terminator.resolve(w.terminator.bind(w));
        });
    });
    let terminate = async () => {
        let terminator = await deferred_terminator;
        terminator();
    }
    return (promise, terminate);
}
