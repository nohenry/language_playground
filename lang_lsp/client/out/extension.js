"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
const path = require("path");
const vscode_1 = require("vscode");
const net = require("net");
const child_process_1 = require("child_process");
const node_1 = require("vscode-languageclient/node");
let client;
async function activate(context) {
    let serverModule = vscode_1.workspace.getConfiguration('lang').get('lsPath');
    let debug = vscode_1.workspace.getConfiguration('lang').get('debug');
    if (serverModule === '' || serverModule === undefined) {
        serverModule = await promptLSPath();
    }
    if (serverModule === '' || serverModule === undefined) {
        console.error('Unable to find language server binary!');
        return;
    }
    let connectionInfo = {
        port: 5007,
        host: '127.0.0.1',
    };
    // Options to control the language client
    const clientOptions = {
        // Register the server for plain text documents
        documentSelector: [{ scheme: 'file', language: 'lang' }],
        synchronize: {
            // Notify the server about file changes to '.clientrc files contained in the workspace
            fileEvents: vscode_1.workspace.createFileSystemWatcher('**/.clientrc'),
        },
    };
    let serverOptions = () => {
        return new Promise((resolve, reject) => {
            if (debug) {
                let socket = net.connect(connectionInfo);
                let result = {
                    writer: socket,
                    reader: socket,
                };
                resolve(result);
            }
            else {
                let ls = (0, child_process_1.exec)(`${serverModule}`, (stderr, stdout, _) => {
                    if (stderr) {
                        console.error(`exec error: ${stderr} ${serverModule}`);
                        return;
                    }
                    console.log(`stdout: ${stdout}`);
                    console.error(`stderr: ${stderr}`);
                });
                console.log(ls);
                ls.stderr.on('data', data => {
                    console.error(data);
                });
                ls.stderr.on('error', console.error);
                ls.stdout.on('data', data => {
                    console.log('fjsdklf', data);
                    let socket = net.connect(connectionInfo);
                    let result = {
                        writer: socket,
                        reader: socket,
                    };
                    resolve(result);
                });
                ls.stdout.on('error', console.error);
            }
        });
    };
    // Create the language client and start the client.
    client = new node_1.LanguageClient('languageServerExample', 'Language Server Example', serverOptions, clientOptions);
    // Start the client. This will also launch the server
    client.start();
    let clientPromise = new Promise((resolve, reject) => {
        client.onReady().then(() => {
            resolve(client);
        }, error => {
            reject(error);
        });
    });
    vscode_1.workspace.registerFileSystemProvider('lsif', new LsifFS(clientPromise), {
        isCaseSensitive: true,
        isReadonly: true,
    });
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
async function promptLSPath() {
    let options = {
        prompt: 'Please enter path to lang langauage server: ',
    };
    let value = await vscode_1.window.showInputBox(options);
    value = path.normalize(value);
    vscode_1.workspace.getConfiguration('lang').update('lsPath', value, vscode_1.ConfigurationTarget.Global);
    return value;
}
var FileType;
(function (FileType) {
    FileType.Unknown = 0;
    FileType.File = 1;
    FileType.Directory = 2;
    FileType.SymbolicLink = 64;
})(FileType || (FileType = {}));
var StatFileRequest;
(function (StatFileRequest) {
    StatFileRequest.type = new node_1.RequestType('lsif/statFile');
})(StatFileRequest || (StatFileRequest = {}));
var ReadFileRequest;
(function (ReadFileRequest) {
    ReadFileRequest.type = new node_1.RequestType('lsif/readfile');
})(ReadFileRequest || (ReadFileRequest = {}));
var ReadDirectoryRequest;
(function (ReadDirectoryRequest) {
    ReadDirectoryRequest.type = new node_1.RequestType('lsif/readDirectory');
})(ReadDirectoryRequest || (ReadDirectoryRequest = {}));
class LsifFS {
    constructor(client) {
        this.client = client;
        this.emitter = new vscode_1.EventEmitter();
        this.onDidChangeFile = this.emitter.event;
    }
    watch(uri, options) {
        // The LSIF file systrem never changes.
        return node_1.Disposable.create(() => { });
    }
    async stat(uri) {
        const client = await this.client;
        return client
            .sendRequest(StatFileRequest.type, {
            uri: client.code2ProtocolConverter.asUri(uri),
        })
            .then(value => {
            if (!value) {
                throw vscode_1.FileSystemError.FileNotFound(uri);
            }
            return value;
        }, error => {
            throw vscode_1.FileSystemError.FileNotFound(uri);
        });
    }
    async readDirectory(uri) {
        const client = await this.client;
        const params = {
            uri: client.code2ProtocolConverter.asUri(uri),
        };
        return client
            .sendRequest(ReadDirectoryRequest.type, params)
            .then(values => {
            return values;
        });
    }
    async readFile(uri) {
        const client = await this.client;
        const params = {
            uri: client.code2ProtocolConverter.asUri(uri),
        };
        return client.sendRequest(ReadFileRequest.type, params).then(value => {
            const result = new Uint8Array(Buffer.from(value, 'base64'));
            return result;
        });
    }
    createDirectory(uri) {
        throw new Error('File system is readonly.');
    }
    writeFile(uri, content, options) {
        throw new Error('File system is readonly.');
    }
    delete(uri, options) {
        throw new Error('File system is readonly.');
    }
    rename(oldUri, newUri, options) {
        throw new Error('File system is readonly.');
    }
}
//# sourceMappingURL=extension.js.map