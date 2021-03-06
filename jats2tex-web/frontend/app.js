"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = Object.setPrototypeOf ||
        ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
        function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var __assign = (this && this.__assign) || Object.assign || function(t) {
    for (var s, i = 1, n = arguments.length; i < n; i++) {
        s = arguments[i];
        for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
            t[p] = s[p];
    }
    return t;
};
exports.__esModule = true;
require("codemirror");
require("codemirror/lib/codemirror.css");
require("codemirror/mode/lua/lua");
require("codemirror/mode/stex/stex");
require("codemirror/mode/xml/xml");
require("codemirror/mode/yaml/yaml");
require("codemirror/theme/material.css");
require("react-tabs/style/react-tabs.css");
var Codemirror = require("react-codemirror");
var React = require("react");
var SplitPane = require("react-split-pane");
var debounce = require("lodash/debounce");
var querystring = require("querystring");
var moment = require("moment");
var react_router_dom_1 = require("react-router-dom");
var react_1 = require("react");
var react_tabs_1 = require("react-tabs");
var react_split_pane_1 = require("react-split-pane");
require("./app.css");
var SourceEditor = /** @class */ (function (_super) {
    __extends(SourceEditor, _super);
    function SourceEditor() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    SourceEditor.prototype.render = function () {
        var options = {
            lineNumbers: true,
            theme: 'material',
            mode: this.props.mode
        };
        return React.createElement(Codemirror, __assign({ options: options }, this.props));
    };
    return SourceEditor;
}(react_1.Component));
var PreviewPdf = /** @class */ (function (_super) {
    __extends(PreviewPdf, _super);
    function PreviewPdf() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.state = {
            url: null
        };
        return _this;
    }
    PreviewPdf.prototype.componentDidUpdate = function (prevProps) {
        // if (this.props.value && this.props.value !== prevProps.value) {
        this.runLaTeX();
        // }
    };
    PreviewPdf.prototype.componentDidMount = function () {
        if (this.props.value) {
            this.runLaTeX();
        }
    };
    PreviewPdf.prototype.runLaTeX = function () {
        var _this = this;
        fetch('https://tex.beijaflor.io', {
            method: 'post',
            headers: {
                'content-type': 'application/json'
            },
            mode: 'cors',
            body: JSON.stringify({
                text: this.props.value
            })
        }).then(function (res) {
            _this.setState({
                url: res.url
            });
        });
    };
    PreviewPdf.prototype.renderNoPreview = function () {
        return (React.createElement("div", { style: {
                display: 'flex',
                height: '100%',
                width: '100%',
                alignItems: 'center',
                justifyContent: 'center'
            } }, "Preview not available yet"));
    };
    PreviewPdf.prototype.render = function () {
        if (this.props.value && this.state.url) {
            return (React.createElement("iframe", { style: { height: '100%', width: '100%' }, frameborder: "0", src: this.state.url }));
        }
        if (this.props.value && !this.state.url) {
            return (React.createElement("div", { style: {
                    display: 'flex',
                    height: '100%',
                    width: '100%',
                    alignItems: 'center',
                    justifyContent: 'center'
                } }, "Rendering LaTeX..."));
        }
        return (React.createElement("div", { style: { height: '100%', width: '100%' } }, this.renderNoPreview()));
    };
    return PreviewPdf;
}(react_1.Component));
var Workspace = /** @class */ (function (_super) {
    __extends(Workspace, _super);
    function Workspace() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.state = {
            conversionResult: '',
            template: '',
            text: '',
            yamlTemplate: '',
            title: '',
            isLoading: true,
            isSaving: false,
            serverData: {},
            error: null,
            isDirty: false,
            lastSaveTime: null
        };
        _this.autoSave = debounce(function () {
            _this.save();
        }, 2000);
        _this.save = function () {
            _this.setState({
                isSaving: true
            });
            fetch(document.location.pathname, {
                method: 'put',
                credentials: 'include',
                headers: {
                    'content-type': 'application/x-www-form-urlencoded; charset=utf-8'
                },
                body: querystring.stringify({
                    title: _this.state.title || '',
                    latex: _this.state.conversionResult || null,
                    template: _this.state.yamlTemplate || '',
                    xml: _this.state.text || '',
                    pdfUrl: _this.state.pdfUrl || null,
                    isPublic: _this.state.isPublic || false
                })
            }).then(function () {
                _this.setState({
                    isSaving: false,
                    isDirty: false,
                    lastSaveTime: moment()
                });
            });
        };
        _this.runConvert = function () {
            _this.setState({
                isConverting: true,
                error: null
            });
            fetch('/', {
                method: 'post',
                headers: {
                    'content-type': 'application/x-www-form-urlencoded'
                },
                body: querystring.stringify({
                    text: _this.state.text,
                    template: _this.state.yamlTemplate
                })
            })
                .then(function (res) {
                return res.text()
                    .then(function (t) {
                    if (res.status === 500) {
                        var errorUnescaped = t.slice(t.indexOf("<pre>") + 5, t.indexOf("</pre>"));
                        var parser = new DOMParser()
                            .parseFromString(errorUnescaped, "text/html");
                        var error = (parser.documentElement.textContent || "Failed to convert")
                            .replace(/\\n/g, ' ');
                        _this.setState({ isConverting: false, error: error });
                    }
                    else if (res.status !== 200) {
                        _this.setState({ isConverting: false, error: "Failed to convert" });
                    }
                    else {
                        _this.setState({ conversionResult: t, isConverting: false });
                    }
                });
            }, function (err) {
                _this.setState({
                    isConverting: false,
                    error: 'Failed to connect to server'
                });
            });
        };
        return _this;
    }
    Workspace.prototype.componentDidMount = function () {
        var _this = this;
        this.fetchWorkspace();
        setInterval(function () {
            _this.forceUpdate();
        }, 60000);
    };
    Workspace.prototype.fetchWorkspace = function () {
        var _this = this;
        this.setState({
            isLoading: true
        });
        fetch(document.location.pathname, {
            method: 'get',
            credentials: 'include',
            headers: {
                accept: 'application/json'
            }
        })
            .then(function (res) { return res.json(); })
            .then(function (workspace) {
            var serverData = {
                yamlTemplate: workspace.template,
                conversionResult: workspace.latex,
                text: workspace.xml,
                title: workspace.title,
                isPublic: workspace.isPublic
            };
            _this.setState(__assign({}, serverData, { isLoading: false, serverData: serverData }));
        });
    };
    Workspace.prototype.onChange = function () {
        this.setState({ isDirty: true });
        this.autoSave();
    };
    Workspace.prototype.renderSaving = function () {
        if (!this.state.isSaving)
            return null;
        return (React.createElement("div", { className: "BottomMessage" }, "Saving workspace..."));
    };
    Workspace.prototype.renderConverting = function () {
        if (!this.state.isConverting)
            return null;
        return (React.createElement("div", { className: "BottomMessage" }, "Converting XML..."));
    };
    Workspace.prototype.renderLoading = function () {
        if (!this.state.isLoading)
            return null;
        return (React.createElement("div", { className: "BottomMessage" }, "Fetching workspace..."));
    };
    Workspace.prototype.renderDirty = function () {
        if (!this.state.isDirty)
            return null;
        return (React.createElement("div", { className: "BottomMessage" }, "Changes detected"));
    };
    Workspace.prototype.renderBottomMessage = function () {
        return (this.renderSaving() ||
            this.renderLoading() ||
            this.renderConverting() ||
            this.renderDirty());
    };
    Workspace.prototype.getSaveStatus = function () {
        return this.state.isSaving && "Saving..."
            || this.state.isDirty && "Changes detected"
            || this.state.lastSaveTime && ("Saved " + this.state.lastSaveTime.fromNow())
            || "";
    };
    Workspace.prototype.render = function () {
        var _this = this;
        if (this.state.isLoading)
            return null;
        return (React.createElement("div", null,
            React.createElement("div", { style: {
                    position: 'relative',
                    height: 'calc(100vh - 98px)',
                    width: '100vw',
                    marginTop: '-98px',
                    marginBottom: '98px',
                    paddingTop: '77px'
                } },
                React.createElement(SplitPane, { split: "vertical", defaultSize: document.body.offsetWidth / 2, minSize: 0.2 * document.body.offsetWidth },
                    React.createElement("div", { style: __assign({}, react_split_pane_1.paneStyle, react_split_pane_1.pane1Style) },
                        React.createElement(react_tabs_1.Tabs, null,
                            React.createElement(react_tabs_1.TabList, null,
                                React.createElement(react_tabs_1.Tab, null, "XML"),
                                React.createElement(react_tabs_1.Tab, null, "Template"),
                                React.createElement(react_tabs_1.Tab, null, "Split"),
                                React.createElement(react_tabs_1.Tab, null, "Workspace Data")),
                            React.createElement(react_tabs_1.TabPanel, null,
                                React.createElement(SourceEditor, { mode: "xml", value: this.state.text, onChange: function (e) {
                                        _this.setState({ text: e });
                                        _this.onChange();
                                    } })),
                            React.createElement(react_tabs_1.TabPanel, null,
                                React.createElement(SourceEditor, { mode: "yaml", value: this.state.yamlTemplate, onChange: function (e) {
                                        _this.setState({ yamlTemplate: e });
                                        _this.onChange();
                                    } })),
                            React.createElement(react_tabs_1.TabPanel, null,
                                React.createElement(SplitPane, { split: "horizontal" },
                                    React.createElement("div", { style: __assign({}, react_split_pane_1.paneStyle, react_split_pane_1.pane1Style) },
                                        React.createElement(SourceEditor, { mode: "xml", autoSave: true, value: this.state.text, onChange: function (e) {
                                                _this.setState({ text: e });
                                                _this.onChange();
                                            } })),
                                    React.createElement("div", { style: __assign({}, react_split_pane_1.paneStyle, react_split_pane_1.pane2Style) },
                                        React.createElement(SourceEditor, { mode: "yaml", autoSave: true, value: this.state.yamlTemplate, onChange: function (e) {
                                                _this.setState({ yamlTemplate: e });
                                                _this.onChange();
                                            } })))),
                            React.createElement(react_tabs_1.TabPanel, null,
                                React.createElement("div", { className: "container-fluid" },
                                    React.createElement("form", { onSubmit: function (e) {
                                            e.preventDefault();
                                            _this.save();
                                        } },
                                        React.createElement("div", { className: "form-group" },
                                            React.createElement("label", { htmlFor: "title" }, "Workspace Title"),
                                            React.createElement("input", { name: "title", type: "text", className: "form-control", value: this.state.title, onChange: function (e) {
                                                    _this.setState({ title: e.target.value });
                                                    _this.onChange();
                                                }, placeholder: "Name this workspace" })),
                                        React.createElement("div", { className: "form-group" },
                                            React.createElement("label", { htmlFor: "isPublic" },
                                                React.createElement("input", { id: "isPublic", name: "isPublic", type: "checkbox", checked: this.state.isPublic, onChange: function (e) {
                                                        return _this.setState({ isPublic: e.target.checked });
                                                    } }),
                                                ' ',
                                                "Make this a publically accessible and editable workspace")),
                                        React.createElement("div", { className: "form-group" },
                                            React.createElement("button", { className: "btn btn-default", disabled: this.state.isLoading || this.state.isSaving }, "Save"))))))),
                    React.createElement("div", { style: __assign({}, react_split_pane_1.paneStyle, react_split_pane_1.pane2Style) },
                        React.createElement(react_tabs_1.Tabs, null,
                            React.createElement(react_tabs_1.TabList, null,
                                React.createElement(react_tabs_1.Tab, null, "LaTeX"),
                                React.createElement(react_tabs_1.Tab, null, "PDF"),
                                React.createElement(react_tabs_1.Tab, null, "Split")),
                            this.state.error
                                ? React.createElement("pre", { className: "ErrorMessage alert alert-danger" }, this.state.error)
                                : '',
                            React.createElement(react_tabs_1.TabPanel, null,
                                React.createElement(SourceEditor, { mode: "stex", key: this.state.conversionResult, autoSave: true, value: this.state.conversionResult, onChange: function () { return _this.onChange(); } })),
                            React.createElement(react_tabs_1.TabPanel, null,
                                React.createElement("div", { style: { height: '100%' } },
                                    React.createElement(PreviewPdf, { value: this.state.conversionResult }))),
                            React.createElement(react_tabs_1.TabPanel, null,
                                React.createElement(SplitPane, { split: "horizontal", defaultSize: document.body.offsetHeight * 0.45, minSize: 0.2 },
                                    React.createElement("div", { style: __assign({}, react_split_pane_1.paneStyle, react_split_pane_1.pane1Style) },
                                        React.createElement(SourceEditor, { mode: "stex", key: this.state.conversionResult, autoSave: true, value: this.state.conversionResult })),
                                    React.createElement("div", { style: __assign({}, react_split_pane_1.paneStyle, react_split_pane_1.pane2Style) },
                                        React.createElement(PreviewPdf, { value: this.state.conversionResult })))))))),
            React.createElement("hr", null),
            React.createElement("div", { style: {
                    position: 'absolute',
                    top: 63,
                    right: 0,
                    zIndex: 10,
                    display: 'flex',
                    alignItems: 'flex-start'
                } },
                React.createElement("div", { key: this.getSaveStatus(), className: "StatusMessage" }, this.getSaveStatus()),
                React.createElement("button", { style: {
                        borderRadius: 0,
                        marginRight: 10
                    }, className: "btn btn-primary", onClick: this.save, disabled: this.state.isLoading || this.state.isSaving }, "Save"),
                React.createElement("button", { style: {
                        borderRadius: 0
                    }, className: "btn btn-primary", onClick: this.runConvert, disabled: this.state.isLoading || this.state.isSaving }, "Run jats2tex")),
            this.renderBottomMessage));
    };
    return Workspace;
}(react_1.Component));
var App = /** @class */ (function (_super) {
    __extends(App, _super);
    function App() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    App.prototype.render = function () {
        return (React.createElement("div", null,
            React.createElement(react_router_dom_1.BrowserRouter, null,
                React.createElement(react_router_dom_1.Route, { path: "/workspaces/:id", component: Workspace }))));
    };
    return App;
}(react_1.Component));
exports["default"] = App;
