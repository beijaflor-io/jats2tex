import 'codemirror';
import 'codemirror/lib/codemirror.css';
import 'codemirror/mode/lua/lua';
import 'codemirror/mode/stex/stex';
import 'codemirror/mode/xml/xml';
import 'codemirror/mode/yaml/yaml';
import 'codemirror/theme/material.css';
import 'react-tabs/style/react-tabs.css';
import * as Codemirror from 'react-codemirror';
import * as React from 'react';
import * as SplitPane from 'react-split-pane';
import * as debounce from 'lodash/debounce';
import * as isEqual from 'lodash/isEqual';
import * as pick from 'lodash/pick';
import * as querystring from 'querystring';
import {BrowserRouter, Route} from 'react-router-dom';
import {Component} from 'react';
import {Tabs, TabList, Tab, TabPanel} from 'react-tabs';
import {paneStyle, pane1Style, pane2Style} from 'react-split-pane';

import './app.css';

class SourceEditor extends Component<{mode: string}, {}> {
  render() {
    const options = {
      lineNumbers: true,
      theme: 'material',
      mode: this.props.mode,
    };
    return <Codemirror options={options} {...this.props} />;
  }
}

class PreviewPdf extends Component<{value: string}, {url: string | null}> {
  state = {
    url: null,
  };

  componentDidUpdate(prevProps) {
    // if (this.props.value && this.props.value !== prevProps.value) {
      this.runLaTeX();
    // }
  }

  componentDidMount() {
    if (this.props.value) {
      this.runLaTeX();
    }
  }

  runLaTeX() {
    fetch('https://tex.beijaflor.io', {
      method: 'post',
      headers: {
        'content-type': 'application/json',
      },
      mode: 'cors',
      body: JSON.stringify({
        text: this.props.value,
      }),
    }).then(res => {
      this.setState({
        url: res.url,
      });
    });
  }

  renderNoPreview() {
    return (
      <div
        style={{
          display: 'flex',
          height: '100%',
          width: '100%',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        Preview not available yet
      </div>
    );
  }

  render() {
    if (this.props.value && this.state.url) {
      return (
        <iframe
          style={{height: '100%', width: '100%'}}
          frameborder="0"
          src={this.state.url}
        />
      );
    }

    if (this.props.value && !this.state.url) {
      return (
        <div
          style={{
            display: 'flex',
            height: '100%',
            width: '100%',
            alignItems: 'center',
            justifyContent: 'center',
          }}
        >
          Rendering LaTeX...
        </div>
      );
    }

    return (
      <div style={{height: '100%', width: '100%'}}>
        {this.renderNoPreview()}
      </div>
    );
  }
}

class Workspace extends Component {
  state = {
    conversionResult: '',
    template: '',
    text: '',
    yamlTemplate: '',
    title: '',
    isLoading: true,
    serverData: {},
  };

  componentDidMount() {
    this.fetchWorkspace();
  }

  fetchWorkspace() {
    this.setState({
      isLoading: true,
    });
    fetch(document.location.pathname, {
      method: 'get',
      credentials: 'include',
      headers: {
        accept: 'application/json',
      },
    })
      .then(res => res.json())
      .then(workspace => {
        const serverData = {
          yamlTemplate: workspace.template,
          conversionResult: workspace.latex,
          text: workspace.xml,
          title: workspace.title,
          isPublic: workspace.isPublic,
        };
        this.setState({
          ...serverData,
          isLoading: false,
          serverData,
        });
      });
  }

  componentDidUpdate() {
    this.autoSave();
  }

  isDirty() {
    const userKeys = [
      'conversionResult',
      'template',
      'text',
      'yamlTemplate',
      'title',
    ];
    return isEqual(
      pick(this.state, userKeys),
      pick(this.state.serverData, userKeys),
    );
  }

  autoSave = debounce(() => {
    /*if (this.state.isLoading || this.state.isConverting || this.state.isSaving)*/
      /*return;*/
    if (!this.isDirty()) return;
    this.save();
  }, 5000);

  save = () => {
    this.setState({
      isSaving: true,
    });

    fetch(document.location.pathname, {
      method: 'put',
      credentials: 'include',
      headers: {
        'content-type': 'application/x-www-form-urlencoded; charset=utf-8',
      },
      body: querystring.stringify({
        title: this.state.title || '',
        latex: this.state.conversionResult || null,
        template: this.state.yamlTemplate || '',
        xml: this.state.text || '',
        pdfUrl: this.state.pdfUrl || null,
        isPublic: this.state.isPublic || false,
      }),
    }).then(() => {
      this.setState({
        isSaving: false,
      });
    });
  };

  runConvert = () => {
    this.setState({
      isConverting: true,
    });

    fetch('/', {
      method: 'post',
      headers: {
        'content-type': 'application/x-www-form-urlencoded',
      },
      body: querystring.stringify({
        text: this.state.text,
        template: this.state.yamlTemplate,
      }),
    })
      .then(res => res.text())
      .then(
        t => {
          this.setState({conversionResult: t, isConverting: false});
        },
        err => {
          console.error(err);
          alert('Error converting.' + err);
          this.setState({
            isConverting: false,
          });
        },
      );
  };

  renderSaving() {
    if (!this.state.isSaving) return null;
    return (
      <div className="BottomMessage">
        Saving workspace...
      </div>
    );
  }

  renderConverting() {
    if (!this.state.isConverting) return null;
    return (
      <div className="BottomMessage">
        Converting XML...
      </div>
    );
  }

  renderLoading() {
    if (!this.state.isLoading) return null;
    return (
      <div className="BottomMessage">
        Fetching workspace...
      </div>
    );
  }

  renderDirty() {
    if (!this.isDirty()) return null;
    return (
      <div className="BottomMessage">
        Changes detected
      </div>
    );
  }

  renderBottomMessage() {
    return (
      this.renderSaving() ||
      this.renderLoading() ||
      this.renderConverting() ||
      this.renderDirty()
    );
  }

  render() {
    if (this.state.isLoading) return null;
    return (
      <div>
        <div
          style={{
            position: 'relative',
            height: 'calc(100vh - 98px)',
            width: '100vw',
            marginTop: '-98px',
            marginBottom: '98px',
            paddingTop: '77px',
          }}
        >
          <SplitPane
            split="vertical"
            defaultSize={document.body.offsetWidth / 2}
            minSize={0.2 * document.body.offsetWidth}
          >
            <div style={{...paneStyle, ...pane1Style}}>
              <Tabs>
                <TabList>
                  <Tab>XML</Tab>
                  <Tab>Template</Tab>
                  <Tab>Split</Tab>
                  <Tab>Workspace Data</Tab>
                </TabList>
                <TabPanel>
                  <SourceEditor
                    mode="xml"
                    value={this.state.text}
                    onChange={e => {
                      console.log('change XML');
                      this.setState({text: e});
                    }}
                  />
                </TabPanel>
                <TabPanel>
                  <SourceEditor
                    mode="yaml"
                    value={this.state.yamlTemplate}
                    onChange={e => this.setState({yamlTemplate: e})}
                  />
                </TabPanel>
                <TabPanel>
                  <SplitPane split="horizontal">
                    <div style={{...paneStyle, ...pane1Style}}>
                      <SourceEditor
                        mode="xml"
                        autoSave
                        value={this.state.text}
                        onChange={e => this.setState({text: e})}
                      />
                    </div>
                    <div style={{...paneStyle, ...pane2Style}}>
                      <SourceEditor
                        mode="yaml"
                        autoSave
                        value={this.state.yamlTemplate}
                        onChange={e => this.setState({yamlTemplate: e})}
                      />
                    </div>
                  </SplitPane>
                </TabPanel>
                <TabPanel>
                  <div className="container-fluid">
                    <form
                      onSubmit={e => {
                        e.preventDefault();
                        this.save();
                      }}
                    >
                      <div className="form-group">
                        <label htmlFor="title">Workspace Title</label>
                        <input
                          name="title"
                          type="text"
                          className="form-control"
                          value={this.state.title}
                          onChange={e => this.setState({title: e.target.value})}
                          placeholder="Name this workspace"
                        />
                      </div>
                      <div className="form-group">
                        <label htmlFor="isPublic">
                          <input
                            id="isPublic"
                            name="isPublic"
                            type="checkbox"
                            value={this.state.isPublic}
                            onChange={e =>
                              this.setState({isPublic: e.target.checked})}
                          />
                          {' '}
                          Make this a publically accessible and editable workspace
                        </label>
                      </div>
                      <div className="form-group">
                        <button
                          className="btn btn-default"
                          disabled={this.state.isLoading || this.state.isSaving}
                        >
                          Save
                        </button>
                      </div>
                    </form>
                  </div>
                </TabPanel>
              </Tabs>
            </div>

            <div style={{...paneStyle, ...pane2Style}}>
              <Tabs>
                <TabList>
                  <Tab>LaTeX</Tab>
                  <Tab>PDF</Tab>
                  <Tab>Split</Tab>
                </TabList>
                <TabPanel>
                  <SourceEditor
                    mode="stex"
                    key={this.state.conversionResult}
                    autoSave
                    value={this.state.conversionResult}
                  />
                </TabPanel>
                <TabPanel>
                  <div style={{height: '100%'}}>
                    <PreviewPdf value={this.state.conversionResult} />
                  </div>
                </TabPanel>
                <TabPanel>
                  <SplitPane
                    split="horizontal"
                    defaultSize={document.body.offsetHeight * 0.45}
                    minSize={0.2}
                  >
                    <div style={{...paneStyle, ...pane1Style}}>
                      <SourceEditor
                        mode="stex"
                        key={this.state.conversionResult}
                        autoSave
                        value={this.state.conversionResult}
                      />
                    </div>
                    <div style={{...paneStyle, ...pane2Style}}>
                      <PreviewPdf value={this.state.conversionResult} />
                    </div>
                  </SplitPane>
                </TabPanel>
              </Tabs>
            </div>
          </SplitPane>
        </div>
        <hr />

        <button
          style={{
            position: 'absolute',
            top: 60,
            borderRadius: 0,
            right: 0,
            zIndex: 10,
          }}
          className="btn btn-primary"
          onClick={this.runConvert}
          disabled={this.state.isLoading || this.state.isSaving}
        >
          Run jats2tex
        </button>

        {this.renderBottomMessage}
      </div>
    );
  }
}

export default class App extends Component {
  render() {
    return (
      <div>
        <BrowserRouter>
          <Route path="/workspaces/:id" component={Workspace} />
        </BrowserRouter>
      </div>
    );
  }
}
