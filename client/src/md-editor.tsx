import { useState, useCallback } from 'react'
import styled from 'styled-components'

import ReactMarkdown from 'react-markdown'
import remarkGfm from 'remark-gfm'

const Root = styled.div`
  border: 1px solid red;
  background-color: white;
  height: 100%;
  width: 100%;
  display: flex;
  flex-flow: column nowrap;
`
const Editor = styled.div`
  flex-grow: 1;

  .task-list-item {
    display: flex;
    flex-flow: row wrap;
    align-items: center;
    input {
      display: inline;
      width: 20px;
      height: 20px;
      margin-right: 10px;
    }
  }
`
const Textarea = styled.textarea`
  box-sizing: border-box;
  width: 100%;
  height: 200px;
`

export default () => {
    const [markdown, setMarkdown] = useState('');
    const [mode, setMode] = useState('preview');

    const toggleMode = useCallback(() => {
        if (mode === 'preview') {
            setMode('write');
        } else {
            setMode('preview');
        }
    });

    return (
      <Root>
        <Editor>
          <Textarea
            value={markdown}
            onChange={e => setMarkdown(e.target.value)}
          />
          <ReactMarkdown remarkPlugins={[remarkGfm]}>{markdown}</ReactMarkdown>
        </Editor>
        <button onClick={toggleMode}>{mode === 'preview' ? "Edit" : "Preview"}</button>
      </Root>
    );
}

